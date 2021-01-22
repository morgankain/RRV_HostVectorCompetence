#######
### Fit titer curves to empirical host infection profiles
#######

### Assumptions
 ## 1) Detectability is 2.2 log10 titer units (the limit found for RRV in Vero cells, see McLeanetal.2021 Vector-Borne and Zoonotic Diseases); 
    ## see comment in loop below where this has the potential to have an impact on the simulation -- above "while (any(peak_vals < 2.2))")
 ## 2) Infectious profile is quadratic
 ## 3) Only peak and duration available for many organisms. To turn this into infection curves we:
  ## A) n_samps times, simulate curves from the number of infected individuals from the study based on the sd on duration and sd on peak reported
  ## B) For each set of curves fit a quadratic linear model and predict the mean
  ## C) Produce CI on the n_samps predicted curves
    ## We note that the CI produced here are rather approximate of what our true uncertainty on titer profiles should be

### Notes/Possible Issues with this method:
 ## 1) Peak titer predicted well for species with a long duration, but for species that spike and decay quickly, the quadratic
  ## smooths a bit, under-predicting peak, but leading to a bit longer of a duration. E.g. Bushtail possum peak a bit under-predicted, but
  ## the peak ends up lasting a bit longer than suggested by the empirical data.
   ## -- These could basically offset each other, but because these quadratic curves get translated through the non-linear host-mosquito infection
    ## probability, the full implications of this modeling choice isn't immediately obvious
   ## -- It is possible simulating with a positive correlation between titer peak and duration could solve some of this, but it isn't obvious, nor is 
    ## it actually clear that there should be a positive correlation or what a realistic value would be

## First, manually change the black duck sd in peak titre to something realistic. The data from the paper gives the peak as 1.8 (which is beneath
 ## the detection limit), and an sd of 2.1, which really makes no sense. Simply drop it to a small value to produce a much more realistic SD value
titer_emp[titer_emp$host == "black_duck", ]$sd.titre <- 0.5

## Second, store the number viraemic before grouping by hosts and summarizing
## (probability that a host becomes infected given that it was "bit" -- injected in experiments)
 ## This will get added into the H -> M arm of the life cycle to weight the proportion of hosts that will lead to the
  ## estimated mosquito infection probability 
   ## That is, some proportion of hosts never actually generate a viremia, hence don't actaully get infected
titer_emp.prop <- titer_emp %>% group_by(host) %>%
  summarize(
    total.tested = sum(total.tested)
  , num.viraemic = sum(num.viraemic)
  ) %>% mutate(num_inf = num.viraemic / total.tested) 
names(titer_emp.prop) <- c("host", "tot_inf", "tot_vir", "num_inf")

## Select only those entries where at least one host was viraemic (some species were infected but none of the individuals had detectable titer)
titer_emp <- titer_emp %>% filter(num.viraemic > 0)
uni_spec  <- unique(titer_emp$host)

## Extract the sd in duration of infection and peak titer
dur_sd_mean  <- titer_emp %>% filter(sd.duration != 0) %>% dplyr::select(sd.duration, num.viraemic) %>% 
  summarize(dur_sd_mean = weighted.mean(sd.duration, num.viraemic)) %>% unlist()
peak_sd_mean <- titer_emp %>% filter(sd.titre != 0) %>% dplyr::select(sd.titre, num.viraemic) %>% 
  summarize(peak_sd_mean = weighted.mean(sd.titre, num.viraemic)) %>% unlist()

## Old paper with hard to understand summaries. Some standard deviations listed as zero despite precision to 0.X and moderate 
 ## sample sizes, which seems impossible. To be extra conservative, fill in all zero standard deviation estimates with the average
titer_emp[titer_emp$sd.titre == 0, ]$sd.titre       <- peak_sd_mean
titer_emp[titer_emp$sd.duration == 0, ]$sd.duration <- dur_sd_mean

## Most hosts' titer profiles were only measured once, but some were measured at multiple doses. Could deal with this
 ## possibly deal with this in some kind of statistical model, but with so few hosts having multiple doses, taking the 
  ## easiest route here and taking the weighted mean (based on sample size)
titer_emp <- titer_emp %>% group_by(host) %>%
  summarise(
      infected.dose = weighted.mean(infected.dose, num.viraemic)
    , total.tested  = sum(total.tested)
    , peak.titre    = weighted.mean(peak.titre, num.viraemic)
    , sd.titre      = weighted.mean(sd.titre, num.viraemic)
    , titre.duration.days = weighted.mean(titre.duration.days, num.viraemic)
    , sd.duration   = weighted.mean(sd.duration, num.viraemic)
    , num.viraemic  = sum(num.viraemic))

host_titer_all_samps           <- array(dim = c(length(inf_days), n_samps, length(uni_spec) + 1), data = 0)
host_titer_AUC_all_samps       <- matrix(ncol = n_samps, nrow = length(uni_spec) + 1, data = 0)
dimnames(host_titer_all_samps) <- list(NULL, NULL, c(uni_spec, "human"))

####
## Loop over all unique non-human species
####
for (i in seq_along(uni_spec)) {

## Extract a given species
temp_dat <- titer_emp %>% filter(host == uni_spec[i])

temp_samps <- matrix(nrow = length(inf_days), ncol = n_samps, data = 0)

## Loop over the number of titer curves to draw for uncertainty
for (j in 1:n_samps) {
  
## Simulate peak values and values in-between the beginning and end of detectable viremia based on the data sd
 ## Assume quadratic with a peak in the center
peak_vals <- -1
## How sd can be so large for species with a mean right at the detection limit is very confusing. Manually create a truncated
 ## normal by rejecting values less than 0
## Could also consider sampling these with some correlation between them (i.e. individuals with higher peaks have longer
 ## duration, but it would be an arbitrary choice and could honestly go either way (positive or negative), so leave at 0 for now)

## The one place where we really need an assumption to constrain the simulation to realistic peak values. Because of the age of these studies (from the early 70s), detailed 
 ## information about the methods and data are very limited. For example, for black ducks the peak titre is 1.8, but sd in this value is 2.1 which seems impossible given that
  ## the detectability threshold is somewhere around 1.7 (though it may vary a bit by study). Need this constraint to make sure peak titre is retuned as an actually detectable
   ## value, though we do note that this could bias upwards peaks of some organisms with very low peaks and large sd 
while (any(peak_vals < thresh_titer)) {
peak_vals <- with(temp_dat, rnorm(num.viraemic, peak.titre, sd.titre))
  }
dur_vals  <- with(temp_dat, rnorm(num.viraemic, titre.duration.days, sd.duration))

if (length(peak_vals) != 0) {
  resp_temp <- data.frame(
    host  = temp_dat$host
  , indiv = rep(seq(1, temp_dat$num.viraemic), 3)
  ## What is assumed here is that the organism goes from titer at threshold on day 1 to its peak then returns to its threshold.
    ## In this way, this models titer above the detection limit for the duration of titer reported in the data
  , day   = c(
      rep(1, length(dur_vals))
    , rep(1, length(dur_vals)) + dur_vals/2
    , rep(1, length(dur_vals)) + dur_vals)
  , titer = c(
  ## any values beneath detection limit just place to zero. While this is a simplification (titre could be any where between 0 and the detection limit),
     ## because detection limit may vary a bit by study/technique and because we are on a log scale, 0 vs a low detection limit number will have a relatively small effect
    rep(thresh_titer, length(peak_vals))
  ## then peak values in the middle
  , peak_vals
  ## followed by titer after it comes down from the peak
  , rep(thresh_titer, length(peak_vals)))
  )
  ## No individuals tested produced any viremia
}

## Would probably prefer a random effect method here, but would need to simulate more data inbetween the peak and the 
 ## first and last day to get a non-singular fit, so instead just fit a different model for each individual and average them.
  ## Not a perfect solution but re-creates a reasonable peak and duration for each species

## Basically, the idea here is to "draw" the titer curve for each individual, and then take the mean of these curves as the 
 ## average response of this species. Do this X times as a pseudo-bootstrap like procedure and we end with CI on that mean response
temp_titer_meas <- matrix(data = 0, nrow = temp_dat$num.viraemic, ncol = length(inf_days))
AUC_temp        <- numeric(length(unique(resp_temp$indiv)))
for (k in 1:length(unique(resp_temp$indiv))) {
host_titer.mod  <- lm(
    log(titer) ~ poly(day, 2) 
  , data = (resp_temp %>% filter(indiv == k))
  )
temp_titer_meas[k, ] <- exp(predict(host_titer.mod, newdata = data.frame(day = seq(1, length(inf_days), by = 1))))

temp_coef <- coef(host_titer.mod)

## log( ) in the x in the lm complicates the integral, just compute a fine-scale enough "brute force" integral to get the AUC
AUC_temp[k] <- log10(sum(10^exp(predict(host_titer.mod, newdata = data.frame(day = seq(1, length(inf_days), by = .001))))) * 0.001)

}

## Export the predictions from the simulated data. Done in this way, this curve is one possible _average_ reality for
 ## the shape of the titer curves.
  ## Repeated over n_samps random samples generates a range of hypothetical titer curves from the few reported data points from
   ## which the CI are drawn
temp_samps[, j]                <- colMeans(temp_titer_meas)
host_titer_AUC_all_samps[i, j] <- mean(AUC_temp)

}

## Store results for organism i
host_titer_all_samps[ , , uni_spec[i]] <- temp_samps

## Take quantifies for plots and also combine results for further steps
if (i == 1) {
temp_titer <- data.frame(
  day  = seq(1, length(inf_days), by = 1)
, host = rep(temp_dat$host[1], length(inf_days))
, lwr  = apply(temp_samps, 1, function (x) quantile(x, c(0.025)))
, est  = apply(temp_samps, 1, function (x) quantile(x, c(0.50)))
, upr  = apply(temp_samps, 1, function (x) quantile(x, c(0.975)))
)
tot_titer <- temp_titer
} else {
temp_titer <- data.frame(
  day  = seq(1, length(inf_days), by = 1)
, host = rep(temp_dat$host[1], length(inf_days))
, lwr  = apply(temp_samps, 1, function (x) quantile(x, c(0.025)))
, est  = apply(temp_samps, 1, function (x) quantile(x, c(0.50)))
, upr  = apply(temp_samps, 1, function (x) quantile(x, c(0.975)))
)  
tot_titer <- rbind(tot_titer, temp_titer)
}

## keep track of progress because this takes a bit 
print(i)

}

####
## Add in human response. !!!**!!! Note: There are many options for modeling human titer given the relatively poor data (see notes below).
####

## The only data we have for human response is an old observational study that captures only the second half of the human titer response
 ## (after symptoms had started). Anecdotal evidence from the lit suggests that human titer will climb and then decline and that
  ## symptoms start many days after titer rises to measurable levels.
## Therefore, we assume that we have captured the peak of human titer (looking at the data this is definitely conceivable).

## To model human titer we:
 ## Construct a quadratic to match the other hosts by treating the first day of tite as the same number of days away from the peak
  ## as the last day of titer, and add a single data point with titer = 0

## ** Could also imagine:
  ## reflecting all the data: but this may result in too narrow of CI
  ## Using an asymmetric function like a Ricker instead of a quadratic 
  ## removing the last day of human infection data because the measured titer is quite high

## Proportion of infected displaying viremia. Needed for later. Do this before the data duplication / titer reflection about the peak
human_titer_vir <- data.frame(host = "human"
  , tot_inf = sum(human_titer_emp$num_tested)
  , tot_vir = sum(human_titer_emp$num_pos)) %>%
  mutate(num_inf = tot_vir / tot_inf)

## Repeat the number of humans for the sample size to complete a similar model as for the animal titer data
human_titer_emp <- human_titer_emp[rep(row.names(human_titer_emp), human_titer_emp$num_pos), ]

## *Inject uncertainty* based on the variation in other host's titer
human_titer_emp <- human_titer_emp %>% mutate(sd = peak_sd_mean)

## Only model human data from days 4-16, chopping off the tails to _conservatively_ estimate a human's titer profile (to help the possible
 ## over-estimate of a quadratic and "control for" the lower titer detection late in infection)
temp_samps <- matrix(nrow = length(seq(-5, 9, by = 1)), ncol = n_samps, data = 0)
human_AUC  <- numeric(n_samps)

## Similar modeling strategy as before
for (j in 1:n_samps) {

temp_titer <- human_titer_emp

temp_titer <- rbind(
  data.frame(host = "human", num_pos = 1, num_tested = 1, titre = 0.01
    ## Each time add a single infected at a radom day that they may have started infection
    , day = sample(seq(-11, -1), 1)
    ## Assume no variation at the start and end (need non-0 sd for rgamma so make it really small)
    , titre_unit = "LD50", ref = "rosen 1981", sd = 0.001) 
, human_titer_emp
)

temp_titer <- temp_titer %>% mutate(titer = rgamma(nrow(temp_titer)
  , shape = (titre / sd)^2
  , scale  = (sd)^2 / titre))
ggplot(temp_titer, aes(day, titer)) + geom_point()

host_titer.mod  <- lm(
  log(titer) ~ poly(day, 2) 
, data = temp_titer
)

temp_samps[, j] <- exp(predict(host_titer.mod, newdata = data.frame(day = seq(-5, 9, by = 1))))

## Same AUC procedure for humans, but just use the titer window that is actually used in the predictions (5:13)
if (red_hum_titer) {
human_AUC[j]    <- log10(sum(10^exp(predict(host_titer.mod, newdata = data.frame(day = seq(2, 7, by = .001))))) * .001)
} else {
human_AUC[j]    <- log10(sum(10^exp(predict(host_titer.mod, newdata = data.frame(day = seq(-1, 7, by = .001))))) * .001)
}

}

hum_titer <- data.frame(
  day = seq(-5, 9, by = 1)
, host = rep("human", length(seq(-5, 9, by = 1)))
, lwr = apply(temp_samps, 1, function (x) quantile(x, c(0.025)))
, est = apply(temp_samps, 1, function (x) quantile(x, c(0.50)))
, upr = apply(temp_samps, 1, function (x) quantile(x, c(0.975))))

## Not the best of fits day by day, but we really don't care about that actually, just care about peak
 ## and duration (not the exact match of the peak _location_), which seems reasonable here
hum_titer.gg <- ggplot(hum_titer) + geom_ribbon(aes(x = day, ymin = lwr, ymax = upr), alpha = 0.2) + 
  geom_point(data = human_titer_emp, aes(day, titre)) + 
  geom_errorbar(data = human_titer_emp, aes(x = day, ymin = titre + sd, ymax = titre - sd))

## reduce titer for the supplement
if (red_hum_titer) {
 temp_samps[5:8, ] <- 0
}

## Fill in the last array entry with the human response. 
host_titer_all_samps[ , , "human"] <- temp_samps[5:13, ]

## Titer from non-human and human hosts
tot_titer <- rbind(tot_titer, (hum_titer %>% filter(day > -2, day < 8)))

## Place into an array for the full model
host_titer      <- matrix(
   ncol = dim(host_titer_all_samps)[3]
 , nrow = dim(host_titer_all_samps)[1]
 , data = c(
     tot_titer$est
   )
  )

host_titer_AUC_all_samps[nrow(host_titer_AUC_all_samps), ] <- human_AUC

## Name the entries of the array
dimnames(host_titer)[[2]]               <- unique(tot_titer$host)
dimnames(host_titer_AUC_all_samps)[[1]] <- unique(tot_titer$host)

## Add human titer response to proportion infected
prop_inf <- rbind(titer_emp.prop, human_titer_vir)

#################################################################
### Supplemental Titer plots
#####

## Raw titer curves

tot_titer.gg <- tot_titer
host_names   <- strsplit(as.character(tot_titer.gg$host), "_") %>% 
  sapply(., FUN = function(x) c(paste(x, collapse = " "))) %>% 
  sapply(., FUN = function(x) paste(toupper(substring(x, 1, 1)), substring(x, 2), sep = "", collapse = " "))
tot_titer.gg$Host <- host_names

## Change a few names for the supplemental plot
tot_titer.gg <- tot_titer.gg %>% mutate(
  Host = mapvalues(Host, from = "Grey h f fox", to = "Grey flying fox")
)

human_titer_emp.gg <- human_titer_emp %>% rename(Host = host, est = titre) %>%
  mutate(
    Host = mapvalues(Host, from = "human", to = "Human")
  , day = day + 3)

host_names   <- strsplit(as.character(titer_emp$host), "_") %>% 
  sapply(., FUN = function(x) c(paste(x, collapse = " "))) %>% 
  sapply(., FUN = function(x) paste(toupper(substring(x, 1, 1)), substring(x, 2), sep = "", collapse = " "))

titer_emp.gg      <- titer_emp
titer_emp.gg$Host <- host_names

titer_emp.gg <- titer_emp.gg %>% 
  mutate(
  Host = mapvalues(Host, from = "Grey h f fox", to = "Grey flying fox")
      )
titer_emp.gg <- titer_emp.gg %>% mutate(day = 1)

## Export at 18x12
gg.emp.titer <- ggplot(tot_titer.gg, aes(day, est)) + 
  geom_line() + 
  geom_point(data = human_titer_emp.gg
    , aes(day, est)) + 
  geom_errorbar(data = human_titer_emp.gg
    , aes(x = day, ymin = est + sd, ymax = est - sd)) +
  geom_ribbon(aes(day, ymin = lwr, ymax = upr), alpha = 0.2) +
  ylab(bquote('Titer ('*log[10]*')')) +
  xlab("Day") +
## peak titre
  geom_hline(data = titer_emp.gg, aes(yintercept = peak.titre), linetype = "dashed", lwd = 0.75, colour = "dodgerblue4") +
  geom_hline(data = titer_emp.gg, aes(yintercept = peak.titre - sd.titre), linetype = "dotted", lwd = 0.5, colour = "dodgerblue4") +
  geom_hline(data = titer_emp.gg, aes(yintercept = peak.titre + sd.titre), linetype = "dotted", lwd = 0.5, colour = "dodgerblue4") +
## titre duration
  geom_vline(data = titer_emp.gg, aes(xintercept = 1 + titre.duration.days)
    , linetype = "dashed", lwd = 0.75, colour = "deeppink4") +
  geom_vline(data = titer_emp.gg, aes(xintercept = 1 + titre.duration.days - sd.duration)
    , linetype = "dotted", lwd = 0.5, colour = "deeppink4") +
  geom_vline(data = titer_emp.gg, aes(xintercept = 1 + titre.duration.days + sd.duration)
    , linetype = "dotted", lwd = 0.5, colour = "deeppink4") +
  geom_hline(yintercept = thresh_titer, lwd = 0.25) +
  theme(
    axis.text.x = element_text(size = 16)
  , axis.text.y = element_text(size = 16)
  , axis.title.x = element_text(size = 19)
  , axis.title.y = element_text(size = 19)
  ) + facet_wrap(~Host)

## plot for conceptual figure
gg.emp.titer.r <- ggplot(
  (tot_titer.gg %>% filter(Host == "Grey kangaroo"))
  , aes(day, est)) + 
  geom_line() + 
  geom_ribbon(aes(day, ymin = lwr, ymax = upr), alpha = 0.2) +
  ylab(bquote('Titer ('*log[10]*')')) +
  xlab("Day") +
## peak titre
  theme(
    axis.text.x = element_text(size = 16)
  , axis.text.y = element_text(size = 16)
  , axis.title.x = element_text(size = 19)
  , axis.title.y = element_text(size = 19)
  ) 
  