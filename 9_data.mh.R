#####################################
### Mosquito-to-host transmission ###
#####################################

## Mosquito to host transmission, allowing each host to plateau at a different plateau.

## Fit mosquito as a random effect, for each mosquito to have the same incubation rate but a different peak
 ## Yes, some singularity problems but that is hardly unexpected given the tiny number of data points we have. 
  ## Extracting the conditional covariances for uncertainty on the random effects seems to return reasonable uncertainty
   ## (see below) so proceeding with this model. Essentially no other model is going to be more robust anyway given these data...
m_to_h_trans.mod <- glmer(
    cbind(num_transmitting, total_transmit_tested - num_transmitting) ~ day + 
    (1 + day | mos_species) +
    (1 | ref)
  , data    = m_to_h_emp
  , family  = "binomial")

## Container for the predictions for the actual titers we care about (those seen in hosts)
m_to_h_trans.preddat <- expand.grid(
    day          = seq(1, 30)
  , mos_species  = unique(m_to_h_emp$mos_species))

## Fill in without uncertainty 
m_to_h_trans.pred <- m_to_h_trans.preddat %>%
  mutate(prob = plogis(predict(m_to_h_trans.mod
  , newdata = m_to_h_trans.preddat
  , re.form = ~(1 + day | mos_species))))

### Use the conditional modes and conditional covariances and bootstrap to get uncertainty on individual species responses 
source("10_cond_cov.R")
rand_eff_est         <- getME(m_to_h_trans.mod, c("b"))@x[1:(num_spec*2)]
#cond_cov_mat         <- lme4:::condVar(m_to_h_trans.mod)
#condvar_branch_array <- array(data = 0, dim = c(2, 2, length(unique(m_to_h_emp$mos_species))))

#for (i in 1:dim(condvar_branch_array)[3]) {
  ## Based on the structure of the model, extracting the intercept and slope and setting into the right structure
   ## This just counts 1,2 for i = 1, 3, 4 for i = 2 etc. 
#  condvar_branch_array[,,i] <- as.matrix(cond_cov_mat[(i*2 - 1):(i*2), (i*2 - 1):(i*2)])
#}

## Samples for each species
ran_ef_array <- array(dim = c(n_samps, 2, num_spec))

## Draws from the covar matrix
for (i in 1:dim(ran_ef_array)[3]) {
  ran_ef_array[,,i] <- mvrnorm(n_samps, rand_eff_est[(i*2 - 1):(i*2)], condvar_branch_array[,,i])
}

dimnames(ran_ef_array)[[3]] <- unique(m_to_h_emp$mos_species)

## Fixed effects
fix_eff_matrix <- mvrnorm(n_samps
  , mu    = getME(m_to_h_trans.mod, c("beta"))
  , Sigma = summary(m_to_h_trans.mod)[["vcov"]])

## estimates for each species
pred_vals <- expand.grid(
  day         = seq(1, mosq_days_max)
, mos_species = unique(m_to_h_emp$mos_species)
, sim         = seq(1, n_samps, by = 1)
, prob        = 0
)

pred_vals$mos_species      <- as.character(pred_vals$mos_species)

## Need estimates for all species, so in a classic mixed-effects model move assume that all species without data are at 
 ## the grand mean and all species with data are at their own estimates (which are influenced by the grand mean but also
  ## their own data)
m_to_h_trans_all_samps <- array(
  dim = c(
    mosq_days_max
  , length(unique(h_to_m_emp$species)) # length(unique(m_to_h_emp$mos_species))
  , n_samps
    )
  , data = 0)

dimnames(m_to_h_trans_all_samps)[[2]] <- as.character(unique(h_to_m_emp$species))
 
## Estimate mosquito to host transmission probability (for all species with host to mosquito transmission ability)
 ## Using estimates for species with data from their model, and the grand mean for species with no data
for (i in 1:length(unique(h_to_m_emp$species))) {
  for (j in 1:n_samps) {
    
## Here use  titer "samples" with a transmission "samples" to understand the joint uncertainty over titer and transmission | titer
 ## That is, what is the probability that any given mosquito picks up infection from any given host given the uncertainty we 
  ## have in that mosquitoes ability and hosts' titer
    
## check if this is a species in the fitted mosquito to host model 
check_for_species <- length(grep(unique(h_to_m_emp$species)[i], dimnames(ran_ef_array)[[3]]))
    
if (check_for_species > 0) {
m_to_h_trans_all_samps[ , i, j] <- plogis(
      ## intercept
    fix_eff_matrix[j, 1] + ran_ef_array[j, 1, unique(h_to_m_emp$species)[i]] +
      ## slope over day * day
    (fix_eff_matrix[j, 2] + ran_ef_array[j, 2, unique(h_to_m_emp$species)[i]]) * seq(1, mosq_days_max, by = 1)
    )

} else {
  
m_to_h_trans_all_samps[ , i, j] <- plogis(
      ## intercept
    fix_eff_matrix[j, 1] +
      ## slope over day * day
    (fix_eff_matrix[j, 2]) * seq(1, mosq_days_max, by = 1)
    )  

}
    
  }

}

## Also "brute force" calculate AUC for mosquito infection probability
mosq_trans_AUC_all_samps                <- matrix(nrow = length(unique(h_to_m_emp$species)), ncol = n_samps, data = 0)
dimnames(mosq_trans_AUC_all_samps)[[1]] <- unique(h_to_m_emp$species)
max_days_AUC <- 25

for (i in 1:length(unique(h_to_m_emp$species))) {
  for (j in 1:n_samps) {

check_for_species <- length(grep(unique(h_to_m_emp$species)[i], dimnames(ran_ef_array)[[3]]))
    
if (check_for_species > 0) {
mosq_trans_AUC_all_samps[i, j] <- sum(plogis(
      ## intercept
    fix_eff_matrix[j, 1] + ran_ef_array[j, 1, unique(h_to_m_emp$species)[i]] +
      ## slope over day * day
    (fix_eff_matrix[j, 2] + ran_ef_array[j, 2, unique(h_to_m_emp$species)[i]]) * seq(1, max_days_AUC, by = 0.001)
    )) * 0.001

} else {
  
mosq_trans_AUC_all_samps[i, j] <- sum(plogis(
      ## intercept
    fix_eff_matrix[j, 1] +
      ## slope over day * day
    (fix_eff_matrix[j, 2]) * seq(1, max_days_AUC, by = 0.001)
    )) * 0.001

}
    
  }

}

#################################################################
### Supplemental host to mosquito transmission plot
#####

## mosquito transmission probability

m_to_h_trans_all_samps.gg        <- melt(m_to_h_trans_all_samps)
names(m_to_h_trans_all_samps.gg) <- c("day", "mos_species", "samp", "prob")

m_to_h_trans_all_samps.gg.s      <- m_to_h_trans_all_samps.gg %>%
  group_by(mos_species, day) %>% 
  summarize(
    lwr = quantile(prob, 0.100)
  , est = quantile(prob, 0.500)
  , upr = quantile(prob, 0.900)
  )

mosq_names <- strsplit(as.character(m_to_h_trans_all_samps.gg.s$mos_species), "_") %>% 
  sapply(., FUN = function(x) c(paste(x, collapse = " "))) %>% 
  sapply(., FUN = function(x) paste(toupper(substring(x, 1, 1)), substring(x, 2), sep = "", collapse = " "))
m_to_h_trans_all_samps.gg.s$mos_species <- mosq_names

m_to_h_emp.gg <- m_to_h_emp

mosq_names <- strsplit(as.character(m_to_h_emp.gg$mos_species), "_") %>% 
  sapply(., FUN = function(x) c(paste(x, collapse = " "))) %>% 
  sapply(., FUN = function(x) paste(toupper(substring(x, 1, 1)), substring(x, 2), sep = "", collapse = " "))
m_to_h_emp.gg$mos_species <- mosq_names

m_to_h_with_data <- m_to_h_emp

ggplot(
  (m_to_h_trans_all_samps.gg.s %>% filter(day < 35, (mos_species %in% m_to_h_emp.gg$mos_species) | mos_species == "Ve lineata"))
  , aes(day, est)) + 
  geom_line() +
  geom_ribbon(aes(x = day, ymin = lwr, ymax = upr), alpha = 0.25) +
  geom_point(data = (m_to_h_emp.gg %>% filter(mos_species != "Ae multiplex"))
    , aes(day, num_transmitting / total_transmit_tested, colour = dose), lwd = 3) +
  geom_line(
   data = m_to_h_emp.gg, aes(day, num_transmitting / total_transmit_tested, group = exp),
    lwd = 0.5, linetype = "dotted"
  ) +
  scale_color_gradient(low = "orangered1", high = "blue3", name = bquote('Dose ('*log[10]*')')) +
  facet_wrap(~mos_species) +
  xlab("Day") + 
  ylab("Transmission Probability") +
  theme(
    legend.key.size = unit(.75, "cm")
  , legend.title = element_text(size = 18)
  , legend.text = element_text(size = 16))

## Single panel for conceptual figure
mos.cf <- "Ae procax"
ggplot(
  (m_to_h_trans_all_samps.gg.s %>% filter(day < 35, mos_species == mos.cf))
  , aes(day, est)) + 
  geom_line() +
  xlim(c(0, 26)) +
  geom_ribbon(aes(x = day, ymin = lwr, ymax = upr), alpha = 0.25) +
  geom_point(data = (m_to_h_emp.gg %>% filter(mos_species == mos.cf))
    , aes(day, num_transmitting / total_transmit_tested)) +
  xlab("Day") + 
  ylab("Transmission 
Probability")

## mosquito transmission probability AUC
mosq_trans_AUC.gg <- melt(mosq_trans_AUC_all_samps)
names(mosq_trans_AUC.gg) <- c("mosq", "samp", "AUC")
mosq_names <- strsplit(as.character(mosq_trans_AUC.gg$mosq), "_") %>% 
  sapply(., FUN = function(x) c(paste(x, collapse = " "))) %>% 
  sapply(., FUN = function(x) paste(toupper(substring(x, 1, 1)), substring(x, 2), sep = "", collapse = " "))
mosq_trans_AUC.gg$mosq <- mosq_names
mosq_trans_AUC.gg.s    <- mosq_trans_AUC.gg %>% group_by(mosq) %>%
  summarize(
    lwr = quantile(AUC, 0.025)
  , est = quantile(AUC, 0.500)
  , upr = quantile(AUC, 0.975)
  )  

mosq_trans_AUC.gg.s <- mosq_trans_AUC.gg.s[order(mosq_trans_AUC.gg.s$est), ]
mosq_trans_AUC.gg.s$mosq <- factor(mosq_trans_AUC.gg.s$mosq, levels = mosq_trans_AUC.gg.s$mosq)

gg.mosq_trans.AUC <- ggplot(
  (mosq_trans_AUC.gg.s %>% filter((mosq %in% m_to_h_emp.gg$mos_species) | mosq == "Ve lineata"))
  , aes(est, mosq)) + 
  geom_point(lwd = 2) +
  geom_errorbarh(aes(xmin = lwr, xmax = upr, y = mosq), height = 0.5) +
  ylab("Mosquito") +
  xlab("Area Under Transmission Probability Curve") +
  theme(
    axis.text.y = element_text(size = 16)
  , axis.title.x = element_text(size = 19)
  , axis.title.y = element_text(size = 19)
  )
