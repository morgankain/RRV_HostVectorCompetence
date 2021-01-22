#######
### Host-to-mosquito transmission
#######

max_dose_val <- 9.5

## First, drop Aedes multiplex because of tbe odd data and opposite predictions (Aedes multiplex isn't found in
 ## Brisbane anyway)
h_to_m_emp <- h_to_m_emp %>% filter(species != "ae_multiplex")

## Convert to proportion and sample size
h_to_m_emp <- h_to_m_emp %>% 
  mutate(prop.infected = perc.infected / 100) %>%
  mutate(num_inf = round(prop.infected * total.infect.exp))

## Fit a binomial model to the proportion infected of those bit
h_to_m_trans.mod  <- glmer(
    cbind(num_inf, total.infect.exp - num_inf) ~ infectious.dose 
  + (1 + infectious.dose | species) 
  , data    = h_to_m_emp
  , family  = "binomial")

## Empty data frame for predictions from the GLMM over infectious dose
h_to_m_trans.preddat <- expand.grid(
    infectious.dose = seq(0, max_dose_val, by = 0.2)
  , species         = unique(h_to_m_emp$species)
  )

## Estimates without uncertainty, just for the ggplot
h_to_m_trans.pred <- h_to_m_trans.preddat %>%
  mutate(prob = plogis(predict(h_to_m_trans.mod
  , newdata = h_to_m_trans.preddat
  , re.form = ~(1 + infectious.dose | species)))
    )

gg.emp.h_to_m <- ggplot(h_to_m_emp, aes(infectious.dose, perc.infected/100)) + 
  geom_point(aes(colour = species), lwd = 2.5) +
  geom_line(data = h_to_m_trans.pred, aes(infectious.dose, prob, colour = species)) +
  xlab("Titer") +
  ylab("Percentage infected")

## Check by genus
gg.emp.h_to_m.p <- ggplot(h_to_m_emp, aes(infectious.dose, perc.infected/100)) + 
  geom_point(aes(colour = species, shape = genus, size = total.infect.exp)) +
  geom_line(data = h_to_m_trans.pred, aes(infectious.dose, prob, colour = species)) +
  xlab("Titer") +
  ylab("Percentage infected") 

### The actual data needed for R0, with and without uncertainty
samp_inf <- h_to_m_emp

## Container for the predictions for the actual titers we care about (those seen in hosts)
h_to_m_trans.preddat <- expand.grid(
    infectious.dose =  c(host_titer)
  , species         =  unique(h_to_m_emp$species))

## Fill in without uncertainty 
h_to_m_trans.pred <- h_to_m_trans.preddat %>%
  mutate(prob = plogis(predict(h_to_m_trans.mod
  , newdata = h_to_m_trans.preddat
  , re.form = ~(1 + infectious.dose | species)))
    )

### Use the conditional modes and conditional covariances and bootstrap to get uncertainty on individual species responses 
rand_eff_est         <- getME(h_to_m_trans.mod, c("b"))@x#[(length(unique(h_to_m_emp$ref)) + 1):(length(unique(h_to_m_emp$ref)) + length(unique(h_to_m_emp$species))*2)]
cond_cov_mat         <- lme4:::condVar(h_to_m_trans.mod)
condvar_branch_array <- array(data = 0, dim = c(2, 2, length(unique(h_to_m_emp$species))))

for (i in 1:dim(condvar_branch_array)[3]) {
  ## Based on the structure of the model, extracting the intercept and slope and setting into the right structure
   ## This just counts 1,2 for i = 1, 3, 4 for i = 2 etc. 
  condvar_branch_array[,,i] <- as.matrix(cond_cov_mat[(i*2 - 1):(i*2), (i*2 - 1):(i*2)])
}

## Samples for each species
ran_ef_array <- array(dim = c(n_samps, 2, length(unique(h_to_m_emp$species))))

## Draws from the covar matrix
for (i in 1:dim(ran_ef_array)[3]) {
  ran_ef_array[,,i] <- mvrnorm(n_samps, rand_eff_est[(i*2 - 1):(i*2)], condvar_branch_array[,,i])
}

## Fixed effects
fix_eff_matrix <- mvrnorm(n_samps
  , mu    = getME(h_to_m_trans.mod, c("beta"))
  , Sigma = summary(h_to_m_trans.mod)[["vcov"]])

## estimates for each species
pred_vals <- expand.grid(
  infectious.dose = c(host_titer)
, species         = unique(h_to_m_emp$species)
, sim             = seq(1, n_samps, by = 1)
, prob            = 0
)
pred_vals$species <- as.character(pred_vals$species)

h_to_m_trans_all_samps <- array(
  dim = c(
    nrow(host_titer)
  , length(unique(tot_titer$host))
  , length(unique(h_to_m_emp$species))
  , n_samps
    )
  , data = 0)

dimnames(h_to_m_trans_all_samps) <- list(
  NULL
, unique(tot_titer$host)
, unique(h_to_m_emp$species)
, NULL
)

## Estimate host to mosquito transmission probability taking into consideration uncertainty in the host titer 
for (i in 1:length(unique(h_to_m_emp$species))) {
  for (j in 1:n_samps) {
    
## Here use  titer "samples" with a transmission "samples" to understand the joint uncertainty over titer and transmission | titer
 ## That is, what is the probability that any given mosquito picks up infection from any given host given the uncertainty we 
  ## have in that mosquitoes ability and hosts' titer
h_to_m_trans_all_samps[ , , i, j] <- plogis(
      ## intercept
    fix_eff_matrix[j, 1] + ran_ef_array[j,1,i] +
      ## slope over titer * titer, with uncertainty in titer
    (fix_eff_matrix[j, 2] + ran_ef_array[j,2,i]) * 
    host_titer_all_samps[, j, ]
    )
    
  }
  
}

## Also "brute force" calculate AUC for mosquito infection probability
mosq_inf_AUC_all_samps                <- matrix(nrow = length(unique(h_to_m_emp$species)), ncol = n_samps, data = 0)
dimnames(mosq_inf_AUC_all_samps)[[1]] <- unique(h_to_m_emp$species)

for (i in 1:length(unique(h_to_m_emp$species))) {
  for (j in 1:n_samps) {
    
temp_prob <- plogis(
      ## intercept
    fix_eff_matrix[j, 1] + ran_ef_array[j,1,i] +
      ## slope over titer * titer, with uncertainty in titer
    (fix_eff_matrix[j, 2] + ran_ef_array[j,2,i]) * 
      ## very fine scale titer for the AUC
    seq(1, max_dose_val, by = 0.001)
    )
    
mosq_inf_AUC_all_samps[i, j] <- sum(temp_prob) * 0.001
    
  }
  
}

## Finally, just need the probability that each mosquito obtains infection at some specific dose
mosq_inf_single_dose <- matrix(nrow = length(unique(h_to_m_emp$species)), ncol = n_samps, data = 0)
dimnames(mosq_inf_single_dose)[[1]] <- unique(h_to_m_emp$species)

for (i in 1:length(unique(h_to_m_emp$species))) {
  for (j in 1:n_samps) {
    
mosq_inf_single_dose[i, j] <- plogis(
      ## intercept
    fix_eff_matrix[j, 1] + ran_ef_array[j,1,i] +
      ## slope over titer * titer, with uncertainty in titer
    (fix_eff_matrix[j, 2] + ran_ef_array[j,2,i]) * 
      ## very fine scale titer for the AUC
    titer_dose # titer_dose
    )
    
  }
}

## And clean it up for use in mosquito to host competence (only place this is used is to weight the probability that 
 ## a mosquito would become infected for mosquito to host transmission)
mosq_inf_single_dose.gg <- melt(mosq_inf_single_dose)
names(mosq_inf_single_dose.gg) <- c("mosq", "samp", "prob")
mosq_names <- strsplit(as.character(mosq_inf_single_dose.gg$mosq), "_") %>% 
  sapply(., FUN = function(x) c(paste(x, collapse = " "))) %>% 
  sapply(., FUN = function(x) paste(toupper(substring(x, 1, 1)), substring(x, 2), sep = "", collapse = " "))
mosq_inf_single_dose.gg$mosq <- mosq_names

#################################################################
### Supplemental host to mosquito transmission plot
#####

## Estimated host to mosquito transmission probabilities

h_to_m_trans_all_samps.gg    <- array(
  dim = c(
    length(seq(0, max_dose_val, by = .1))
  , length(unique(h_to_m_emp$species))
  , n_samps
    )
  , data = 0)

dimnames(h_to_m_trans_all_samps.gg) <- list(
  seq(0, max_dose_val, by = .1)
, unique(h_to_m_emp$species))

## Estimate host to mosquito transmission probability taking into consideration uncertainty in the host titer 
for (i in 1:length(unique(h_to_m_emp$species))) {
  for (j in 1:n_samps) {
    
## Here use  titer "samples" with a transmission "samples" to understand the joint uncertainty over titer and transmission | titer
 ## That is, what is the probability that any given mosquito picks up infection from any given host given the uncertainty we 
  ## have in that mosquitoes ability and hosts' titer
h_to_m_trans_all_samps.gg[ , i, j] <- plogis(
      ## intercept
    fix_eff_matrix[j, 1] + ran_ef_array[j,1,i] +
      ## slope over titer * titer, with uncertainty in titer
    (fix_eff_matrix[j, 2] + ran_ef_array[j,2,i]) * 
    seq(0, max_dose_val, by = .1)
    )
    
  }
  
}

h_to_m_trans_all_samps.gg        <- melt(h_to_m_trans_all_samps.gg)
names(h_to_m_trans_all_samps.gg) <- c("titre", "species", "samp", "prob")

h_to_m_trans_all_samps.gg.s      <- h_to_m_trans_all_samps.gg %>%
  group_by(species, titre) %>% 
  summarize(
    lwr = quantile(prob, 0.100)
  , est = quantile(prob, 0.500)
  , upr = quantile(prob, 0.900)
  )

mosq_names <- strsplit(as.character(h_to_m_trans_all_samps.gg.s$species), "_") %>% 
  sapply(., FUN = function(x) c(paste(x, collapse = " "))) %>% 
  sapply(., FUN = function(x) paste(toupper(substring(x, 1, 1)), substring(x, 2), sep = "", collapse = " "))
h_to_m_trans_all_samps.gg.s$species <- mosq_names

h_to_m_emp.gg <- h_to_m_emp
mosq_names <- strsplit(as.character(h_to_m_emp.gg$species), "_") %>% 
  sapply(., FUN = function(x) c(paste(x, collapse = " "))) %>% 
  sapply(., FUN = function(x) paste(toupper(substring(x, 1, 1)), substring(x, 2), sep = "", collapse = " "))
h_to_m_emp.gg$species <- mosq_names

## Export at 18x12
ggplot(h_to_m_trans_all_samps.gg.s
  , aes(titre, est)) + 
  geom_line() +
  geom_ribbon(aes(x = titre, ymin = lwr, ymax = upr), alpha = 0.25) +
  geom_point(data = h_to_m_emp.gg, aes(infectious.dose, num_inf / total.infect.exp)) +
  facet_wrap(~species) +
  xlab(bquote('Infectious Dose -- Titer ('*log[10]*')')) +
  ylab("Infection Probability")

## Single panel for conceptual figure
mos.cf <- "Cq linealis"

ggplot(
  (h_to_m_trans_all_samps.gg.s %>% filter(species == mos.cf))
  , aes(titre, est)) + 
  geom_line() +
  geom_ribbon(aes(x = titre, ymin = lwr, ymax = upr), alpha = 0.25) +
  geom_point(data = (h_to_m_emp.gg %>% filter(species == mos.cf))
    , aes(infectious.dose, num_inf / total.infect.exp)) +
  xlab(bquote('Titer ('*log[10]*') (dose)' )) +
  ylab("Infection 
Probability")

## Calculated AUC for these same species
mosq_inf_AUC.gg        <- melt(mosq_inf_AUC_all_samps)
names(mosq_inf_AUC.gg) <- c("mosq", "samp", "AUC")
mosq_names             <- strsplit(as.character(mosq_inf_AUC.gg$mosq), "_") %>% 
  sapply(., FUN = function(x) c(paste(x, collapse = " "))) %>% 
  sapply(., FUN = function(x) paste(toupper(substring(x, 1, 1)), substring(x, 2), sep = "", collapse = " "))
mosq_inf_AUC.gg$mosq <- mosq_names
mosq_inf_AUC.gg.s    <- mosq_inf_AUC.gg %>% group_by(mosq) %>%
  summarize(
    lwr = quantile(AUC, 0.025)
  , est = quantile(AUC, 0.500)
  , upr = quantile(AUC, 0.975)
  )  

mosq_inf_AUC.gg.s <- mosq_inf_AUC.gg.s[order(mosq_inf_AUC.gg.s$est), ]
mosq_inf_AUC.gg.s$mosq <- factor(mosq_inf_AUC.gg.s$mosq, levels = mosq_inf_AUC.gg.s$mosq)

gg.mosq_inf.AUC <- ggplot(mosq_inf_AUC.gg.s, aes(est, mosq)) + 
  geom_point(lwd = 2) +
  geom_errorbarh(aes(xmin = lwr, xmax = upr, y = mosq), height = 0.5) +
  ylab("Mosquito") +
  xlab("Area Under Infection Probability Curve") +
  theme(
    axis.text.y = element_text(size = 16)
  , axis.title.x = element_text(size = 19)
  , axis.title.y = element_text(size = 19)
  )

## Reorder
prop_inf_gbite <- prop_inf[match(dimnames(h_to_m_trans_all_samps)[[2]], prop_inf$host), ]

## AUC for these titer curves
AUC_titer.gg        <- melt(host_titer_AUC_all_samps)
names(AUC_titer.gg) <- c("host", "sample", "AUC")
host_names          <- strsplit(as.character(AUC_titer.gg$host), "_") %>% 
  sapply(., FUN = function(x) c(paste(x, collapse = " "))) %>% 
  sapply(., FUN = function(x) paste(toupper(substring(x, 1, 1)), substring(x, 2), sep = "", collapse = " "))
AUC_titer.gg$Host <- host_names

## Change a few names for the supplemental plot
AUC_titer.gg <- AUC_titer.gg %>% mutate(
  Host = mapvalues(Host, from = "Grey h f fox", to = "Grey flying fox")
)

## Multiply the AUC by the proportion of the hosts displaying titer
prop_inf_gbite.gg <- prop_inf_gbite
host_names        <- strsplit(as.character(prop_inf_gbite.gg$host), "_") %>% 
  sapply(., FUN = function(x) c(paste(x, collapse = " "))) %>% 
  sapply(., FUN = function(x) paste(toupper(substring(x, 1, 1)), substring(x, 2), sep = "", collapse = " "))
prop_inf_gbite.gg$Host <- host_names
prop_inf_gbite.gg <- prop_inf_gbite.gg %>% mutate(
  Host = mapvalues(Host, from = "Grey h f fox", to = "Grey flying fox")
)

AUC_titer.gg <- AUC_titer.gg %>% left_join(., (prop_inf_gbite.gg %>% dplyr::select(Host, num_inf)))
AUC_titer.gg <- AUC_titer.gg %>% mutate(AUC_weighted = AUC * num_inf)

## Finally, summarize into CI
AUC_titer.gg.s.w <- AUC_titer.gg %>%
  group_by(Host) %>% 
  summarize(
    est = quantile(AUC_weighted, 0.50)
  , lwr = quantile(AUC_weighted, 0.025)
  , upr = quantile(AUC_weighted, 0.975)
  ) %>% mutate(AUC = "Yes")

AUC_titer.gg.s.w      <- AUC_titer.gg.s.w[order(AUC_titer.gg.s.w$est), ]
AUC_titer.gg.s.w$Host <- factor(AUC_titer.gg.s.w$Host, levels = AUC_titer.gg.s.w$Host)

AUC_titer.gg.s.uw <- AUC_titer.gg %>%
  group_by(Host) %>% 
  summarize(
    est = quantile(AUC, 0.50)
  , lwr = quantile(AUC, 0.025)
  , upr = quantile(AUC, 0.975)
  ) %>% mutate(AUC = "No")

AUC_titer.gg.s      <- rbind(AUC_titer.gg.s.w, AUC_titer.gg.s.uw)

AUC_titer.gg.s      <- AUC_titer.gg.s[order(AUC_titer.gg.s$est), ]
AUC_titer.gg.s$Host <- factor(AUC_titer.gg.s$Host, levels = AUC_titer.gg.s.w$Host)

## Export at 18x12
gg.titer.AUC <- ggplot(AUC_titer.gg.s, aes(est, Host)) + 
  geom_point(aes(colour = AUC), lwd = 2, position = position_dodge(width = 0.5)) +
  geom_errorbarh(aes(xmin = lwr, xmax = upr, y = Host, colour = AUC), height = 0.5, position=position_dodge(width=0.5)) +
  ylab("Host") +
  xlab(bquote('Titre Days -- '*log[10]*'(AUC of titer profile)')) +
  scale_color_brewer(palette = "Dark2", name = "Weighted by proportion
of exposed hosts that
develop detectable viremia?") +
  theme(
    axis.text.y = element_text(size = 16)
  , axis.title.x = element_text(size = 19)
  , axis.title.y = element_text(size = 19)
  , legend.key.size = unit(.65, "cm")
  , legend.title = element_text(size = 18)
  , legend.text = element_text(size = 16)
  , legend.position = c(0.80, 0.14)
  )
