###############################################################
## Figure 2 individual panels require a lot of extra cleanup ##
###############################################################

####
## Host panel 1, just AUC of the hosts in the community
####

AUC_titer.gg <- melt(host_titer_AUC_all_samps_adj_to_com)
names(AUC_titer.gg) <- c("host", "sample", "AUC")
host_names   <- strsplit(as.character(AUC_titer.gg$host), "_") %>% 
  sapply(., FUN = function(x) c(paste(x, collapse = " "))) %>% 
  sapply(., FUN = function(x) paste(toupper(substring(x, 1, 1)), substring(x, 2), sep = "", collapse = " "))
AUC_titer.gg$Host <- host_names

AUC_titer.gg <- AUC_titer.gg %>% left_join(., prop_inf_for_R0)
AUC_titer.gg <- AUC_titer.gg %>% mutate(AUC_weighted = AUC * num_inf)

## Finally, summarize into CI
AUC_titer.gg.s <- AUC_titer.gg %>%
  group_by(Host) %>% 
  summarize(
    est = quantile(AUC_weighted, 0.50)
  , lwr = quantile(AUC_weighted, 0.025)
  , upr = quantile(AUC_weighted, 0.975)
  )

## Export at 18x12
AUC_titer.gg.s <- AUC_titer.gg.s[order(AUC_titer.gg.s$est), ]
AUC_titer.gg.s$Host <- factor(AUC_titer.gg.s$Host, levels = AUC_titer.gg.s$Host)

####
## Mosquito panel 1, just AUC of the mosquitoes in the community
####

mosq_inf_AUC_all_samps_adj_to_com.gg        <- melt(mosq_inf_AUC_all_samps_adj_to_com)
names(mosq_inf_AUC_all_samps_adj_to_com.gg) <- c("mosq", "samp", "AUC_inf")
mosq_trans_AUC_all_samps_adj_to_com.gg      <- melt(mosq_trans_AUC_all_samps_adj_to_com)
names(mosq_trans_AUC_all_samps_adj_to_com.gg) <- c("mosq", "samp", "AUC_trans")

mosq_inf_AUC_all_samps_adj_to_com.gg <- left_join(
  mosq_inf_AUC_all_samps_adj_to_com.gg
, mosq_trans_AUC_all_samps_adj_to_com.gg) %>% mutate(
  AUC_weighted = AUC_inf * AUC_trans
)

mosq_inf_AUC_all_samps_adj_to_com.gg.s <- mosq_inf_AUC_all_samps_adj_to_com.gg %>% 
  group_by(mosq) %>%
  summarize(
    est = quantile(AUC_weighted, 0.500)
  , lwr = quantile(AUC_weighted, 0.025)
  , upr = quantile(AUC_weighted, 0.975)
  )

mosq_names <- strsplit(as.character(mosq_inf_AUC_all_samps_adj_to_com.gg.s$mosq), "_") %>% 
  sapply(., FUN = function(x) c(paste(x, collapse = " "))) %>% 
  sapply(., FUN = function(x) paste(toupper(substring(x, 1, 1)), substring(x, 2), sep = "", collapse = " "))
mosq_inf_AUC_all_samps_adj_to_com.gg.s$mosq <- mosq_names

mosq_inf_AUC_all_samps_adj_to_com.gg.s <- mosq_inf_AUC_all_samps_adj_to_com.gg.s[order(mosq_inf_AUC_all_samps_adj_to_com.gg.s$est), ]
mosq_inf_AUC_all_samps_adj_to_com.gg.s$mosq <- factor(mosq_inf_AUC_all_samps_adj_to_com.gg.s$mosq, levels = mosq_inf_AUC_all_samps_adj_to_com.gg.s$mosq)

####
## Host panel 2, Host to Mosquito transmission matrix and summary CI plot
####

## The summary part of the panel
host_competence.gg.heat.s.p      <- host_competence.gg.heat.s %>% filter(model.complexity == "TRUE_TRUE_TRUE_TRUE_TRUE")
host_competence.gg.heat.s.p$host <- factor(host_competence.gg.heat.s.p$host, levels = AUC_titer.gg.s$Host)

## The matrix part of the panel
host_competence.mat.gg        <- melt(host_competence)
names(host_competence.mat.gg) <- c("mosq", "host", "sample", "transmission")
mosq_names <- strsplit(as.character(host_competence.mat.gg$mosq), "_") %>% 
  sapply(., FUN = function(x) c(paste(x, collapse = " "))) %>% 
  sapply(., FUN = function(x) paste(toupper(substring(x, 1, 1)), substring(x, 2), sep = "", collapse = " "))
host_competence.mat.gg$mosq <- mosq_names
host_names <- strsplit(as.character(host_competence.mat.gg$host), "_") %>% 
  sapply(., FUN = function(x) c(paste(x, collapse = " "))) %>% 
  sapply(., FUN = function(x) paste(toupper(substring(x, 1, 1)), substring(x, 2), sep = "", collapse = " "))
host_competence.mat.gg$host <- host_names

host_competence.mat.gg.s <- host_competence.mat.gg %>% 
  group_by(mosq, host) %>%
  summarize(
    est = quantile(transmission, 0.500)
  , lwr = quantile(transmission, 0.025)
  , upr = quantile(transmission, 0.975)
  )

host_competence.mat.gg.s$host <- factor(host_competence.mat.gg.s$host, levels = AUC_titer.gg.s$Host)

####
## Mosquito panel 2, Mosquito to Host transmission matrix and summary CI plot
####

## First need to calculate the probability that a mosquito is a transmitter at a chosen dose, lets say some reasonable host average titer of 4.5
# mosq_inf_single_dose.gg ## calculated in data.hm.R

## Then multiply the WAIFW_left, which is the mosquito to host transmission part by this probability
m_to_h_mat.gg           <- WAIFW_left
dimnames(m_to_h_mat.gg) <- list(
  host_sero$host_species
, dimnames(mosq_trans_piece)[[1]]
, NULL)
m_to_h_mat.gg        <- melt(m_to_h_mat.gg)
names(m_to_h_mat.gg) <- c("host", "mosquito", "samp", "estimate")

mosq_names <- strsplit(as.character(m_to_h_mat.gg$mosq), "_") %>% 
  sapply(., FUN = function(x) c(paste(x, collapse = " "))) %>% 
  sapply(., FUN = function(x) paste(toupper(substring(x, 1, 1)), substring(x, 2), sep = "", collapse = " "))
m_to_h_mat.gg$mosq <- mosq_names
host_names <- strsplit(as.character(m_to_h_mat.gg$host), "_") %>% 
  sapply(., FUN = function(x) c(paste(x, collapse = " "))) %>% 
  sapply(., FUN = function(x) paste(toupper(substring(x, 1, 1)), substring(x, 2), sep = "", collapse = " "))
m_to_h_mat.gg$host <- host_names

## Combine to weight by mosquito infection probability so that we aren't starting with an infected mosquito 
m_to_h_mat.gg <- left_join(m_to_h_mat.gg, mosq_inf_single_dose.gg)
m_to_h_mat.gg <- m_to_h_mat.gg %>% mutate(trans = estimate * prob) ## This will now be used for the full matrix

## just the median for the heatmap
m_to_h_mat.gg.mh <- m_to_h_mat.gg %>%
  group_by(mosq, host) %>%
  summarize(
    est = quantile(trans, 0.500)
  , lwr = quantile(trans, 0.025)
  , upr = quantile(trans, 0.975)
  )

m_to_h_mat.gg.mh$mosq <- factor(m_to_h_mat.gg.mh$mosq, levels = mosq_inf_AUC_all_samps_adj_to_com.gg.s$mosq)
m_to_h_mat.gg.mh$host <- factor(m_to_h_mat.gg.mh$host, levels = rev(AUC_titer.gg.s$Host))

m_to_h_mat.gg <- m_to_h_mat.gg %>% group_by(mosq, samp) %>% 
  summarize(trans = sum(trans)) %>% ungroup()

## And a summary of this will be used for the side panel
m_to_h_mat.gg.s <- m_to_h_mat.gg %>% group_by(mosq) %>%
  summarize(
    est = quantile(trans, 0.500)
  , lwr = quantile(trans, 0.025)
  , upr = quantile(trans, 0.975)
  )

m_to_h_mat.gg.s$mosq <- factor(m_to_h_mat.gg.s$mosq, levels = mosq_inf_AUC_all_samps_adj_to_com.gg.s$mosq)

####
## Host Panel 3, Host to Host transmission matrix and summary CI plot
####

## Matrix panel component
physiol_mat.gg.f.p    <- physiol_mat.gg.f %>% filter(model.form == model_form.plot)
physiol_mat.gg.f.p$G1 <- factor(physiol_mat.gg.f.p$G1, levels = AUC_titer.gg.s$Host)
physiol_mat.gg.f.p$G2 <- factor(physiol_mat.gg.f.p$G2, levels = rev(AUC_titer.gg.s$Host))

## Summary panel component
physiol_mat.s <- melt(physiol_mat)
names(physiol_mat.s) <- c("G2", "G1", "samp", "estimate")
physiol_mat.s.same <- physiol_mat.s %>% filter(G1 == G2) %>% 
  mutate(G2 = "Self") %>% dplyr::select(-G2)
physiol_mat.s.oth <- physiol_mat.s %>% filter(G1 != G2) %>% 
  mutate(G2 = "Non-Self") %>% group_by(G1, samp) %>%
  summarize(estimate_n = sum(estimate))
physiol_mat.s.all <- left_join(
  physiol_mat.s.same, physiol_mat.s.oth
) %>% mutate(
 sum       = estimate + estimate_n
) %>% mutate(
  prop_self = estimate / sum 
)

physiol_mat.s.all.gg <- physiol_mat.s.all %>%
  group_by(G1) %>%
  summarize(
    est_prop = quantile(prop_self, 0.50, na.rm = T)
  , lwr_prop = quantile(prop_self, 0.025, na.rm = T)
  , upr_prop = quantile(prop_self, 0.975, na.rm = T)
  , est_tot = quantile(sum, 0.50, na.rm = T)
  , lwr_tot = quantile(sum, 0.025, na.rm = T)
  , upr_tot = quantile(sum, 0.975, na.rm = T)
  )

host_names   <- strsplit(as.character(physiol_mat.s.all.gg$G1), "_") %>% 
  sapply(., FUN = function(x) c(paste(x, collapse = " "))) %>% 
  sapply(., FUN = function(x) paste(toupper(substring(x, 1, 1)), substring(x, 2), sep = "", collapse = " "))
physiol_mat.s.all.gg$G1 <- host_names

physiol_mat.s.all.gg$G1 <- factor(physiol_mat.s.all.gg$G1, levels = AUC_titer.gg.s$Host)

## Repeat this for the supplemental figure that multiplies the estimates by the proportions of these hosts that become infectious to some degree

physiol_mat.s.supp <- left_join(
  physiol_mat.s, (prop_inf_for_R0[, c(1, 4)] %>% rename(G2 = host))
) %>% mutate(
  estimate = estimate * num_inf
)
  
physiol_mat.s.same.supp <- physiol_mat.s.supp %>% filter(G1 == G2) %>% 
  mutate(G2 = "Self") %>% dplyr::select(-G2)
physiol_mat.s.oth.supp <- physiol_mat.s.supp %>% filter(G1 != G2) %>% 
  mutate(G2 = "Non-Self") %>% group_by(G1, samp) %>%
  summarize(estimate_n = sum(estimate))
physiol_mat.s.all.supp <- left_join(
  physiol_mat.s.same.supp, physiol_mat.s.oth.supp
) %>% mutate(
 sum       = estimate + estimate_n
) %>% mutate(
  prop_self = estimate / sum 
)

physiol_mat.s.all.gg.supp <- physiol_mat.s.all.supp %>%
  group_by(G1) %>%
  summarize(
    est_prop = quantile(prop_self, 0.50, na.rm = T)
  , lwr_prop = quantile(prop_self, 0.025, na.rm = T)
  , upr_prop = quantile(prop_self, 0.975, na.rm = T)
  , est_tot = quantile(sum, 0.50, na.rm = T)
  , lwr_tot = quantile(sum, 0.025, na.rm = T)
  , upr_tot = quantile(sum, 0.975, na.rm = T)
  )

host_names   <- strsplit(as.character(physiol_mat.s.all.gg.supp$G1), "_") %>% 
  sapply(., FUN = function(x) c(paste(x, collapse = " "))) %>% 
  sapply(., FUN = function(x) paste(toupper(substring(x, 1, 1)), substring(x, 2), sep = "", collapse = " "))
physiol_mat.s.all.gg.supp$G1 <- host_names

physiol_mat.s.all.gg.supp$G1 <- factor(physiol_mat.s.all.gg.supp$G1, levels = AUC_titer.gg.s$Host)

host_names   <- strsplit(as.character(prop_inf_for_R0$host), "_") %>% 
  sapply(., FUN = function(x) c(paste(x, collapse = " "))) %>% 
  sapply(., FUN = function(x) paste(toupper(substring(x, 1, 1)), substring(x, 2), sep = "", collapse = " "))
prop_inf_for_R0$host <- host_names

physiol_mat.gg.f.p.supp <- physiol_mat.gg.f.p %>% left_join(., (prop_inf_for_R0[, c(1, 4)] %>% rename(G2 = host))) %>%
  mutate(comp.est = comp.est * num_inf)

physiol_mat.gg.f.p.supp$G1 <- factor(physiol_mat.gg.f.p.supp$G1, levels = AUC_titer.gg.s$Host)
physiol_mat.gg.f.p.supp$G2 <- factor(physiol_mat.gg.f.p.supp$G2, levels = rev(AUC_titer.gg.s$Host))

####
## Mosquito Panel 3, mosquito to mosquito transmission matrix and summary CI plot
####

## Matrix panel component
physiol_mat.gg.f.p.mm    <- physiol_mat.gg.f.mm %>% filter(model.form == model_form.plot)
physiol_mat.gg.f.p.mm$G1 <- factor(physiol_mat.gg.f.p.mm$G1, levels = mosq_inf_AUC_all_samps_adj_to_com.gg.s$mosq)
physiol_mat.gg.f.p.mm$G2 <- factor(physiol_mat.gg.f.p.mm$G2, levels = rev(mosq_inf_AUC_all_samps_adj_to_com.gg.s$mosq))

## Summary panel component
physiol_mat.s.mm <- melt(physiol_mat_mm)
names(physiol_mat.s.mm) <- c("G2", "G1", "samp", "estimate")

mosq_names   <- strsplit(as.character(physiol_mat.s.mm$G1), "_") %>% 
  sapply(., FUN = function(x) c(paste(x, collapse = " "))) %>% 
  sapply(., FUN = function(x) paste(toupper(substring(x, 1, 1)), substring(x, 2), sep = "", collapse = " "))
physiol_mat.s.mm$G1 <- mosq_names

mosq_names   <- strsplit(as.character(physiol_mat.s.mm$G2), "_") %>% 
  sapply(., FUN = function(x) c(paste(x, collapse = " "))) %>% 
  sapply(., FUN = function(x) paste(toupper(substring(x, 1, 1)), substring(x, 2), sep = "", collapse = " "))
physiol_mat.s.mm$G2 <- mosq_names

## Once again scale by the prob of infection given exposure (see mosq_competence_cleanup.R for more comments)
names(mosq_inf_single_dose.gg.f)    <- c("G1", "samp", "prob")

physiol_mat.s.mm <- left_join(physiol_mat.s.mm, mosq_inf_single_dose.gg.f)
physiol_mat.s.mm <- physiol_mat.s.mm %>% mutate(estimate = estimate * prob)

physiol_mat.s.same.mm <- physiol_mat.s.mm %>% filter(G1 == G2) %>% 
  mutate(G2 = "Self") %>% dplyr::select(-G2)
physiol_mat.s.oth.mm <- physiol_mat.s.mm %>% filter(G1 != G2) %>% 
  mutate(G2 = "Non-Self") %>% group_by(G1, samp) %>%
  summarize(estimate_n = sum(estimate))
physiol_mat.s.all.mm <- left_join(
  physiol_mat.s.same.mm, physiol_mat.s.oth.mm
) %>% mutate(
 sum       = estimate + estimate_n
) %>% mutate(
  prop_self = estimate / sum 
)

physiol_mat.s.all.gg.mm <- physiol_mat.s.all.mm %>%
  group_by(G1) %>%
  summarize(
    est_prop = quantile(prop_self, 0.50, na.rm = T)
  , lwr_prop = quantile(prop_self, 0.025, na.rm = T)
  , upr_prop = quantile(prop_self, 0.975, na.rm = T)
  , est_tot = quantile(sum, 0.50, na.rm = T)
  , lwr_tot = quantile(sum, 0.025, na.rm = T)
  , upr_tot = quantile(sum, 0.975, na.rm = T)
  )

physiol_mat.s.all.gg.mm$G1 <- factor(physiol_mat.s.all.gg.mm$G1, levels = mosq_inf_AUC_all_samps_adj_to_com.gg.s$mosq)
