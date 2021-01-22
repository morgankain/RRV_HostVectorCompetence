############################################################################################
### Mosquito Survival, giving all mosquitoes the same survival because of a lack of data ###
############################################################################################

## Create a matrix of all mosquitoes for which we have any competence data
mosq_comp_sp    <- unique(h_to_m_emp$species)

mosq_surv_for_R0           <- matrix(nrow = mosq_days_max, ncol = length(mosq_comp_sp), data = 0)
dimnames(mosq_surv_for_R0) <- list(
  seq(1, mosq_days_max, by = 1)
, mosq_comp_sp
)

## Exponential rates survival model based on daily survival probability 
for (i in 1:ncol(mosq_surv_for_R0)) {
  mosq_surv_for_R0[, i] <- exp(-mosq_daily_surv) ^ seq(1, nrow(mosq_surv_for_R0), by = 1)
}

## Could inject uncertainty in survival, but since we have no species-variability in survival
 ## this would not serve to change any relative importance so seems to be a bit unnecessary for now
  ## so just repeat the predicted mean for all matrix n_samps to align with other models with uncertainty
mosq_surv_for_R0_all_samps <- array(dim = c(nrow(mosq_surv_for_R0), ncol(mosq_surv_for_R0), n_samps), data = c(mosq_surv_for_R0))

dimnames(mosq_surv_for_R0_all_samps)[[2]] <- as.character(unique(h_to_m_emp$species))

mosq_surv_for_R0_all_samps.gg        <- melt(mosq_surv_for_R0_all_samps)
names(mosq_surv_for_R0_all_samps.gg) <- c("day", "mosq", "samp", "proportion")
mosq_surv_for_R0_all_samps.gg        <- mosq_surv_for_R0_all_samps.gg %>%
  group_by(day, mosq) %>% 
  summarize(
    est = mean(proportion)
  )

mosq_surv_for_R0_all_samps.gg.s <- mosq_surv_for_R0_all_samps.gg %>% 
  filter(mosq == "cx_annulirostris") %>% 
  mutate(mosq = mapvalues(mosq, from = "cx_annulirostris", to = "Cx annnulirostris"))

ggplot(mosq_surv_for_R0_all_samps.gg.s, aes(day, est)) + geom_line() + 
  xlab("Day") + ylab("Proportion Surviving") +
  ggtitle("Culex annulirostris survival at half max of optimal laboratory
conditions from Shocket et al. 2018 Elife")

## for the conceptual figure
ggplot(mosq_surv_for_R0_all_samps.gg.s, aes(day, est)) + geom_line(lwd = 1) + 
  xlab("Day") + ylab("Proportion
Surviving") 
