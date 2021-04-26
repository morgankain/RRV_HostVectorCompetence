####
## Goal here is to compare ranks between a focal trait TRUE vs FALSE over all possible combinations of the rest of the 
## traits being TRUE and FALSE. That is, comparing change in ranks when the focal component is added across all possible ways in 
## which that focal trait can be added
####

eco.components <- colnames(model.runs)

for (i in seq_along(eco.components)) {
  
## Subset the current focal ecological component to TRUE or FALSE
temp_F <- host_competence.r.gg.hh.f %>% filter(get(eco.components[i]) == FALSE) %>% 
  dplyr::select(-eco.components[i], -model.complexity)
temp_T <- host_competence.r.gg.hh.f %>% filter(get(eco.components[i]) == TRUE) %>%
  dplyr::select(-eco.components[i], -model.complexity)
names(temp_F)[3] <- "comp_F"
names(temp_T)[3] <- "comp_T" 
 
## Combine the ranks for true or false to line up all of the orders of eco components
temp_C <- left_join(temp_T, temp_F); rm(temp_F); rm(temp_T)
temp_C %<>% mutate(diff_rank = comp_F - comp_T)

## Summarize difference in rank between including eco component X or not for all ways of including eco component X
temp_C.heat <- temp_C %>% 
  group_by(host, diff_rank) %>% 
  summarize(den = length(diff_rank) / (nrow(temp_C) / 12)) %>% 
    mutate(
  eco_component = strsplit(eco.components[i], split = "[.]")[[1]][2]
    )

## Or summarize the raw rank of each host in all models in which a given ecological component is considered
temp_C.heat.raw <- temp_C %>% 
  group_by(host, comp_T) %>% 
  summarize(den = length(comp_T) / (nrow(temp_C) / 12)) %>% 
    mutate(
  eco_component = strsplit(eco.components[i], split = "[.]")[[1]][2]
    )

temp_C.sum <- temp_C %>% 
  group_by(host) %>%
  summarize(
    est = quantile(diff_rank, c(0.50))
  , lwr = quantile(diff_rank, c(0.025))
  , upr = quantile(diff_rank, c(0.975))
    ) %>% 
  mutate(
  eco_component = strsplit(eco.components[i], split = "[.]")[[1]][2]
    )

temp_C.sum.raw <- temp_C %>% 
  group_by(host) %>%
  summarize(
    est = quantile(comp_T, c(0.50))
  , lwr = quantile(comp_T, c(0.025))
  , upr = quantile(comp_T, c(0.975))
    ) %>% 
  mutate(
  eco_component = strsplit(eco.components[i], split = "[.]")[[1]][2]
    )

if (i == 1) {

full_permuted.heat_hh <- temp_C.heat
full_permuted.sum_hh  <- temp_C.sum

full_permuted.heat_hh.raw <- temp_C.heat.raw
full_permuted.sum_hh.raw  <- temp_C.sum.raw
  
} else {
  
full_permuted.heat_hh <- rbind(full_permuted.heat_hh, temp_C.heat)
full_permuted.sum_hh  <- rbind(full_permuted.sum_hh, temp_C.sum)

full_permuted.heat_hh.raw <- rbind(full_permuted.heat_hh.raw, temp_C.heat.raw)
full_permuted.sum_hh.raw  <- rbind(full_permuted.sum_hh.raw, temp_C.sum.raw)
  
}

}

for (i in seq_along(eco.components)) {
  
## Subset the current focal ecological component to TRUE or FALSE
temp_F <- mosq_competence.r.gg.mm.f %>% filter(get(eco.components[i]) == FALSE) %>% 
  dplyr::select(-eco.components[i], -model.complexity)
temp_T <- mosq_competence.r.gg.mm.f %>% filter(get(eco.components[i]) == TRUE) %>%
  dplyr::select(-eco.components[i], -model.complexity)
names(temp_F)[3] <- "comp_F"
names(temp_T)[3] <- "comp_T" 
 
## Combine the ranks for true or false to line up all of the orders of eco components
temp_C <- left_join(temp_T, temp_F); rm(temp_F); rm(temp_T)
temp_C %<>% mutate(diff_rank = comp_F - comp_T)

temp_C.heat <- temp_C %>% 
  group_by(mosq, diff_rank) %>% 
  summarize(den = length(diff_rank) / (nrow(temp_C) / 12)) %>% 
    mutate(
  eco_component = strsplit(eco.components[i], split = "[.]")[[1]][2]
    )

## Or summarize the raw rank of each host in all models in which a given ecological component is considered
temp_C.heat.raw <- temp_C %>% 
  group_by(mosq, comp_T) %>% 
  summarize(den = length(comp_T) / (nrow(temp_C) / 12)) %>% 
    mutate(
  eco_component = strsplit(eco.components[i], split = "[.]")[[1]][2]
    )

temp_C.sum <- temp_C %>% 
  group_by(mosq) %>%
  summarize(
    est = quantile(diff_rank, c(0.50))
  , lwr = quantile(diff_rank, c(0.025))
  , upr = quantile(diff_rank, c(0.975))
    ) %>% 
  mutate(
  eco_component = strsplit(eco.components[i], split = "[.]")[[1]][2]
    )

temp_C.sum.raw <- temp_C %>% 
  group_by(mosq) %>%
  summarize(
    est = quantile(comp_T, c(0.50))
  , lwr = quantile(comp_T, c(0.025))
  , upr = quantile(comp_T, c(0.975))
    ) %>% 
  mutate(
  eco_component = strsplit(eco.components[i], split = "[.]")[[1]][2]
    )

if (i == 1) {

full_permuted.heat_mm <- temp_C.heat
full_permuted.sum_mm  <- temp_C.sum

full_permuted.heat_mm.raw <- temp_C.heat.raw
full_permuted.sum_mm.raw  <- temp_C.sum.raw
  
} else {
  
full_permuted.heat_mm <- rbind(full_permuted.heat_mm, temp_C.heat)
full_permuted.sum_mm  <- rbind(full_permuted.sum_mm, temp_C.sum)

full_permuted.heat_mm.raw <- rbind(full_permuted.heat_mm.raw, temp_C.heat.raw)
full_permuted.sum_mm.raw  <- rbind(full_permuted.sum_mm.raw, temp_C.sum.raw)
  
}

}
