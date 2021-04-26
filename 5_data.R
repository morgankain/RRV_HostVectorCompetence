###################################
### Load and clean data sources ###
###################################

####
## Data independent of location
####
titer_emp        <- read.csv("data/host_response.csv")
human_titer_emp  <- read.csv("data/human_titre.csv")
h_to_m_emp       <- read.csv("data/mosquito_infection.csv")
m_to_h_emp       <- read.csv("data/mosquito_transmission.csv")

## Drop one row about sheep with too little information
titer_emp <- titer_emp %>% filter(!is.na(sd.titre)) %>% mutate(
  infected.dose = as.numeric(infected.dose), titre.duration.days = as.numeric(titre.duration.days))

## Raw calculated mosquito survival from Russel 1987 (not actually used because of many issues connecting these data to actual survival rates)
#mosq_surv        <- read.csv("data/mosq_surv_raw.csv") %>% dplyr::select(mos_species, trap_method, gonotrophic_cycle_days, SR1, SR2) %>%
#  rename(gon_len = gonotrophic_cycle_days) %>% 
#  group_by(mos_species) %>% 
#  summarize(SR1 = mean(SR1, na.rm = T), SR2 = mean(SR2, na.rm = T), gon_len = mean(gon_len)) %>% 
#  mutate(mos_species = as.character(mos_species))
#mosq_surv        <- rbind(mosq_surv
#    , data.frame(mos_species = "average", SR1 = mean(mosq_surv$SR1), SR2 = mean(mosq_surv$SR2), gon_len = 3))

####
## Data dependent on location
####
mosq_blood       <- read.csv("data/mosquito_feeding.csv") %>% filter(Area.name == focal.location) %>% 
   dplyr::select(-Reference, -Area.name, -Habitat, -Climatic.Zone, -Total, -mixed, -other) %>% rename(mos_species = Vector) %>%
   pivot_longer(-mos_species, names_to = "host_species", values_to = "prop") %>% group_by(mos_species, host_species) %>% 
   summarize(prop = sum(prop, na.rm = T)) %>% pivot_wider(id_cols = mos_species, values_from = "prop", names_from = host_species) %>% 
   rename(macropod = marsupial)

host_prop        <- read.csv("data/host_abundance.csv") %>% mutate(dens = count / area_km2) %>%
   filter(area == focal.location) %>% rename(host_species = species) %>% dplyr::select(host_species, dens)
host_prop[is.na(host_prop$dens), ]$dens <- 0

mosq_prop        <- read.csv("data/mosquito_abundance.csv") %>% dplyr::select(site, sp, count) %>% 
  filter(site == focal.location) %>% dplyr::rename(mos_species = sp) %>% group_by(mos_species) %>% summarize(count = sum(count))

## Drop all species for which we have no biting data and no observed host abundance
no_blood        <- which(mosq_blood %>% ungroup(mos_species) %>% dplyr::select(-mos_species) %>% colSums(.) == 0) %>% names()
no_blood_no_obs <- no_blood[!(no_blood %in% host_prop$host_species)]
no_blood_in_obs <- host_prop[which(host_prop$dens == 0), ]$host_species[host_prop[which(host_prop$dens == 0), ]$host_species %in% no_blood]
hosts.remove    <- c(no_blood_no_obs, no_blood_in_obs)

mosq_blood      <- mosq_blood %>% dplyr::select(-hosts.remove)
host_prop       <- host_prop %>% filter(host_species %!in% hosts.remove)

## For host seroprevalence we have to assume the same across locations. Combine "marsupial" because that is what we have for other data sources
host_sero        <- read.csv("data/host_seroprevalence.csv")
host_sero.m      <- host_sero %>% filter(host_species == "macropod" | host_species == "bandicoot")
host_sero.m      <- data.frame(host_species = "marsupial", prop_positive = mean(host_sero.m$prop_positive)
                               , reference = "see raw data", note = "combined bandicoot and macropod")
host_sero.o      <- host_sero %>% filter(!(host_species == "macropod" | host_species == "bandicoot")) 
host_sero        <- rbind(host_sero.o, host_sero.m) %>% mutate(host_species = mapvalues(host_species, to = "macropod", from = "marsupial"))
