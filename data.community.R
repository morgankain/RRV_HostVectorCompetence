################################################################################
### Set up the mosquito and host communities for competence and R0 estimates ###
################################################################################

## Note: Because we do not have mosquito physiological responses and blood meals for all of the species in the community
 ## we simply scale up the mosquitoes that we do have these data for to assume that they are the whole mosquito community.
  ## For Brisbane the mosquitoes we do have data for are ~ 90% of the community, and for Cairns a bit over 70%

## Convert to proportions
mosq_prop <- mosq_prop %>% mutate(prop = count / sum(count))

## Pull out only the mosquito species that we have BOTH physiological data and mosquito biting data
mosq_prop_for_R0 <- mosq_prop %>% 
  filter(mos_species %in% as.character(unique(h_to_m_trans.pred$species))) %>%
  filter(mos_species %in% as.character(mosq_blood.prop$mos_species))

## Re-scale
mosq_prop_for_R0$prop <- mosq_prop_for_R0$prop / sum(mosq_prop_for_R0$prop)

## All the hosts that we have titer data for
physiology_species.h <- as.character(unique(tot_titer$host))
physiology_species.m <- as.character(unique(h_to_m_emp$species))

## All the hosts that show up in the community of interest
ecological_species.h <- as.character(unique(host_prop$host_species))
ecological_species.m <- as.character(unique(mosq_blood.prop$mos_species))

## Connections here between hosts with physiological responses and ecological data are the following:
 ## Note, some simplifications are needed here to align the various data sources that we have
  ## A potentially minor problem with the simple average taken here is that titer profiles aren't getting weighted
   ## appropriately by abundance (e.g. within marsupials); however we lack that level of detailed abundance data

 ## bushtail possum -> possum
 ## agile wallaby   -> within marsupial 
 ## grey kangaroo   -> within marsupial 
 ## bandicoot       -> dropped
 ## marsupial mouse -> dropped 
 ## horse           -> horse
 ## sheep           -> sheep
 ## pig             -> dropped because they are not allowed in urban settings
 ## cow             -> cattle
 ## grey h f fox    -> flying fox
 ## rat             -> rat
 ## rabbit          -> rabbit
 ## chicken         -> within bird
 ## black duck      -> within bird 
 ## little corella  -> within bird
 ## human           -> human

#####
## Steps to adjust h_to_m_trans, which already has titer incorporated
#####
 ## 1) Select only the mosquitoes in the community for which we have biting preferences
mosq.which        <- which(dimnames(h_to_m_trans_all_samps)[[3]] %in% ecological_species.m)
mosq.ordered      <- dimnames(h_to_m_trans_all_samps)[[3]][mosq.which]
mosq_prop_for_R0  <- mosq_prop_for_R0[match(mosq.ordered, as.character(mosq_prop_for_R0$mos_species)), ]

 ## 2) Average marsupials (actually macropods given the removal of bandicoot and marsupial mouse)
marsupials        <- c("agile_wallaby", "grey_kangaroo")
marsupials.data   <- h_to_m_trans_all_samps[, which(dimnames(h_to_m_trans_all_samps)[[2]] %in% marsupials), , ]
marsupials.data.s <- apply(marsupials.data, c(1, 3, 4), mean)
marsupials.data.s <- marsupials.data.s[, mosq.which, ]
## Do the same manipulation for AUC for the first panel of Figure 2: host. Not used in the rest of the model
marsupials.data.AUC   <- host_titer_AUC_all_samps[which(dimnames(host_titer_AUC_all_samps)[[1]] %in% marsupials), ]
marsupials.data.s.AUC <- colMeans(marsupials.data.AUC)

 ## 3) Average birds
birds             <- c("chicken", "black_duck", "little_corella")
birds.data        <- h_to_m_trans_all_samps[, which(dimnames(h_to_m_trans_all_samps)[[2]] %in% birds), , ]
birds.data.s      <- apply(birds.data, c(1, 3, 4), mean)
birds.data.s      <- birds.data.s[, mosq.which, ]
## Do the same manipulation for AUC for the first panel of Figure 2: host. Not used in the rest of the model
birds.data.AUC   <- host_titer_AUC_all_samps[which(dimnames(host_titer_AUC_all_samps)[[1]] %in% birds), ]
birds.data.s.AUC <- colMeans(birds.data.AUC)

h_to_m_trans_adj_to_com                <- array(dim = c(length(inf_days)
                                                        ## total length - summaries + cat and dog which don't have data because they never show titer
                                                          , length(ecological_species.h)
                                                          , length(mosq.which)))
h_to_m_trans_all_samps_adj_to_com      <- array(dim = c(length(inf_days)
                                                        ## total length - summaries + cat and dog which don't have data because they never show titer
                                                        , length(ecological_species.h)
                                                        , length(mosq.which), n_samps))
dimnames(h_to_m_trans_adj_to_com)[[2]]           <- ecological_species.h
dimnames(h_to_m_trans_all_samps_adj_to_com)[[2]] <- ecological_species.h

## Create a smaller matrix for AUC as well
host_titer_AUC_all_samps_adj_to_com                <- matrix(nrow = length(ecological_species.h), ncol = n_samps, data = 0)
dimnames(host_titer_AUC_all_samps_adj_to_com)[[1]] <- ecological_species.h

### And then subset all of the mosquito data to these species
dimnames(h_to_m_trans_all_samps_adj_to_com)[[3]]   <- mosq.ordered

## The summaries and cat and dog
h_to_m_trans_all_samps_adj_to_com[, dimnames(h_to_m_trans_all_samps_adj_to_com)[[2]] == "bird", , ]      <- birds.data.s
h_to_m_trans_all_samps_adj_to_com[, dimnames(h_to_m_trans_all_samps_adj_to_com)[[2]] == "macropod", , ]  <- marsupials.data.s 
h_to_m_trans_all_samps_adj_to_com[, dimnames(h_to_m_trans_all_samps_adj_to_com)[[2]] == "cat", , ]       <- 0
h_to_m_trans_all_samps_adj_to_com[, dimnames(h_to_m_trans_all_samps_adj_to_com)[[2]] == "dog", , ]       <- 0

## The rest of the species (select the estimates for all the mosquitoes we are retaining and make the name change if necessary)
h_to_m_trans_all_samps_adj_to_com[, dimnames(h_to_m_trans_all_samps_adj_to_com)[[2]] == "human", , ]       <- 
  h_to_m_trans_all_samps[, dimnames(h_to_m_trans_all_samps)[[2]] == "human", mosq.which, ]
h_to_m_trans_all_samps_adj_to_com[, dimnames(h_to_m_trans_all_samps_adj_to_com)[[2]] == "possum", , ]      <- 
  h_to_m_trans_all_samps[, dimnames(h_to_m_trans_all_samps)[[2]] == "brushtail_possum", mosq.which, ]
h_to_m_trans_all_samps_adj_to_com[, dimnames(h_to_m_trans_all_samps_adj_to_com)[[2]] == "flying_fox", , ]  <- 
  h_to_m_trans_all_samps[, dimnames(h_to_m_trans_all_samps)[[2]] == "grey_h_f_fox", mosq.which, ]
h_to_m_trans_all_samps_adj_to_com[, dimnames(h_to_m_trans_all_samps_adj_to_com)[[2]] == "cattle", , ]      <- 
  h_to_m_trans_all_samps[, dimnames(h_to_m_trans_all_samps)[[2]] == "cow", mosq.which, ]
h_to_m_trans_all_samps_adj_to_com[, dimnames(h_to_m_trans_all_samps_adj_to_com)[[2]] == "horse", , ]       <- 
  h_to_m_trans_all_samps[, dimnames(h_to_m_trans_all_samps)[[2]] == "horse", mosq.which, ]
h_to_m_trans_all_samps_adj_to_com[, dimnames(h_to_m_trans_all_samps_adj_to_com)[[2]] == "sheep", , ]       <- 
  h_to_m_trans_all_samps[, dimnames(h_to_m_trans_all_samps)[[2]] == "sheep", mosq.which, ]
h_to_m_trans_all_samps_adj_to_com[, dimnames(h_to_m_trans_all_samps_adj_to_com)[[2]] == "rabbit", , ]      <- 
  h_to_m_trans_all_samps[, dimnames(h_to_m_trans_all_samps)[[2]] == "rabbit", mosq.which, ]
h_to_m_trans_all_samps_adj_to_com[, dimnames(h_to_m_trans_all_samps_adj_to_com)[[2]] == "rat", , ]         <- 
  h_to_m_trans_all_samps[, dimnames(h_to_m_trans_all_samps)[[2]] == "rat", mosq.which, ]

## fill in for AUC 
host_titer_AUC_all_samps_adj_to_com[dimnames(host_titer_AUC_all_samps_adj_to_com)[[1]] == "bird", ]       <- birds.data.s.AUC
host_titer_AUC_all_samps_adj_to_com[dimnames(host_titer_AUC_all_samps_adj_to_com)[[1]] == "macropod", ]   <- marsupials.data.s.AUC 
host_titer_AUC_all_samps_adj_to_com[dimnames(host_titer_AUC_all_samps_adj_to_com)[[1]] == "cat", ]        <- 0
host_titer_AUC_all_samps_adj_to_com[dimnames(host_titer_AUC_all_samps_adj_to_com)[[1]] == "dog", ]        <- 0
host_titer_AUC_all_samps_adj_to_com[dimnames(host_titer_AUC_all_samps_adj_to_com)[[1]] == "human", ]      <- 
  host_titer_AUC_all_samps[dimnames(host_titer_AUC_all_samps)[[1]] == "human", ]
host_titer_AUC_all_samps_adj_to_com[dimnames(host_titer_AUC_all_samps_adj_to_com)[[1]] == "possum", ]     <- 
  host_titer_AUC_all_samps[dimnames(host_titer_AUC_all_samps)[[1]] == "brushtail_possum", ]
host_titer_AUC_all_samps_adj_to_com[dimnames(host_titer_AUC_all_samps_adj_to_com)[[1]] == "flying_fox", ] <- 
  host_titer_AUC_all_samps[dimnames(host_titer_AUC_all_samps)[[1]] == "grey_h_f_fox", ]
host_titer_AUC_all_samps_adj_to_com[dimnames(host_titer_AUC_all_samps_adj_to_com)[[1]] == "cattle", ]     <- 
  host_titer_AUC_all_samps[dimnames(host_titer_AUC_all_samps)[[1]] == "cow", ]
host_titer_AUC_all_samps_adj_to_com[dimnames(host_titer_AUC_all_samps_adj_to_com)[[1]] == "horse", ]      <- 
  host_titer_AUC_all_samps[dimnames(host_titer_AUC_all_samps)[[1]] == "horse", ]
host_titer_AUC_all_samps_adj_to_com[dimnames(host_titer_AUC_all_samps_adj_to_com)[[1]] == "sheep", ]      <- 
  host_titer_AUC_all_samps[dimnames(host_titer_AUC_all_samps)[[1]] == "sheep", ]
host_titer_AUC_all_samps_adj_to_com[dimnames(host_titer_AUC_all_samps_adj_to_com)[[1]] == "rabbit", ]     <- 
  host_titer_AUC_all_samps[dimnames(host_titer_AUC_all_samps)[[1]] == "rabbit", ]
host_titer_AUC_all_samps_adj_to_com[dimnames(host_titer_AUC_all_samps_adj_to_com)[[1]] == "rat", ]        <- 
  host_titer_AUC_all_samps[dimnames(host_titer_AUC_all_samps)[[1]] == "rat", ]

#####
## And subset prop_inf for the community of interest
#####
prop_inf_for_R0 <- prop_inf
prop_inf_for_R0 <- prop_inf_for_R0 %>% mutate(
  host = mapvalues(host
                   , from = c(
  "brushtail_possum"  
, "agile_wallaby"     
, "grey_kangaroo"         
, "bandicoot"        
, "marsupial_mouse"      
, "horse"             
, "sheep"              
, "pig"              
, "cow"                 
, "grey_h_f_fox"      
, "rat"                
, "rabbit"            
, "chicken"             
, "black_duck"        
, "little_corella"        
, "human"              
, "cat"                    
, "dog"
) , to = c(
  "possum"  
, "macropod"     
, "macropod"         
, "marsupial"        
, "marsupial_mouse"      
, "horse"             
, "sheep"              
, "pig"              
, "cattle"                 
, "flying_fox"      
, "rat"                
, "rabbit"            
, "bird"             
, "bird"        
, "bird"        
, "human"              
, "cat"                    
, "dog"                     
)
))

## Last adjustment for proportion that become infected from the infection experiments
prop_inf_for_R0 <- prop_inf_for_R0 %>% 
  group_by(host) %>%
  summarize(tot_inf = sum(tot_inf), tot_vir = sum(tot_vir)) %>%
  mutate(num_inf = tot_vir / tot_inf)

prop_inf_for_R0[prop_inf_for_R0$host == "cat" | prop_inf_for_R0$host == "dog", 4] <- 0

prop_inf_for_R0 <- prop_inf_for_R0[match(host_prop$host_species, prop_inf_for_R0$host), ]

## Grab the info for host seroprevalence for Brisbane for use in both Brisbane and Cairns since we do not have this data for Cairns
host_sero <- host_sero[match(prop_inf_for_R0$host, host_sero$host), ]

#################################################################
### Supplemental Titer plot #2: averaged titer curves (to be made, maybe...??)
#####

####
## Plot the probabilities of infection over time from the titre curves and the mosquito infection probabilities scaled
## to consider the proportion of infected hosts that show a viraemic response
####

h_to_m_trans_all_samps_adj_to_com.gg        <- melt(h_to_m_trans_all_samps_adj_to_com)
names(h_to_m_trans_all_samps_adj_to_com.gg) <- c("day", "host", "mosquito", "sample", "probability")
h_to_m_trans_all_samps_adj_to_com.gg        <- h_to_m_trans_all_samps_adj_to_com.gg %>%
  group_by(host, day) %>% 
  summarise(
    lwr = quantile(probability, 0.20)
  , est = quantile(probability, 0.500)
  , upr = quantile(probability, 0.80)
    )

inf_prob_plots <- ggplot(h_to_m_trans_all_samps_adj_to_com.gg, aes(day, est)) + 
  geom_line() + 
  geom_ribbon(aes(x = day, ymin = lwr, ymax = upr), alpha = 0.25) +
  facet_wrap(~host)
