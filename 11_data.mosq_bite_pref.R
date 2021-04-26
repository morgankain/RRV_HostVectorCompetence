#####################################################################
### Mosquito biting preference: Stan model for biting preferences ###
### random effect for variation among mosquito species            ###
#####################################################################

## Figure out which species in the blood meal data didn't show up in the host survey and add them as zeros (they wont
 ## end up as zero because of the prior)
missing_host <- names(mosq_blood)[-1][which(is.na(match(names(mosq_blood)[-1], as.character(host_prop$host_species))))]

## Sort the columns in the biting pref matrix so that they line up with the host_prop labels
host_prop <- host_prop[order(host_prop$dens, decreasing = TRUE), ]
host_prop <- transform(host_prop, host_species = as.character(host_species))

## Add the missing species
host_prop <- rbind(host_prop, data.frame(host_species = missing_host, dens = rep(0, length(missing_host))))

## Order the biting matrix to match the host_prop data frame
mosq_blood.have  <- match(host_prop$host_species, names(mosq_blood))
mosq_blood.prop  <- mosq_blood[, c(1, mosq_blood.have)]

## Note: this model requires a prior on host abundance. Optimally this would be contained from a different data source, but
 ## no such luck here, so instead the prior is going to be informed by the regional data, modified a bit with the knowledge
  ## that some host's abundance is a bit different in the location that was used for the mosquito trapping
   ## Using info about where the blood meal analyses were conducted (more peri-urban)
    ## to reduce human abundance, increase horse abundance and convert 0s to low numbers
prev_obs_p <- host_prop$dens
## Make rabbit and rat density equal to the rarest host 
prev_obs_p[c(11, 12)] <- prev_obs_p[10]
if (focal.location == "Brisbane") {
## Blood meal analyses area -- Bump horses up
prev_obs_p[10] <- prev_obs_p[10] + 30
## Blood meal analyses area -- Bump humans down
prev_obs_p[1] <- prev_obs_p[1] - prev_obs_p[1] * (1/3)
}

## Priors on the gammas. Want these close to 1, 1 to allow them to be a bit diffuse, but using 4, 4 to be just a bit
 ## tighter to solve some divergent transition problems
sigma_pb_a <- sigma_pb_b <- 4 

## remove the mosquito species identifier for the stan model
mosq_blood_data <- mosq_blood.prop %>% ungroup(mos_species) %>% dplyr::select(-mos_species)

## needed for later, the number of hosts and the number of mosquitoes
num.hosts <- nrow(host_prop)
num.mosq  <- nrow(mosq_blood)

## Data
mosq_bite.data <- list(
       "N"            = nrow(host_prop)                  ## Number of hosts
    ,  "J"            = nrow(mosq_blood_data)            ## Number of mosquitoes
  
## Note: The model requires counts of hosts while we have density per square km. 
 ## Convert to counts, by rounding after multiplying to a large enough area to get > 1. 
  ## Hopefully a reasonable choice in the context of the blood meal survey data | mosquito flight distance
    ,  "host_counts"  = round(c(host_prop$dens) * 5)        
  
    ,  "bites"        = as.matrix(mosq_blood_data)       ## Observed blood meals
    ,  "flat_alpha_p" = rep(1, nrow(host_prop))          ## Scale for prior abundance
    ,  "prev_obs_p"   = prev_obs_p
    ,  "sigma_pb_a"   = sigma_pb_a                       ## Hyper-prior for biting pref
    ,  "sigma_pb_b"   = sigma_pb_b                       ## Hyper-prior for biting pref
  )

## Load a previously fit stan model or fit a new one. Somewhat but not terribly slow
if (file.exists(paste(paste("mosq_bite_model_out", focal.location, sep = "_"), "Rds", sep = "."))) {
mosq_bite_model_out <- readRDS(paste(paste("mosq_bite_model_out", focal.location, sep = "_"), "Rds", sep = "."))
} else {
  ## Run Model
mosq_bite_model_out <- stan(
  file     = "mosq_bite_random.stan"
, data     = mosq_bite.data
, iter     = 6000
, thin     = 6
, warmup   = 2000
, refresh  = max(4000/100, 1)
, control  = list(max_treedepth = 18, adapt_delta = .999, stepsize = 0.5)
, chains   = 4
)
saveRDS(mosq_bite_model_out
  , paste(paste("mosq_bite_model_out", focal.location, sep = "_"), "Rds", sep = "."))
}

## Pleasant way to look at convergence of the model
# launch_shinystan(mosq_bite_model_out)

mosq_bite_model_out_summ <- summary(mosq_bite_model_out)
mosq_bite                <- mosq_bite_model_out_summ$summary

## Extract the median biting preferences
mosq_bite.gg  <- mosq_bite[grep("bite_pref", dimnames(mosq_bite)[[1]]), 6]
mosq_bite.gg  <- matrix(nrow = num.mosq, ncol = num.hosts, data = mosq_bite.gg, byrow = TRUE)

## Extract the samples from the stan model as well
detach("package:tidyr", unload = TRUE)
samps_mosq_bite          <- extract(mosq_bite_model_out, permuted = FALSE)
library(tidyr)

## Data for community R0 and FOI calculation
mosq_bite_pref             <- mosq_bite.gg
mosq_bite_pref_all_samps   <- array(dim = c(num.mosq, num.hosts, n_samps))
host_abund_all_samps       <- matrix(nrow = num.hosts, ncol = n_samps)

## Combine the samples across the chains, and then take a random n_samps from all of those samples
samps_mosq_bite.all <- apply(samps_mosq_bite, 3L, c)
ran_samp            <- sample(seq(1, dim(samps_mosq_bite.all)[1]), n_samps)

for (i in seq_along(ran_samp)) {
  mosq_bite_pref_all_samps[,,i] <- matrix(nrow = num.mosq, ncol = num.hosts
                                          , data = samps_mosq_bite.all[ran_samp[i], grep("bite_pref", dimnames(mosq_bite)[[1]])])
  host_abund_all_samps[, i]     <- matrix(nrow = num.hosts, ncol = 1
                                          , data = samps_mosq_bite.all[ran_samp[i], grep("theta_p", dimnames(mosq_bite)[[1]])])
}

mosq_bite.gg        <- melt(mosq_bite.gg)
names(mosq_bite.gg) <- c("Mosq", "Host", "pref")

## Also grab the proportions of the hosts in the community from the model for calculating the transmission matrix,
 ## which includes the data and the prior together, which will be used for R0 and host competence calculations that include
  ## host abundance
host_prop_for_R0 <- mosq_bite[1:num.hosts, 6]

host_prop_for_R0_all_samps <- matrix(nrow = num.hosts, ncol = n_samps)
for (i in seq_along(ran_samp)) {
  host_prop_for_R0_all_samps[,i] <- samps_mosq_bite.all[ran_samp[i], 1:num.hosts]
}

dimnames(mosq_bite_pref)[[1]]           <- as.character(unique(mosq_blood.prop$mos_species))
dimnames(mosq_bite_pref)[[2]]           <- host_prop$host_species
dimnames(mosq_bite_pref_all_samps)[[1]] <- as.character(unique(mosq_blood.prop$mos_species))
dimnames(mosq_bite_pref_all_samps)[[2]] <- host_prop$host_species
  
names(host_prop_for_R0)                   <- host_prop$host_species
dimnames(host_prop_for_R0_all_samps)[[1]] <- host_prop$host_species

## Reorder prop_inf to match the order in a previous model component 
prop_inf       <- prop_inf[match(dimnames(h_to_m_trans_all_samps)[[2]], prop_inf$host), ]
prop_inf$host  <- as.character(prop_inf$host)

## Stick 0 in for cats and dogs (all zeroes in the titer data)
prop_inf  <- rbind(
  prop_inf
, data.frame(
  host    = c("cat", "dog")
, tot_inf = c(0, 0)
, tot_vir = c(0, 0)
, num_inf = c(0, 0)
))
