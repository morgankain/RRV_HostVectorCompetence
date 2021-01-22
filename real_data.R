####################################################################################
### Fit models to empirical data from the RRV system to be used in the NGM model ###
####################################################################################

####
## The scripts here are used to create the following data frames which are used to predict
## community R0 and host/vector competence
####

# h_to_m_trans      : host to mosquito transmission for the community of interest
# m_to_h_trans      : mosquito to host transmission
# mos_surv_for_R0   : mosquito survival
# mosq_bite_pref    : mosquito biting preference given the host community
# host_prop_for_R0  : proportions of each host in the given community

####
## Host titer
####
source("data.titer.R")

####
## Host to mosquito transmission
####
source("data.hm.R")

####
## Mosquito to host transmission
####
source("data.mh.R")

####
## Mosquito survival.
####
source("data.mosq_surv_same.R")

####
## Mosquito biting preference
####
source("data.mosq_bite_pref.R")

####
## Mosquito biting rate
####

## Just treat biting rate as the same across all hosts for now. When set up this way,
 ## just adjust the desired parameter value in parameters.R
