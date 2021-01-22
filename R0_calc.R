#####
## Calculate R0 and all metrics of host competence using model components
#####

## Grab the parameters and the output from the model components
num_hosts      <- dim(h_to_m_trans_all_samps_adj_to_com)[2]
num_mosq       <- dim(h_to_m_trans_all_samps_adj_to_com)[3]  
prop_hosts     <- host_prop_for_R0_all_samps
inf_days       <- seq(1, length(inf_days), 1) 
prop_mosq      <- mosq_prop_for_R0$prop
daily_bites    <- rep(bite_rate, num_mosq)
 ## Bump up Aedes aegypti to take into account the multiple feedings within a gonotrophic cycle (if Ae aegypti is present in the dataset)
if (length(grep("aegypti", dimnames(h_to_m_trans_all_samps_adj_to_com)[[3]])) > 0) {
daily_bites[grep("aegypti", dimnames(h_to_m_trans_all_samps_adj_to_com)[[3]])] <-   
  daily_bites[grep("aegypti", dimnames(h_to_m_trans_all_samps_adj_to_com)[[3]])] * 2
}
mos_surv       <- mosq_surv_for_R0_all_samps_adj_to_com
mosq_bite_pref <- mosq_bite_pref_all_samps_adj_to_com
R0.out         <- data.frame(
  model          = "Community_with_uncertainty"
, R0_primary     = numeric(n_samps)
, foi_on_A       = numeric(n_samps)
, foi_on_H       = numeric(n_samps)
, rel_foi_on_H   = numeric(n_samps)
, foi_from_A     = numeric(n_samps)
, foi_from_H     = numeric(n_samps)
, rel_foi_from_H = numeric(n_samps))

## Setup mosquito biting preference with and without including host abundance.
 ## Note: mosquito biting _preference_ is modeled as their intrinsic propensity to bite a given species, which is given as their proportional
  ## increase or decrease relative to 1 which designates no preference and an equivalence to biting that species randomly (in proportion to the
   ## relative abundance of that species in the community). These intrinsic preferences are then multiplied by the actual observed abundance and 
    ## scaled to a proportion. Note that this can be calculated assuming that all hosts are equally abundant, but this isn't very ecologically interesting
     ## because it isn't a real scenario
mosq_bite        <- array(dim = dim(mosq_bite_pref))
for (k in 1:n_samps) {
mosq_bite[,,k]   <- mosq_bite_pref[,,k] * 
  {
    if (use.host_abundance) {
      matrix(nrow = num_mosq, ncol = num_hosts, data = rep(prop_hosts[, k], num_mosq), byrow = TRUE)
    } else {
      matrix(nrow = num_mosq, ncol = num_hosts, data = 1/num_hosts, byrow = TRUE) 
    }
  }
 
mosq_bite[,,k]   <- sweep(mosq_bite[,,k], 1, rowSums(mosq_bite[,,k]), "/")
}

## Total number of new hosts infected by a mosquito over its lifespan...
mosq_trans_piece <- colSums(
  ## Set mosquito survival to vary by species ore set all species to the mean
  if (use.mosq_survival) {
  mosq_surv_for_R0_all_samps_adj_to_com * m_to_h_trans_all_samps_adj_to_com
  } else {
  array(dim = dim(mosq_surv_for_R0_all_samps_adj_to_com), data = rowMeans(mosq_surv_for_R0_all_samps_adj_to_com[,,1])) * m_to_h_trans_all_samps_adj_to_com
  }
  )

## ... which also includes a mosquitoes daily bite rate...
mosq_trans_piece <- mosq_trans_piece * matrix(ncol = n_samps, nrow = num_mosq, data = rep(daily_bites, n_samps))

## ... and the status of the hosts that the mosquitoes are biting:
  ## WAIFW_left is I mosquitoes infecting S hosts. Can consider mosquito biting preference, or some subset of that such as host abundance, or none of the above
WAIFW_left <- array(data = NA, dim = c(num_hosts, num_mosq, n_samps))

for (k in 1:n_samps) {
  
## Proportion of these hosts susceptible. From the disease's perspective a proportion of bites are "wasted" (don't lead to a new infection) in the 
 ## proportion of hosts that are seropositive already
if (use.mosq_bite_pref) {
  
 WAIFW_left[,,k] <- t(
     {
    if (use.host_seroprev) {
      ## Here use: mosquito biting preference (already scaled or not by host abundance) scaled by host seropositivity
     sweep(mosq_bite[,,k], 2, (1 - host_sero$prop_positive), "*") * 
        matrix(nrow = num_mosq, ncol = num_hosts, data = rep(mosq_trans_piece[, k], num_hosts))
    } else {
     ## Or just mosquito biting preference 
     mosq_bite[,,k] * 
        matrix(nrow = num_mosq, ncol = num_hosts, data = rep(mosq_trans_piece[, k], num_hosts))
    } 
     }
    )
   
} else {
  
 WAIFW_left[,,k] <- t(
   {
  if (use.host_abundance) {
    if (use.host_seroprev) {
      ## Here use host abundance, but assume mosquito biting is random in the community (bites only determined by host abundance)
      sweep(t(matrix(nrow = num_hosts, ncol = num_mosq, data = prop_hosts[, k])), 2, (1 - host_sero$prop_positive), "*") * 
        matrix(nrow = num_mosq, ncol = num_hosts, data = rep(mosq_trans_piece[, k], num_hosts))
    } else {
      ## Same thing, just don't weight by seropositivity
      t(matrix(nrow = num_hosts, ncol = num_mosq, data = prop_hosts[, k])) * 
        matrix(nrow = num_mosq, ncol = num_hosts, data = rep(mosq_trans_piece[, k], num_hosts))
    } 
  } else {
    if (use.host_seroprev) {
      sweep(t(matrix(nrow = num_hosts, ncol = num_mosq, data = 1/num_hosts)), 2, (1 - host_sero$prop_positive), "*") * 
        matrix(nrow = num_mosq, ncol = num_hosts, data = rep(mosq_trans_piece[, k], num_hosts))
    } else {
      t(matrix(nrow = num_hosts, ncol = num_mosq, data = 1/num_hosts)) *
        matrix(nrow = num_mosq, ncol = num_hosts, data = rep(mosq_trans_piece[, k], num_hosts))
    } 
  }
   }
    )
 
    }
}

## WAIFW_right is I hosts infecting S mosquitoes. Can consider just titer and infection probability or scale by the abundance of hosts and mosquitoes
 ## In brief to explain the logic here: since we have number of mosquitoes per host in the community, one infected host among many of its kind is assumed to be bit 
  ## at random by X mosquitoes per each of this host type (scaled by the mosquitoes biting preference etc.). This mosquito/host ratio trick and assumption of homogeneous
   ## mixing of the mosquito population and no preferential biting on I or S hosts translates one infected host to the total number of infected mosquitoes per day or 
    ## over that infected hosts infectious period
WAIFW_right_s1  <- apply(h_to_m_trans_all_samps_adj_to_com, 3:4, FUN = colSums) ## This can be interpreted as one mosquito of each type biting each host once per day, how many total mosquitoes of each type would be infected
WAIFW_right_s2  <- array(dim = dim(WAIFW_right_s1))
WAIFW_right     <- array(data = NA, dim = c(num_mosq, num_hosts, n_samps))

#####
## !! For full details on the calculation before see the notes page:  --- transmission_notes.R ---
## The calculation below is as in the notes page, order of operations are just a bit different
#####

for (k in 1:n_samps) {
  
## This step calculates the raw capability of _AN INFECTED_ host of _species X_ to infect each mosquito which is given by:
 ## The total number of mosquitoes this infected host infects over its infectious period, which is determined by:
  ## its titer on each day, and the number and identity of mosquitoes biting it each day, which in turn is determined by:
   ##  that mosquitoes capability of picking up infection, the abundance of each mosquito relative to this infected host, and the biting preference of these mosquitoes
  
## NOTE: If mosquito biting preference is NOT considered, the relative abundance of the species of the infected host is irrelevant for this 
 ## arm of the transmission cycle because N mosquitoes are simply biting randomly. However, if mosquito biting preference is considered (for which
  ## host abundance does play a role), the single infected host of species X may be bit more or less frequently than random 
temp_WAIFW <- {
  if (use.mosq_bite_pref) {
    WAIFW_right_s1[,,k] *
## mosquito bites on each host per day = the proportion of each of their bites on each host type * their daily biting rate
      t(mosq_bite[,,k]) *
      matrix(nrow = num_hosts, ncol = num_mosq, data = rep(daily_bites, num_hosts)) *
## Mosquito to host ratio (the number of S mosquitoes biting each infected host at the bite rate).
 ## Because mosquito _relative abundance_ is used (below), this m_to_h_ratio captures the sum of all mosquito individuals in the community
  ## relative to the sum of all host individuals in the community
      m_to_h_ratio
  } else {
    WAIFW_right_s1[,,k] * 
      matrix(nrow = num_hosts, ncol = num_mosq, data = 1/num_hosts) *
      matrix(nrow = num_hosts, ncol = num_mosq, data = rep(daily_bites, num_hosts)) *
      m_to_h_ratio
  }
}

WAIFW_right_s2[,,k] <- {
 ## mosquito _relative abundance_
  if (use.mosq_abundance) {
  temp_WAIFW * matrix(nrow = num_hosts, ncol = num_mosq, data = rep(prop_mosq, num_hosts), byrow = TRUE)
  } else {
  temp_WAIFW * matrix(nrow = num_hosts, ncol = num_mosq, data = 1/num_mosq) 
  }
}
  
## If scaling by the proportion of all hosts that show titer, multiply by that ratio to get the average hosts' response (i.e. some hosts are going to lead
 ## to no infection). By including this here we are assuming essentially that a host of each type becomes _exposed_ but there is some probability that that
  ## host is actually _infectious_
if (use.cond_titer) {
 WAIFW_right_s2[,,k]   <- sweep(WAIFW_right_s2[,,k], 1, prop_inf_for_R0$num_inf, "*")
}

 WAIFW_right[,,k]      <- t(WAIFW_right_s2[,,k])
}

## Rename WAIFW_right as host competence (host-mosquito, the first form of host competence) for use later
host_competence <- WAIFW_right
dimnames(host_competence) <- list(
  dimnames(h_to_m_trans_all_samps_adj_to_com)[[3]]
, dimnames(h_to_m_trans_all_samps_adj_to_com)[[2]]
, NULL)

## Calculate R0
physiol_mat <- array(dim = c(num_hosts, num_hosts, n_samps))
temp_mmat.f <- array(dim = c(num_hosts, num_hosts, n_samps))

## For host-to-host transmission which we are defining here as the second measure of host competence
FOI_on_t   <- matrix(nrow = n_samps, ncol = num_hosts, data = 0)
FOI_from_t <- matrix(nrow = n_samps, ncol = num_hosts, data = 0)

physiol_mat_mm <- array(dim = c(num_mosq, num_mosq, n_samps))
temp_mmat.f_mm <- array(dim = c(num_mosq, num_mosq, n_samps))
FOI_on_t_mm    <- matrix(nrow = n_samps, ncol = num_mosq, data = 0)
FOI_from_t_mm  <- matrix(nrow = n_samps, ncol = num_mosq, data = 0)

## Method for calculating R0 sticks these matrices in the diagonal of a larger matrix then takes the eigen

## For details on the matrix algebra component here see:
 ## source("matrix_algebra_exploration.R")

for (k in 1:n_samps) {
temp_mat           <- matrix(data = 0, nrow = num_hosts + num_mosq, ncol = num_hosts + num_mosq)
temp_mat[(num_hosts+1):(num_hosts + num_mosq), 1:num_hosts] <- WAIFW_right[,,k]
temp_mat[1:(num_hosts), (1+num_hosts):(num_hosts+num_mosq)] <- WAIFW_left[,,k]
R0                 <- Re(eigen(temp_mat)$values)
R0                 <- R0[which(R0 > 0)]
R0.out[k, 2]       <- R0[1]

## And for FOI, the pairwise host-host or mosquito-mosquito transmission capability:
temp_mmat           <- WAIFW_left[,,k] %*% WAIFW_right[,,k]
temp_mmat.f[,,k]    <- temp_mmat
physiol_mat[,,k]    <- temp_mmat
## New infected hosts of each type after a generation of host-mosquito-host. Will be all the same when there is no preference weighting
FOI_on_t[k, ]       <- rowSums(temp_mmat)
FOI_from_t[k, ]     <- colSums(temp_mmat)

## Full transmission pairwise identity matrix 
temp_mmat_mm        <- WAIFW_right[,,k] %*% WAIFW_left[,,k]
temp_mmat.f_mm[,,k] <- temp_mmat_mm
physiol_mat_mm[,,k] <- temp_mmat_mm

## FOI on and from using definition 1 from above 
FOI_on_t_mm[k, ]    <- rowSums(temp_mmat_mm)
FOI_from_t_mm[k, ]  <- colSums(temp_mmat_mm)

## The largest FOI value ON and FOI ON humans
R0.out[k, c(3, 4)] <- FOI_on_t[k, ][c(which(FOI_on_t == max(FOI_on_t[k, ]))[1], which(host_prop$host_species == "human"))]
## FOI ON humans relative to the average FOI ON
R0.out[k, 5]       <- FOI_on_t[k, ][which(host_prop$host_species == "human")] / mean(FOI_on_t[k, ])

## The largest FOI value FROM and FOI FROM humans
R0.out[k, c(6, 7)] <- FOI_from_t[k, ][c(which(FOI_from_t[k, ] == max(FOI_from_t[k, ]))[1], which(host_prop$host_species == "human"))]
## FOI FROM humans relative to the average FOI FROM
R0.out[k, 8]       <- FOI_from_t[k, ][which(host_prop$host_species == "human")] / mean(FOI_from_t[k, ])
}

####
## Calculate host and mosquito competence for just the one-step physiology (hosts ability to infect mosquitoes
## and mosquitoes ability to pick up infection from hosts)
####
source("host_mosq_comp_remaining.R")

####
## Summary statistics on FOI_on and FOI_from (which is in fact host-to-host transmission competence)
####
source("host_competence_cleanup.R")
source("mosq_competence_cleanup.R")

## And for all calculations stick together previous and current estimates
if (complexity_counter == 1) {
  
####
## Compile summary from hosts 
####
  
## Host competence as a host's ability to infect mosquitoes
host_competence.r.gg.f    <- host_competence.r.gg
host_competence.gg.f      <- host_competence.gg
## Host competence as a host's ability to complete the life cycle of host-host
host_competence.r.gg.hh.f <- host_competence.r.gg.hh
host_competence.gg.hh.f   <- host_competence.gg.hh
## FOI ON (host)
FOI_on_t.gg.f             <- FOI_on_t.gg
## Host to host transmission matrix
physiol_mat.gg.f          <- physiol_mat.gg

####
## Compile summary from mosquitoes
####

## Mosquito competence as a mosquitoes ability to pick up infection
mosq_competence.r.gg.f    <- mosq_competence.r.gg
mosq_competence.gg.f      <- mosq_competence.gg
## Mosquito competence as a mosquitoes ability to complete a mosquito-mosquito life cycle
mosq_competence.r.gg.mm.f <- mosq_competence.r.gg.mm
mosq_competence.gg.mm.f   <- mosq_competence.gg.mm
## FOI ON (mosq)
FOI_on_t.gg.f_mm          <- FOI_on_t.gg_mm
## mosquito to mosquito transmission matrix
physiol_mat.gg.f.mm       <- physiol_mat.gg_mm

} else {
  
host_competence.r.gg.f    <- rbind(host_competence.r.gg.f, host_competence.r.gg)
host_competence.gg.f      <- rbind(host_competence.gg.f, host_competence.gg)
host_competence.r.gg.hh.f <- rbind(host_competence.r.gg.hh.f, host_competence.r.gg.hh)
host_competence.gg.hh.f   <- rbind(host_competence.gg.hh.f, host_competence.gg.hh)
  
mosq_competence.r.gg.f    <- rbind(mosq_competence.r.gg.f, mosq_competence.r.gg.mm)
mosq_competence.gg.f      <- rbind(mosq_competence.gg.f, mosq_competence.gg.mm)
mosq_competence.r.gg.mm.f <- rbind(mosq_competence.r.gg.mm.f, mosq_competence.r.gg.mm)
mosq_competence.gg.mm.f   <- rbind(mosq_competence.gg.mm.f, mosq_competence.gg.mm)

FOI_on_t.gg.f_mm          <- rbind(FOI_on_t.gg.f_mm, FOI_on_t.gg_mm)
FOI_on_t.gg.f             <- rbind(FOI_on_t.gg.f, FOI_on_t.gg)
physiol_mat.gg.f          <- rbind(physiol_mat.gg.f, physiol_mat.gg)
physiol_mat.gg.f.mm       <- rbind(physiol_mat.gg.f.mm, physiol_mat.gg_mm)

}

print(
 paste(round(complexity_counter / nrow(model.runs), 2)*100, "%", "Complete", sep = " ") 
)
