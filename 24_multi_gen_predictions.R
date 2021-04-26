#####
## Calculate the number of host infected starting with one infected host of a given type after X generations
#####

## Much of this code is redundant from real_data_R0_2.R

## However, for this simulation, start from an S population
use.host_seroprev <- F

num_gen <- 5  ## Number of generations over which to run the simulation

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

## Takes maybe a minute for each host
which_host_multigen <- "human"
which_host_multigen <- which(dimnames(h_to_m_trans_all_samps_adj_to_com)[[2]] == which_host_multigen)

for (k in 1:n_samps) {

## mosquito to host
WAIFW_left.t       <- WAIFW_left[,,k]
## host to mosquito
WAIFW_right.t      <- WAIFW_right[,,k]

## Set up the data frame for the 
next_gen.gg.f <- data.frame(
  Next    = dimnames(h_to_m_trans_all_samps_adj_to_com)[[2]]
, Current = dimnames(h_to_m_trans_all_samps_adj_to_com)[[2]][which_host_multigen]
, R       = 0
, gen     = 0)

next_gen.gg.f[next_gen.gg.f$Next == dimnames(h_to_m_trans_all_samps_adj_to_com)[[2]][which_host_multigen], ]$R <- 1

for (m in 1:num_gen) {
  
## In generation one, we begin with just a single infected individual of a given host type, and after the matrix calculation determine
 ## how many of each host will be infected in the next generation
 if (m == 1) {
WAIFW_right.t[,-which_host_multigen] <- 0
next_gen           <- WAIFW_left.t %*% WAIFW_right.t
dimnames(next_gen) <- list(  
  dimnames(h_to_m_trans_all_samps_adj_to_com)[[2]]
, dimnames(h_to_m_trans_all_samps_adj_to_com)[[2]]
  )
next_gen.gg        <- melt(next_gen)
names(next_gen.gg) <- c("Next", "Current", "R")
next_gen.gg        <- next_gen.gg %>% mutate(gen = m)
next_gen.gg.f      <- rbind(next_gen.gg.f, next_gen.gg)
## In generation 2 + , use the number of newly infected hosts from the previous generation 
 } else {
WAIFW_right.t <- WAIFW_right[,,k] * matrix(data = rep(rowSums(next_gen), num_mosq), nrow = num_mosq, byrow = T)
next_gen      <- WAIFW_left.t %*% WAIFW_right.t
next_gen           <- WAIFW_left.t %*% WAIFW_right.t
dimnames(next_gen) <- list(  
  dimnames(h_to_m_trans_all_samps_adj_to_com)[[2]]
, dimnames(h_to_m_trans_all_samps_adj_to_com)[[2]]
  )
next_gen.gg        <- melt(next_gen)
names(next_gen.gg) <- c("Next", "Current", "R")
next_gen.gg        <- next_gen.gg %>% mutate(gen = m)
next_gen.gg.f      <- rbind(next_gen.gg.f, next_gen.gg)
 }

}

next_gen.gg.f <- next_gen.gg.f %>% mutate(samp = k)

if (k == 1) {
  next_gen.gg.f.f <- next_gen.gg.f
} else {
  next_gen.gg.f.f <- rbind(next_gen.gg.f.f, next_gen.gg.f)
}

}

## Takes maybe a minute for each host
which_mosq_multigen <- "ma_uniformis"
which_mosq_multigen <- which(dimnames(m_to_h_trans_all_samps_adj_to_com)[[2]] == which_mosq_multigen)

for (k in 1:n_samps) {

## mosquito to host
WAIFW_left.t       <- WAIFW_left[,,k]
## host to mosquito
WAIFW_right.t      <- WAIFW_right[,,k]

## Set up the data frame for the 
next_gen.gg.f.m <- data.frame(
  Next    = dimnames(m_to_h_trans_all_samps_adj_to_com)[[2]]
, Current = dimnames(m_to_h_trans_all_samps_adj_to_com)[[2]][which_mosq_multigen]
, R       = 0
, gen     = 0)

next_gen.gg.f.m[next_gen.gg.f.m$Next == dimnames(m_to_h_trans_all_samps_adj_to_com)[[2]][which_mosq_multigen], ]$R <- 1

for (m in 1:num_gen) {
  
## In generation one, we begin with just a single infected individual of a given host type, and after the matrix calculation determine
 ## how many of each host will be infected in the next generation
 if (m == 1) {
WAIFW_left.t[,-which_mosq_multigen] <- 0
next_gen           <- WAIFW_right.t %*% WAIFW_left.t
dimnames(next_gen) <- list(  
  dimnames(m_to_h_trans_all_samps_adj_to_com)[[2]]
, dimnames(m_to_h_trans_all_samps_adj_to_com)[[2]]
  )
next_gen.gg        <- melt(next_gen)
names(next_gen.gg) <- c("Next", "Current", "R")
next_gen.gg        <- next_gen.gg %>% mutate(gen = m)
next_gen.gg.f.m    <- rbind(next_gen.gg.f.m, next_gen.gg)
## In generation 2 + , use the number of newly infected mosquitoes from the previous generation 
 } else {
WAIFW_left.t  <- WAIFW_left[,,k] * matrix(data = rep(rowSums(next_gen), num_hosts), nrow = num_hosts, byrow = T)
next_gen      <- WAIFW_right.t %*% WAIFW_left.t
dimnames(next_gen) <- list(  
  dimnames(m_to_h_trans_all_samps_adj_to_com)[[2]]
, dimnames(m_to_h_trans_all_samps_adj_to_com)[[2]]
  )
next_gen.gg        <- melt(next_gen)
names(next_gen.gg) <- c("Next", "Current", "R")
next_gen.gg        <- next_gen.gg %>% mutate(gen = m)
next_gen.gg.f.m    <- rbind(next_gen.gg.f.m, next_gen.gg)
 }

}

next_gen.gg.f.m <- next_gen.gg.f.m %>% mutate(samp = k)

if (k == 1) {
  next_gen.gg.f.f.m <- next_gen.gg.f.m
} else {
  next_gen.gg.f.f.m <- rbind(next_gen.gg.f.f.m, next_gen.gg.f.m)
}

if (((k / 50) %% 1) == 0) { print(k) }

}

####
## Summarize and change the names for the multi-gen plots
####

next_gen.gg.f.f.s <- next_gen.gg.f.f %>% 
  group_by(Next, gen, samp) %>%
  summarize(tot_I = sum(R)) %>%
  ungroup() %>%
  group_by(Next, gen) %>%
  summarize(
    lwr = quantile(tot_I, 0.025)
  , est = quantile(tot_I, 0.500)
  , upr = quantile(tot_I, 0.975)
  )

host.names <- strsplit(as.character(next_gen.gg.f.f.s$Next), "_") %>% 
  sapply(., FUN = function(x) c(paste(x, collapse = " "))) %>% 
  sapply(., FUN = function(x) paste(toupper(substring(x, 1, 1)), substring(x, 2), sep = "", collapse = " "))

next_gen.gg.f.f.s$Next <- host.names

next_gen.gg.f.f.s2 <- next_gen.gg.f.f %>% 
  group_by(Next, Current, gen) %>%
  summarize(
    lwr = quantile(R, 0.025)
  , est = quantile(R, 0.500)
  , upr = quantile(R, 0.975)
  )

host.names <- strsplit(as.character(next_gen.gg.f.f.s2$Next), "_") %>% 
  sapply(., FUN = function(x) c(paste(x, collapse = " "))) %>% 
  sapply(., FUN = function(x) paste(toupper(substring(x, 1, 1)), substring(x, 2), sep = "", collapse = " "))

next_gen.gg.f.f.s2$Next <- host.names

host.names <- strsplit(as.character(next_gen.gg.f.f.s2$Current), "_") %>% 
  sapply(., FUN = function(x) c(paste(x, collapse = " "))) %>% 
  sapply(., FUN = function(x) paste(toupper(substring(x, 1, 1)), substring(x, 2), sep = "", collapse = " "))

next_gen.gg.f.f.s2$Current <- host.names

next_gen.gg.f.f.s2$Next <- factor(next_gen.gg.f.f.s2$Next, levels = rev(AUC_titer.gg.s$Host))
next_gen.gg.f.f.s2$Current <- factor(next_gen.gg.f.f.s2$Current, levels = rev(AUC_titer.gg.s$Host))

## Store individual multi-gen starting hosts
next_gen.gg.f.f.s.h <- next_gen.gg.f.f.s
next_gen.gg.f.f.s.h$Next <- factor(next_gen.gg.f.f.s.h$Next, levels = rev(AUC_titer.gg.s$Host))

### Mosquitoes 
next_gen.gg.f.f.s.m <- next_gen.gg.f.f.m %>% 
  group_by(Next, gen, samp) %>%
  summarize(tot_I = sum(R)) %>%
  ungroup() %>%
  group_by(Next, gen) %>%
  summarize(
    lwr = quantile(tot_I, 0.025)
  , est = quantile(tot_I, 0.500)
  , upr = quantile(tot_I, 0.975)
  )

mosq.names <- strsplit(as.character(next_gen.gg.f.f.s.m$Next), "_") %>% 
  sapply(., FUN = function(x) c(paste(x, collapse = " "))) %>% 
  sapply(., FUN = function(x) paste(toupper(substring(x, 1, 1)), substring(x, 2), sep = "", collapse = " "))

next_gen.gg.f.f.s.m$Next <- mosq.names

next_gen.gg.f.f.s2.m <- next_gen.gg.f.f.m %>% 
  group_by(Next, Current, gen) %>%
  summarize(
    lwr = quantile(R, 0.025)
  , est = quantile(R, 0.500)
  , upr = quantile(R, 0.975)
  )

mosq.names <- strsplit(as.character(next_gen.gg.f.f.s2.m$Next), "_") %>% 
  sapply(., FUN = function(x) c(paste(x, collapse = " "))) %>% 
  sapply(., FUN = function(x) paste(toupper(substring(x, 1, 1)), substring(x, 2), sep = "", collapse = " "))

next_gen.gg.f.f.s2.m$Next <- mosq.names

mosq.names <- strsplit(as.character(next_gen.gg.f.f.s2.m$Current), "_") %>% 
  sapply(., FUN = function(x) c(paste(x, collapse = " "))) %>% 
  sapply(., FUN = function(x) paste(toupper(substring(x, 1, 1)), substring(x, 2), sep = "", collapse = " "))

next_gen.gg.f.f.s2.m$Current <- mosq.names

next_gen.gg.f.f.s2.m$Next    <- factor(next_gen.gg.f.f.s2.m$Next, levels = rev(mosq_inf_AUC_all_samps_adj_to_com.gg.s$mosq))
next_gen.gg.f.f.s2.m$Current <- factor(next_gen.gg.f.f.s2.m$Current, levels = rev(mosq_inf_AUC_all_samps_adj_to_com.gg.s$mosq))
