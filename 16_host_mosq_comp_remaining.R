####
## Calculate a series of summary stats using host competence
####

## To get the total number of mosquitoes infected by each host, sum over each mosquito
host_competence.one                   <- apply(host_competence, 3, FUN = colSums)

## Classic way to get CI
if (summary.type == "raw") {
  
host_competence.one.ci <- data.frame(
  host        = dimnames(host_competence.one)[[1]]
, comp.est    = apply(host_competence.one   , 1, FUN = function (x) quantile(x, 0.500))
, comp.lwr    = apply(host_competence.one   , 1, FUN = function (x) quantile(x, 0.025))
, comp.upr    = apply(host_competence.one   , 1, FUN = function (x) quantile(x, 0.975)))  

} else if (summary.type == "proportion") {
  
host_competence.one.prop        <- melt(t(host_competence.one))
names(host_competence.one.prop) <- c("sample", "host", "est")
host_competence.one.prop        <- host_competence.one.prop %>%
  group_by(sample) %>%
  mutate(prop_est = est / max(est))
  
host_competence.one.ci <- host_competence.one.prop %>%
  group_by(host) %>%
  summarize(
    comp.est = quantile(prop_est, 0.500)
  , comp.lwr = quantile(prop_est, 0.025)
  , comp.upr = quantile(prop_est, 0.975))

} else {
  print("summary.type option not supported"); break
}

host_competence.one.ci <- host_competence.one.ci[order(host_competence.one.ci$comp.est), ]
host_competence.one.ci <- transform(host_competence.one.ci, host = factor(host, levels = host))

## So also just melt the raw and produce a violin plot to show the actual density
host_competence.gg           <- melt(host_competence.one)

names(host_competence.gg)    <- c("host", "sample", "comp")

## If proportional competence is desired, calculate that now
if (summary.type == "proportion") {
  host_competence.gg <- host_competence.gg %>% 
    group_by(sample) %>% 
    mutate(comp = comp / max(comp))
}

host_competence.gg.ord       <- host_competence.gg %>% group_by(host) %>% summarize(med = median(comp))
host_competence.gg.ord       <- host_competence.gg.ord[order(host_competence.gg.ord$med), ]
host_competence.gg.ord       <- transform(host_competence.gg.ord, host = factor(host, levels = host))
host_competence.gg           <- transform(host_competence.gg, host = factor(host, levels = host_competence.gg.ord$host))

## Alternative rank based approach
host_competence.one.r                   <- apply(host_competence.one, 2, FUN = function(x) num_hosts + 1 - rank(x))

## Classic way to get CI, good for raw, but not positive about this for CI on ranks
host_competence.one.ci.r <- data.frame(
  host        = dimnames(host_competence.one.r)[[1]]
, comp.est    = apply(host_competence.one.r   , 1, FUN = function (x) quantile(x, 0.500))
, comp.lwr    = apply(host_competence.one.r   , 1, FUN = function (x) quantile(x, 0.025))
, comp.upr    = apply(host_competence.one.r   , 1, FUN = function (x) quantile(x, 0.975)))

host_competence.one.ci.r <- host_competence.one.ci.r[order(host_competence.one.ci.r$comp.est, decreasing = TRUE), ]
host_competence.one.ci.r <- transform(host_competence.one.ci.r, host = factor(host, levels = host))

## So also just melt the raw and produce a violin plot to show the actual density
host_competence.r.gg           <- melt(host_competence.one.r)
names(host_competence.r.gg)    <- c("host", "sample", "comp")
host_competence.r.gg.ord       <- host_competence.r.gg %>% group_by(host) %>% summarize(med = median(comp))
host_competence.r.gg.ord       <- host_competence.r.gg.ord[order(host_competence.r.gg.ord$med), ]
host_competence.r.gg.ord       <- transform(host_competence.r.gg.ord, host = factor(host, levels = host))
host_competence.r.gg           <- transform(host_competence.r.gg, host = factor(host, levels = host_competence.r.gg.ord$host))

 ## Tally number of times a mosquito shows up as a given rank, and cut off those with < 0.025 representation,
  ## effectively chopping off the 5% 
host_competence.r.gg.s <- host_competence.r.gg %>% 
  group_by(host, comp) %>%
  tally() %>% 
  mutate(cum_dens_up = cumsum(n)) 
host_competence.r.gg.s <- host_competence.r.gg.s[order(host_competence.r.gg.s$comp, decreasing = TRUE), ]
host_competence.r.gg.s <- host_competence.r.gg.s %>%
  group_by(host) %>%
  mutate(cum_dens_down = cumsum(n)) 
host_competence.r.gg.s <- host_competence.r.gg.s[order(host_competence.r.gg.s$host), ]
host_competence.r.gg.s <- host_competence.r.gg.s %>% 
  filter(!(cum_dens_up > 975 & cum_dens_down < 25 & n < 25))

####
## Do the same for mosquitoes (i.e. Here mosquito competence is defined as a mosquitoes ability to pick up infection from hosts)
####

mosq_competence_s1    <- apply(h_to_m_trans_all_samps_adj_to_com, 3:4, FUN = colSums)
mosq_competence_s2    <- array(dim = dim(mosq_competence_s1))
mosq_competence       <- array(data = NA, dim = c(num_mosq, num_hosts, n_samps))

if (use.cond_titer) {
for (i in 1:num_mosq) {
 mosq_competence_s1[,i,] <- sweep(mosq_competence_s1[,i,], 1, prop_inf_for_R0$num_inf, "*")
}
}

for (k in 1:n_samps) {
  
## Same as before, scale by mosquito biting parameters
mosq_competence_s2[,,k] <- {
  
  if (use.mosq_bite_pref) {
  mosq_competence_s1[,,k] * t(mosq_bite[,,k]) * 
  matrix(nrow = num_hosts, ncol = num_mosq, data = rep(daily_bites, num_hosts)) *
        m_to_h_ratio
  } else {
  mosq_competence_s1[,,k] * matrix(nrow = num_hosts, ncol = num_mosq, data = 1/num_hosts) * 
  matrix(nrow = num_hosts, ncol = num_mosq, data = rep(daily_bites, num_hosts)) *
        m_to_h_ratio
  } 
}

mosq_competence_s2[,,k] <- {
  
if (use.mosq_abundance) {
  mosq_competence_s2[,,k] * matrix(nrow = num_hosts, ncol = num_mosq, data = rep(prop_mosq, num_hosts), byrow = TRUE)
} else {
  mosq_competence_s2[,,k] * matrix(nrow = num_hosts, ncol = num_mosq, data = 1/num_mosq) 
}

}

mosq_competence[,,k]  <- t(mosq_competence_s2[,,k])

}

mosq_competence                   <- apply(mosq_competence, 3, FUN = rowMeans)
dimnames(mosq_competence)[[1]]    <- dimnames(h_to_m_trans_all_samps_adj_to_com)[[3]]
 
## Classic way to get CI, good for raw, but not sure about for ranks (see lower below) but not so certain about this when we are getting CI on ranks
if (summary.type == "raw") {

mosq_competence.ci <- data.frame(
  mosq        = dimnames(h_to_m_trans_all_samps_adj_to_com)[[3]]
, comp.est    = apply(mosq_competence   , 1, FUN = function (x) quantile(x, 0.500))
, comp.lwr    = apply(mosq_competence   , 1, FUN = function (x) quantile(x, 0.025))
, comp.upr    = apply(mosq_competence   , 1, FUN = function (x) quantile(x, 0.975)))

} else if (summary.type == "proportion") {
  
mosq_competence.prop        <- melt(t(mosq_competence))
names(mosq_competence.prop) <- c("sample", "mosq", "est")
mosq_competence.prop        <- mosq_competence.prop %>%
  group_by(sample) %>%
  mutate(prop_est = est / max(est))
  
mosq_competence.ci <- mosq_competence.prop %>%
  group_by(mosq) %>%
  summarize(
    comp.est = quantile(prop_est, 0.500)
  , comp.lwr = quantile(prop_est, 0.025)
  , comp.upr = quantile(prop_est, 0.975))
  
} else {
  print("summary.type option not supported"); break
}

mosq_competence.ci <- mosq_competence.ci[order(mosq_competence.ci$comp.est), ]
mosq_competence.ci <- transform(mosq_competence.ci, mosq = factor(mosq, levels = mosq))

## So also just melt the raw and produce a violin plot to show the actual density
mosq_competence.gg           <- melt(mosq_competence)
names(mosq_competence.gg)    <- c("mosq", "sample", "comp")

## If proportional competence is desired, calculate that now
if (summary.type == "proportion") {
mosq_competence.gg <- mosq_competence.gg %>% 
    group_by(sample) %>% 
    mutate(comp = comp / max(comp))
}

mosq_competence.gg.ord       <- mosq_competence.gg %>% group_by(mosq) %>% summarize(med = median(comp))
mosq_competence.gg.ord       <- mosq_competence.gg.ord[order(mosq_competence.gg.ord$med), ]
mosq_competence.gg.ord       <- transform(mosq_competence.gg.ord, mosq = factor(mosq, levels = mosq))
mosq_competence.gg           <- transform(mosq_competence.gg, mosq = factor(mosq, levels = mosq_competence.gg.ord$mosq))

## Alternative rank based approach
mosq_competence.r                   <- apply(mosq_competence, 2, FUN = function(x) num_mosq + 1 - rank(x))
dimnames(mosq_competence.r)[[1]]    <- dimnames(h_to_m_trans_all_samps_adj_to_com)[[3]]

## Classic way to get CI, good for raw, but not sure about for ranks here
mosq_competence.ci.r <- data.frame(
  mosq        = dimnames(h_to_m_trans_all_samps_adj_to_com)[[3]]
, comp.est    = apply(mosq_competence.r   , 1, FUN = function (x) quantile(x, 0.500))
, comp.lwr    = apply(mosq_competence.r   , 1, FUN = function (x) quantile(x, 0.025))
, comp.upr    = apply(mosq_competence.r   , 1, FUN = function (x) quantile(x, 0.975)))

mosq_competence.ci.r <- mosq_competence.ci.r[order(mosq_competence.ci.r$comp.est, decreasing = TRUE), ]
mosq_competence.ci.r <- transform(mosq_competence.ci.r, mosq = factor(mosq, levels = mosq))

## So also just melt the raw and produce a violin plot to show the actual density
mosq_competence.r.gg           <- melt(mosq_competence.r)
names(mosq_competence.r.gg)    <- c("mosq", "sample", "comp")
mosq_competence.r.gg.ord       <- mosq_competence.r.gg %>% group_by(mosq) %>% summarize(med = median(comp))
mosq_competence.r.gg.ord       <- mosq_competence.r.gg.ord[order(mosq_competence.r.gg.ord$med), ]
mosq_competence.r.gg.ord       <- transform(mosq_competence.r.gg.ord, mosq = factor(mosq, levels = mosq))
mosq_competence.r.gg           <- transform(mosq_competence.r.gg, mosq = factor(mosq, levels = mosq_competence.r.gg.ord$mosq))

## remove outer densities, just try from one data frame for now
 ## Tally number of times a mosquito shows up as a given rank, and cut off those with < 0.025 representation,
  ## effectively chopping off the 5% 
mosq_competence.r.gg.s <- mosq_competence.r.gg %>% 
  group_by(mosq, comp) %>%
  tally() %>% 
  mutate(cum_dens_up = cumsum(n)) 
mosq_competence.r.gg.s <- mosq_competence.r.gg.s[order(mosq_competence.r.gg.s$comp, decreasing = TRUE), ]
mosq_competence.r.gg.s <- mosq_competence.r.gg.s %>%
  group_by(mosq) %>%
  mutate(cum_dens_down = cumsum(n)) 
mosq_competence.r.gg.s <- mosq_competence.r.gg.s[order(mosq_competence.r.gg.s$mosq), ]
mosq_competence.r.gg.s <- mosq_competence.r.gg.s %>% 
  filter(!(cum_dens_up > 975 & cum_dens_down < 25 & n < 25))

####
## Finally, for both hosts and mosquitoes summarize for the ecological characteristics currently being considered
####

mosq_competence.r.gg %<>% mutate(
  use.host_abundance = use.host_abundance
, use.mosq_abundance = use.mosq_abundance
, use.mosq_bite_pref = use.mosq_bite_pref
, use.mosq_survival  = use.mosq_survival
, use.host_seroprev  = use.host_seroprev
, use.cond_titer     = use.cond_titer
, model.complexity   = model.complexity
)

if (one_off) {
  mosq_competence.r.gg %<>% mutate(
    which_missing = {
      if (length(which(model.runs[complexity_counter, ] == FALSE)) > 0) {
        strsplit(colnames(model.runs)[which(model.runs[complexity_counter, ] == FALSE)], split = "[.]")[[1]][2]
      } else {
        "None"
      }
    }
  )
}

mosq_competence.gg %<>% mutate(
  use.host_abundance = use.host_abundance
, use.mosq_abundance = use.mosq_abundance
, use.mosq_bite_pref = use.mosq_bite_pref
, use.mosq_survival  = use.mosq_survival
, use.host_seroprev  = use.host_seroprev
, use.cond_titer     = use.cond_titer
, model.complexity   = model.complexity
)

if (one_off) {
  mosq_competence.gg %<>% mutate(
    which_missing = {
      if (length(which(model.runs[complexity_counter, ] == FALSE)) > 0) {
        strsplit(colnames(model.runs)[which(model.runs[complexity_counter, ] == FALSE)], split = "[.]")[[1]][2]
      } else {
        "None"
      }
    }
  )
}

host_competence.r.gg %<>% mutate(
  use.host_abundance = use.host_abundance
, use.mosq_abundance = use.mosq_abundance
, use.mosq_bite_pref = use.mosq_bite_pref
, use.mosq_survival  = use.mosq_survival
, use.host_seroprev  = use.host_seroprev
, use.cond_titer     = use.cond_titer
, model.complexity   = model.complexity
)

if (one_off) {
  host_competence.r.gg %<>% mutate(
    which_missing = {
      if (length(which(model.runs[complexity_counter, ] == FALSE)) > 0) {
        strsplit(colnames(model.runs)[which(model.runs[complexity_counter, ] == FALSE)], split = "[.]")[[1]][2]
      } else {
        "None"
      }
    }
  )
}

host_competence.gg %<>% mutate(
  use.host_abundance = use.host_abundance
, use.mosq_abundance = use.mosq_abundance
, use.mosq_bite_pref = use.mosq_bite_pref
, use.mosq_survival  = use.mosq_survival
, use.host_seroprev  = use.host_seroprev
, use.cond_titer     = use.cond_titer
, model.complexity   = model.complexity
)

if (one_off) {
  host_competence.gg %<>% mutate(
    which_missing = {
      if (length(which(model.runs[complexity_counter, ] == FALSE)) > 0) {
        strsplit(colnames(model.runs)[which(model.runs[complexity_counter, ] == FALSE)], split = "[.]")[[1]][2]
      } else {
        "None"
      }
    }
  )
}
