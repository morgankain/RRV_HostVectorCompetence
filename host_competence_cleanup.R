## To get the total number of hosts infected by each host, use the sum over the identities of those hosts infected by each initial host infection
host_competence.one.hh           <- t(FOI_from_t)
dimnames(host_competence.one.hh) <- list(
  dimnames(host_competence.one)[[1]]
, NULL)

## Classic way to get CI
if (summary.type == "raw") {
  
host_competence.one.ci.hh <- data.frame(
  host        = dimnames(host_competence.one.hh)[[1]]
, comp.est    = apply(host_competence.one.hh   , 1, FUN = function (x) quantile(x, 0.500))
, comp.lwr    = apply(host_competence.one.hh   , 1, FUN = function (x) quantile(x, 0.025))
, comp.upr    = apply(host_competence.one.hh   , 1, FUN = function (x) quantile(x, 0.975))) 
  
} else if (summary.type == "proportion") {
  
host_competence.one.prop.hh        <- melt(t(host_competence.one.hh))
names(host_competence.one.prop.hh) <- c("sample", "host", "est")
host_competence.one.prop.hh        <- host_competence.one.prop.hh %>%
  group_by(sample) %>%
  mutate(prop_est = est / max(est))
  
host_competence.one.ci.hh <- host_competence.one.prop.hh %>%
  group_by(host) %>%
  summarize(
    comp.est = quantile(prop_est, 0.500)
  , comp.lwr = quantile(prop_est, 0.025)
  , comp.upr = quantile(prop_est, 0.975))
  
} else {
  print("summary.type option not supported"); break
}

host_competence.one.ci.hh <- host_competence.one.ci.hh[order(host_competence.one.ci.hh$comp.est), ]
host_competence.one.ci.hh <- transform(host_competence.one.ci.hh, host = factor(host, levels = host))

## So also just melt the raw and produce a violin plot to show the actual density
host_competence.gg.hh           <- melt(host_competence.one.hh)
names(host_competence.gg.hh)    <- c("host", "sample", "comp")

## If proportional competence is desired, calculate that now
if (summary.type == "proportion") {
  host_competence.gg.hh <- host_competence.gg.hh %>% 
    group_by(sample) %>% 
    mutate(comp = comp / max(comp))
}

host_competence.gg.ord.hh       <- host_competence.gg.hh %>% group_by(host) %>% summarize(med = median(comp))
host_competence.gg.ord.hh       <- host_competence.gg.ord.hh[order(host_competence.gg.ord.hh$med), ]
host_competence.gg.ord.hh       <- transform(host_competence.gg.ord.hh, host = factor(host, levels = host))
host_competence.gg.hh           <- transform(host_competence.gg.hh, host = factor(host, levels = host_competence.gg.ord.hh$host))

## Alternative rank based approach
host_competence.one.r.hh                   <- apply(host_competence.one.hh, 2, FUN = function(x) num_hosts + 1 - rank(x))

## Classic way to get CI, good for raw, but not positive about this for CI on ranks
host_competence.one.ci.r.hh <- data.frame(
  host        = dimnames(host_competence.one.r.hh)[[1]]
, comp.est    = apply(host_competence.one.r.hh   , 1, FUN = function (x) quantile(x, 0.500))
, comp.lwr    = apply(host_competence.one.r.hh   , 1, FUN = function (x) quantile(x, 0.025))
, comp.upr    = apply(host_competence.one.r.hh   , 1, FUN = function (x) quantile(x, 0.975)))

host_competence.one.ci.r.hh <- host_competence.one.ci.r.hh[order(host_competence.one.ci.r.hh$comp.est, decreasing = TRUE), ]
host_competence.one.ci.r.hh <- transform(host_competence.one.ci.r.hh, host = factor(host, levels = host))

## So also just melt the raw and produce a violin plot to show the actual density
host_competence.r.gg.hh           <- melt(host_competence.one.r.hh)
names(host_competence.r.gg.hh)    <- c("host", "sample", "comp")
host_competence.r.gg.ord.hh       <- host_competence.r.gg.hh %>% group_by(host) %>% summarize(med = median(comp))
host_competence.r.gg.ord.hh       <- host_competence.r.gg.ord.hh[order(host_competence.r.gg.ord.hh$med), ]
host_competence.r.gg.ord.hh       <- transform(host_competence.r.gg.ord.hh, host = factor(host, levels = host))
host_competence.r.gg.hh           <- transform(host_competence.r.gg.hh, host = factor(host, levels = host_competence.r.gg.ord.hh$host))

 ## Tally number of times a mosquito shows up as a given rank, and cut off those with < 0.025 representation,
  ## effectively chopping off the 5% 
host_competence.r.gg.s.hh <- host_competence.r.gg.hh %>% 
  group_by(host, comp) %>%
  tally() %>% 
  mutate(cum_dens_up = cumsum(n)) 
host_competence.r.gg.s.hh <- host_competence.r.gg.s.hh[order(host_competence.r.gg.s.hh$comp, decreasing = TRUE), ]
host_competence.r.gg.s.hh <- host_competence.r.gg.s.hh %>%
  group_by(host) %>%
  mutate(cum_dens_down = cumsum(n)) 
host_competence.r.gg.s.hh <- host_competence.r.gg.s.hh[order(host_competence.r.gg.s.hh$host), ]
host_competence.r.gg.s.hh <- host_competence.r.gg.s.hh %>% 
  filter(!(cum_dens_up > 975 & cum_dens_down < 25 & n < 25))

host_competence.r.gg.hh %<>% mutate(
  use.host_abundance = use.host_abundance
, use.mosq_abundance = use.mosq_abundance
, use.mosq_bite_pref = use.mosq_bite_pref
, use.mosq_survival  = use.mosq_survival
, use.host_seroprev  = use.host_seroprev
, use.cond_titer     = use.cond_titer
, model.complexity   = model.complexity
)

if (one_off) {
  host_competence.r.gg.hh %<>% mutate(
    which_missing = {
      if (length(which(model.runs[complexity_counter, ] == FALSE)) > 0) {
        strsplit(colnames(model.runs)[which(model.runs[complexity_counter, ] == FALSE)], split = "[.]")[[1]][2]
      } else {
        "None"
      }
    }
  )
}

## For plots or relative quantitative values, also need to store the actual estimated competence of hosts
host_competence.gg.hh %<>% mutate(
  use.host_abundance = use.host_abundance
, use.mosq_abundance = use.mosq_abundance
, use.mosq_bite_pref = use.mosq_bite_pref
, use.mosq_survival  = use.mosq_survival
, use.host_seroprev  = use.host_seroprev
, use.cond_titer     = use.cond_titer
, model.complexity   = model.complexity
)

if (one_off) {
  host_competence.gg.hh %<>% mutate(
    which_missing = {
      if (length(which(model.runs[complexity_counter, ] == FALSE)) > 0) {
        strsplit(colnames(model.runs)[which(model.runs[complexity_counter, ] == FALSE)], split = "[.]")[[1]][2]
      } else {
        "None"
      }
    }
  )
}


####
## Also calculate FOI (R) ON
####

dimnames(FOI_on_t) <- list(
  seq(1, n_sims)
, dimnames(prop_hosts)[[1]])

FOI_on_t.gg          <- melt(FOI_on_t)
names(FOI_on_t.gg)   <- c("rep", "host", "FOI_from")

FOI_on_t.gg %<>% mutate(
  use.host_abundance = use.host_abundance
, use.mosq_abundance = use.mosq_abundance
, use.mosq_bite_pref = use.mosq_bite_pref
, use.mosq_survival  = use.mosq_survival
, use.host_seroprev  = use.host_seroprev
, use.cond_titer     = use.cond_titer
, model.complexity   = model.complexity)

if (one_off) {
  FOI_on_t.gg %<>% mutate(
    which_missing = {
      if (length(which(model.runs[complexity_counter, ] == FALSE)) > 0) {
        strsplit(colnames(model.runs)[which(model.runs[complexity_counter, ] == FALSE)], split = "[.]")[[1]][2]
      } else {
        "None"
      }
    }
  )
}

####
## Also, summarize host_to_host matrix for ggplot
####

dimnames(physiol_mat) <- list(
  dimnames(prop_hosts)[[1]]
, dimnames(prop_hosts)[[1]]
, NULL)

physiol_mat.gg        <- melt(physiol_mat)
names(physiol_mat.gg) <- c("G2", "G1", "Run", "Count")

if (summary.type == "raw") {
  
physiol_mat.gg        <- physiol_mat.gg %>% 
  group_by(G2, G1) %>% summarize(
  comp.est    = quantile(Count, 0.500)
, comp.lwr    = quantile(Count, 0.025)
, comp.upr    = quantile(Count, 0.975))
  
} else if (summary.type == "proportion") {
  
physiol_mat.gg        <- physiol_mat.gg %>% 
  group_by(G2, Run) %>%
  mutate(prop_est = Count / max(Count)) %>%
  ungroup(Run) %>%
  group_by(G1, G2) %>%
  summarize(
  comp.est    = quantile(prop_est, 0.500, na.rm = T)
, comp.lwr    = quantile(prop_est, 0.025, na.rm = T)
, comp.upr    = quantile(prop_est, 0.975, na.rm = T))
  
physiol_mat.gg[physiol_mat.gg$G1 == "dog" | physiol_mat.gg$G1 == "cat", ][, -c(1, 2)] <- 0

} else {
  print("summary.type option not supported"); break
}

host.names.G1 <- strsplit(as.character(physiol_mat.gg$G1), "_") %>% 
  sapply(., FUN = function(x) c(paste(x, collapse = " "))) %>% 
  sapply(., FUN = function(x) paste(toupper(substring(x, 1, 1)), substring(x, 2), sep = "", collapse = " "))
host.names.G2 <- strsplit(as.character(physiol_mat.gg$G2), "_") %>% 
  sapply(., FUN = function(x) c(paste(x, collapse = " "))) %>% 
  sapply(., FUN = function(x) paste(toupper(substring(x, 1, 1)), substring(x, 2), sep = "", collapse = " "))

physiol_mat.gg$G1 <- host.names.G1 
physiol_mat.gg$G2 <- host.names.G2

physiol_mat.gg %<>% mutate(
  use.host_abundance = use.host_abundance
, use.mosq_abundance = use.mosq_abundance
, use.mosq_bite_pref = use.mosq_bite_pref
, use.mosq_survival  = use.mosq_survival
, use.host_seroprev  = use.host_seroprev
, use.cond_titer     = use.cond_titer
, model.complexity   = model.complexity)

if (one_off) {
  physiol_mat.gg %<>% mutate(
    which_missing = {
      if (length(which(model.runs[complexity_counter, ] == FALSE)) > 0) {
        strsplit(colnames(model.runs)[which(model.runs[complexity_counter, ] == FALSE)], split = "[.]")[[1]][2]
      } else {
        "None"
      }
    }
  )
}

