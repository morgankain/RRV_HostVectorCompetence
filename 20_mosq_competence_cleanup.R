## To get the total number of mosquitoes infected by each mosquito, use the sum over the identities of those mosquitoes infected by each initial mosquito infection
mosq_competence.mm           <- t(FOI_from_t_mm)
dimnames(mosq_competence.mm) <- list(
  dimnames(h_to_m_trans_all_samps_adj_to_com)[[3]]
, NULL 
)

## Classic way to get CI, good for raw, but not sure about for ranks (see lower below) but not so certain about this when we are getting CI on ranks
mosq_competence.ci.mm <- data.frame(
  mosq        = dimnames(h_to_m_trans_all_samps_adj_to_com)[[3]]
, comp.est    = apply(mosq_competence.mm   , 1, FUN = function (x) quantile(x, 0.500))
, comp.lwr    = apply(mosq_competence.mm   , 1, FUN = function (x) quantile(x, 0.025))
, comp.upr    = apply(mosq_competence.mm   , 1, FUN = function (x) quantile(x, 0.975)))

mosq_competence.ci.mm <- mosq_competence.ci.mm[order(mosq_competence.ci.mm$comp.est), ]
mosq_competence.ci.mm <- transform(mosq_competence.ci.mm, mosq = factor(mosq, levels = mosq))

## So also just melt the raw and produce a violin plot to show the actual density
mosq_competence.gg.mm           <- melt(mosq_competence.mm)
names(mosq_competence.gg.mm)    <- c("mosq", "sample", "comp")

## If proportional competence is desired, calculate that now
if (summary.type == "proportion") {
  mosq_competence.gg.mm <- mosq_competence.gg.mm %>% 
    group_by(sample) %>% 
    mutate(comp = comp / max(comp))
}

mosq_competence.gg.ord.mm       <- mosq_competence.gg.mm %>% group_by(mosq) %>% summarize(med = median(comp))
mosq_competence.gg.ord.mm       <- mosq_competence.gg.ord.mm[order(mosq_competence.gg.ord.mm$med), ]
mosq_competence.gg.ord.mm       <- transform(mosq_competence.gg.ord.mm, mosq = factor(mosq, levels = mosq))
mosq_competence.gg.mm           <- transform(mosq_competence.gg.mm, mosq = factor(mosq, levels = mosq_competence.gg.ord.mm$mosq))

## Alternative rank based approach
mosq_competence.r.mm                   <- apply(mosq_competence.mm, 2, FUN = function(x) num_mosq + 1 - rank(x))
dimnames(mosq_competence.r.mm)[[1]]    <- dimnames(h_to_m_trans_all_samps_adj_to_com)[[3]]

## Classic way to get CI, good for raw, but not sure about for ranks here
mosq_competence.ci.r.mm <- data.frame(
  mosq        = dimnames(h_to_m_trans_all_samps_adj_to_com)[[3]]
, comp.est    = apply(mosq_competence.r.mm   , 1, FUN = function (x) quantile(x, 0.500))
, comp.lwr    = apply(mosq_competence.r.mm   , 1, FUN = function (x) quantile(x, 0.025))
, comp.upr    = apply(mosq_competence.r.mm   , 1, FUN = function (x) quantile(x, 0.975)))

mosq_competence.ci.r.mm <- mosq_competence.ci.r.mm[order(mosq_competence.ci.r.mm$comp.est, decreasing = TRUE), ]
mosq_competence.ci.r.mm <- transform(mosq_competence.ci.r.mm, mosq = factor(mosq, levels = mosq))

## So also just melt the raw and produce a violin plot to show the actual density
mosq_competence.r.gg.mm           <- melt(mosq_competence.r.mm)
names(mosq_competence.r.gg.mm)    <- c("mosq", "sample", "comp")
mosq_competence.r.gg.ord.mm       <- mosq_competence.r.gg.mm %>% group_by(mosq) %>% summarize(med = median(comp))
mosq_competence.r.gg.ord.mm       <- mosq_competence.r.gg.ord.mm[order(mosq_competence.r.gg.ord.mm$med), ]
mosq_competence.r.gg.ord.mm       <- transform(mosq_competence.r.gg.ord.mm, mosq = factor(mosq, levels = mosq))
mosq_competence.r.gg.mm           <- transform(mosq_competence.r.gg.mm, mosq = factor(mosq, levels = mosq_competence.r.gg.ord.mm$mosq))

## remove outer densities, just try from one data frame for now
 ## Tally number of times a mosquito shows up as a given rank, and cut off those with < 0.025 representation,
  ## effectively chopping off the 5% 
mosq_competence.r.gg.s.mm <- mosq_competence.r.gg.mm %>% 
  group_by(mosq, comp) %>%
  tally() %>% 
  mutate(cum_dens_up = cumsum(n)) 
mosq_competence.r.gg.s.mm <- mosq_competence.r.gg.s.mm[order(mosq_competence.r.gg.s.mm$comp, decreasing = TRUE), ]
mosq_competence.r.gg.s.mm <- mosq_competence.r.gg.s.mm %>%
  group_by(mosq) %>%
  mutate(cum_dens_down = cumsum(n)) 
mosq_competence.r.gg.s.mm <- mosq_competence.r.gg.s.mm[order(mosq_competence.r.gg.s.mm$mosq), ]
mosq_competence.r.gg.s.mm <- mosq_competence.r.gg.s.mm %>% 
  filter(!(cum_dens_up > 975 & cum_dens_down < 25 & n < 25))

mosq_competence.r.gg.mm %<>% mutate(
  use.host_abundance = use.host_abundance
, use.mosq_abundance = use.mosq_abundance
, use.mosq_bite_pref = use.mosq_bite_pref
, use.mosq_survival  = use.mosq_survival
, use.host_seroprev  = use.host_seroprev
, use.cond_titer     = use.cond_titer
, model.complexity   = model.complexity
)

if (one_off) {
  mosq_competence.r.gg.mm %<>% mutate(
    which_missing = {
      if (length(which(model.runs[complexity_counter, ] == FALSE)) > 0) {
        strsplit(colnames(model.runs)[which(model.runs[complexity_counter, ] == FALSE)], split = "[.]")[[1]][2]
      } else {
        "None"
      }
    }
  )
}

## For plots or relative quantitative values, also need to store the actual estimated competence of mosquitoes
mosq_competence.gg.mm %<>% mutate(
  use.host_abundance = use.host_abundance
, use.mosq_abundance = use.mosq_abundance
, use.mosq_bite_pref = use.mosq_bite_pref
, use.mosq_survival  = use.mosq_survival
, use.host_seroprev  = use.host_seroprev
, use.cond_titer     = use.cond_titer
, model.complexity   = model.complexity
)

if (one_off) {
  mosq_competence.gg.mm %<>% mutate(
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

dimnames(FOI_on_t_mm) <- list(
  seq(1, n_sims)
, dimnames(h_to_m_trans_all_samps_adj_to_com)[[3]]
  )

FOI_on_t.gg_mm        <- melt(FOI_on_t_mm)
names(FOI_on_t.gg_mm) <- c("rep", "mosq", "FOI_from")

FOI_on_t.gg_mm %<>% mutate(
  use.host_abundance = use.host_abundance
, use.mosq_abundance = use.mosq_abundance
, use.mosq_bite_pref = use.mosq_bite_pref
, use.mosq_survival  = use.mosq_survival
, use.host_seroprev  = use.host_seroprev
, use.cond_titer     = use.cond_titer
, model.complexity   = model.complexity)

if (one_off) {
  FOI_on_t.gg_mm %<>% mutate(
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

dimnames(physiol_mat_mm) <- list(
  dimnames(h_to_m_trans_all_samps_adj_to_com)[[3]]
, dimnames(h_to_m_trans_all_samps_adj_to_com)[[3]]
, NULL
)

physiol_mat.gg_mm        <- melt(physiol_mat_mm)
names(physiol_mat.gg_mm) <- c("G2", "G1", "Run", "Count")

mosq.names.G1 <- strsplit(as.character(physiol_mat.gg_mm$G1), "_") %>% 
  sapply(., FUN = function(x) c(paste(x, collapse = " "))) %>% 
  sapply(., FUN = function(x) paste(toupper(substring(x, 1, 1)), substring(x, 2), sep = "", collapse = " "))
mosq.names.G2 <- strsplit(as.character(physiol_mat.gg_mm$G2), "_") %>% 
  sapply(., FUN = function(x) c(paste(x, collapse = " "))) %>% 
  sapply(., FUN = function(x) paste(toupper(substring(x, 1, 1)), substring(x, 2), sep = "", collapse = " "))

physiol_mat.gg_mm$G1 <- mosq.names.G1 
physiol_mat.gg_mm$G2 <- mosq.names.G2

## Add in the probability that the starting exposed mosquito would have gotten infected in the first place. 
 ## Could also add this into the R0 calc in real_data_R0_2.R but since it is just a multiplication, can do it post to
  ## keep the R0 calc clean and using a classic definition of assuming starting with an infected 
   ## multiplying here by exposure -> infection probability can just be thought of as scaling back from one initial infected
    ## to some proportion of one initial infected
mosq_inf_single_dose.gg.f           <- mosq_inf_single_dose.gg
names(mosq_inf_single_dose.gg.f)    <- c("G1", "Run", "prob")

physiol_mat.gg_mm <- left_join(physiol_mat.gg_mm, mosq_inf_single_dose.gg.f)
physiol_mat.gg_mm <- physiol_mat.gg_mm %>% mutate(Count = Count * prob)

if (summary.type == "raw") {
  
physiol_mat.gg_mm        <- physiol_mat.gg_mm %>% 
  group_by(G2, G1) %>% summarize(
  comp.est    = quantile(Count, 0.500)
, comp.lwr    = quantile(Count, 0.025)
, comp.upr    = quantile(Count, 0.975)
  )
  
} else if (summary.type == "proportion") {
  
physiol_mat.gg_mm        <- physiol_mat.gg_mm %>% 
  group_by(G2, Run) %>%
  mutate(prop_est = Count / max(Count)) %>%
  ungroup(Run) %>%
  group_by(G1, G2) %>%
  summarize(
  comp.est    = quantile(prop_est, 0.500, na.rm = T)
, comp.lwr    = quantile(prop_est, 0.025, na.rm = T)
, comp.upr    = quantile(prop_est, 0.975, na.rm = T))

} else {
  print("summary.type option not supported"); break
}

physiol_mat.gg_mm %<>% mutate(
  use.host_abundance = use.host_abundance
, use.mosq_abundance = use.mosq_abundance
, use.mosq_bite_pref = use.mosq_bite_pref
, use.mosq_survival  = use.mosq_survival
, use.host_seroprev  = use.host_seroprev
, use.cond_titer     = use.cond_titer
, model.complexity   = model.complexity)

if (one_off) {
  physiol_mat.gg_mm %<>% mutate(
    which_missing = {
      if (length(which(model.runs[complexity_counter, ] == FALSE)) > 0) {
        strsplit(colnames(model.runs)[which(model.runs[complexity_counter, ] == FALSE)], split = "[.]")[[1]][2]
      } else {
        "None"
      }
    }
  )
}
