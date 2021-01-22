##############################################################
### Summarize the output from real_data_R0_2.R for ggplots ###
##############################################################

## Note: For nearly all steps 4 data frames get summarized (with various flavors of the following names)
 ## 1) XX_competence.r.gg.XX.f -- host to host or mosquito to mosquito transmission for ranks 
 ## 2) XX_competence.gg.XX.f   -- host to host or mosquito to mosquito transmission for raw value
 ## 3) XX_competence.r.gg.f    -- host to mosquito from the host or mosquitoes point of few for ranks
 ## 4) XX_competence.gg.f      -- host to mosquito from the host or mosquitoes point of few for raw value

####
## Step 1.1H: Create base data frames for rank plots for hosts
####

## For the purposes of this script if !one_off and !all_permuted, set all_permuted == TRUE
if (!one_off & !all_permuted) {
  all_permuted <- TRUE
}

### New column for host_competence.r.gg.hh.f when permuted as model.complexity is shared by many combinations as it is just a count
if (!one_off & all_permuted) {
  
host_competence.r.gg.hh.f <- host_competence.r.gg.hh.f %>%
  mutate(model.form = paste(
    use.host_abundance, use.mosq_abundance, use.mosq_bite_pref, use.mosq_survival, use.host_seroprev
  , sep = "_"))

host_competence.gg.hh.f <- host_competence.gg.hh.f %>%
  mutate(model.form = paste(
    use.host_abundance, use.mosq_abundance, use.mosq_bite_pref, use.mosq_survival, use.host_seroprev
  , sep = "_"))

host_competence.r.gg.f <- host_competence.r.gg.f %>%
  mutate(model.form = paste(
    use.host_abundance, use.mosq_abundance, use.mosq_bite_pref, use.mosq_survival, use.host_seroprev
  , sep = "_"))

host_competence.gg.f <- host_competence.gg.f %>%
  mutate(model.form = paste(
    use.host_abundance, use.mosq_abundance, use.mosq_bite_pref, use.mosq_survival, use.host_seroprev
  , sep = "_"))
}

host_competence.r.gg.heat.hh <- host_competence.r.gg.hh.f %>% 
  group_by(host, comp
    , {
      if (one_off) {
      which_missing
      } else {
      model.form 
      }
    }
    ) %>% summarize(den = length(comp) / 1000)

## Don't actually use this for anything because the density plot only makes sense for a categorical grouping,
 ## so just include this as a placeholder to have a data frame for all possible name combinations
host_competence.gg.heat.hh <- host_competence.gg.hh.f %>% 
  group_by(host
    , {
      if (one_off) {
      which_missing
      } else {
      model.form 
      }
    }
    ) %>% summarize(
      est = quantile(comp, 0.500)
    , lwr = quantile(comp, 0.025)
    , upr = quantile(comp, 0.975)
    )

host_competence.r.gg.heat <- host_competence.r.gg.f %>%
    group_by(host, comp
    , {
      if (one_off) {
      which_missing
      } else {
      model.form 
      }
    }
    ) %>% summarize(den = length(comp) / 1000)

host_competence.gg.heat <- host_competence.gg.f %>%
    group_by(host
    , {
      if (one_off) {
      which_missing
      } else {
      model.form 
      }
    }
    ) %>% summarize(
      est = quantile(comp, 0.500)
    , lwr = quantile(comp, 0.025)
    , upr = quantile(comp, 0.975)
    )

if (one_off) {
names(host_competence.r.gg.heat.hh)[3] <- "which_missing"
names(host_competence.gg.heat.hh)[2]   <- "which_missing"
names(host_competence.r.gg.heat)[3]    <- "which_missing"
names(host_competence.gg.heat)[2]      <- "which_missing"
} else {
names(host_competence.r.gg.heat.hh)[3] <- "model.complexity" 
names(host_competence.gg.heat.hh)[2]   <- "model.complexity"  
names(host_competence.r.gg.heat)[3]    <- "model.complexity"
names(host_competence.gg.heat)[2]      <- "model.complexity" 
}

host_competence.r.gg.heat.s.hh <- host_competence.r.gg.hh.f %>% 
  group_by(host
    , {
      if (one_off) {
      which_missing
      } else {
      model.form 
      }
    }
    ) %>% summarize(
    est = quantile(comp, c(0.50))
  , lwr = quantile(comp, c(0.025))
  , upr = quantile(comp, c(0.975)))

host_competence.gg.heat.s.hh <- host_competence.gg.hh.f %>% 
  group_by(host
    , {
      if (one_off) {
      which_missing
      } else {
      model.form 
      }
    }
    ) %>% summarize(
    est = quantile(comp, c(0.50))
  , lwr = quantile(comp, c(0.025))
  , upr = quantile(comp, c(0.975)))

host_competence.r.gg.heat.s <- host_competence.r.gg.f %>% 
  group_by(host
    , {
      if (one_off) {
      which_missing
      } else {
      model.form 
      }
    }
    ) %>% summarize(
    est = quantile(comp, c(0.50))
  , lwr = quantile(comp, c(0.025))
  , upr = quantile(comp, c(0.975)))

host_competence.gg.heat.s <- host_competence.gg.f %>% 
  group_by(host
    , {
      if (one_off) {
      which_missing
      } else {
      model.form 
      }
    }
    ) %>% summarize(
    est = quantile(comp, c(0.50))
  , lwr = quantile(comp, c(0.025))
  , upr = quantile(comp, c(0.975)))

if (one_off) {
names(host_competence.r.gg.heat.s.hh)[2] <- "which_missing"
names(host_competence.gg.heat.s.hh)[2]   <- "which_missing"
names(host_competence.r.gg.heat.s)[2]    <- "which_missing"
names(host_competence.gg.heat.s)[2]      <- "which_missing"
} else {
names(host_competence.r.gg.heat.s.hh)[2] <- "model.complexity"  
names(host_competence.gg.heat.s.hh)[2]   <- "model.complexity"  
names(host_competence.r.gg.heat.s)[2]    <- "model.complexity"  
names(host_competence.gg.heat.s)[2]      <- "model.complexity"  
}

####
## Step 1.2H: Adjust the species names for ggplot beautification for hosts
####

## host to host: rank
host.names <- strsplit(as.character(host_competence.r.gg.heat.s.hh$host), "_") %>% 
  sapply(., FUN = function(x) c(paste(x, collapse = " "))) %>% 
  sapply(., FUN = function(x) paste(toupper(substring(x, 1, 1)), substring(x, 2), sep = "", collapse = " "))
host_competence.r.gg.heat.s.hh$host <- host.names

host.names <- strsplit(as.character(host_competence.r.gg.heat.hh$host), "_") %>% 
  sapply(., FUN = function(x) c(paste(x, collapse = " "))) %>% 
  sapply(., FUN = function(x) paste(toupper(substring(x, 1, 1)), substring(x, 2), sep = "", collapse = " "))
host_competence.r.gg.heat.hh$host <- host.names

## host to host: raw
host.names <- strsplit(as.character(host_competence.gg.heat.s.hh$host), "_") %>% 
  sapply(., FUN = function(x) c(paste(x, collapse = " "))) %>% 
  sapply(., FUN = function(x) paste(toupper(substring(x, 1, 1)), substring(x, 2), sep = "", collapse = " "))
host_competence.gg.heat.s.hh$host <- host.names

host.names <- strsplit(as.character(host_competence.gg.heat.hh$host), "_") %>% 
  sapply(., FUN = function(x) c(paste(x, collapse = " "))) %>% 
  sapply(., FUN = function(x) paste(toupper(substring(x, 1, 1)), substring(x, 2), sep = "", collapse = " "))
host_competence.gg.heat.hh$host <- host.names

## host to mosquito: rank
host.names <- strsplit(as.character(host_competence.r.gg.heat.s$host), "_") %>% 
  sapply(., FUN = function(x) c(paste(x, collapse = " "))) %>% 
  sapply(., FUN = function(x) paste(toupper(substring(x, 1, 1)), substring(x, 2), sep = "", collapse = " "))
host_competence.r.gg.heat.s$host <- host.names

host.names <- strsplit(as.character(host_competence.r.gg.heat$host), "_") %>% 
  sapply(., FUN = function(x) c(paste(x, collapse = " "))) %>% 
  sapply(., FUN = function(x) paste(toupper(substring(x, 1, 1)), substring(x, 2), sep = "", collapse = " "))
host_competence.r.gg.heat$host <- host.names

## host to mosquito: raw
host.names <- strsplit(as.character(host_competence.gg.heat.s$host), "_") %>% 
  sapply(., FUN = function(x) c(paste(x, collapse = " "))) %>% 
  sapply(., FUN = function(x) paste(toupper(substring(x, 1, 1)), substring(x, 2), sep = "", collapse = " "))
host_competence.gg.heat.s$host <- host.names

host.names <- strsplit(as.character(host_competence.gg.heat$host), "_") %>% 
  sapply(., FUN = function(x) c(paste(x, collapse = " "))) %>% 
  sapply(., FUN = function(x) paste(toupper(substring(x, 1, 1)), substring(x, 2), sep = "", collapse = " "))
host_competence.gg.heat$host <- host.names

if (one_off) {
  
host_competence.r.gg.heat.s.hh$host <- factor(host_competence.r.gg.heat.s.hh$host
  , levels = host_competence.r.gg.heat.s.hh[host_competence.r.gg.heat.s.hh$which_missing == "None", ]$host)

host_competence.gg.heat.s.hh$host <- factor(host_competence.gg.heat.s.hh$host
  , levels = host_competence.gg.heat.s.hh[host_competence.gg.heat.s.hh$which_missing == "None", ]$host)

host_competence.r.gg.heat.s$host <- factor(host_competence.r.gg.heat.s$host
  , levels = host_competence.r.gg.heat.s[host_competence.r.gg.heat.s$which_missing == "None", ]$host)

host_competence.gg.heat.s$host <- factor(host_competence.gg.heat.s$host
  , levels = host_competence.gg.heat.s[host_competence.gg.heat.s$which_missing == "None", ]$host)

} else {
  
## Order these host names by the most complex model
host_competence.r.gg.heat.s.hh$host <- factor(host_competence.r.gg.heat.s.hh$host
  , levels = host_competence.r.gg.heat.s.hh[host_competence.r.gg.heat.s.hh$model.complexity == "TRUE_TRUE_TRUE_TRUE_TRUE", ]$host)

host_competence.gg.heat.s.hh$host <- factor(host_competence.gg.heat.s.hh$host
  , levels = host_competence.gg.heat.s.hh[host_competence.gg.heat.s.hh$model.complexity == "TRUE_TRUE_TRUE_TRUE_TRUE", ]$host)

host_competence.r.gg.heat.s$host <- factor(host_competence.r.gg.heat.s$host
  , levels = host_competence.r.gg.heat.s[host_competence.r.gg.heat.s$model.complexity == "TRUE_TRUE_TRUE_TRUE_TRUE", ]$host) 

host_competence.gg.heat.s$host <- factor(host_competence.gg.heat.s$host
  , levels = host_competence.gg.heat.s[host_competence.gg.heat.s$model.complexity == "TRUE_TRUE_TRUE_TRUE_TRUE", ]$host)

}

####
## Step 1.1M: Create base data frames for rank plots for mosquitoes 
####

### New column for host_competence.r.gg.hh.f when permuted as model.complexity is shared by many combinations as it is just a count
if (!one_off & all_permuted) {
  
mosq_competence.r.gg.mm.f <- mosq_competence.r.gg.mm.f %>%
  mutate(model.form = paste(
    use.host_abundance, use.mosq_abundance, use.mosq_bite_pref, use.mosq_survival, use.host_seroprev
  , sep = "_"))

mosq_competence.gg.mm.f <- mosq_competence.gg.mm.f %>%
  mutate(model.form = paste(
    use.host_abundance, use.mosq_abundance, use.mosq_bite_pref, use.mosq_survival, use.host_seroprev
  , sep = "_"))

mosq_competence.r.gg.f <- mosq_competence.r.gg.f %>%
  mutate(model.form = paste(
    use.host_abundance, use.mosq_abundance, use.mosq_bite_pref, use.mosq_survival, use.host_seroprev
  , sep = "_"))

mosq_competence.gg.f <- mosq_competence.gg.f %>%
  mutate(model.form = paste(
    use.host_abundance, use.mosq_abundance, use.mosq_bite_pref, use.mosq_survival, use.host_seroprev
  , sep = "_"))

}

mosq_competence.r.gg.heat.mm <- mosq_competence.r.gg.mm.f %>% 
  group_by(mosq, comp
    , {
      if (one_off) {
      which_missing
      } else {
      model.form
      }
    }
    ) %>% summarize(den = length(comp) / 1000)

mosq_competence.gg.heat.mm <- mosq_competence.gg.mm.f %>% 
  group_by(mosq
    , {
      if (one_off) {
      which_missing
      } else {
      model.form
      }
    }
    ) %>% summarize(
      est = quantile(comp, 0.500)
    , lwr = quantile(comp, 0.025)
    , upr = quantile(comp, 0.975)
    )

mosq_competence.r.gg.heat <- mosq_competence.r.gg.f %>% 
  group_by(mosq, comp
    , {
      if (one_off) {
      which_missing
      } else {
      model.form
      }
    }
    ) %>% summarize(den = length(comp) / 1000)

mosq_competence.gg.heat <- mosq_competence.gg.f %>% 
  group_by(mosq
    , {
      if (one_off) {
      which_missing
      } else {
      model.form
      }
    }
    ) %>% summarize(
      est = quantile(comp, 0.500)
    , lwr = quantile(comp, 0.025)
    , upr = quantile(comp, 0.975)
    )

if (one_off) {
names(mosq_competence.r.gg.heat.mm)[3] <- "which_missing"
names(mosq_competence.gg.heat.mm)[2]   <- "which_missing"
names(mosq_competence.r.gg.heat)[3]    <- "which_missing"
names(mosq_competence.gg.heat)[2]      <- "which_missing"
} else {
names(mosq_competence.r.gg.heat.mm)[3] <- "model.complexity"
names(mosq_competence.gg.heat.mm)[2]   <- "model.complexity"
names(mosq_competence.r.gg.heat)[3]    <- "model.complexity" 
names(mosq_competence.gg.heat)[2]      <- "model.complexity" 
}

mosq_competence.r.gg.heat.s.mm <- mosq_competence.r.gg.mm.f %>% 
  group_by(mosq
    , {
      if (one_off) {
      which_missing
      } else {
      model.form
      }
    }
    ) %>% summarize(
    est = quantile(comp, c(0.50))
  , lwr = quantile(comp, c(0.025))
  , upr = quantile(comp, c(0.975)))

mosq_competence.gg.heat.s.mm <- mosq_competence.gg.mm.f %>% 
  group_by(mosq
    , {
      if (one_off) {
      which_missing
      } else {
      model.form
      }
    }
    ) %>% summarize(
    est = quantile(comp, c(0.50))
  , lwr = quantile(comp, c(0.025))
  , upr = quantile(comp, c(0.975)))

mosq_competence.r.gg.heat.s <- mosq_competence.r.gg.f %>% 
  group_by(mosq
    , {
      if (one_off) {
      which_missing
      } else {
      model.form
      }
    }
    ) %>% summarize(
    est = quantile(comp, c(0.50))
  , lwr = quantile(comp, c(0.025))
  , upr = quantile(comp, c(0.975)))

mosq_competence.gg.heat.s <- mosq_competence.gg.f %>% 
  group_by(mosq
    , {
      if (one_off) {
      which_missing
      } else {
      model.form
      }
    }
    ) %>% summarize(
    est = quantile(comp, c(0.50))
  , lwr = quantile(comp, c(0.025))
  , upr = quantile(comp, c(0.975)))

if (one_off) {
names(mosq_competence.r.gg.heat.s.mm)[2] <- "which_missing"
names(mosq_competence.gg.heat.s.mm)[2]   <- "which_missing"
names(mosq_competence.r.gg.heat.s)[2]    <- "which_missing"
names(mosq_competence.gg.heat.s)[2]      <- "which_missing"
} else {
names(mosq_competence.r.gg.heat.s.mm)[2] <- "model.complexity"  
names(mosq_competence.gg.heat.s.mm)[2]   <- "model.complexity"  
names(mosq_competence.r.gg.heat.s)[2]    <- "model.complexity"  
names(mosq_competence.gg.heat.s)[2]      <- "model.complexity"  
}


####
## Step 1.2M: Adjust the species names for ggplot beautification for mosquitoes
####

## mosquito to mosquito: ranks
mosq.names <- strsplit(as.character(mosq_competence.r.gg.heat.s.mm$mosq), "_") %>% 
  sapply(., FUN = function(x) c(paste(x, collapse = " "))) %>% 
  sapply(., FUN = function(x) paste(toupper(substring(x, 1, 1)), substring(x, 2), sep = "", collapse = " "))
mosq_competence.r.gg.heat.s.mm$mosq <- mosq.names

mosq.names <- strsplit(as.character(mosq_competence.r.gg.heat.mm$mosq), "_") %>% 
  sapply(., FUN = function(x) c(paste(x, collapse = " "))) %>% 
  sapply(., FUN = function(x) paste(toupper(substring(x, 1, 1)), substring(x, 2), sep = "", collapse = " "))
mosq_competence.r.gg.heat.mm$mosq <- mosq.names

## mosquito to mosquito: raw
mosq.names <- strsplit(as.character(mosq_competence.gg.heat.s.mm$mosq), "_") %>% 
  sapply(., FUN = function(x) c(paste(x, collapse = " "))) %>% 
  sapply(., FUN = function(x) paste(toupper(substring(x, 1, 1)), substring(x, 2), sep = "", collapse = " "))
mosq_competence.gg.heat.s.mm$mosq <- mosq.names

mosq.names <- strsplit(as.character(mosq_competence.gg.heat.mm$mosq), "_") %>% 
  sapply(., FUN = function(x) c(paste(x, collapse = " "))) %>% 
  sapply(., FUN = function(x) paste(toupper(substring(x, 1, 1)), substring(x, 2), sep = "", collapse = " "))
mosq_competence.gg.heat.mm$mosq <- mosq.names

## mosquito competence as just their ability to get infected: ranks
mosq.names <- strsplit(as.character(mosq_competence.r.gg.heat.s$mosq), "_") %>% 
  sapply(., FUN = function(x) c(paste(x, collapse = " "))) %>% 
  sapply(., FUN = function(x) paste(toupper(substring(x, 1, 1)), substring(x, 2), sep = "", collapse = " "))
mosq_competence.r.gg.heat.s$mosq <- mosq.names

mosq.names <- strsplit(as.character(mosq_competence.r.gg.heat$mosq), "_") %>% 
  sapply(., FUN = function(x) c(paste(x, collapse = " "))) %>% 
  sapply(., FUN = function(x) paste(toupper(substring(x, 1, 1)), substring(x, 2), sep = "", collapse = " "))
mosq_competence.r.gg.heat$mosq <- mosq.names

## mosquito competence as just their ability to get infected: raw
mosq.names <- strsplit(as.character(mosq_competence.gg.heat.s$mosq), "_") %>% 
  sapply(., FUN = function(x) c(paste(x, collapse = " "))) %>% 
  sapply(., FUN = function(x) paste(toupper(substring(x, 1, 1)), substring(x, 2), sep = "", collapse = " "))
mosq_competence.gg.heat.s$mosq <- mosq.names

mosq.names <- strsplit(as.character(mosq_competence.gg.heat$mosq), "_") %>% 
  sapply(., FUN = function(x) c(paste(x, collapse = " "))) %>% 
  sapply(., FUN = function(x) paste(toupper(substring(x, 1, 1)), substring(x, 2), sep = "", collapse = " "))
mosq_competence.gg.heat$mosq <- mosq.names

if (one_off) {
  
mosq_competence.r.gg.heat.s.mm$mosq <- factor(mosq_competence.r.gg.heat.s.mm$mosq
  , levels = mosq_competence.r.gg.heat.s.mm[mosq_competence.r.gg.heat.s.mm$which_missing == "None", ]$mosq)

mosq_competence.gg.heat.s.mm$mosq <- factor(mosq_competence.gg.heat.s.mm$mosq
  , levels = mosq_competence.gg.heat.s.mm[mosq_competence.gg.heat.s.mm$which_missing == "None", ]$mosq)
  
mosq_competence.r.gg.heat.s$mosq <- factor(mosq_competence.r.gg.heat.s$mosq
  , levels = mosq_competence.r.gg.heat.s[mosq_competence.r.gg.heat.s$which_missing == "None", ]$mosq)

mosq_competence.gg.heat.s$mosq <- factor(mosq_competence.gg.heat.s$mosq
  , levels = mosq_competence.gg.heat.s[mosq_competence.gg.heat.s$which_missing == "None", ]$mosq)

} else {
  
mosq_competence.r.gg.heat.s.mm$mosq <- factor(mosq_competence.r.gg.heat.s.mm$mosq
  , levels = mosq_competence.r.gg.heat.s.mm[mosq_competence.r.gg.heat.s.mm$model.complexity == "TRUE_TRUE_TRUE_TRUE_TRUE", ]$mosq)  

mosq_competence.gg.heat.s.mm$mosq <- factor(mosq_competence.gg.heat.s.mm$mosq
  , levels = mosq_competence.gg.heat.s.mm[mosq_competence.gg.heat.s.mm$model.complexity == "TRUE_TRUE_TRUE_TRUE_TRUE", ]$mosq) 
  
mosq_competence.r.gg.heat.s$mosq <- factor(mosq_competence.r.gg.heat.s$mosq
  , levels = mosq_competence.r.gg.heat.s[mosq_competence.r.gg.heat.s$model.complexity == "TRUE_TRUE_TRUE_TRUE_TRUE", ]$mosq)  

mosq_competence.gg.heat.s$mosq <- factor(mosq_competence.gg.heat.s$mosq
  , levels = mosq_competence.gg.heat.s[mosq_competence.gg.heat.s$model.complexity == "TRUE_TRUE_TRUE_TRUE_TRUE", ]$mosq)  

}

####
## Step 2H: Similar-ish summary to the above for hosts, but for the data frame that controls the density in each rank
####

model_form.plot <- "TRUE_TRUE_TRUE_TRUE_TRUE"

if (!one_off) {
  
## host to host: rank
host_competence.r.gg.heat.hh.full   <- host_competence.r.gg.heat.hh %>% filter(model.complexity == model_form.plot)
host_competence.r.gg.heat.s.hh.full <- host_competence.r.gg.heat.s.hh %>% filter(model.complexity == model_form.plot)

## host to host: raw
host_competence.gg.heat.hh.full   <- host_competence.gg.heat.hh %>% filter(model.complexity == model_form.plot)
host_competence.gg.heat.s.hh.full <- host_competence.gg.heat.s.hh %>% filter(model.complexity == model_form.plot)

## rank
host.order.s <- host_competence.r.gg.heat.s.hh %>% filter(model.complexity == "FALSE_FALSE_FALSE_FALSE_FALSE")
host.order.s <- host.order.s[order(host.order.s$est), ]
host.order.s$host <- factor(host.order.s$host, levels = host.order.s$host)

host_competence.r.gg.heat.s.hh.full      <- host_competence.r.gg.heat.s.hh.full[order(host_competence.r.gg.heat.s.hh.full$est), ]
host_competence.r.gg.heat.s.hh.full$host <- factor(host_competence.r.gg.heat.s.hh.full$host, levels = host.order.s$host)
host_competence.r.gg.heat.hh.full$host   <- factor(host_competence.r.gg.heat.hh.full$host, levels = rev(host.order.s$host))

## raw
host.order.s      <- host_competence.gg.heat.s.hh %>% filter(model.complexity == "FALSE_FALSE_FALSE_FALSE_FALSE")
host.order.s      <- host.order.s[order(host.order.s$est), ]
host.order.s$host <- factor(host.order.s$host, levels = host.order.s$host)

host_competence.gg.heat.s.hh.full      <- host_competence.gg.heat.s.hh.full[order(host_competence.gg.heat.s.hh.full$est), ]
host_competence.gg.heat.s.hh.full$host <- factor(host_competence.gg.heat.s.hh.full$host, levels = host.order.s$host)
host_competence.gg.heat.hh.full$host   <- factor(host_competence.gg.heat.hh.full$host, levels = host.order.s$host)

## host to mosquito: rank
host_competence.r.gg.heat.full   <- host_competence.r.gg.heat %>% filter(model.complexity == model_form.plot)
host_competence.r.gg.heat.s.full <- host_competence.r.gg.heat.s %>% filter(model.complexity == model_form.plot)

## host to mosquito: raw
host_competence.gg.heat.full   <- host_competence.gg.heat %>% filter(model.complexity == model_form.plot)
host_competence.gg.heat.s.full <- host_competence.gg.heat.s %>% filter(model.complexity == model_form.plot)

## rank
host.order.s      <- host_competence.r.gg.heat.s %>% filter(model.complexity == "FALSE_FALSE_FALSE_FALSE_FALSE")
host.order.s      <- host.order.s[order(host.order.s$est), ]
host.order.s$host <- factor(host.order.s$host, levels = host.order.s$host)

host_competence.r.gg.heat.s.full      <- host_competence.r.gg.heat.s.full[order(host_competence.r.gg.heat.s.full$est), ]
host_competence.r.gg.heat.s.full$host <- factor(host_competence.r.gg.heat.s.full$host, levels = host.order.s$host)
host_competence.r.gg.heat.full$host   <- factor(host_competence.r.gg.heat.full$host, levels = rev(host.order.s$host))

## raw
host.order.s      <- host_competence.gg.heat.s %>% filter(model.complexity == "FALSE_FALSE_FALSE_FALSE_FALSE")
host.order.s      <- host.order.s[order(host.order.s$est), ]
host.order.s$host <- factor(host.order.s$host, levels = host.order.s$host)

host_competence.gg.heat.s.full      <- host_competence.gg.heat.s.full[order(host_competence.r.gg.heat.s.full$est), ]
host_competence.gg.heat.s.full$host <- factor(host_competence.gg.heat.s.full$host, levels = host.order.s$host)
host_competence.gg.heat.full$host   <- factor(host_competence.gg.heat.full$host, levels = host.order.s$host)

} else {
  
## host to host: rank
host_competence.r.gg.heat.hh.full   <- host_competence.r.gg.heat.hh
host_competence.r.gg.heat.s.hh.full <- host_competence.r.gg.heat.s.hh

host_competence.gg.host_order <- host_competence.r.gg.heat.s.hh %>% filter(which_missing == "None")
host_competence.gg.host_order <- host_competence.gg.host_order[order(host_competence.gg.host_order$est, decreasing = T), ]

host_competence.r.gg.heat.s.hh.full$host <- factor(host_competence.r.gg.heat.s.hh.full$host, levels = host_competence.gg.host_order$host)
host_competence.r.gg.heat.hh.full$host   <- factor(host_competence.r.gg.heat.hh.full$host, levels = host_competence.gg.host_order$host)

host_competence.r.gg.heat.s.hh.full$which_missing <- factor(host_competence.r.gg.heat.s.hh.full$which_missing
  , levels = c("None", "host_abundance", "host_seroprev", "mosq_bite_pref", "mosq_abundance", "mosq_survival"))
host_competence.r.gg.heat.hh.full$which_missing <- factor(host_competence.r.gg.heat.hh.full$which_missing
  , levels = c("None", "host_abundance", "host_seroprev", "mosq_bite_pref", "mosq_abundance", "mosq_survival"))

## host to host: raw
host_competence.gg.heat.hh.full   <- host_competence.gg.heat.hh
host_competence.gg.heat.s.hh.full <- host_competence.gg.heat.s.hh

host_competence.gg.host_order <- host_competence.gg.heat.s.hh %>% filter(which_missing == "None")
host_competence.gg.host_order <- host_competence.gg.host_order[order(host_competence.gg.host_order$est, decreasing = T), ]

host_competence.gg.heat.s.hh.full$host <- factor(host_competence.gg.heat.s.hh.full$host, levels = host_competence.gg.host_order$host)
host_competence.gg.heat.hh.full$host   <- factor(host_competence.gg.heat.hh.full$host, levels = host_competence.gg.host_order$host)

host_competence.gg.heat.s.hh.full$which_missing <- factor(host_competence.gg.heat.s.hh.full$which_missing
  , levels = c("None", "host_abundance", "host_seroprev", "mosq_bite_pref", "mosq_abundance", "mosq_survival"))
host_competence.gg.heat.hh.full$which_missing <- factor(host_competence.gg.heat.hh.full$which_missing
  , levels = c("None", "host_abundance", "host_seroprev", "mosq_bite_pref", "mosq_abundance", "mosq_survival"))

## host to mosquito: rank
host_competence.r.gg.heat.full   <- host_competence.r.gg.heat
host_competence.r.gg.heat.s.full <- host_competence.r.gg.heat.s

host_competence.gg.host_order <- host_competence.r.gg.heat.s %>% filter(which_missing == "None")
host_competence.gg.host_order <- host_competence.gg.host_order[order(host_competence.gg.host_order$est, decreasing = T), ]

host_competence.r.gg.heat.s.full$host <- factor(host_competence.r.gg.heat.s.full$host, levels = host_competence.gg.host_order$host)
host_competence.r.gg.heat.full$host   <- factor(host_competence.r.gg.heat.full$host, levels = host_competence.gg.host_order$host)

host_competence.r.gg.heat.s.full$which_missing <- factor(host_competence.r.gg.heat.s.full$which_missing
  , levels = c("None", "host_abundance", "host_seroprev", "mosq_bite_pref", "mosq_abundance", "mosq_survival"))
host_competence.r.gg.heat.full$which_missing <- factor(host_competence.r.gg.heat.full$which_missing
  , levels = c("None", "host_abundance", "host_seroprev", "mosq_bite_pref", "mosq_abundance", "mosq_survival"))

## host to mosquito: raw
host_competence.gg.heat.full   <- host_competence.gg.heat
host_competence.gg.heat.s.full <- host_competence.gg.heat.s

host_competence.gg.host_order <- host_competence.gg.heat.s %>% filter(which_missing == "None")
host_competence.gg.host_order <- host_competence.gg.host_order[order(host_competence.gg.host_order$est, decreasing = T), ]

host_competence.gg.heat.s.full$host <- factor(host_competence.gg.heat.s.full$host, levels = host_competence.gg.host_order$host)
host_competence.gg.heat.full$host   <- factor(host_competence.gg.heat.full$host, levels = host_competence.gg.host_order$host)

host_competence.gg.heat.s.full$which_missing <- factor(host_competence.gg.heat.s.full$which_missing
  , levels = c("None", "host_abundance", "host_seroprev", "mosq_bite_pref", "mosq_abundance", "mosq_survival"))
host_competence.gg.heat.full$which_missing <- factor(host_competence.gg.heat.full$which_missing
  , levels = c("None", "host_abundance", "host_seroprev", "mosq_bite_pref", "mosq_abundance", "mosq_survival"))

}

####
## Step 2H: Similar-ish summary to the above for mosquitoes, but for the data frame that controls the density in each rank
####

if (!one_off) {
  
## mosquito to mosquito: rank
mosq_competence.r.gg.heat.mm.full   <- mosq_competence.r.gg.heat.mm %>% filter(model.complexity == model_form.plot)
mosq_competence.r.gg.heat.s.mm.full <- mosq_competence.r.gg.heat.s.mm %>% filter(model.complexity == model_form.plot)

## mosquito to mosquito: raw
mosq_competence.gg.heat.mm.full   <- mosq_competence.gg.heat.mm %>% filter(model.complexity == model_form.plot)
mosq_competence.gg.heat.s.mm.full <- mosq_competence.gg.heat.s.mm %>% filter(model.complexity == model_form.plot)

## rank
mosq.order.s      <- mosq_competence.r.gg.heat.s.mm %>% filter(model.complexity == "FALSE_FALSE_FALSE_FALSE_FALSE")
mosq.order.s      <- mosq.order.s[order(mosq.order.s$est), ]
mosq.order.s$mosq <- factor(mosq.order.s$mosq, levels = mosq.order.s$mosq)

mosq_competence.r.gg.heat.s.mm.full <- mosq_competence.r.gg.heat.s.mm.full[order(mosq_competence.r.gg.heat.s.mm.full$est), ]
mosq_competence.r.gg.heat.s.mm.full$mosq <- factor(mosq_competence.r.gg.heat.s.mm.full$mosq
  , levels = mosq_competence.r.gg.heat.s.mm.full$mosq)
mosq_competence.r.gg.heat.mm.full$mosq   <- factor(mosq_competence.r.gg.heat.mm.full$mosq
  , levels = rev(mosq_competence.r.gg.heat.s.mm.full$mosq))

## raw
mosq.order.s      <- mosq_competence.gg.heat.s.mm %>% filter(model.complexity == "FALSE_FALSE_FALSE_FALSE_FALSE")
mosq.order.s      <- mosq.order.s[order(mosq.order.s$est), ]
mosq.order.s$mosq <- factor(mosq.order.s$mosq, levels = mosq.order.s$mosq)

mosq_competence.gg.heat.s.mm.full <- mosq_competence.gg.heat.s.mm.full[order(mosq_competence.gg.heat.s.mm.full$est), ]
mosq_competence.gg.heat.s.mm.full$mosq <- factor(mosq_competence.gg.heat.s.mm.full$mosq
  , levels = mosq_competence.gg.heat.s.mm.full$mosq)
mosq_competence.gg.heat.mm.full$mosq   <- factor(mosq_competence.gg.heat.mm.full$mosq
  , levels = rev(mosq_competence.gg.heat.s.mm.full$mosq))

## infection of mosquitoes from hosts: rank
mosq_competence.r.gg.heat.full   <- mosq_competence.r.gg.heat %>% filter(model.complexity == model_form.plot)
mosq_competence.r.gg.heat.s.full <- mosq_competence.r.gg.heat.s %>% filter(model.complexity == model_form.plot)

## infection of mosquitoes from hosts: raw
mosq_competence.gg.heat.full   <- mosq_competence.gg.heat %>% filter(model.complexity == model_form.plot)
mosq_competence.gg.heat.s.full <- mosq_competence.gg.heat.s %>% filter(model.complexity == model_form.plot)

## rank
mosq.order.s      <- mosq_competence.r.gg.heat.s %>% filter(model.complexity == "FALSE_FALSE_FALSE_FALSE_FALSE")
mosq.order.s      <- mosq.order.s[order(mosq.order.s$est), ]
mosq.order.s$mosq <- factor(mosq.order.s$mosq, levels = mosq.order.s$mosq)

mosq_competence.r.gg.heat.s.full <- mosq_competence.r.gg.heat.s.full[order(mosq_competence.r.gg.heat.s.full$est), ]
mosq_competence.r.gg.heat.s.full$mosq <- factor(mosq_competence.r.gg.heat.s.full$mosq
  , levels = mosq_competence.r.gg.heat.s.full$mosq)
mosq_competence.r.gg.heat.full$mosq   <- factor(mosq_competence.r.gg.heat.full$mosq
  , levels = rev(mosq_competence.r.gg.heat.s.full$mosq))

## raw
mosq.order.s      <- mosq_competence.gg.heat.s %>% filter(model.complexity == "FALSE_FALSE_FALSE_FALSE_FALSE")
mosq.order.s      <- mosq.order.s[order(mosq.order.s$est), ]
mosq.order.s$mosq <- factor(mosq.order.s$mosq, levels = mosq.order.s$mosq)

mosq_competence.gg.heat.s.full <- mosq_competence.gg.heat.s.full[order(mosq_competence.r.gg.heat.s.full$est), ]
mosq_competence.gg.heat.s.full$mosq <- factor(mosq_competence.gg.heat.s.full$mosq
  , levels = mosq_competence.gg.heat.s.full$mosq)
mosq_competence.gg.heat.full$mosq   <- factor(mosq_competence.gg.heat.full$mosq
  , levels = rev(mosq_competence.gg.heat.s.full$mosq))

} else {
  
## mosquito to mosquito: rank
mosq_competence.r.gg.heat.mm.full   <- mosq_competence.r.gg.heat.mm
mosq_competence.r.gg.heat.s.mm.full <- mosq_competence.r.gg.heat.s.mm

## mosquito to mosquito: raw
mosq_competence.gg.heat.mm.full   <- mosq_competence.gg.heat.mm
mosq_competence.gg.heat.s.mm.full <- mosq_competence.gg.heat.s.mm

## rank
mosq_competence.gg.mosq_order <- mosq_competence.r.gg.heat.s.mm %>% filter(which_missing == "None")
mosq_competence.gg.mosq_order <- mosq_competence.gg.mosq_order[order(mosq_competence.gg.mosq_order$est, decreasing = T), ]

mosq_competence.r.gg.heat.s.mm.full$mosq <- factor(mosq_competence.r.gg.heat.s.mm.full$mosq
  , levels = mosq_competence.gg.mosq_order$mosq)
mosq_competence.r.gg.heat.mm.full$mosq   <- factor(mosq_competence.r.gg.heat.mm.full$mosq
  , levels = mosq_competence.gg.mosq_order$mosq)

mosq_competence.r.gg.heat.s.mm.full$which_missing <- factor(mosq_competence.r.gg.heat.s.mm.full$which_missing
  , levels = c("None", "host_abundance", "host_seroprev", "mosq_bite_pref", "mosq_abundance", "mosq_survival"))
mosq_competence.r.gg.heat.mm.full$which_missing <- factor(mosq_competence.r.gg.heat.mm.full$which_missing
  , levels = c("None", "host_abundance", "host_seroprev", "mosq_bite_pref", "mosq_abundance", "mosq_survival"))

## raw
mosq_competence.gg.mosq_order <- mosq_competence.gg.heat.s.mm %>% filter(which_missing == "None")
mosq_competence.gg.mosq_order <- mosq_competence.gg.mosq_order[order(mosq_competence.gg.mosq_order$est, decreasing = T), ]

mosq_competence.gg.heat.s.mm.full$mosq <- factor(mosq_competence.gg.heat.s.mm.full$mosq
  , levels = mosq_competence.gg.mosq_order$mosq)
mosq_competence.gg.heat.mm.full$mosq   <- factor(mosq_competence.gg.heat.mm.full$mosq
  , levels = mosq_competence.gg.mosq_order$mosq)

mosq_competence.gg.heat.s.mm.full$which_missing <- factor(mosq_competence.gg.heat.s.mm.full$which_missing
  , levels = c("None", "host_abundance", "host_seroprev", "mosq_bite_pref", "mosq_abundance", "mosq_survival"))
mosq_competence.gg.heat.mm.full$which_missing <- factor(mosq_competence.gg.heat.mm.full$which_missing
  , levels = c("None", "host_abundance", "host_seroprev", "mosq_bite_pref", "mosq_abundance", "mosq_survival"))


## Mosquito infection: rank
mosq_competence.r.gg.heat.full   <- mosq_competence.r.gg.heat
mosq_competence.r.gg.heat.s.full <- mosq_competence.r.gg.heat.s

## Mosquito infection: raw
mosq_competence.gg.heat.full   <- mosq_competence.gg.heat
mosq_competence.gg.heat.s.full <- mosq_competence.gg.heat.s

## rank
mosq_competence.gg.mosq_order <- mosq_competence.r.gg.heat.s %>% filter(which_missing == "None")
mosq_competence.gg.mosq_order <- mosq_competence.gg.mosq_order[order(mosq_competence.gg.mosq_order$est, decreasing = T), ]

mosq_competence.r.gg.heat.s.full$mosq <- factor(mosq_competence.r.gg.heat.s.full$mosq
  , levels = mosq_competence.gg.mosq_order$mosq)
mosq_competence.r.gg.heat.full$mosq   <- factor(mosq_competence.r.gg.heat.full$mosq
  , levels = mosq_competence.gg.mosq_order$mosq)

mosq_competence.r.gg.heat.s.full$which_missing <- factor(mosq_competence.r.gg.heat.s.full$which_missing
  , levels = c("None", "host_abundance", "host_seroprev", "mosq_bite_pref", "mosq_abundance", "mosq_survival"))
mosq_competence.r.gg.heat.full$which_missing <- factor(mosq_competence.r.gg.heat.full$which_missing
  , levels = c("None", "host_abundance", "host_seroprev", "mosq_bite_pref", "mosq_abundance", "mosq_survival"))

## raw
mosq_competence.gg.mosq_order <- mosq_competence.gg.heat.s %>% filter(which_missing == "None")
mosq_competence.gg.mosq_order <- mosq_competence.gg.mosq_order[order(mosq_competence.gg.mosq_order$est, decreasing = T), ]

mosq_competence.gg.heat.s.full$mosq <- factor(mosq_competence.gg.heat.s.full$mosq
  , levels = mosq_competence.gg.mosq_order$mosq)
mosq_competence.gg.heat.full$mosq   <- factor(mosq_competence.gg.heat.full$mosq
  , levels = mosq_competence.gg.mosq_order$mosq)

mosq_competence.gg.heat.s.full$which_missing <- factor(mosq_competence.gg.heat.s.full$which_missing
  , levels = c("None", "host_abundance", "host_seroprev", "mosq_bite_pref", "mosq_abundance", "mosq_survival"))
mosq_competence.gg.heat.full$which_missing <- factor(mosq_competence.gg.heat.full$which_missing
  , levels = c("None", "host_abundance", "host_seroprev", "mosq_bite_pref", "mosq_abundance", "mosq_survival"))

}

####
## Step 3: Host to Host and Mosquito to Mosquito second generation infections for pairwise matrix
####

model_form.plot <- "TRUE_TRUE_TRUE_TRUE_TRUE"

physiol_mat.gg.f <- physiol_mat.gg.f %>%
  mutate(model.form = paste(
    use.host_abundance, use.mosq_abundance, use.mosq_bite_pref, use.mosq_survival, use.host_seroprev
  , sep = "_"))

physiol_mat.gg.host_order <- physiol_mat.gg.f %>% 
  filter(model.form == model_form.plot) %>%
  group_by(G1) %>% summarize(tot = sum(comp.est))

physiol_mat.gg.host_order <- physiol_mat.gg.host_order[order(physiol_mat.gg.host_order$tot, decreasing = T), ]

physiol_mat.gg.f    <- physiol_mat.gg.f[order(physiol_mat.gg.f$comp.est, decreasing = T), ]
physiol_mat.gg.f$G1 <- factor(physiol_mat.gg.f$G1, levels = rev(physiol_mat.gg.host_order$G1))
physiol_mat.gg.f$G2 <- factor(physiol_mat.gg.f$G2, levels = physiol_mat.gg.host_order$G1)

physiol_mat.gg.f.mm <- physiol_mat.gg.f.mm %>%
  mutate(model.form = paste(
    use.host_abundance, use.mosq_abundance, use.mosq_bite_pref, use.mosq_survival, use.host_seroprev
  , sep = "_"))

physiol_mat.gg.mosq_order <- physiol_mat.gg.f.mm %>% 
  filter(model.form == model_form.plot) %>%
  group_by(G1) %>% summarize(tot = sum(comp.est))

physiol_mat.gg.mosq_order <- physiol_mat.gg.mosq_order[order(physiol_mat.gg.mosq_order$tot, decreasing = T), ]

physiol_mat.gg.f.mm    <- physiol_mat.gg.f.mm[order(physiol_mat.gg.f.mm$comp.est, decreasing = T), ]
physiol_mat.gg.f.mm$G1 <- factor(physiol_mat.gg.f.mm$G1, levels = rev(physiol_mat.gg.mosq_order$G1))
physiol_mat.gg.f.mm$G2 <- factor(physiol_mat.gg.f.mm$G2, levels = physiol_mat.gg.mosq_order$G1)
