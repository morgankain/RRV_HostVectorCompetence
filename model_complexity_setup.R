if (one_off & !all_permuted) {
  
## sequence of increasing model complexity
model.runs <- data.frame(
  use.host_abundance = c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE)
, use.mosq_abundance = c(TRUE, FALSE, TRUE, TRUE, TRUE, TRUE)
, use.mosq_survival  = c(TRUE, TRUE, FALSE, TRUE, TRUE, TRUE)
, use.mosq_bite_pref = c(TRUE, TRUE, TRUE, FALSE, TRUE, TRUE)
, use.host_seroprev  = c(TRUE, TRUE, TRUE, TRUE, FALSE, TRUE)
  )

use.cond_titer     <- TRUE  

} else if (all_permuted & !one_off) {
  
## Check the host ranks when a given component is set to true
  
model.runs <- expand.grid(
  use.host_abundance = c(TRUE, FALSE)
, use.mosq_abundance = c(TRUE, FALSE)
, use.mosq_survival  = c(TRUE, FALSE)
, use.mosq_bite_pref = c(TRUE, FALSE)
, use.host_seroprev  = c(TRUE, FALSE)
)

use.cond_titer     <- TRUE  

} else {

model.runs <- data.frame(
  use.host_abundance = use.host_abundance
, use.mosq_abundance = use.mosq_abundance
, use.mosq_survival  = use.mosq_survival
, use.mosq_bite_pref = use.mosq_bite_pref
, use.host_seroprev  = use.host_seroprev
  )

use.cond_titer     <- TRUE

}
