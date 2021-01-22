#############################
### Functions for RRV NGM ###
#############################

'%!in%' <- function (x, table) match(x, table, nomatch = 0L) == 0L

simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1, 1)), substring(s, 2),
          sep = "", collapse = " ")
}

## Assume for now a few tiers of competence, which is determined by the titer in the host, lets assume for now
 ## titer follows a Ricker function and that the duration of infection is about 8 days (viremia may fall to levels
  ## beneath what is infectious, so in practice this duration will be less)
titer_prof       <- function (day, a, b) {
  a * day * exp(-b * day)
}

titer_prof_quad  <- function (day, a, b, c) {
  a + b*day + c*day^2
}

## host to mosquito transmission probability follows a logistic function (could divide by k or something if it 
 ## turns out transmission probability never actually reaches 1 (basically does for WNV, so just working with that for now))
h_to_m_inf_prob <- function (titer, a, b) {
  1 / (1 + exp(-(a + b * titer)))
}

## Not too clear on the incubation period in the mosquito, lit seems a bit foggy on it
 ## so for now just assume a less steep logistic 
m_to_h_inf_prob <- function (day, a, b) {
  1 / (1 + exp(-(day - a)/b))
}

## Proportional hazards (I think) (constant survival probability across all times)
mosq_surv_mod <- function (surv_prob, mosq_days) {
  exp(surv_prob * mosq_days)
}

## Cconstant survival probability across all times
mosq_surv_sim <- function (surv_prob, mosq_days) {
  for (i in seq_along(mosq_days)) {
    mosq_days[i]
 cumprod(surv_prob, mosq_days)
  }
}
