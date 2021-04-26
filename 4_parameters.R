########################################
### Parameters for the RRV NGM model ###
########################################

## location of interest
focal.location <-  "brisbane" # focal.location <-  "cairns"

## maximum number of days animals are infectious (larger number than the data shows allowing titer to fall beneath 
 ## detection limit so as to not miss infectious days)
inf_days       <- seq(1, 9, 1)    

## Number of bites per mosquito per day -- Unfortunately the raw R0 estimate treats this as a scalar, and we don't have a good value for it.
 ## Thankfully _as long as we assume this  number is the same for all mosquitoes_ the value used here will not change any of the relative answers
bite_rate      <- 1/2 ## Based on a loose average gonotrophic cycle length for the mosquitoes

## Mark recapture doesn't find mosquitoes after about 10 days however data is really bad... Mosquito survival in the field is notoriously difficult and often
 ## falls back to "expert opinion". In lab, however we do have some data. For example, daily mortality probability of 
 ## 1/25 for Culex annulirostris from Shocket et al. 2018 Elife, then mosq_days_max needs to be _much_ higher to capture the full
  ## lifespan of all possible infected mosquitoes. When using exponential rates exp(-mu) ^ N, it will take till inf to actually decline to zero,
   ## so just taking 1/exp(3) (3 * (1/mu)) and treating all other possiblities for survival after that to be 0
mosq_daily_surv <- 1/12.5 ## From Shocket et al. 2018 Elife max for Culex annulirostris in optimal temperature in the lab is ~1/25. Definitely over-predicts
                         ## survival in the field, so using 1/2 max instead as a loose approximation of the unknown conversion from the lab to the field 
    
## Maximum number of days a mosquito may live (for use to scale infectious period of the mosquito) -- set to be much longer than detection in the field
 ## (similar to host infectious days, set to be longer than the data -- capture miniscule numbers as it decays to zero)
mosq_days_max  <- round((1/mosq_daily_surv) * 3)

## Number of mosquitoes (in total) per host  -- Unfortunately the raw R0 estimate treats this as a scalar, and we don't have a good value for it
 ## Thankfully the number used here gets scaled later by mosquito relative abundance and thus will not change any of the relative answers
m_to_h_ratio   <- 40

## Used in various places to capture uncertainty (e.g. Stan model posterior samples and simulated titer profiles)
n_sims         <- 1000 

## A few parameters to control what uncertainty is propagated
titer.uncer    <- TRUE ## Always set this to true

## Reduce human titer in a strong way as a form of a conservative counter-factual -- i.e. weaker assumption about human titer 
 ## than the full quadratic
red_hum_titer  <- FALSE

## Simulations for titer profiles (see data.titer.R)
n_samps        <- 1000

## Threshold titer detection limit. Found to be 2.2 for Vero cells for RRV. See McLeanetal.2021 Vector-Borne and Zoonotic Diseases
thresh_titer   <- 2.2

## Assumed dose that mosquitoes get exposed to for mosquito -> host and mosquito -> host -> mosquito (this is the median of all infection experiments)
titer_dose     <- 6.4  
