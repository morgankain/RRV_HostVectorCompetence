################################
### Wrapper for RRV pipeline ###
################################

## Required setup
source("1_needed_packages.R")    ## Packages for analysis and data manipulation
source("2_functions.R")          ## Custom functions for data cleaning, plotting, etc.
source("3_ggplot_theme.R")       ## Theme for beautified plots
source("4_parameters.R")         ## Parameters for the model (for parameters without empirirical data)

## Bring in and clean raw data
source("5_data.R")

## Series of scripts to fit models and arrange data for community-level analysis
source("6_real_data.R")

## Adjust the host and mosquito community for the location of interest
source("12_data.community.R")

## Last step of arrangement before R0 and other downstream analyses
source("13_data.R0_setup.R")

## Identify the specific pieces of data that we want to consider in our estimates of competence.
 ## This can be done:
  ## A) manually (with details on each option below)
  ## B) in a more extensive permuted manner or by removing one at a time with all others on (see one_off, all_permuted, and model_complexity_setup.R)

## Set all host species to be identically abundant or use abundance data?
use.host_abundance <- TRUE
## Set all mosquito species to be identically abundant or use abundance data?
use.mosq_abundance <- TRUE
## Set mosquitoes to bite randomly or use abundance data?
use.mosq_bite_pref <- TRUE
## Let mosquito survival vary by mosquito species or assume all mosquitoes have equal survival?
use.mosq_survival  <- TRUE
## Consider an epidemic setting (FALSE) or data on background community seroprevalence (TRUE)?
 ## uses 1 - host_sero$prop_positive
use.host_seroprev  <- TRUE
## Consider titer | successful infection (TRUE), or just assume each host that gets bit develops the viremia profile (FALSE)?
 ## uses prop_inf_for_R0$num_inf, which is a subset of prop_inf_gbite$num_inf
use.cond_titer     <- TRUE

## overall measure of the complexity of the model (number of ecological characteristics to include)
model.complexity   <- use.host_abundance + use.mosq_abundance + use.mosq_bite_pref + use.mosq_survival + use.host_seroprev + use.cond_titer

## Setup the structure for which ecological components to include/exclude
one_off            <- FALSE         ## Run with all ecological components and with each one removed one at a time
all_permuted       <- FALSE         ## Run the model adding in ecological components in all possible orders in an attempt to capture the relative impact
                                    ## If both are false, just takes the above specifications
 ## of each (i.e. average change in median across all possible orders of adding the ecological components)
source("14_model_complexity_setup.R")

for (complexity_counter in 1:nrow(model.runs)) {
  
use.host_abundance <- model.runs[complexity_counter, ]$use.host_abundance  ## has uncertainty (very minimally -- from mosquito biting preference model)
use.mosq_abundance <- model.runs[complexity_counter, ]$use.mosq_abundance  ## no uncertainty
use.mosq_bite_pref <- model.runs[complexity_counter, ]$use.mosq_bite_pref  ## has uncertainty
use.mosq_survival  <- model.runs[complexity_counter, ]$use.mosq_survival   ## no uncertainty
use.host_seroprev  <- model.runs[complexity_counter, ]$use.host_seroprev   ## no uncertainty
model.complexity   <- use.host_abundance + use.mosq_abundance + use.mosq_bite_pref + use.mosq_survival + use.host_seroprev

#### 
## Two options for summarizing the "raw" quantitative competence estimates. 
## 1) Take 95% CI etc. of the raw estimates themselves -- needed for the proportional infection matrices for the main text figures
## 2) First take proportion of the max for each species then take 95% CI etc. on that -- needed for the supplemental species importance figures
####
summary.type <- "raw" # "proportion" #   

## 3) Calculate host competence as host-mosquito host-host and host R0. Also calculate R0 of the whole community
source("15_R0_calc.R")
  
}

### Many objects are returned from the above loop, with possibly confusing names:

## 1) WAIFW_right is renamed as host_competence, which is the raw host-mosquito transmission. This is summarized to host_competence_one.ci.r which is used (with a bit of plot cleanup)
 ## to plot host competence as defined by host-mosquito transmission

## 2) physiol_mat provides the raw values for all host-host transmission pairs over all uncertainty

## 3) host_competence.one.hh (which is calculated as the sum over the matrix returned by WAIFW_left %*% WAIFW_right) measures the total number of hosts in generation 2 infected by
 ## infected hosts of each type in generation 1. Summarized to host_competence.one.ci.r.hh for plotting

## 4) mosq_competence, summarized to mosq_competence.ci.r provides the plotting object for mosquito competence defined as a mosquitoes ability to pick up infection from a host
 ## Note: when weighted by mosquito abundance this just gives the number of each mosquito that would become infected given an initial host infection

## 5) mosq_competence.mm, summarized to mosq_competence.r.gg.mm is the plotting object for mosquito competence defined as mosquito back to mosquito transmission

## If using all permuted == TRUE, summarize the range of changes in ranks that occur when each ecological component is added in in
 ## all possible manners (as a measure of relative impact)
if (all_permuted & !one_off) {
source("19_summarize_all_permuted.R")
}

## Clean up results for plots
source("20_results_summary_for_plots.R")

## Extra cleanup for figure 2 individual panels
source("21_results_summary_for_plots_figure2_panels.R")

## Plots in progress for the manuscript !!!!! Open and run, don't source these !!!!!!
# source("22_manuscript_plots_fig2_panels.R")
# source("23_manuscript_plots.R")

## The multi-generation approximation is run separately from the above
source("24_multi_gen_predictions.R")

