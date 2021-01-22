Code for running the model presented in: "Physiology and ecology combine to determine host and vector importance for Ross River virus and other vector-borne diseases" submitted to eLife on January, 22, 2021.
All code needed to run the model is included now. Data needed to run the model will be made public upon paper acceptance to eLife or elsewhere.

---------
R scripts
---------

top_level_script.R      -- The first/main script to load to run the model. All other needed scripts are sourced from within this script

**In alphabetical order, not in the order of use / sourcing**

cond_cov.R              -- Extract the conditional modes and covariances from the random effects model for mosquito transmission
data.community.R        -- Organize the statistical model output into the appropriate orientation/groupings for the data.hm.R               -- Statistical sub-model and predictions for mosquito infection probability (hm = host to mosquito)data.mh.R               -- Statistical sub-model and predictions for mosquito transmission probability (mh = mosquito to host)data.mosq_bite_pref.R   -- Statistical sub-model and predictions for mosquito feeding preferences data.mosq_surv_same.R   -- Statistical sub-model and predictions for mosquito survival (same = no variation among species)data.R                  -- Load and clean all required data filesdata.R0_setup.R         -- Organize sub-model predictions for the R0 calculationdata.titer.R            -- Statistical sub-model and predictions for mosquito survival (same = no variation among species)functions.R             -- Functions used at various points in the other scriptsggplot_theme.R          -- theme for cleaner ggplotshost_competence_cleanup.R       -- Clean up host competence from the pairwise transmission matriceshost_mosq_comp_remaining.R      -- Calculate the remaining components of host physiological and ecological competence that are not calculated "for free" in the main R0 calculation scriptmanuscript_plots_fig2_panels.R  -- All panels for main text figure 2 and figure 3manuscript_plots.R              -- Remaining manuscripts plotsmatrix_algebra_exploration.R    -- Script to walk through the R0 and competence calculations from the HM and MH matricesmodel_complexity_setup.R        -- Set up for which components of the model uncertainty will be propagated  
mosq_competence_cleanup.R       -- Clean up mosquito competence from the pairwise transmission matrices
multi_gen_predictions.R         -- Run the multiple generation approximationneeded_packages.R       -- All required packagesparameters.R            -- Define the values for the free parameters in the model
R0_calc.R               -- Set up MH, HM and Calculate R0 and some of the competence metrics from all of the statistical sub-modelsreal_data.R             -- Wrapper to source all of the Statistical sub-model scripts results_summary_for_plots_figure2_panels.R  -- Summarize and clean the output from the calculation scripts for result figure 2 and 3 ggplotsresults_summary_for_plots.R     -- Summarize and clean the output from the calculation scripts for remaining results and supplement figures summarize_all_permuted.R        -- Analysis that isn't actually presented in the manuscript, but allows for calculating the addition of all physiological and ecological components one at a time to do a form of sensitivity analysis

----
Data
----

mosquito_transmission.csv    -- Mosquito transmission probability over time following experimental infectionhost_abundance.csv           -- Abundance of hosts in Brisbanehost_response.csv            -- Non-human host titer response to RRV infectionhost_seroprevalence.csv      -- Proportion of hosts seroprevalent in Brisbane human_titre.csv              -- Human titer response to RRV infectionmosquito_abundance.csv       -- Abundance of mosquitoes in Brisbanemosquito_feeding.csv         -- Blood meals of wild caught mosquitoes in Brisbanemosquito_infection.csv       -- Mosquito infection probability over dose following experimental infection


-----------
Other files
-----------

mosq_bite_random.stan   -- Stan model for mosquito feeding preferences
