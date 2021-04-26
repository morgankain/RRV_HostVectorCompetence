#####
## Finally, make sure all the pieces are in the correct orientation for downstream calculations
#####

## Now have XXXX with and without _adj_to_com for physiological responses at the level of the individual host and
 ## a full community-wide response given the full community makeup

## Note: using only _all_samps for now for the full uncertainty

####
## h_to_m_trans_all_samps    : host to mosquito transmission for the community of interest
####
 ##         -- num_days x hosts x mosquitoes x samples
 ##         -- 8        x 16    x 17         x 1000

# h_to_m_trans_all_samps_adj_to_com -- already complete

 ##         -- 8         x 12    x 8         x 1000        FOR BRISBANE
 ##         -- 8         x 12    x 7         x 1000        FOR CAIRNS

####
## m_to_h_trans_all_samps    : mosquito to host transmission
####
 ##         -- num_days x mosquitoes x samples
 ##         -- 40       x 17         x 1000

# just last arrangement needed
m_to_h_trans_all_samps_adj_to_com    <- m_to_h_trans_all_samps[ , match(mosq.ordered, dimnames(m_to_h_trans_all_samps)[[2]]), ]

 ##         -- 40       x 8         x 1000        FOR BRISBANE
 ##         -- 40       x 7         x 1000        FOR CAIRNS

####
## AUC for the mosquitoes
####
mosq_inf_AUC_all_samps_adj_to_com   <- mosq_inf_AUC_all_samps[match(mosq.ordered, dimnames(mosq_inf_AUC_all_samps)[[1]]), ]
mosq_trans_AUC_all_samps_adj_to_com <- mosq_trans_AUC_all_samps[match(mosq.ordered, dimnames(mosq_inf_AUC_all_samps)[[1]]), ]

####
# mos_surv_for_R0_all_samps : mosquito survival
####
 ##         -- num_days x mosquitoes x samples
 ##         -- 40       x 17         x 1000

# just last arrangement needed
mosq_surv_for_R0_all_samps_adj_to_com <- mosq_surv_for_R0_all_samps[ , match(mosq.ordered, dimnames(mosq_surv_for_R0_all_samps)[[2]]), ]

 ##         -- 40       x 8         x 1000        FOR BRISBANE
 ##         -- 40       x 7         x 1000        FOR CAIRNS

####
# mosq_bite_pref_all_samps  : mosquito biting preference given the host community
####
 ##         -- mosquitoes x hosts x samples
 ##         -- 10       x 12    x 1000        

# just last arrangement needed
mosq_bite_pref_all_samps_adj_to_com  <- mosq_bite_pref_all_samps[match(mosq.ordered, dimnames(mosq_bite_pref_all_samps)[[1]]),, ]

 ##         -- 8         x 12    x 1000        FOR BRISBANE
 ##         -- 7         x 12    x 1000        FOR CAIRNS

# host_prop_for_R0       : proportions of each host in the given community
 ##         -- hosts x samples
 ##         -- 12    x 1000                     FOR BRISBANE
 ##         -- 12    x 1000                     FOR CAIRNS

# host_prop_for_R0_adj_to_com  -- already complete

### Really only want full uncertainty so comment this stuff out for now
# m_to_h_trans_adj_to_com      <- m_to_h_trans[ , match(mosq.ordered, dimnames(m_to_h_trans)[[2]])]
# mos_surv_for_R0_adj_to_com   <- mos_surv_for_R0[ , match(mosq.ordered, dimnames(mos_surv_for_R0)[[2]])]
# mosq_bite_pref_adj_to_com    <- mosq_bite_pref[match(mosq.ordered, dimnames(mosq_bite_pref)[[1]]), ]
