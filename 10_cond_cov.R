## rename
lme4_fit <- m_to_h_trans.mod

num_spec <- length(unique(m_to_h_emp$mos_species))

## extract random effect estimates
rand_eff_est    <- getME(lme4_fit, c("b"))@x

## break up the random effects vector into the estimates for each random effect by looking at the order of the random effects
rand_ef_lengths <- numeric(length = length(unlist(lme4_fit@cnms)))

## expand lme4fit@cnms to repeat the focal random effect (species)
ran_ef_terms <- unlist(lapply(lme4_fit@cnms, function (x) length(x)))
names_vec    <- rep(names(lme4_fit@cnms), ran_ef_terms)

## Store number of species random effects (intercept + slope) (estimates + sd)
total_ran_efs    <- length(names_vec)

## determine the lengths of each random effect
for (i in seq_along(names_vec)) {
  rand_ef_lengths[i] <- ifelse(
    names_vec[i] == "mos_species"
  , num_spec  ## number of branches in the phylogeny
  , length(unique(m_to_h_emp$ref)) ## length of the unique values in the data
  )
}

## combine estimates with identifiers
rand_ef_id  <- rbind(names_vec, unlist(lme4_fit@cnms))

cond_cov_mat   <- lme4:::condVar(lme4_fit)
rand_coefs_sd <- vector("list", length = total_ran_efs)
  
## Estimates and names of the random effects
ran_ef_levels      <- getME(lme4_fit, "flist")
which_rand_names   <- apply(rand_ef_id, 2, function(x) paste(x, collapse = "_"))
ran_ef_terms       <- unlist(lapply(lme4_fit@cnms, function (x) length(x))) 
total_ran_efs      <- length(which_rand_names)
temp_ran_names     <- lapply(ran_ef_levels, function(x) rep(unique(x)))

cond_cov_mat   <- lme4:::condVar(lme4_fit)
rand_coefs_sd <- vector("list", length = total_ran_efs)

## Just need the species-level random effect here
ran_ef_terms <- apply(matrix(unique(rand_ef_lengths)), 1, function(x) length(which(rand_ef_lengths == x)))
names_vec    <- rep(names(lme4_fit@cnms), ran_ef_terms)

## row and col of the first val for each random effect
start_loc <- cumsum(rand_ef_lengths) - rand_ef_lengths + 1

## row and col of the last val for each random effect
end_loc <- c(
  num_spec
, num_spec * 2
, length(unique(m_to_h_emp$ref)) + num_spec * 2)

which_sci_rand <- grep("species", which_rand_names)

ranef2 <- cond_cov_mat[
  min(start_loc[which_sci_rand]):max(end_loc[which_sci_rand])
, min(start_loc[which_sci_rand]):max(end_loc[which_sci_rand])
  ]

ranef2_info <- data.frame(
  ranef_level    = rep(seq(1, num_spec, by = 1), each = ran_ef_terms[grep("species", names(temp_ran_names))])
, ranef          = rep(which_rand_names[which_sci_rand], num_spec)
, which_mod_coef = rep(which_sci_rand, num_spec))

ranef2 <- cbind(ranef2_info, as.matrix(ranef2))

model_coefs <- vector("list", 3)

## Sort out the species-level random effect
for (i in which_sci_rand) {
  which_subset          <- which(ranef2[, 3] == i)
  model_coefs[[i]]      <- ranef2[, -c(1, 2, 3)][which_subset, which_subset]
  names(model_coefs)[i] <- paste(paste(rand_ef_id[, i], collapse = "_"), "sd", sep = "_")
}

condvar_branch_array <- array(
  data = 0
  , dim = c(length(which_sci_rand)
  , length(which_sci_rand)
  , rand_ef_lengths[which_sci_rand[1]]))

submat_size_start <- seq(1, nrow(ranef2), by = dim(condvar_branch_array)[1])
submat_size_end   <- seq(dim(condvar_branch_array)[1], nrow(ranef2), by = dim(condvar_branch_array)[1])

## phylo vcov by branch
for (i in seq_along(submat_size_start)) {
  condvar_branch_array[,,i] <- 
    as.matrix(
    ranef2[, -c(1, 2, 3)][
    submat_size_start[i]:submat_size_end[i]
  , submat_size_start[i]:submat_size_end[i]]
    )
}
