data {
 
	int<lower=0> N;                      	                // number of observations in proportion data
	int<lower=0> J;                      	                // number of mosquito species
 	int<lower=0> host_counts[N];		         	// observed host abundance data
	int<lower=0> bites[J, N];         			// bite data of length N
	vector<lower=0>[N] flat_alpha_p;			// Vector that could be informed by some other data to weight previous "observations" that form the prior
	vector<lower=0>[N] prev_obs_p;				// The previous observations that can get scaled by flat_alpha_p (for now flat_alpha_p is all 1s)
	real sigma_pb_a;					// first of gamma parameters for hyper-prior that establishes the variation in a mosquitoes biting preference
	real sigma_pb_b;					// second of gamma parameters for hyper-prior that establishes the variation in a mosquitoes biting preference

}

transformed data {
	 vector<lower=0>[N] alpha_p;
	 alpha_p = flat_alpha_p .* prev_obs_p;			// sets up strength of prior
}

parameters { 

 	// host proportions
	simplex[N] theta_p;	 				// vector of length = K dimensions that sums to 1
	
	// biting preference
	matrix<lower=0>[J, N] bite_pref;                        // underlying latent parameter of interest. Intrinsic preference of a mosquito on a given host
	
	// Variation among mosquitoes in their biting preference distribution
	// Goal is to have this large enough that it allows some hosts to be generalists and some hosts to be specialists
	vector<lower=0>[J] prev_obs_bA;	
	vector<lower=0>[J] prev_obs_bB;				 
				
} 

model { 
 	// host data. host_counts is the raw data that informs theta_p (which gets mixed with the prior on host proportions) to inform the bite data
	theta_p ~ dirichlet(alpha_p);				// prior on host proportions
	host_counts ~ multinomial(theta_p);			// Actual observed abundance is some multinomial draw from the underlying proportions

	// biting preference 
for (j in 1:J) {
	// Currently the prior on each mosquitoes preference across hosts is gamma distributed, where the parameters are drawn from the hyper
	prev_obs_bA[j] ~ gamma(sigma_pb_a, sigma_pb_a);	
	prev_obs_bB[j] ~ gamma(sigma_pb_b, sigma_pb_b);	
	bite_pref[j, ] ~ gamma(prev_obs_bA[j], prev_obs_bB[j]);		
}


	// model fit to blood meal data
for (j in 1:J) {

	bites[j, ] ~ multinomial((to_vector(theta_p) .* to_vector(bite_pref[j, ])) /
		 sum(to_vector(theta_p) .* to_vector(bite_pref[j, ])));	
	
}

} 

