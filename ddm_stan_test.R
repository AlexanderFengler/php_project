# Stan testing
library("rstan") # observe startup messages
library('rstanarm') # 
source("ddm_simulator.R")
# Not using multiple cores atm
#options(mc.cores = parallel::detectCores())

rstan_options(auto_write = TRUE)

# SIMPLE DDM ONE SUBJECT ----------
samples = ddm_sim(n_samples = 1000, v = 1, a = 2, w = 0.5, n_dt = 0.5)

ddm_data = list(N = 1000,
                rts = samples$rts,
                choices = samples$choices,
                min_rt = min(samples$rts),
                switch_idx = max(which(samples$choices == -1)))

fit_simple = stan(file = 'wiener_simple.stan', 
                  data = ddm_data, 
                  control = list(adapt_delta = 0.99, max_treedepth = 15),
                  chains = 1)
# ---------------------------------


# DDM MULTIPLE SUBJECTS COLLAPSED ---------------

# Pool subjects
n_subj = 5
n_samples = 500

# Param vectors
v = rep(0, n_subj)
a = rep(0, n_subj)
w = rep(0, n_subj)
n_dt = rep(0, n_subj)

# Data 
min_rt = rep(0, n_subj)
choices = matrix(data = 0, nrow = n_subj, ncol = n_samples)
rts = matrix(data = 0, nrow = n_subj, ncol = n_samples)

for (i in 1:n_subj){
  v[i] = rnorm(n = 1, mean = 0, sd = 1)
  a[i] = runif(n = 1, min = 0, max = 5)
  w[i] = runif(n = 1, min = 0, max = 1)
  n_dt[i] = runif(n = 1, min = 0, max = 1)
  
  dat_tmp = ddm_sim(v = v[i],
                    a = a[i],
                    w = w[i], 
                    n_dt = n_dt[i], 
                    n_samples = n_samples)
  
  choices[i,] = dat_tmp$choices
  rts[i,] = dat_tmp$rts
}

min_rt = min(rts)

n_dt_init = runif(n = 1, 
                  min = 0, 
                  max = min_rt)

ddm_data = list(N = n_samples,
                J = n_subj,
                rts = rts,
                choices = choices,
                min_rt = min_rt)

fit_subj_collapsed = stan(file = 'wiener_subj_collapsed.stan', 
                  data = ddm_data, 
                  control = list(adapt_delta = 0.99, max_treedepth = 15),
                  init = list('1' = list(a = 1, v = 0, w = 0.5, n_dt = n_dt_init)),
                  chains = 1,
                  verbose = TRUE,
                  iter = 2000)
# -----------------------------------------------

# Pool subjects
n_subj = 5
n_samples = 500

# Param vectors
v = rep(0, n_subj)
a = rep(0, n_subj)
w = rep(0, n_subj)
n_dt = rep(0, n_subj)

# Data 
min_rt = rep(0, n_subj)
choices = matrix(data = 0, nrow = n_subj, ncol = n_samples)
rts = matrix(data = 0, nrow = n_subj, ncol = n_samples)

for (i in 1:n_subj){
  v[i] = rnorm(n = 1, mean = 0, sd = 1)
  a[i] = runif(n = 1, min = 0, max = 5)
  w[i] = runif(n = 1, min = 0, max = 1)
  #n_dt[i] = runif(n = 1, min = 0, max = 1)
  
  dat_tmp = ddm_sim(v = v[i],
                    a = a[i],
                    w = w[i], 
                    n_dt = 0, 
                    n_samples = n_samples)
  
  choices[i,] = dat_tmp$choices
  rts[i,] = dat_tmp$rts
}

# min_rts = apply(rts, 1, FUN = min)

# n_dt_init = runif(n = 1, 
#                   min = 0, 
#                   max = min_rt)

ddm_data = list(N = n_samples,
                J = n_subj,
                rts = rts,
                choices = choices)
               # min_rt = min_rt)

fit_subj = stan(file = 'wiener_subj.stan', 
                  data = ddm_data, 
                  control = list(adapt_delta = 0.99, max_treedepth = 15),
                  init = list('1' = list(a = a, v = v, w = w)),
                  chains = 1,
                  verbose = TRUE,
                  iter = 2000)

