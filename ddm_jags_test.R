# Data set generators
source("ddm_simulator.R")

library("rjags") # observe startup messages
load.module('wiener')
load.module('paretoprior')
library('coda') # 

# Not using multiple cores atm
#options(mc.cores = parallel::detectCores())

#rstan_options(auto_write = TRUE)
#source('ddm_simulator.R')

# JAGS
# SIMPLE DDM ONE SUBJECT ----------
n_samples = 1000
samples = ddm_sim(n_samples = n_samples, 
                  v = 1, 
                  a = 2, 
                  w = 0.5, 
                  n_dt = 0.5)

ddm_data = list(N = n_samples,
                rts = samples$rts,
                min_rt = min(samples$rts),
                switch_idx = max(which(samples$choices == -1)))

jags_fit_simple = jags.model(file = 'wiener_simple.txt', 
                              data = ddm_data,
                              #inits = list('1' = list(a = 1.0, v = 0.0, w = 0.0, n_dt = 0.01)),
                              n.chains = 1,
                              n.adapt = 1000)
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
switch_idx = rep(0, n_subj)

# Data 
min_rt = rep(0, n_subj)
# choices = matrix(data = 0, nrow = n_subj, ncol = n_samples)
rts = matrix(data = 0, nrow = n_subj, ncol = n_samples)

for (i in 1:n_subj){
  v[i] = rnorm(n = 1, mean = 0, sd = 1)
  a[i] = runif(n = 1, min = 0, max = 5)
  w[i] = runif(n = 1, min = 0, max = 1)
  #n_dt[i] = runif(n = 1, min = 0, max = 1)
  
  dat_tmp = ddm_sim(v = v[i],
                    a = a[i],
                    w = w[i], 
                    n_dt = n_dt[i], 
                    n_samples = n_samples)
  
  # choices[i,] = dat_tmp$choices
  rts[i,] = dat_tmp$rts
  switch_idx[i] = max(which(dat_tmp$choices == -1))
}

min_rt = min(rts)

n_dt_init = runif(n = 1, 
                  min = 0, 
                  max = min_rt)

ddm_data = list(N = n_samples,
                J = n_subj,
                rts = rts,
                #min_rt = min_rt,
                switch_idx = switch_idx)

jags_fit_collapsed = jags.model(file = 'wiener_collapsed.txt', 
                             data = ddm_data,
                             #inits = list('1' = list(a = 1.0, v = 0.0, w = 0.0, n_dt = 0.01)),
                             n.chains = 1,
                             n.adapt = 1000)

# fit_subj_collapsed = stan(file = 'wiener_subj_collapsed.stan', 
#                           data = ddm_data, 
#                           control = list(adapt_delta = 0.99, max_treedepth = 15),
#                           init = list('1' = list(a = 1, v = 0, w = 0.5, n_dt = n_dt_init)),
#                           chains = 1,
#                           verbose = TRUE,
#                           iter = 2000)
# -----------------------------------------------


# DDM MULTIPLE SUBJECTS HIERARCHICAL IN V -------

# Pool subjects
n_subj = 10
n_samples = 500

# Param vectors
v = rep(0, n_subj)
a = rep(0, n_subj)
w = rep(0, n_subj)
n_dt = rep(0, n_subj)
switch_idx = rep(0, n_subj)

# Data 
min_rt = rep(0, n_subj)
# choices = matrix(data = 0, nrow = n_subj, ncol = n_samples)
rts = matrix(data = 0, nrow = n_subj, ncol = n_samples)

while(min(switch_idx) < 10){
  sim_idx = which(switch_idx < 10)
  print(sim_idx)
  for (i in sim_idx){
    v[i] = rnorm(n = 1, mean = 0, sd = 1)
    a[i] = runif(n = 1, min = 1, max = 5)
    w[i] = runif(n = 1, min = 0.2, max = 0.8)
    #n_dt[i] = runif(n = 1, min = 0, max = 1)
    
    dat_tmp = ddm_sim(v = v[i],
                      a = a[i],
                      w = w[i], 
                      n_dt = n_dt[i], 
                      n_samples = n_samples)
    
    # choices[i,] = dat_tmp$choices
    rts[i,] = dat_tmp$rts
    switch_idx[i] = max(which(dat_tmp$choices == -1))
  }
}
#min_rt = min(rts)
# n_dt_init = runif(n = 1, 
#                   min = 0, 
#                   max = min_rt)

ddm_data = list(N = n_samples,
                J = n_subj,
                rts = rts,
                #min_rt = min_rt,
                switch_idx = switch_idx)

jags_hierarchical_v = jags.model(file = 'wiener_hierarchical_v.txt', 
                                data = ddm_data,
                                #inits = list('1' = list(a = 1.0, v = 0.0, w = 0.0, n_dt = 0.01)),
                                n.chains = 1,
                                n.adapt = 1000)

mcmc_obj = coda.samples(jags_hierarchical_v, 
                        c('mu_v', 'prec_v', 'v', 'a', 'w'), 
                        n.iter = 10000)

# fit_subj_collapsed = stan(file = 'wiener_subj_collapsed.stan', 
#                           data = ddm_data, 
#                           control = list(adapt_delta = 0.99, max_treedepth = 15),
#                           init = list('1' = list(a = 1, v = 0, w = 0.5, n_dt = n_dt_init)),
#                           chains = 1,
#                           verbose = TRUE,
#                           iter = 2000)

# ------------------------------------------------

# DDM MULTIPLE SUBJECTS HIERARCHICAL IN A -------

# Pool subjects
n_subj = 10
n_samples = 500

# Param vectors
v = rep(0, n_subj)
a = rep(0, n_subj)
w = rep(0, n_subj)
n_dt = rep(0, n_subj)
switch_idx = rep(0, n_subj)

# Data 
min_rt = rep(0, n_subj)
# choices = matrix(data = 0, nrow = n_subj, ncol = n_samples)
rts = matrix(data = 0, nrow = n_subj, ncol = n_samples)

while(min(switch_idx) < 10){
  sim_idx = which(switch_idx < 10)
  print(sim_idx)
  for (i in sim_idx){
    v[i] = rnorm(n = 1, mean = 0, sd = 1)
    # Now changed for gamma
    a[i] = rgamma(n = 1, rate = 1, shape = 5)
    w[i] = runif(n = 1, min = 0.2, max = 0.8)
    #n_dt[i] = runif(n = 1, min = 0, max = 1)
    
    dat_tmp = ddm_sim(v = v[i],
                      a = a[i],
                      w = w[i], 
                      n_dt = n_dt[i], 
                      n_samples = n_samples)
    
    # choices[i,] = dat_tmp$choices
    rts[i,] = dat_tmp$rts
    if(length(unique(dat_tmp$choices)) > 1){
    switch_idx[i] = max(which(dat_tmp$choices == -1))
    } else{
    switch_idx[i] = 0
    }
    
  }
}

#min_rt = min(rts)
# n_dt_init = runif(n = 1, 
#                   min = 0, 
#                   max = min_rt)

ddm_data = list(N = n_samples,
                J = n_subj,
                rts = rts,
                #min_rt = min_rt,
                switch_idx = switch_idx)

jags_hierarchical_a = jags.model(file = 'wiener_hierarchical_a.txt', 
                                 data = ddm_data,
                                 #inits = list('1' = list(a = 1.0, v = 0.0, w = 0.0, n_dt = 0.01)),
                                 n.chains = 1,
                                 n.adapt = 1000)

mcmc_obj = coda.samples(jags_hierarchical_a, 
                        c('rate_a', 'shape_a', 'v', 'a', 'w'), 
                        n.iter = 10000)

# fit_subj_collapsed = stan(file = 'wiener_subj_collapsed.stan', 
#                           data = ddm_data, 
#                           control = list(adapt_delta = 0.99, max_treedepth = 15),
#                           init = list('1' = list(a = 1, v = 0, w = 0.5, n_dt = n_dt_init)),
#                           chains = 1,
#                           verbose = TRUE,
#                           iter = 2000)
# ------------------------------------------------

# DDM MULTIPLE SUBJECTS HIERARCHICAL IN A -------

# Pool subjects
n_subj = 10
n_samples = 500

# Param vectors
v = rep(0, n_subj)
a = rep(0, n_subj)
w = rep(0, n_subj)
n_dt = rep(0, n_subj)
switch_idx = rep(0, n_subj)

# Data 
min_rt = rep(0, n_subj)
# choices = matrix(data = 0, nrow = n_subj, ncol = n_samples)
rts = matrix(data = 0, nrow = n_subj, ncol = n_samples)

while(min(switch_idx) < 10){
  sim_idx = which(switch_idx < 10)
  print(sim_idx)
  for (i in sim_idx){
    v[i] = rnorm(n = 1, mean = 0, sd = 1)
    # Now changed for gamma
    a[i] = rgamma(n = 1, rate = 1, shape = 5)
    w[i] = rbeta(n = 1, shape1 = 10, shape2 = 10)
    #n_dt[i] = runif(n = 1, min = 0, max = 1)
    
    dat_tmp = ddm_sim(v = v[i],
                      a = a[i],
                      w = w[i], 
                      n_dt = n_dt[i], 
                      n_samples = n_samples)
    
    # choices[i,] = dat_tmp$choices
    rts[i,] = dat_tmp$rts
    if(length(unique(dat_tmp$choices)) > 1){
      switch_idx[i] = max(which(dat_tmp$choices == -1))
    } else{
      switch_idx[i] = 0
    }
    
  }
}

#min_rt = min(rts)
# n_dt_init = runif(n = 1, 
#                   min = 0, 
#                   max = min_rt)

ddm_data = list(N = n_samples,
                J = n_subj,
                rts = rts,
                #min_rt = min_rt,
                switch_idx = switch_idx)

jags_hierarchical_w = jags.model(file = 'wiener_hierarchical_w.txt', 
                                 data = ddm_data,
                                 #inits = list('1' = list(a = 1.0, v = 0.0, w = 0.0, n_dt = 0.01)),
                                 n.chains = 1,
                                 n.adapt = 1000)

mcmc_obj = coda.samples(jags_hierarchical_w, 
                        c('w_u', 'w_v', 'v', 'a', 'w'), 
                        n.iter = 10000)

# fit_subj_collapsed = stan(file = 'wiener_subj_collapsed.stan', 
#                           data = ddm_data, 
#                           control = list(adapt_delta = 0.99, max_treedepth = 15),
#                           init = list('1' = list(a = 1, v = 0, w = 0.5, n_dt = n_dt_init)),
#                           chains = 1,
#                           verbose = TRUE,
#                           iter = 2000)
# ------------------------------------------------

# DDM HIERARCHICAL FULL --------------------------

# Pool subjects
n_subj = 10
n_samples = 500

# Param vectors
v = rep(0, n_subj)
a = rep(0, n_subj)
w = rep(0, n_subj)
n_dt = rep(0, n_subj)
switch_idx = rep(0, n_subj)

# Data 
min_rt = rep(0, n_subj)
# choices = matrix(data = 0, nrow = n_subj, ncol = n_samples)
rts = matrix(data = 0, nrow = n_subj, ncol = n_samples)

while(min(switch_idx) < 10){
  sim_idx = which(switch_idx < 10)
  print(sim_idx)
  for (i in sim_idx){
    v[i] = rnorm(n = 1, mean = 0, sd = 1)
    # Now changed for gamma
    a[i] = rgamma(n = 1, rate = 1, shape = 5)
    w[i] = rbeta(n = 1, shape1 = 10, shape2 = 10)
    #n_dt[i] = runif(n = 1, min = 0, max = 1)
    
    dat_tmp = ddm_sim(v = v[i],
                      a = a[i],
                      w = w[i], 
                      n_dt = n_dt[i], 
                      n_samples = n_samples)
    
    # choices[i,] = dat_tmp$choices
    rts[i,] = dat_tmp$rts
    if(length(unique(dat_tmp$choices)) > 1){
      switch_idx[i] = max(which(dat_tmp$choices == -1))
    } else{
      switch_idx[i] = 0
    }
    
  }
}

#min_rt = min(rts)
# n_dt_init = runif(n = 1, 
#                   min = 0, 
#                   max = min_rt)

ddm_data = list(N = n_samples,
                J = n_subj,
                rts = rts,
                #min_rt = min_rt,
                switch_idx = switch_idx)

jags_hierarchical_w = jags.model(file = 'wiener_hierarchical.txt', 
                                 data = ddm_data,
                                 #inits = list('1' = list(a = 1.0, v = 0.0, w = 0.0, n_dt = 0.01)),
                                 n.chains = 1,
                                 n.adapt = 1000)

mcmc_obj = coda.samples(jags_hierarchical_w, 
                        c('w_u', 'w_v','mu_v', 'prec_v', 'shape_a', 'rate_a', 'v', 'a', 'w'), 
                        n.iter = 10000)

# fit_subj_collapsed = stan(file = 'wiener_subj_collapsed.stan', 
#                           data = ddm_data, 
#                           control = list(adapt_delta = 0.99, max_treedepth = 15),
#                           init = list('1' = list(a = 1, v = 0, w = 0.5, n_dt = n_dt_init)),
#                           chains = 1,
#                           verbose = TRUE,
#                           iter = 2000)




# ------------------------------------------------

# SOME TESTS -------------------------------------
dat = list(N = 3000)
mod = jags.model(file = 'jags_data_sim_test.txt', 
                 data = dat,
                 n.chains = 1,
                 n.adapt = 0)

out = coda.samples(mod, 
                   c('y'), 
                   n.iter = 1)
# ------------------------------------------------