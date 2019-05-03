# Procedures on samples
source('ddm_simulator.R')

posterior_predictive_samples = function(post_samples = data.frame(a = 0, 
                                                                  b = 0, 
                                                                  c = 0),
                                        model = 'simple',
                                        n = 2000){

idx = sample(1:n, size = n, replace = TRUE)
samples = data.frame(rts = rep(0, n), 
                     choices = rep(0, n))

if (model == 'simple'){
  cnt = 1
  for (i in idx){
    a_tmp = post_samples$a[i]
    w_tmp = post_samples$w[i]
    v_tmp = post_samples$v[i]
    n_dt_tmp = post_samples$n_dt[i]
      
    samples[cnt,] = ddm_sim(v = v_tmp, 
                        a = a_tmp, 
                        w = w_tmp, 
                        n_dt = n_dt_tmp, 
                        n_samples = 1)
    cnt = cnt + 1
    if ((cnt %% 100) == 0){
      print(cnt)
    }
  }
  
  return(samples)
  }
}