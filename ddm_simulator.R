# DDM Functions:
ddm_sim = function(v = 0,
                   a = 1,
                   w = 0.5,
                   n_dt = 0.3,
                   s = 1,
                   delta_t = 0.001,
                   max_t = 20,
                   n_samples = 20000,
                   print_info = TRUE){
  
  rts  = rep(0, n_samples)
  choices = rep(0, n_samples)
  delta_t_sqrt = sqrt(delta_t)
  particle_pos = 0
  t = 0
  
  for (i in 1:n_samples){

    particle_pos[1] = w * a
    t[1] = 0
    
    while (particle_pos[1] <= a & particle_pos[1] >=0 & t[1] <= max_t){
      particle_pos[1] = particle_pos[1] + (v * delta_t) + (delta_t_sqrt * rnorm(n = 1, mean = 0, sd = 1))
      t[1] = t[1] + delta_t
    }
    # Store choice and reaction time
    rts[i] = t + n_dt
    #choices[i] = (-1) * sign(particle_pos)
    choices[i] = sign(particle_pos)
    
    # Print 
    if (print_info == TRUE){
      if ((i %% 1000) == 0){
        print(paste('Samples finished: ', i)) 
      }
    }
  }
  dat = data.frame(rts = rts, choices = choices)
  dat = dat[order(dat$choices), ]
  return(dat)
}


