library(esc)


# EFFECT SIZE CONVERSIONS

# LoGiudice et al. (2008) *whole model chi-square, df = 1*

esc_chisq(chisq = 9.37, # richness
          totaln = 37,   # sample size of sites        
          es.type = "logit") # OR = 2.1125, SE =  0.6901

1/2.1125 # adjusted OR = 0.4733728 (accounting for negative relationship)


esc_chisq(chisq = 2.1, # shannon H
          totaln = 37,   # sample size of sites        
          es.type = "logit") # OR = 0.8898, SE = 0.6141

# Prusinski et al. (2006)
esc_rpb(r = -0.08,      
        grp1n = 12,   # sample size of sites for small mammals      
        grp2n = 12, # sample size of sites for small mammals
        es.type = "logit") # OR =  -0.2911, SE = 0.7417


# States et al. (2014) *interaction coefficient* 
exp(-0.7) # OR = 0.4965853

log(0.4965853) # OR = -0.7

# Werden et al. (2014) *interaction coefficient* 
exp(0.67) # OR = 1.954237

log(1.954237) # OR = 0.6699998

# Allen et al. (2003)
sqrt(0.43) # r = -0.6557439, negative because negative relationship

esc_rpb(r = -0.6557439,      
        grp1n = 14,   # sample size of sites for small mammals      
        grp2n = 14, # sample size of sites for ticks
        es.type = "logit") # OR = -3.1508, SE = 0.8045
