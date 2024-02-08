library(esc)


# EFFECT SIZE CONVERSIONS

# LoGiudice et al. (2008) 
esc_chisq(chisq = 6.19,
          totaln = 37,   # sample size of sites        
          es.type = "cox.or") # OR = 4.3892, SE = 0.5948


# Prusinski et al. (2006)
esc_rpb(r = -0.08,      
        grp1n = 12,   # sample size of sites for small mammals      
        grp2n = 12, # sample size of sites for small mammals
        es.type = "cox.or") # OR =  0.7673, SE = 0.6750


# States et al. (2014) *interaction coefficient* 
exp(-0.7) # OR = 0.4965853


# Werden et al. (2014) *interaction coefficient* 
exp(0.67) # OR = 1.954237

# Allen et al. (2003)
sqrt(0.43) # r = -0.6557439, negative because negative relationship

esc_rpb(r = -0.6557439,      
        grp1n = 14,   # sample size of sites for small mammals      
        grp2n = 14, # sample size of sites for ticks
        es.type = "cox.or") # OR = 0.0569, SE = 0.7322
