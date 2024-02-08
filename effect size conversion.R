library(esc)


# EFFECT SIZE CONVERSIONS

# LoGiudice et al. (2008) 
esc_chisq(chisq = 6.19,
          totaln = 37,   # sample size of sites        
          es.type = "cox.or") # OR = 4.3892, SE = 0.5948

esc_chisq(chisq = 6.19,
          totaln = 37,   # sample size of sites        
          es.type = "g") # g = 0.8771, SE = 0.3603


# Prusinski et al. (2006)
esc_rpb(r = -0.08,      
        grp1n = 12,   # sample size of sites      
        grp2n = 3163, # sample size of small mammals tested for infection
        es.type = "cox.or") # OR = 0.1155, SE = 0.4782

esc_rpb(r = -0.08,      
        grp1n = 12,   # sample size of sites      
        grp2n = 3163, # sample size of small mammals tested for infection
        es.type = "g") # g = -1.3076, SE = 0.2897


