library(readxl)
library(vegan)
library(tidyverse)

# load data 
data_Millien <- read_excel("~/school/BIOL4000/raw data/01_Millien_2023_data/01_Millien_2023_compiled.xlsx")
View(data_Millien)

data_Ginsberg <- read_excel("~/school/BIOL4000/raw data/02_Ginsberg_2021_data/02_Ginsberg_2021_compiled.xlsx")
View(data_Ginsberg)



# add shannon diversity index

# Millien et al. (2023)
spp_matrix_Millien <- select(data_Millien, 6:14)
View(spp_matrix_Millien)

spp_H_Millien <- diversity(spp_matrix_Millien)
spp_H_Millien

data_Millien <- cbind(data_Millien, spp_H = spp_H_Millien) # 0 length error 

length(spp_matrix_Millien) # length 9? interesting...
print(is.vector(spp_H_Millien)) # TRUE, so it IS a vector for sure



# logistic regression of tick infection prevalence ~ small mammal richness 

# Millien et al. (2023)
lr_Millien_rich <- glm(formula = prev_quest ~ spp_rich, family = "binomial", data = data_Millien) 
summary(lr_Millien_rich) # beta = 0.2600, SE = 0.5449

exp(0.26) # OR = 1.29693

# Ginsberg et al. (2021)
lr_Ginsberg_rich <- glm(formula = prev_quest ~ spp_rich, family = "binomial", data = data_Ginsberg)
summary(lr_Ginsberg_rich) # beta = -0.2162, SE = 0.6566

exp(-0.2162) # OR = 0.8055742
