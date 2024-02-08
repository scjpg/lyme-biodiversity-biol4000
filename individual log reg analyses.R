library(readxl)
library(vegan)
library(tidyverse)
library(esc)

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

data_Millien <- cbind(data_Millien, spp_H = spp_H_Millien)
View(data_Millien)

# Ginsberg et al. (2021)
spp_matrix_Ginsberg <- select(data_Ginsberg, 7:18)
View(spp_matrix_Ginsberg)

spp_H_Ginsberg <- diversity(spp_matrix_Ginsberg)
spp_H_Ginsberg

data_Ginsberg <- cbind(data_Ginsberg, spp_H = spp_H_Ginsberg)
View(data_Ginsberg)




# logistic regression of tick infection prevalence ~ small mammal richness 

# Millien et al. (2023)
lr_Millien_rich <- glm(formula = prev_quest ~ spp_rich, family = "binomial", data = data_Millien) 
summary(lr_Millien_rich) # beta = 0.2600, SE = 0.5449

exp(0.26) # OR = 1.29693

convert_or2d(or = 1.29693,
             se = 0.5449,
             totaln = 29,
             es.type = "g") # g = 0.1393, SE = 0.3004

# Ginsberg et al. (2021)
lr_Ginsberg_rich <- glm(formula = prev_quest ~ spp_rich, family = "binomial", data = data_Ginsberg)
summary(lr_Ginsberg_rich) # beta = -0.2162, SE = 0.6566

exp(-0.2162) # OR = 0.8055742

convert_or2d(or = 0.8055742,
             se = 0.6566,
             totaln = 3,
             es.type = "g") # g = 0, SE = 0.3620

# logistic regression of tick infection prevalence ~ small mammal Shannon H

# Millien et al. (2023)
lr_Millien_H <- glm(formula = prev_quest ~ spp_H, family = "binomial", data = data_Millien) 
summary(lr_Millien_H) # beta = 1.530, SE = 2.221

exp(1.530) # OR = 4.618177


# Ginsberg et al. (2021)
lr_Ginsberg_H <- glm(formula = prev_quest ~ spp_H, family = "binomial", data = data_Ginsberg)
summary(lr_Ginsberg_H) # beta = -1.055, SE = 3.016

exp(-1.055) # OR = 0.3481924
