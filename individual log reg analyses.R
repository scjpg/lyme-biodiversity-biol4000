library(readxl)
library(vegan)
library(tidyverse)
library(esc)

# load data 
data_Millien <- read_excel("~/school/BIOL4000/raw data/01_Millien_2023_data/01_Millien_2023_compiled.xlsx")
View(data_Millien)

data_Ginsberg <- read_excel("~/school/BIOL4000/raw data/02_Ginsberg_2021_data/02_Ginsberg_2021_compiled.xlsx")
View(data_Ginsberg)

data_Anderson <- read_excel("~/school/BIOL4000/raw data/03_Anderson_2006_data/03_Anderson_2006_compiled.xlsx")
View(data_Anderson)


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

# Anderson et al. (2006)
spp_matrix_Anderson <- select(data_Anderson, 4:10)
View(spp_matrix_Anderson)

spp_H_Anderson <- diversity(spp_matrix_Anderson)
spp_H_Anderson

data_Anderson <- cbind(data_Anderson, spp_H = spp_H_Anderson)
View(data_Anderson)

# logistic regression of tick infection prevalence ~ small mammal richness 

# Millien et al. (2023)
lr_Millien_rich <- glm(formula = prev_quest ~ spp_rich, family = "binomial", data = data_Millien) 
summary(lr_Millien_rich) # beta = 0.2600, SE = 0.5449, p = 0.633


# Ginsberg et al. (2021)
lr_Ginsberg_rich <- glm(formula = prev_quest ~ spp_rich, family = "binomial", data = data_Ginsberg)
summary(lr_Ginsberg_rich) # beta = -0.2162, SE = 0.6566, p = 0.742


# Anderson et al. (2006)
lr_Anderson_rich <- glm(formula = prev_pool ~ spp_rich, family = "binomial", data = data_Anderson)
summary(lr_Anderson_rich) # beta = 0.1656, SE = 0.6339, p = 0.816



# logistic regression of tick infection prevalence ~ small mammal Shannon H

# Millien et al. (2023)
lr_Millien_H <- glm(formula = prev_quest ~ spp_H, family = "binomial", data = data_Millien) 
summary(lr_Millien_H) # beta = 1.530, SE = 2.221, p = 0.4909


# Ginsberg et al. (2021)
lr_Ginsberg_H <- glm(formula = prev_quest ~ spp_H, family = "binomial", data = data_Ginsberg)
summary(lr_Ginsberg_H) # beta = -1.055, SE = 3.016, p = 0.726


# Anderson et al. (2006)
lr_Anderson_H <- glm(formula = prev_pool ~ spp_H, family = "binomial", data = data_Anderson) 
summary(lr_Anderson_H) # beta = 0.2682, SE = 6.2746, p = 0.983
