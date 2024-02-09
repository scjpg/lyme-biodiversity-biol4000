library(readxl)
library(dplyr)
library(meta)

all_studies <- read_excel("~/school/BIOL4000/meta analysis values.xlsx")

# meta using richness predictor for studies analyzed independently 
studies_rich <- subset(all_studies, meta1 == "1")
View(studies_rich)

meta_rich <- metagen(TE = OR,
                 seTE = SE,
                 studlab = source,
                 data = studies_rich,
                 sm = "OR",
                 fixed = FALSE,
                 random = TRUE,
                 method.tau = "PM",
                 hakn = TRUE)

summary(meta_rich)

forest(meta_rich, 
            sortvar = TE,
            prediction = TRUE, 
            print.tau2 = FALSE,
            leftlabs = c("Author (Year)", "OR", "SE"))

# meta using shannon H predictor for studies analyzed independently
studies_H <- subset(all_studies, meta2 == "1")
View(studies_H)

meta_H <- metagen(TE = OR,
                     seTE = SE,
                     studlab = source,
                     data = studies_H,
                     sm = "OR",
                     fixed = FALSE,
                     random = TRUE,
                     method.tau = "PM",
                     hakn = TRUE)

summary(meta_H)

forest(meta_H, 
       sortvar = TE,
       prediction = TRUE, 
       print.tau2 = FALSE,
       leftlabs = c("Author (Year)", "OR", "SE"))


# meta using only richness and site 
studies_onlyrich <- subset(all_studies, biodiversity_metric == "richness" | biodiversity_metric == "site")
View(studies_onlyrich)

meta_onlyrich <- metagen(TE = OR,
                  seTE = SE,
                  studlab = source,
                  data = studies_onlyrich,
                  sm = "OR",
                  fixed = FALSE,
                  random = TRUE,
                  method.tau = "PM",
                  hakn = TRUE)

summary(meta_onlyrich)

forest(meta_onlyrich, 
       sortvar = TE,
       prediction = TRUE, 
       print.tau2 = FALSE,
       leftlabs = c("Author (Year)", "OR", "SE"))

# meta using only shannon H and site
studies_onlyH <- subset(all_studies, biodiversity_metric == "shannon H" | biodiversity_metric == "site")
View(studies_onlyH)

meta_onlyH <- metagen(TE = OR,
                         seTE = SE,
                         studlab = source,
                         data = studies_onlyH,
                         sm = "OR",
                         fixed = FALSE,
                         random = TRUE,
                         method.tau = "PM",
                         hakn = TRUE)

summary(meta_onlyH)

forest(meta_onlyH, 
       sortvar = TE,
       prediction = TRUE, 
       print.tau2 = FALSE,
       leftlabs = c("Author (Year)", "OR", "SE"))

# meta without site proxy


# meta without small mammal prevalence outcome 
