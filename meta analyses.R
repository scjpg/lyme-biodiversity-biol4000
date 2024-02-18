library(readxl)
library(dplyr)
library(meta)

all_studies <- read_excel("~/school/BIOL4000/meta analysis values.xlsx")

# meta using richness predictor for studies analyzed independently 
studies_rich <- subset(all_studies, meta1 == "1")
View(studies_rich)

meta_rich <- metagen(TE = lnOR,
                 seTE = lnSE,
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
       leftlabs = c("Author (Year)", "OR", "SE"),
       xlab = "Log Odds Ratio")

# meta using shannon H predictor for studies analyzed independently
studies_H <- subset(all_studies, meta2 == "1")
View(studies_H)

meta_H <- metagen(TE = lnOR,
                     seTE = lnSE,
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
       leftlabs = c("Author (Year)", "OR", "SE"),
       xlab = "Log Odds Ratio")


# meta using only richness and site 
studies_onlyrich <- subset(all_studies, biodiversity_metric == "richness" | biodiversity_metric == "site")
View(studies_onlyrich)

meta_onlyrich <- metagen(TE = lnOR,
                  seTE = lnSE,
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
       leftlabs = c("Author (Year)", "OR", "SE"),
       xlab = "Log Odds Ratio")

# meta using only shannon H and site
studies_onlyH <- subset(all_studies, biodiversity_metric == "shannon H" | biodiversity_metric == "site")
View(studies_onlyH)

meta_onlyH <- metagen(TE = lnOR,
                         seTE = lnSE,
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
       leftlabs = c("Author (Year)", "OR", "SE"),
       xlab = "Log Odds Ratio")

# meta without site proxy
studies_rich_nosite <- subset(studies_rich, biodiversity_metric == "richness" | biodiversity_metric == "shannon H")
View(studies_rich_nosite)

meta_rich_nosite <- metagen(TE = lnOR,
                      seTE = lnSE,
                      studlab = source,
                      data = studies_rich_nosite,
                      sm = "OR",
                      fixed = FALSE,
                      random = TRUE,
                      method.tau = "PM",
                      hakn = TRUE)

summary(meta_rich_nosite) 

forest(meta_rich_nosite, 
       sortvar = TE,
       prediction = TRUE, 
       print.tau2 = FALSE,
       leftlabs = c("Author (Year)", "OR", "SE"),
       xlab = "Log Odds Ratio")

# meta regression w/o LoGiudice et al. (no sampling area data)
studies_rich_nosite_noLoGiudice <- subset(studies_rich_nosite, source != "LoGiudice et al. (2008)") 
View(studies_rich_nosite_noLoGiudice)

meta_rich_nosite_noLoGiudice <- metagen(TE = lnOR,
                            seTE = lnSE,
                            studlab = source,
                            data = studies_rich_nosite_noLoGiudice,
                            sm = "OR",
                            fixed = FALSE,
                            random = TRUE,
                            method.tau = "PM",
                            hakn = TRUE)

summary(meta_rich_nosite_noLoGiudice)

forest(meta_rich_nosite_noLoGiudice, 
       sortvar = TE,
       prediction = TRUE, 
       print.tau2 = FALSE,
       leftlabs = c("Author (Year)", "OR", "SE"),
       xlab = "Log Odds Ratio")

metareg_noLoGiudice_scale <- metareg(meta_rich_nosite_noLoGiudice, ~ samp_ha)
summary(metareg_noLoGiudice_scale)

bubble(metareg_noLoGiudice_scale, 
       studlab = TRUE,
       min.cex = 0.8,
       cex.studlab = 0.6,
       pos.studlab = 2,
       offset = 1,
       xlab = "Total Sample Site Area (ha)",
       ylab = "Log Odds Ratio")
