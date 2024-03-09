library(readxl)
library(dplyr)
library(meta)
library(ggplot2)
library(metafor)

all_studies <- read_excel("~/school/BIOL4000/meta analysis values.xlsx")

# meta using richness predictor for studies analyzed independently 
studies_rich <- subset(all_studies, meta1 == "1")

meta_rich <- metagen(TE = lnOR,
                 seTE = lnSE,
                 studlab = source,
                 data = studies_rich,
                 sm = "OR",
                 fixed = FALSE,
                 random = TRUE,
                 method.tau = "SJ",
                 hakn = TRUE,
                 prediction = TRUE)

summary(meta_rich)

forest(meta_rich, 
       sortvar = TE,
       prediction = TRUE, 
       print.tau2 = FALSE,
       leftlabs = c("Author (Year)", "OR", "SE"),
       xlab = "Log Odds Ratio")

# funnel plot tests 
col.contour = c("gray70", "gray85", "gray95")

funnel(meta_rich,
       studlab = TRUE,
       contour = c(0.9, 0.95, 0.99),
       col.contour = col.contour,
       pos.studlab = 4,
       xlab = "Odds Ratio",
       cex.studlab = 0.6) +
  legend(x = 0.05, y = 0.05, 
       legend = c("p < 0.1", "p < 0.05", "p < 0.01"),
       fill = col.contour)

metabias(meta_rich, 
         method.bias = "thompson",
         k.min = 8,
         plotit = TRUE)

# meta using only richness and site 
studies_onlyrich <- subset(all_studies, biodiversity_metric == "richness" | biodiversity_metric == "site")

meta_onlyrich <- metagen(TE = lnOR,
                  seTE = lnSE,
                  studlab = source,
                  data = studies_onlyrich,
                  sm = "OR",
                  fixed = FALSE,
                  random = TRUE,
                  method.tau = "SJ",
                  hakn = TRUE,
                  prediction = TRUE)

summary(meta_onlyrich)

forest(meta_onlyrich, 
       sortvar = TE,
       prediction = TRUE, 
       print.tau2 = FALSE,
       print.I2.ci = TRUE,
       addrows.below.overall = 3,
       leftcols = "studlab",
       leftlabs = "Author (Year)",
       rightlabs = c("OR", "95% CI", "Weight"),
       text.random = "Random effects model (K-H)")

# meta using only shannon H and site
studies_onlyH <- subset(all_studies, biodiversity_metric == "shannon H" | biodiversity_metric == "site")

meta_onlyH <- metagen(TE = lnOR,
                         seTE = lnSE,
                         studlab = source,
                         data = studies_onlyH,
                         sm = "OR",
                         fixed = FALSE,
                         random = TRUE,
                         method.tau = "SJ",
                         hakn = TRUE,
                      prediction = TRUE)

summary(meta_onlyH)

forest(meta_onlyH, 
       sortvar = TE,
       prediction = TRUE, 
       print.tau2 = FALSE,
       print.I2.ci = TRUE,
       addrows.below.overall = 3,
       leftcols = "studlab",
       leftlabs = "Author (Year)",
       rightlabs = c("OR", "95% CI", "Weight"),
       text.random = "Random effects model (K-H)")


# meta without site proxy
studies_rich_nosite <- subset(studies_rich, biodiversity_metric == "richness" | biodiversity_metric == "shannon H")

meta_rich_nosite <- metagen(TE = lnOR,
                      seTE = lnSE,
                      studlab = source,
                      data = studies_rich_nosite,
                      sm = "OR",
                      fixed = TRUE,
                      random = TRUE,
                      method.tau = "SJ",
                      hakn = TRUE,
                      prediction = TRUE)

summary(meta_rich_nosite) 


forest(meta_rich_nosite, 
       sortvar = TE,
       weight.study = "common",
       prediction = TRUE, 
       print.tau2 = FALSE,
       print.I2.ci = TRUE,
       addrows.below.overall = 3,
       leftcols = "studlab",
       leftlabs = "Author (Year)",
       rightlabs = c("OR", "95% CI", "Weight"),
       lab.NA.weight = " ",
       text.w.common = "fixed",
       text.common = "Fixed effect model",
       text.random = "Random effects model (K-H)")

# only random effects 
meta_rich_nosite_r <- metagen(TE = lnOR,
                            seTE = lnSE,
                            studlab = source,
                            data = studies_rich_nosite,
                            sm = "OR",
                            fixed = FALSE,
                            random = TRUE,
                            method.tau = "SJ",
                            hakn = TRUE,
                            prediction = TRUE)

forest(meta_rich_nosite_r, 
       sortvar = TE,
       prediction = TRUE, 
       print.tau2 = FALSE,
       print.I2.ci = TRUE,
       addrows.below.overall = 3,
       leftcols = "studlab",
       leftlabs = "Author (Year)",
       rightlabs = c("OR", "95% CI", "Weight"),
       text.random = "Random effects model (K-H)")

# only richness 

meta_rich_nosite_f <- metagen(TE = lnOR,
                              seTE = lnSE,
                              studlab = source,
                              data = studies_rich_nosite,
                              sm = "OR",
                              fixed = FALSE,
                              random = FALSE,
                              method.tau = "SJ",
                              hakn = TRUE,
                              prediction = TRUE)

# meta regression w/o LoGiudice et al. (no sampling area data)
studies_rich_nosite_noLoGiudice <- subset(studies_rich_nosite, source != "LoGiudice et al. (2008)") 

meta_rich_nosite_noLoGiudice <- metagen(TE = lnOR,
                            seTE = lnSE,
                            studlab = source,
                            data = studies_rich_nosite_noLoGiudice,
                            sm = "OR",
                            fixed = TRUE,
                            random = TRUE,
                            method.tau = "SJ",
                            hakn = TRUE,
                            prediction = TRUE)

summary(meta_rich_nosite_noLoGiudice)

forest(meta_rich_nosite_noLoGiudice, 
       sortvar = TE,
       prediction = TRUE, 
       print.tau2 = FALSE,
       leftlabs = c("Author (Year)", "OR", "SE"),
       xlab = "Log Odds Ratio")

metareg_noLoGiudice_scale <- metareg(meta_rich_nosite_noLoGiudice, ~ samp_ha, 
                                     method.tau = "SJ",
                                     hakn = TRUE)
summary(metareg_noLoGiudice_scale)

# calculating confidence intervals for I^2

Q <- 2.1782

df <- 3
  
b = sqrt(1/(2*(df-1)*(1-(1/(3*(df-1)^2)))))

# lower CI
lower = exp(0.5*log(Q/df)-1.96*b)

lower_limit = ((lower^2)-1)/(lower^2)*100 # -966.8193 --> round up to 0

# upper CI
upper = exp(0.5*log(Q/df)+1.96*b)

upper_limit = ((upper^2)-1)/(upper^2)*100 # 82.219

# bubble plots

# using meta --> very ugly!

bubble(metareg_noLoGiudice_scale,
       col.ref = "red",
       xlab = "Total Sample Site Area (ha)",
       ylab = "Odds Ratio") 

# using ggplot

metareg_df <- data.frame(meta_rich_nosite_noLoGiudice)

metareg_df <- cbind(metareg_df, samp_ha = studies_rich_nosite_noLoGiudice$samp_ha)

ggplot(metareg_df, aes(x = samp_ha, y = TE, size = w.random, colour = studlab, fill = studlab)) +
  scale_x_continuous(limits = c(5, 55), breaks = c(10, 20, 30, 40, 50)) +
  scale_y_continuous(limits = c(-1.8, 1.6), breaks = seq(-2, 1.6, 0.4)) +
  geom_errorbar(aes(x = samp_ha, y = TE, ymin = lower, ymax = upper), size = 1) +
  geom_point(shape = 21, stroke = 1.5, alpha = 0.5) +
  scale_size(range = c(5, 13)) +
  geom_abline(intercept = coef(metareg_noLoGiudice_scale)[1], slope = coef(metareg_noLoGiudice_scale)[2]) +
  geom_hline(yintercept = 0, colour = "red") +
  labs(x = "\nTotal Sample Site Area (ha)", y = "Log Odds Ratio\n") +
  guides(size = FALSE, fill = FALSE) +
  labs(colour = "Author (Year)") +
  theme_light()
