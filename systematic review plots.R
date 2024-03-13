library(ggplot2)
library(readxl)

sys_df <- read_excel("~/school/BIOL4000/systematic review papers.xlsx", sheet = "systematic data")


ggplot(sys_df, aes(x = year, y = y, colour = relationship, shape = proxy_sim)) +
  geom_point(stroke = 1.5, size = 3) +
  scale_fill_manual(values = c("#8DD675", "#E74F4F", "black")) +
  scale_shape_manual(values = c(22, 16, 24),
                     name="Use of Proxy or\nSimulations",
                     breaks=c("biodiversity proxy", "none", "simulation"),
                     labels=c("Diversity Proxy (29.6%)", "None (33.3%)", "Simulation Study (37.0%)")) +
  scale_colour_manual(values = c("#8DD675", "#E74F4F", "black"),
                      name="Biodiversity-Disease\nRelationship",
                      breaks=c("amplification", "dilution", "none"),
                      labels=c("Amplification (18.5%)", "Dilution (70.4%)", "None (11.1%)")) +
  scale_x_continuous(limits = c(1993, 2023), breaks = c(1993, 1998, 2003, 2008, 2013, 2018, 2023)) +
  labs(x = "\nPublication Year", y = "Number of Seperate Analyses\n") +
  theme_light() 
