library(ggplot2)
library(readxl)

sys_df <- read_excel("~/school/BIOL4000/systematic review.xlsx", sheet = "systematic data")

# calculations

# % of proxy/sim
prop.table(table(sys_df$proxy_sim))*100

# % of relationships
prop.table(table(sys_df$relationship))*100

# % bio metric
prop.table(table(sys_df$bio_metric))*100

# splitting df
amp_df <- subset(sys_df, relationship == "amplification")
dil_df <- subset(sys_df, relationship == "dilution")
none_df <- subset(sys_df, relationship == "none")


# % mechanism
prop.table(table(amp_df$mechanism))*100
prop.table(table(dil_df$mechanism))*100
prop.table(table(none_df$mechanism))*100

# % disease metric
prop.table(table(amp_df$disease_metric))*100
prop.table(table(dil_df$disease_metric))*100
prop.table(table(none_df$disease_metric))*100



# plotting
ggplot(sys_df, aes(x = year, y = y, colour = relationship, shape = proxy_sim)) +
  geom_point(stroke = 1.5, size = 3) +
  scale_fill_manual(values = c("#8DD675", "#E74F4F", "black")) +
  scale_shape_manual(values = c(22, 16, 24),
                     name="Use of Proxy or\nSimulations",
                     breaks=c("biodiversity proxy", "none", "simulation"),
                     labels=c("Diversity Proxy (30.0%)", "None (36.7%)", "Simulation Study (33.3%)")) +
  scale_colour_manual(values = c("#8DD675", "#E74F4F", "black"),
                      name="Biodiversity-Disease\nRelationship",
                      breaks=c("amplification", "dilution", "none"),
                      labels=c("Amplification (23.3%)", "Dilution (60.0%)", "None (16.7%)")) +
  scale_x_continuous(limits = c(1993, 2023), breaks = c(1993, 1998, 2003, 2008, 2013, 2018, 2023)) +
  labs(x = "\nPublication Year", y = "Number of Seperate Analyses\n") +
  theme_light() 
