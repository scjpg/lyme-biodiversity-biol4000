library(ggplot2)
library(ggpubr)
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

# % biodiversity metric
prop.table(table(amp_df$bio_metric))*100
prop.table(table(dil_df$bio_metric))*100
prop.table(table(none_df$bio_metric))*100

# % mechanism
prop.table(table(amp_df$mechanism))*100
prop.table(table(dil_df$mechanism))*100
prop.table(table(none_df$mechanism))*100

# % disease metric
prop.table(table(amp_df$disease_metric))*100
prop.table(table(dil_df$disease_metric))*100
prop.table(table(none_df$disease_metric))*100



# propotional bar plots

colourblind_pal <- c("#0072B2", "#E69F00", "#009E73", "#D55E00", 
                     "#CC79A7", "#F0E442", "#56B4E9", "#000000")

# proxy
proxy_df <- subset(sys_df, proxy_sim == "biodiversity proxy")

proxy_plot <- ggplot(proxy_df) +
  geom_bar(aes(year, fill = relationship)) +
  scale_x_continuous(limits = c(1994, 2024), breaks = seq(1995, 2024, 2)) +
  scale_y_continuous(limits = c(0, 5), breaks = c(0, 1, 2, 3, 4, 5)) +
  scale_fill_manual(values = colourblind_pal,
                    name = "Relationship",
                    labels = c("Amplification", "Dilution", "None")) +
  labs(title = "a) Studies Using a Biodiversity Site Proxy (n = 9)\n", x = " ", y = "\n") +
  theme_bw() +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())

# simulation
sim_df <- subset(sys_df, proxy_sim == "simulation")

sim_plot <- ggplot(sim_df) +
  geom_bar(aes(year, fill = relationship)) +
  scale_x_continuous(limits = c(1994, 2024), breaks = seq(1995, 2024, 2)) +
  scale_y_continuous(limits = c(0, 5), breaks = c(0, 1, 2, 3, 4, 5)) +
  scale_fill_manual(values = colourblind_pal,
                    name = "Relationship",
                    labels = c("Amplification", "Dilution", "None")) +
  labs(title = "b) Studies Using Simulations (n = 10)\n", x = " ", y = "Number of Analyses\n") +
  theme_bw() +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())

# none
direct_df <- subset(sys_df, proxy_sim == "none")

direct_plot <- ggplot(direct_df) +
  geom_bar(aes(year, fill = relationship)) +
  scale_x_continuous(limits = c(1994, 2024), breaks = seq(1995, 2024, 2)) +
  scale_y_continuous(limits = c(0, 5), breaks = c(0, 1, 2, 3, 4, 5)) +
  scale_fill_manual(values = colourblind_pal,
                    name = "Relationship",
                    labels = c("Amplification", "Dilution", "None")) +
  labs(title = "c) Studies Not Using Biodiversity Site Proxies or Simulations (n = 11)\n", x = "\nPublication Year", y = "\n") +
  theme_bw() +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())

# combine plots

fig <- ggarrange(proxy_plot, sim_plot, direct_plot,
                 ncol = 1, nrow = 3,
                 common.legend = TRUE, legend = "right")
fig

