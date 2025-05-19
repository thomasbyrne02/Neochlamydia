# neo vs lp with ddpcr

library(tidyverse)
library(readxl)
library(patchwork)
library(ggpubr)
library(ggforce)
library(ggalt)
library(cowplot)
library(extrafont)
font_import()

# quincy copper

quincy <- read.csv("C:/Users/Jim Byrne/Documents/Phd/research/neochlamydia/Rania/rania_qiime_silva_with_metadata_unrarefied_ratios.csv")

quincy_copper <- quincy %>%
  filter(realorcontrol == "r") %>%
  filter(waterorbiofilm == "w") %>%
  filter(Copper == 2000) %>%
  ggplot(aes(x = Legionella.pneumophila.ddPCR..copies.mL., y = Neochlamydia.ddPCR..copies.mL., color = as.factor(Copper))) +
  geom_point(size = 1.5) +
  theme_bw() +
  geom_vline(xintercept = 1, linetype = "dashed", color = "black") +
  labs(title = "Copper-Dosing Microcosms - High Cu",
       x = NULL,
       y = NULL) +
  theme(
    panel.grid = element_line(color = "white"),
    text = element_text(family = "Arial",size = 8,  color = "black"),
    axis.title = element_text(size = 8,  color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    legend.title = element_text(size = 8,  color = "black"),
    legend.text = element_text(size = 8, color = "black"),
    plot.title = element_text(size = 10, margin = margin(t=0, b=3)),
    legend.position = c(0.7, 0.7),
    plot.margin = margin(10,5,15,35),
    legend.key.size = unit(.1, "in"),
    legend.spacing.y = unit(-.05, "in")
  ) +
  scale_color_manual(name = "Copper (µg/L)",
                     values = c("0" = "red",
                                "4" = "orange",
                                "30" = "green",
                                "250" = "blue",
                                "2000" = "magenta")) +
  xlim(0, 25) +
  ylim(0, 3500)



# quincy low copper

quincy_low_copper <- quincy %>%
  filter(realorcontrol == "r") %>%
  filter(waterorbiofilm == "w") %>%
  filter(Copper != 2000) %>%
  ggplot(aes(x = Legionella.pneumophila.ddPCR..copies.mL., y = Neochlamydia.ddPCR..copies.mL., color = as.factor(Copper))) +
  geom_point(size = 1.5) +
  theme_bw() +
  geom_vline(xintercept = 1, linetype = "dashed", color = "black") +
  labs(title = "Copper-Dosing Microcosms - Low Cu",
       x = NULL,
       y = NULL) +
  theme(
    panel.grid = element_line(color = "white"),
    text = element_text(family = "Arial",size = 8,  color = "black"),
    axis.title = element_text(size = 8,  color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    legend.title = element_text(size = 8,  color = "black"),
    legend.text = element_text(size = 8, color = "black"),
    plot.title = element_text(size = 10, margin = margin(t=0, b=3)),
    legend.position = c(0.7, 0.7),
    plot.margin = margin(10,5,15,35),
    legend.key.size = unit(.1, "in"),
    legend.spacing.y = unit(-.05, "in")
  ) +
  scale_color_manual(name = "Copper (µg/L)",
                     values = c("0" = "red",
                                "4" = "orange",
                                "30" = "green",
                                "250" = "blue",
                                "2000" = "magenta")) +
  xlim(0, 25) +
  ylim(0, 3500)



theme_set(theme_cowplot(font_family = "Arial"))
combined_plot <- plot_grid(quincy_low_copper, quincy_copper, 
                           nrow = 1, ncol = 2, 
                           labels = "AUTO", label_size = 11,
                           label_x = 0.05, label_y = 0.985)

combined_plot <- ggdraw() + 
  draw_plot(combined_plot, 0, 0, 1, 1)  # Adjust position if needed

final_plot <- ggdraw(combined_plot) + 
  draw_plot(combined_plot, 0, 0, 1, 1) +  # Combine the plots
  draw_label(expression(italic("Legionella pneumophila (Lp)") * " gene copies/mL"), fontfamily = "Arial", x = 0.5, y = 0.02, size = 10, fontface = "bold") +  
  draw_label(expression(italic("Neochlamydia") * " gene copies/mL"), fontfamily = "Arial", x = 0.015, y = 0.5, size = 10, angle = 90, fontface = "bold")
final_plot


ggsave("C:/Users/Jim Byrne/Documents/Phd/research/neochlamydia/letter_figs/Draft 14/fig_2.tiff",plot = last_plot(), width = 7, height = 4.167, dpi = 300, units = "in", grDevices::tiff)





#da stats

quincy <- read.csv("C:/Users/Jim Byrne/Documents/Phd/research/neochlamydia/Rania/rania_qiime_silva_with_metadata_unrarefied_ratios.csv")

quincy %>%
  filter(realorcontrol == "r") %>%
  filter(waterorbiofilm == "w") %>%
  filter(Copper == 250) %>%
  filter(Legionella.pneumophila.ddPCR..copies.mL. > 0) %>%
  summarize(mean = mean(Neochlamydia.ddPCR..copies.mL.),
            sd = sd(Neochlamydia.ddPCR..copies.mL.))
#	mean = 253.5576, sd = 115.8479  
253.5576/115.8479
quincy %>%
  filter(realorcontrol == "r") %>%
  filter(waterorbiofilm == "w") %>%
  filter(Copper == 250) %>%
  filter(Legionella.pneumophila.ddPCR..copies.mL. < 0.5) %>%
  summarize(mean = mean(Neochlamydia.ddPCR..copies.mL.),
            sd = sd(Neochlamydia.ddPCR..copies.mL.))
