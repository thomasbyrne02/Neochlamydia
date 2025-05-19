#essentially biofilms of fig 1


library(tidyverse)
library(readxl)
library(patchwork)
library(ggpubr)
library(ggforce)
library(ggalt)
library(cowplot)

# flint copper
flint <- read.csv("C:/Users/Jim Byrne/Documents/Phd/research/neochlamydia/Didier/didier_qiime_silva_with_metadata_unrarefied_ratios.csv")

flint_copper <- flint %>%
  filter(Iron != "NA") %>%
  filter(Pipe != "NA") %>%
  filter(Pipe == "Copper") %>%
  filter(Sampling == "Swab") %>%
  ggplot(aes(x = Legionella, y = Neochlamydia)) +
  geom_point(aes(shape = Pipe, color = Water), size = 1.5) +
  theme_bw() +
  labs(title = "Flint/Detroit Water Microcosms - Cu Pipes",
       x = NULL,
       y = NULL) +
  theme(
    panel.grid = element_line(color = "white"),
    text = element_text(family = "Arial",size = 8,  color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    legend.title = element_text(size = 8,  color = "black", margin = margin(b=0.5)),
    plot.title = element_text(size = 10, margin = margin(t=0, b=3)),
    legend.text = element_text(size = 8, color = "black"),
    legend.position = c(0.7, 0.7),
    plot.margin = margin(5,10,13,30),
    legend.key.size = unit(.1, "in"),
    legend.spacing.y = unit(-.05, "in")
  ) +
  scale_color_manual(name = "Water Source",
                     values = c("Detroit" = "blue",
                                "Flint" = "green")) +
  scale_shape_manual(name = "Pipe Material",
                     values = c("Copper" = 19)) +
  xlim(0, 0.014) +
  ylim(0, 0.002) +
  guides(
    shape = guide_legend(order = 1),  # Pipe Material first
    color = guide_legend(order = 2)   # Water Source second
  )



# flint pex
flint_pex <- flint %>%
  filter(Iron != "NA") %>%
  filter(Pipe != "NA") %>%
  filter(Pipe == "PEX") %>%
  filter(Sampling == "Swab") %>%
  ggplot(aes(x = Legionella, y = Neochlamydia)) +
  geom_point(aes(color = Water, shape = Pipe), size = 1.5) +
  theme_bw() +
  labs(title = "Flint/Detroit Water Microcosms - PEX Pipes",
       x = NULL,
       y = NULL) +
  theme(
    panel.grid = element_line(color = "white"),
    text = element_text(family = "Arial",size = 8,  color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    legend.title = element_text(size = 8,  color = "black", margin = margin(b=0.5)),
    plot.title = element_text(size = 10, margin = margin(t=0, b=3)),
    legend.text = element_text(size = 8, color = "black"),
    legend.position = c(0.7, 0.7),
    plot.margin = margin(5,2,13,45),
    legend.key.size = unit(.1, "in"),
    legend.spacing.y = unit(-.05, "in")
  ) +
  scale_color_manual(name = "Water Source",
                     values = c("Detroit" = "blue",
                                "Flint" = "green")) +
  scale_shape_manual(name = "Pipe Material",
                     values = c("PEX" = 17)) +
  xlim(0, 0.014) +
  ylim(0, 0.002) +
  guides(
    shape = guide_legend(order = 1),  # Pipe Material first
    color = guide_legend(order = 2)   # Water Source second
  ) 



# quincy copper

quincy <- read.csv("C:/Users/Jim Byrne/Documents/Phd/research/neochlamydia/Rania/rania_qiime_silva_with_metadata_unrarefied_ratios.csv")

quincy_copper <- quincy %>%
  filter(realorcontrol == "r") %>%
  filter(waterorbiofilm == "b") %>%
  filter(Copper == 2000) %>%
  ggplot(aes(x = Legionella, y = Neochlamydia, color = as.factor(Copper))) +
  geom_point(size = 1.5) +
  theme_bw() +
  labs(title = "Copper-Dosing Microcosms - High Cu",
       x = NULL,
       y = NULL) +
  theme(
    panel.grid = element_line(color = "white"),
    text = element_text(family = "Arial",size = 8,  color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    legend.title = element_text(size = 8,  color = "black"),
    plot.title = element_text(size = 10, margin = margin(t=0, b=3)),
    legend.text = element_text(size = 8, color = "black"),
    legend.position = c(0.7, 0.7),
    plot.margin = margin(6,10,2,30),
    legend.key.size = unit(.1, "in"),
    legend.spacing.y = unit(-.05, "in")
  ) +
  scale_color_manual(name = "Copper (µg/L)",
                     values = c("0" = "red",
                                "4" = "orange",
                                "30" = "green",
                                "250" = "blue",
                                "2000" = "magenta")) +
  xlim(0, 0.0013) +
  ylim(0, 0.011)



# quincy low copper

quincy_low_copper <- quincy %>%
  filter(realorcontrol == "r") %>%
  filter(waterorbiofilm == "b") %>%
  filter(Copper != 2000) %>%
  ggplot(aes(x = Legionella, y = Neochlamydia, color = as.factor(Copper))) +
  geom_point(size = 1.5) +
  theme_bw() +
  labs(title = "Copper-Dosing Microcosms - Low Cu",
       x = NULL,
       y = NULL) +
  theme(
    panel.grid = element_line(color = "white"),
    text = element_text(family = "Arial",size = 8,  color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    legend.title = element_text(size = 8,  color = "black"),
    plot.title = element_text(size = 10, margin = margin(t=0, b=3)),
    legend.text = element_text(size = 8, color = "black"),
    legend.position = c(0.7, 0.7),
    plot.margin = margin(6,2,2,45),
    legend.key.size = unit(.1, "in"),
    legend.spacing.y = unit(-.05, "in")
  ) +
  scale_color_manual(name = "Copper (µg/L)",
                     values = c("0" = "red",
                                "4" = "orange",
                                "30" = "green",
                                "250" = "blue",
                                "2000" = "magenta")) +
  xlim(0, 0.0013) +
  ylim(0, 0.011)

theme_set(theme_cowplot(font_family = "Arial"))
combined_plot <- plot_grid(quincy_low_copper, quincy_copper, 
                           flint_pex, flint_copper, 
                           nrow = 2, ncol = 2, 
                           labels = "AUTO", label_size = 11,
                           label_x = 0.05, label_y = 1)

# Add a common title
combined_plot <- ggdraw() + 
  draw_plot(combined_plot, 0, 0, 1, 1)  # Adjust position if needed

final_plot <- ggdraw(combined_plot) + 
  draw_plot(combined_plot, 0, 0, 1, 1) +  # Combine the plots
  draw_label(expression(italic("Legionella") * " Relative Abundance"), fontfamily = "Arial", x = 0.5, y = 0.02, size = 10, fontface = "bold") + 
  draw_label(expression(italic("Neochlamydia") * " Relative Abundance"), fontfamily = "Arial", x = 0.015, y = 0.5, size = 10, angle = 90, fontface = "bold")
final_plot

ggsave("C:/Users/Jim Byrne/Documents/Phd/research/neochlamydia/letter_figs/Draft 14/fig_3.tiff",plot = last_plot(), width = 7, height = 4.167, dpi = 300, units = "in", grDevices::tiff)
