
library(tidyverse)
library(readxl)
library(patchwork)
library(ggpubr)
library(ggforce)
library(ggalt)
library(cowplot)
library(extrafont)
#font_import()

# quincy copper

quincy <- read.csv("C:/Users/Jim Byrne/Documents/Phd/research/neochlamydia/Rania/rania_qiime_silva_with_metadata_unrarefied_ratios.csv")

quincy_copper <- quincy %>%
  filter(realorcontrol == "r") %>%
  filter(waterorbiofilm == "w") %>%
  ggplot(aes(x = Legionella.pneumophila.ddPCR..copies.mL., y = Neochlamydia.ddPCR..copies.mL., color = as.factor(Copper))) +
  geom_point(size = 1.5) +
  theme_bw() +
  geom_vline(xintercept = 3, linetype = "dashed", color = "black") +
  geom_hline(yintercept = 400, linetype = "dashed", color = "black") +
  labs(title = "Copper-Dosing Microcosms - High Cu",
       x = expression(italic("Legionella pneumophila (Lp)") * " gene copies/mL"),
       y = expression(italic("Neochlamydia") * " gene copies/mL")) +
  theme(
    panel.grid = element_line(color = "white"),
    text = element_text(family = "Arial",size = 8,  color = "black"),
    axis.title = element_text(size = 8,  color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    legend.title = element_text(size = 8,  color = "black"),
    legend.text = element_text(size = 8, color = "black"),
    plot.title = element_text(size = 10, margin = margin(t=0, b=3)),
    legend.position = c(0.7, 0.7),
    plot.margin = margin(10,5,10,10),
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


ggsave("C:/Users/Jim Byrne/Documents/Phd/research/neochlamydia/letter_figs/Draft 14/fig_S7.tiff",plot = last_plot(), width = 7, height = 4.167, dpi = 300, units = "in", grDevices::tiff)

