
quincy <- read.csv("C:/Users/Jim Byrne/Documents/Phd/research/neochlamydia/Rania/rania_qiime_silva_with_metadata_unrarefied_ratios.csv")

quincy_copper <- quincy %>%
  filter(realorcontrol == "r") %>%
  filter(waterorbiofilm == "w") %>%
  ggplot(aes(x = Legiolert..MPN.mL., y = Neochlamydia, color = as.factor(Copper))) +
  geom_point(size = 2) +
  theme_bw() +
  labs(title = "Copper-Dosing Microcosms",
       x = NULL,
       y = NULL) +
  theme(
    panel.grid = element_line(color = "white"),
    text = element_text(family = "Arial",size = 8,  color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    legend.title = element_text(size = 8,  color = "black"),
    plot.title = element_text(size = 10, margin = margin(t=0, b=3)),
    legend.text = element_text(,size = 8, color = "black"),
    legend.position = c(0.7, 0.7),
    plot.margin = margin(10, 5, 15, 40),
    legend.key.size = unit(.1, "in"),
    legend.spacing.y = unit(-.05, "in")
  ) +
  scale_color_manual(name = "Copper (µg/L)",
                     values = c("0" = "red",
                                "4" = "orange",
                                "30" = "green",
                                "250" = "blue",
                                "2000" = "magenta")) +
  #xlim(0, 0.003) +
  ylim(0, 0.045)


theme_set(theme_cowplot(font_family = "Arial"))
combined_plot <- plot_grid(quincy_copper,
                           nrow = 1, ncol = 1,label_size = 18)

final_plot <- ggdraw(combined_plot) + 
  draw_plot(combined_plot, 0, 0, 1, 1) + 
  draw_label(expression(italic("Lp") * " MPN/mL"), fontfamily = "Arial", x = 0.5, y = 0.025, size = 10, fontface = "bold") + 
  draw_label(expression(italic("Neochlamydia") * " Relative Abundance"), fontfamily = "Arial", x = 0.015, y = 0.5, size = 10, angle = 90, fontface = "bold")
final_plot

ggsave("C:/Users/Jim Byrne/Documents/Phd/research/neochlamydia/letter_figs/Draft 14/fig_S8.tiff",plot = last_plot(), width = 7, height = 4.167, dpi = 300, units = "in", grDevices::tiff)
