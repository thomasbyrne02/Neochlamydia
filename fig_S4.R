#Flint neo vs lp


flint <- read.csv("C:/Users/Jim Byrne/Documents/Phd/research/neochlamydia/Didier/didier_qiime_silva_with_metadata_unrarefied_ratios.csv")

flint_copper <- flint %>%
  filter(Iron != "NA") %>%
  filter(Pipe != "NA") %>%
  filter(Sampling == "Filter") %>%
  ggplot(aes(x = Legionella, y = Neochlamydia)) +
  geom_point(aes(shape = Pipe, color = Water), size = 2) +
  theme_bw() +
  labs(title = "Flint/Detroit Water Microcosms",
       x = NULL,
       y = NULL) +
  geom_vline(xintercept = 0.002, linetype = "dashed", color = "black") +
  geom_hline(yintercept = 0.0002, linetype = "dashed", color = "black") +
  theme(
    panel.grid = element_line(color = "white"),
    text = element_text(family = "Arial",size = 8,  color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    legend.title = element_text(size = 8,  color = "black"),
    plot.title = element_text(size = 10, margin = margin(t=0, b=3)),
    legend.text = element_text(size = 8, color = "black"),
    legend.position = c(0.7, 0.7),
    plot.margin = margin(10, 5, 15, 45),
    legend.key.size = unit(.1, "in"),
    legend.spacing.y = unit(-.05, "in")
  ) +
  scale_color_manual(name = "Water Source",
                     values = c("Detroit" = "blue",
                                "Flint" = "green")) +
  scale_shape_manual(name = "Pipe Material",
                     values = c("Copper" = 19,
                                "PEX" = 17)) +
  scale_x_continuous(breaks = c(0.000, 0.0025, 0.005, 0.0075, 0.01)) +
  ylim(0, 0.0016) +
  guides(
    shape = guide_legend(order = 1), 
    color = guide_legend(order = 2) 
  )




theme_set(theme_cowplot(font_family = "Arial"))
combined_plot <- plot_grid(flint_copper,
                           nrow = 1, ncol = 1,label_size = 18)

final_plot <- ggdraw(combined_plot) + 
  draw_plot(combined_plot, 0, 0, 1, 1) + 
  draw_label(expression(italic("Legionella") * " Relative Abundance"), fontfamily = "Arial", x = 0.5, y = 0.025, size = 10, fontface = "bold") + 
  draw_label(expression(italic("Neochlamydia") * " Relative Abundance"), fontfamily = "Arial", x = 0.015, y = 0.5, size = 10, angle = 90, fontface = "bold")
final_plot

ggsave("C:/Users/Jim Byrne/Documents/Phd/research/neochlamydia/letter_figs/Draft 14/fig_S4.tiff",plot = last_plot(), width = 7, height = 4.167, dpi = 300, units = "in", grDevices::tiff)

