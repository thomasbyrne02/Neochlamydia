
#Flint neo lp biofilm and bulk lines



neo_lp <- read.csv("C:/Users/Jim Byrne/Documents/Phd/research/neochlamydia/Didier/didier_qiime_silva_with_metadata_unrarefied_ratios.csv")

neo_lp$Samples <- substr(neo_lp$Samples, 1, nchar(neo_lp$Samples) - 2)

a <- neo_lp %>%
  filter(Iron != "NA") %>%
  filter(Pipe != "NA") %>%
  filter(Pipe != "Copper") %>%
  ggplot(aes(x = Legionella, y = Neochlamydia)) +
  geom_point(aes(shape = Sampling, color = Water), size = 2) +
  geom_line(aes(group = Samples), size = .25) +
  theme_bw() +
  labs(title = "Flint/Detroit Water Microcosms",
       x = NULL,
       y = NULL) +
  theme(
    panel.grid = element_line(color = "white"),
    text = element_text(family = "Arial", size = 8,  color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    legend.title = element_text(size = 8,  color = "black"),
    plot.title = element_text(size = 10, margin = margin(t=0, b=3)),
    legend.text = element_text(size = 8, color = "black"),
    legend.position = c(0.7, 0.7),
    plot.margin = margin(5, 5, 15, 40),
    legend.key.size = unit(.1, "in"),
    legend.spacing.y = unit(-.05, "in")
  ) +
  scale_shape_manual(
    values = c("Sampling" = 16, "Filter" = 19, "Swab" = 17),  # Optional: different shapes
    labels = c(
      "Sampling" = "Sample Type",
      "Swab" = "Biofilm Swab",
      "Filter" = "Bulk Water"
    )
  ) +
  guides(shape = guide_legend(title = "Sample Type")) + 
  scale_color_manual(name = "Water Source",
                     values = c("Detroit" = "blue",
                                "Flint" = "green"))



neo_lp <- read.csv("C:/Users/Jim Byrne/Documents/Phd/research/neochlamydia/Rania/rania_qiime_silva_with_metadata_unrarefied_ratios.csv")

neo_lp$Sample.ID <- substr(neo_lp$Sample.ID, 1, 2)
neo_lp$Date <- gsub("^...", "", neo_lp$Jar)


neo_lp <- neo_lp %>% 
  filter(realorcontrol == "r") %>%
  filter(Date != "2/5" & Date != "2/8") %>%
  group_by(Sample.ID, waterorbiofilm) %>%
  summarize(Legionella = mean(Legionella),
            Neochlamydia = mean(Neochlamydia),
            Copper = Copper,
            Sample.ID = Sample.ID,
            waterorbiofilm = waterorbiofilm) %>%
  unique()

b <- neo_lp %>%
  ggplot(aes(x = Legionella, y = Neochlamydia)) +
  geom_point(aes(shape = waterorbiofilm, color = as.factor(Copper)), size = 2) +
  geom_line(aes(group = Sample.ID), size = .25) +
  theme_bw() +
  labs(title = "Copper-Dosing Microcosms",
       x = NULL,
       y = NULL) +
  theme(
    panel.grid = element_line(color = "white"),
    text = element_text(family = "Arial", size = 8,  color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    legend.title = element_text(size = 8,  color = "black"),
    plot.title = element_text(size = 10, margin = margin(t=0, b=3)),
    legend.text = element_text(size = 8, color = "black"),
    legend.position = c(0.7, 0.7),
    plot.margin = margin(5, 5, 15, 15),
    legend.key.size = unit(.1, "in"),
    legend.spacing.y = unit(-.05, "in")
  ) +
  scale_x_continuous(breaks = c(0.000, 0.0005, 0.001, 0.0015, 0.002, 0.0025, 0.003),
                     limits = c(0, 0.003)) +
  scale_shape_manual(
    values = c("waterorbiofilm" = 16, "b" = 17, "w" = 19),  # Optional: different shapes
    labels = c(
      "waterorbiofilm" = "Sample Type",
      "w" = "Bulk Water",
      "b" = "Biofilm Swab"
    )
  ) +
  guides(shape = guide_legend(title = "Sample Type")) + 
  scale_color_manual(name = "Copper (µg/L)",
                     values = c("0" = "red",
                                "4" = "orange",
                                "30" = "green",
                                "250" = "blue",
                                "2000" = "magenta"))


theme_set(theme_cowplot(font_family = "Arial"))
combined_plot <- plot_grid(a,b, 
                           nrow = 1, ncol = 2, 
                           labels = "AUTO", label_size = 11)


final_plot <- ggdraw(combined_plot) + 
  draw_plot(combined_plot, 0, 0, 1, 1) + 
  draw_label(expression(italic("Legionella") * " Relative Abundance"), fontfamily = "Arial", x = 0.5, y = 0.025, size = 10, fontface = "bold") + 
  draw_label(expression(italic("Neochlamydia") * " Relative Abundance"), fontfamily = "Arial", x = 0.015, y = 0.5, size = 10, angle = 90, fontface = "bold")
final_plot

ggsave("C:/Users/Jim Byrne/Documents/Phd/research/neochlamydia/letter_figs/Draft 14/fig_S11.tiff",plot = last_plot(), width = 7, height = 4.167, dpi = 300, units = "in", grDevices::tiff)
