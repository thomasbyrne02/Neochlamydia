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

filters <- neo_lp %>%
  subset(waterorbiofilm == "w")

swabs <- neo_lp %>%
  subset(waterorbiofilm == "b")

quincy <- full_join(filters, swabs, by = join_by(Sample.ID))


a <- quincy %>%
  ggplot(aes(x = Neochlamydia.x, y = Neochlamydia.y)) + 
  geom_point() +
  stat_poly_line() +
  stat_poly_eq(small.p = T, mapping = use_label("eq"), label.y = 0.95, label.x = .9, family = "Arial") +
  stat_poly_eq(small.p = T, mapping = use_label("R2"), label.y = 0.9, label.x = .9, family = "Arial") +
  stat_poly_eq(small.p = T, mapping = use_label("P"), label.y = 0.85, label.x = .9, family = "Arial") +
  theme_bw() +
  labs(title = expression("Copper-Dosing Microcosms"),
       x = expression("Bulk Water " * italic("Neochlamydia") * " Relative Abundance"),
       y = expression("Biofilm Swab " * italic("Neochlamydia") * " Relative Abundance")) +
  theme(
    panel.grid = element_line(color = "white"),
    text = element_text(family = "Arial",size = 8,  color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    axis.title = element_text(size = 10, color = "black"),
    legend.title = element_text(size = 8,  color = "black"),
    plot.title = element_text(size = 10, margin = margin(t=0, b=3)),
    legend.text = element_text(size = 8, color = "black"),
    plot.margin = margin(5,5,5,10),
    legend.key.size = unit(.1, "in"),
    legend.spacing.y = unit(-.05, "in"),
    axis.title.y = element_text(margin=margin(r=25)),
    axis.title.x = element_text(margin=margin(t=5)),
    legend.position = c(1.1, 0.5)
  )

b <- quincy %>%
  ggplot(aes(x = Legionella.x, y = Legionella.y)) + 
  geom_point() +
  stat_poly_line() +
  stat_poly_eq(small.p = T, mapping = use_label("eq"), label.y = 0.95, label.x = .9, family = "Arial") +
  stat_poly_eq(small.p = T, mapping = use_label("R2"), label.y = 0.9, label.x = .9, family = "Arial") +
  stat_poly_eq(small.p = T, mapping = use_label("P"), label.y = 0.85, label.x = .9, family = "Arial") +
  theme_bw() +
  labs(title = expression("Copper-Dosing Microcosms"),
       x = expression("Bulk Water " * italic("Legionella") * " Relative Abundance"),
       y = expression("Biofilm Swab " * italic("Legionella") * " Relative Abundance")) +
  theme(
    panel.grid = element_line(color = "white"),
    text = element_text(family = "Arial",size = 8,  color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    axis.title = element_text(size = 10, color = "black"),
    legend.title = element_text(size = 8,  color = "black"),
    plot.title = element_text(size = 10, margin = margin(t=0, b=3)),
    legend.text = element_text(size = 8, color = "black"),
    plot.margin = margin(5,5,5,10),
    legend.key.size = unit(.1, "in"),
    legend.spacing.y = unit(-.05, "in"),
    axis.title.y = element_text(margin=margin(r=25)),
    axis.title.x = element_text(margin=margin(t=5)),
    legend.position = c(1.1, 0.5)
  )






theme_set(theme_cowplot(font_family = "Arial"))
combined_plot <- plot_grid(a,b, 
                           nrow = 1, ncol = 2, 
                           labels = "AUTO", label_size = 11)


final_plot <- ggdraw(combined_plot) + 
  draw_plot(combined_plot, 0, 0, 1, 1)
final_plot

ggsave("C:/Users/Jim Byrne/Documents/Phd/research/neochlamydia/letter_figs/Draft 14/fig_S10.tiff",plot = last_plot(), width = 7, height = 4.167, dpi = 300, units = "in", grDevices::tiff)

