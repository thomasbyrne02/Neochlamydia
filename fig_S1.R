# flint and quincy culture boxplots


Legionella_Combined<-read_excel("C:/Users/Jim Byrne/Documents/Phd/research/neochlamydia/Legionella compiled total.xlsx",sheet="Tracking Triplicates Final ICP")

Legionella_Combined$Pipe.Material<-as.factor(Legionella_Combined$Pipe.Material)
Legionella_Combined$Influent<-as.factor(Legionella_Combined$Influent)
Legionella_Combined$Week<-as.numeric(Legionella_Combined$Week)
levels(Legionella_Combined$Pipe.Material)<-c("Copper","PEX")
Legionella_Combined$Pipe.Material<-factor(Legionella_Combined$Pipe.Material,levels=c("PEX","Copper"))
levels(Legionella_Combined$Influent)<-c("Detroit Water","Treated Flint River Water")
Legionella_Combined$Week<-factor(Legionella_Combined$Week,levels=c("2","3","4","9","18","31","60","70"))
Legionella_Weeks<-split(Legionella_Combined,list(Legionella_Combined$Week))
Legionella_Combined$Treatment <- factor(Legionella_Combined$Treatment, levels = c("Control", "Ferric Chloride", "Ferric Pyrophosphate", "Ferrous Chloride"))

a <- Legionella_Combined %>%
  subset(Week == 60) %>%
  subset(Influent == "Treated Flint River Water") %>%
  ggplot(aes(x = Treatment, y = `Log10(CFU)`, color = Pipe.Material)) +
  geom_boxplot(size = .5, outlier.size = .5) +
  theme_bw() +
  labs(title = "Flint/Detroit Water Microcosms - \nFlint River Water",
       x = "Treatment",
       y = "log(CFU/mL)",
       color = "Pipe Material") +
  theme(
    panel.grid = element_line(color = "white"),
    text = element_text(family = "Arial",size = 8,  color = "black"),
    axis.text = element_text(family = "Arial",size = 8, color = "black"),
    axis.title = element_text(family = "Arial",size = 10, color = "black"),
    legend.title = element_text(family = "Arial",size = 8,  color = "black"),
    plot.title = element_text(family = "Arial",size = 10, margin = margin(t=0, b=3)),
    axis.text.x = element_text(family = "Arial",angle = 20, hjust = 1),
    legend.text = element_text(family = "Arial",size = 8, color = "black"),
    plot.margin = margin(5,60,5,15),
    legend.key.size = unit(.1, "in"),
    legend.spacing.y = unit(-.05, "in"),
    axis.title.y = element_text(family = "Arial",margin=margin(r=10)),
    axis.title.x = element_text(family = "Arial",margin=margin(t=25)),
    legend.position = c(1.1, 0.5)
  )
b <- Legionella_Combined %>%
  subset(Week == 60) %>%
  subset(Influent == "Detroit Water") %>%
  ggplot(aes(x = Treatment, y = `Log10(CFU)`, color = Pipe.Material)) +
  geom_boxplot(size = .5, outlier.size = .5) +
  theme_bw() +
  labs(title = "Flint/Detroit Water Microcosms - \nDetroit Water",
       x = "Treatment",
       y = "log(CFU/mL)",
       color = "Pipe Material") +
  theme(
    panel.grid = element_line(color = "white"),
    text = element_text(family = "Arial",size = 8,  color = "black"),
    axis.text = element_text(family = "Arial",size = 8, color = "black"),
    axis.title = element_text(family = "Arial",size = 10, color = "black"),
    legend.title = element_text(family = "Arial",size = 8,  color = "black"),
    plot.title = element_text(family = "Arial",size = 10, margin = margin(t=0, b=3)),
    axis.text.x = element_text(family = "Arial",angle = 20, hjust = 1),
    legend.text = element_text(family = "Arial",size = 8, color = "black"),
    plot.margin = margin(5,60,5,15),
    legend.key.size = unit(.1, "in"),
    legend.spacing.y = unit(-.05, "in"),
    axis.title.y = element_text(family = "Arial",margin=margin(r=10)),
    axis.title.x = element_text(family = "Arial",margin=margin(t=25)),
    legend.position = c(1.1, 0.5)
  )


# quincy
quincy <- read_excel("C:/Users/Jim Byrne/Documents/Phd/research/neochlamydia/Rania/All TCC & Legiolert.xlsx", sheet = "All Legiolert")
quincy$copper <- factor(quincy$copper, levels = c(0, 4, 30, 250, 2000))
c <- quincy %>%
  subset(since_dosing > 315) %>%
  ggplot(aes(x = as.factor(copper), y = log10(`MPN/mL`), group = Jar, color = as.factor(copper))) +
  geom_boxplot(size = .5, outlier.size = .5) +
  theme_bw() +
  labs(title = "Copper-Dosing Microcosms",
       x = "Copper (µg/L)",
       y = "log(MPN/mL)",
       color = "Copper (µg/L)") +
  theme(
    panel.grid = element_line(color = "white"),
    text = element_text(family = "Arial",size = 8,  color = "black"),
    axis.text = element_text(family = "Arial",size = 8, color = "black"),
    axis.title = element_text(family = "Arial",size = 10, color = "black"),
    legend.title = element_text(family = "Arial",size = 8,  color = "black"),
    plot.title = element_text(family = "Arial",size = 10, margin = margin(t=0, b=3)),
    legend.text = element_text(family = "Arial",size = 8, color = "black"),
    plot.margin = margin(5,80,5,20),
    legend.key.size = unit(.1, "in"),
    legend.spacing.y = unit(-.05, "in"),
    axis.title.y = element_text(family = "Arial",margin=margin(r=15)),
    legend.position = c(1.05, 0.5)
  )



theme_set(theme_cowplot(font_family = "Arial"))
combined_plot <- plot_grid(
                           plot_grid(a, b, nrow = 1, labels = "AUTO", label_size = 11,
                                     label_x = 0, label_y = 1, rel_heights = c(1, 1)),
                           plot_grid(c, nrow = 1, labels = c("C"), label_size = 11,
                                     label_x = 0, label_y = 1, rel_heights = 1),
                           nrow = 2)


final_plot <- ggdraw(combined_plot) + 
  draw_plot(combined_plot, 0, 0, 1, 1)
final_plot

ggsave("C:/Users/Jim Byrne/Documents/Phd/research/neochlamydia/letter_figs/Draft 14/fig_S1.tiff",plot = last_plot(), width = 7, height = 4.167, dpi = 300, units = "in", grDevices::tiff)


