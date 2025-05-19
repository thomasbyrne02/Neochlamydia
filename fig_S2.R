library(tidyverse)
library(RColorBrewer)
library(ggrepel)
library(vegan)
library(qvalue)
# #Welch’s	t-test	with	Storey’s	FDR	method	of	multiple	test	correction	within	the	STAMP	soHware	package
# 
# #kk so do Welch's t-test and then feed the p-vals into Storey's FDR (qvalue function)
# 
# #do genus level
# asv <- data.frame(read.csv("C:/Users/Jim Byrne/Documents/Phd/research/nando/ASVs_counts.csv"))
# taxonomy <- data.frame(read.csv("C:/Users/Jim Byrne/Documents/Phd/research/nando/ASVs_taxonomy.csv"))
# 
# asv$X <- NULL
# asv$Undetermined_S0 <- NULL
# asv <- t(asv)
# rownames(asv) <- sub("^[^.]+\\.([^_]+)_.*$", "\\1", rownames(asv))
# asv <- as.data.frame(asv)
# row.names(taxonomy) <- colnames(asv)
# asv <- data.frame(rrarefy(asv, min(rowSums(asv))))
# asv$Sample.ID <- row.names(asv)
# taxonomy$row_name <- row.names(taxonomy)
# merged <- data.frame(t(asv)) %>%
#   rownames_to_column("row_name") %>%
#   gather(key = "column_title", value = "value", -row_name) %>%
#   merge(taxonomy, by = "row_name", all = F)
# ranking <- merged %>%
#   group_by(Genus) %>%
#   summarize(sum = sum(as.numeric(value)))
# ranking <-  merged %>%
#   mutate(Genus)
# ranking$value <- as.numeric(ranking$value)
# ranking <- ranking[,c("column_title", "Genus", "value")]
# 
# #ogay so I just want the 3 250 µg/Ls for dis me thinks
# ranking <- ranking[ranking$column_title %in% c("B22.5", "O22.5", "Y32.5", "B22.8", "O22.8", "Y32.8", "B22.12", "O22.12", "Y32.12"),]
# unique(ranking$column_title)
# ranking <- ranking %>%
#   group_by(column_title, Genus) %>%
#   summarize(value = sum(as.numeric(value)))
# 
# #i should make it relative abundances by site if the total vals for each isnt comprable but should be cause i rarefied
# ranking %>% 
#   group_by(column_title) %>%
#   summarize(sum = sum(as.numeric(value)))
# #sheeeeeeeeee. Makes sense
# 
# ranking <- ranking %>%
#   pivot_wider(names_from = column_title, values_from = value)
# #kk remove rows with all 0s
# ranking <- ranking %>%
#   filter(rowSums(select(., -Genus)) != 0)
# ranking <- ranking[!is.na(ranking$Genus), ]
# 
# df_long <- tidyr::pivot_longer(ranking, -Genus, names_to = "Column", values_to = "Value")
# # Create boxplot using ggplot2
# ggplot(df_long, aes(x = Column, y = Value)) +
#   geom_boxplot() +
#   labs(title = "Boxplot of Distribution for Each Column", x = "Column", y = "Value") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
# 
# a <- ranking$Genus
# ranking <- ranking[, !(names(ranking) %in% c("Genus"))]
# row.names(ranking) <- a
# 
# high <- select(ranking, c("B22.5", "B22.8", "B22.12"))
# row.names(high) <- a
# 
# low <- select(ranking, !c("B22.5", "B22.8", "B22.12"))
# row.names(low) <- a
# 
# 
# #kk time for welch's t test
# #t.test(data1, data2, var.equal = FALSE)
# high <- t(high)
# low <- t(low)
# p_values <- numeric()
# 
# # Loop through each variable
# for (i in 1:ncol(high)) {
#   # Perform Welch's t-test for each variable
#   result <- t.test(high[, i], low[, i], var.equal = FALSE)
#   
#   # Extract p-value and store it
#   p_values[i] <- result$p.value
# }
# sigs <- data.frame(colnames(high), p_values)
# ggplot(sigs) +
#   geom_histogram(aes(x = p_values))
# qobj<-qvalue(p=sigs$p_values, pi0.method="smoother") #can try to find optimal lambda value but its a shit ton of work and apparenly doesn't rlly matter
# summary(qobj)
# plot(qobj)
# sigs <- data.frame(names = colnames(high), lambda = 0, p_values, qvalue = qobj$qvalues)
# ggplot(sigs) +
#   geom_histogram(aes(x = qvalue))
# 
# #Gonna try using DESEQ2 instead cause it automatically calculates log change
# library("DESeq2")
# mini_meta <- data.frame(name = c("B22.12", "B22.5", "B22.8", "O22.12", "O22.5", "O22.8", "Y32.12", "Y32.5", "Y32.8"), high_or_low = c("high", "high", "high", "low", "low", "low", "low", "low", "low"))
# row.names(mini_meta) <- mini_meta$name
# dds <- DESeqDataSetFromMatrix(countData=ranking, 
#                               colData=mini_meta, 
#                               design=~high_or_low, tidy = F)
# dds <- DESeq(dds)
# res <- results(dds)
# summary(res)
# res <- data.frame(names = rownames(res), res)
# 
# #need to add qvalues to res
# res <- full_join(res, sigs, by = "names")
# row.names(res) <- res$names
# 
# breaks <- seq(0, 5.5, length.out = 100)
# color_palette <- colorRampPalette(c("yellow", "red"))
# par(mfrow=c(1,1))
# 
# with(res, plot(log2FoldChange, -log10(qvalue), pch=20, ylim = c(0,2), xlim=c(-6, 6), cex.lab=1.5, cex = -log10(qvalue)+.75))
# 
# colors <- color_palette(length(breaks))[cut(abs(subset(res, qvalue<.1)$log2FoldChange), breaks = breaks)]
# with(subset(res, qvalue<.1), points(log2FoldChange, -log10(qvalue), pch=20, col=colors, cex=-log10(qvalue)+.75))
# colors <- color_palette(length(breaks))[cut(abs(subset(res, qvalue>.1 & abs(log2FoldChange)>3)$log2FoldChange), breaks = breaks)]
# with(subset(res, qvalue>.1 & abs(log2FoldChange)>3), points(log2FoldChange, -log10(qvalue), pch=20, col=colors, cex=-log10(qvalue)+.75))
# with(subset(res, qvalue<.1), text(log2FoldChange, -log10(qvalue), labels=rownames(subset(res, qvalue<.1)), pos=3, cex=-log10(qvalue)/2+1))
# with(subset(res, qvalue>.1 & abs(log2FoldChange)>3), text(log2FoldChange, -log10(qvalue), labels=rownames(subset(res, qvalue>.1 & abs(log2FoldChange)>3)), pos=3, cex=-log10(qvalue)/2+1))
# 
# 
# 
# 
# 
# 
# 
# breaks <- seq(0, 5.5, length.out = 100)
# color_palette <- colorRampPalette(c("yellow", "red"))
# par(mfrow=c(1,1))
# 
# with(res, plot(log2FoldChange, -log10(pvalue), pch=20, ylim = c(0,20), xlim=c(-6, 6), cex.lab=1.5, cex = -log10(pvalue)/10+.75))
# 
# colors <- color_palette(length(breaks))[cut(abs(subset(res, pvalue<1e-3)$log2FoldChange), breaks = breaks)]
# with(subset(res, pvalue<1e-3), points(log2FoldChange, -log10(pvalue), pch=20, col=colors, cex=-log10(pvalue)/10+.75))
# colors <- color_palette(length(breaks))[cut(abs(subset(res, pvalue>1e-3 & abs(log2FoldChange)>3)$log2FoldChange), breaks = breaks)]
# with(subset(res, pvalue>1e-3 & abs(log2FoldChange)>3), points(log2FoldChange, -log10(pvalue), pch=20, col=colors, cex=-log10(pvalue)/10+.75))
# with(subset(res, pvalue<1e-3), text(log2FoldChange, -log10(pvalue), labels=rownames(subset(res, pvalue<1e-3)), pos=3, cex=-log10(pvalue)/10+1))
# with(subset(res, pvalue>1e-3 & abs(log2FoldChange)>3), text(log2FoldChange, -log10(pvalue), labels=rownames(subset(res, pvalue>1e-3 & abs(log2FoldChange)>3)), pos=3, cex=-log10(pvalue)/10+1))
# 
# png("C:/Users/Jim Byrne/Documents/Phd/research/neochlamydia/letter_figs/Neo_letter_fig_S9.png", width = 7, height = 4.167, res = 300, units = "in")
# 
# breaks <- seq(0, 5.5, length.out = 100)
# color_palette <- colorRampPalette(c("yellow", "red"))
# par(mfrow = c(1, 1), mar = c(4, 4, 1, 1)) 
# 
# with(res, plot(log2FoldChange, -log10(pvalue), pch=20, ylim = c(0,20), xlim=c(-6, 6), cex.lab=1.5, cex = -log10(pvalue)/10+.75))
# 
# colors <- color_palette(length(breaks))[cut(abs(subset(res, pvalue<1e-3)$log2FoldChange), breaks = breaks)]
# with(subset(res, pvalue<1e-3), points(log2FoldChange, -log10(pvalue), pch=20, col=colors, cex=-log10(pvalue)/10+.75))
# colors <- color_palette(length(breaks))[cut(abs(subset(res, pvalue>1e-3 & abs(log2FoldChange)>3)$log2FoldChange), breaks = breaks)]
# with(subset(res, pvalue>1e-3 & abs(log2FoldChange)>3), points(log2FoldChange, -log10(pvalue), pch=20, col=colors, cex=-log10(pvalue)/10+.75))
# with(subset(res, pvalue<1e-3), text(log2FoldChange, -log10(pvalue), labels=rownames(subset(res, pvalue<1e-3)), pos=3, cex=(-log10(pvalue)/30)+1))
# with(subset(res, pvalue>1e-3 & abs(log2FoldChange)>3), text(log2FoldChange, -log10(pvalue), labels=rownames(subset(res, pvalue>1e-3 & abs(log2FoldChange)>3)), pos=3, cex=(-log10(pvalue)/30)+1))
# 
# dev.off()
# 

























#yikes lets be consise this time. or not apparently
set.seed(69)

asv <- data.frame(read.csv("C:/Users/Jim Byrne/Documents/Phd/research/nando/ASVs_counts.csv"))
taxonomy <- data.frame(read.csv("C:/Users/Jim Byrne/Documents/Phd/research/nando/ASVs_taxonomy.csv"))

asv$X <- NULL
asv$Undetermined_S0 <- NULL
asv <- t(asv)
rownames(asv) <- sub("^[^.]+\\.([^_]+)_.*$", "\\1", rownames(asv))
asv <- as.data.frame(asv)
row.names(taxonomy) <- colnames(asv)
asv <- data.frame(rrarefy(asv, min(rowSums(asv))))
asv$Sample.ID <- row.names(asv)
taxonomy$row_name <- row.names(taxonomy)
merged <- data.frame(t(asv)) %>%
  rownames_to_column("row_name") %>%
  gather(key = "column_title", value = "value", -row_name) %>%
  merge(taxonomy, by = "row_name", all = F)
ranking <- merged %>%
  group_by(Genus) %>%
  summarize(sum = sum(as.numeric(value)))
ranking <-  merged %>%
  mutate(Genus)
ranking$value <- as.numeric(ranking$value)
ranking <- ranking[,c("column_title", "Genus", "value")]

#ogay so I just want the 3 250 µg/Ls for dis me thinks
ranking <- ranking[ranking$column_title %in% c("B22.5", "O22.5", "Y32.5", "B22.8", "O22.8", "Y32.8", "B22.12", "O22.12", "Y32.12"),]
unique(ranking$column_title)
ranking <- ranking %>%
  group_by(column_title, Genus) %>%
  summarize(value = sum(as.numeric(value)))

#i should make it relative abundances by site if the total vals for each isnt comprable but should be cause i rarefied
ranking %>% 
  group_by(column_title) %>%
  summarize(sum = sum(as.numeric(value)))
#sheeeeeeeeee. Makes sense

ranking <- ranking %>%
  pivot_wider(names_from = column_title, values_from = value)
#kk remove rows with all 0s
ranking <- ranking %>%
  filter(rowSums(select(., -Genus)) != 0)
ranking <- ranking[!is.na(ranking$Genus), ]


a <- ranking$Genus
ranking <- ranking[, !(names(ranking) %in% c("Genus"))]
row.names(ranking) <- a

library("DESeq2")
mini_meta <- data.frame(name = c("B22.12", "B22.5", "B22.8", "O22.12", "O22.5", "O22.8", "Y32.12", "Y32.5", "Y32.8"), high_or_low = c("high", "high", "high", "low", "low", "low", "low", "low", "low"))
row.names(mini_meta) <- mini_meta$name
dds <- DESeqDataSetFromMatrix(countData=ranking, 
                              colData=mini_meta, 
                              design=~high_or_low, tidy = F)
dds <- DESeq(dds)
res <- results(dds)
df <- as.data.frame(res)
df$taxon <- rownames(df)


ggplot(df, aes(x = as.numeric(log2FoldChange), y = -log10(as.numeric(pvalue)))) +
  geom_point(size = ifelse(-log10(as.numeric(df$pvalue)) > 5, 2, 
                           ifelse(abs(df$log2FoldChange) > 3.8, 2, 1)),
             color = ifelse(-log10(as.numeric(df$pvalue)) > 5, 'red',
                            ifelse(abs(df$log2FoldChange) > 3.8, 'red', 'black'))) +
  geom_text(family = "Wingdings", size = 4, aes(label=ifelse(-log10(as.numeric(pvalue)) > 5,as.character(taxon),'')),vjust=-.75) +
  geom_text(family = "Wingdings", size = 4, aes(label=ifelse(abs(log2FoldChange) > 3.8,as.character(taxon),'')),
            vjust=ifelse(-log10(as.numeric(df$pvalue)) > 2.2, -.75, 1.5)) +
  labs(x = "Log2 Fold Change", y = "-log10(p-value)") +
  theme_bw() +
  theme(
    panel.grid = element_line(color = "white"),
    text = element_text(family = "Wingdings",size = 8,  color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    axis.title = element_text(size = 10, color = "black"),
    legend.title = element_text(size = 8,  color = "black"),
    plot.title = element_text(size = 10, margin = margin(t=0, b=3)),
    axis.text.x = element_text(angle = 20, hjust = 1),
    legend.text = element_text(size = 8, color = "black"),
    plot.margin = margin(5,5,5,5),
    legend.key.size = unit(.1, "in"),
    legend.spacing.y = unit(-.05, "in"),
    axis.title.y = element_text(margin=margin(r=5)),
    axis.title.x = element_text(margin=margin(t=5)),
    legend.position = c(1.1, 0.5)
  ) +
  xlim(-5.6, 4.5) +
  ylim(0, 20)

ggsave("C:/Users/Jim Byrne/Documents/Phd/research/neochlamydia/letter_figs/Draft 14/fig_S2.tiff",plot = last_plot(), width = 7, height = 4.167, dpi = 300, units = "in", grDevices::tiff) 
#putting quotes around tiff makes font not work as expected
