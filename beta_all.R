#Result2,Figuer B,beta_diversity
#Author: Xing Wang

getwd()
rm(list = ls())

library(vegan)
library(ggplot2)
otu_table <- read.table("pcoa.txt",sep="\t", row.names=1, header=T)
meta_tab <- read.table("pcoa_meta.txt", header=T, row.names=1, check.names=F)
otu_table <- data.frame(t(otu_table))


dune_dist <- vegdist(otu_table, method="bray", binary=F)
dune_pcoa <- cmdscale(dune_dist, k = (nrow(otu_table) - 1), eig = TRUE)

dune_pcoa_points <- as.data.frame(dune_pcoa$points)
sum_eig <- sum(dune_pcoa$eig)
eig_percent <- round(dune_pcoa$eig/sum_eig*100,1)
colnames(dune_pcoa_points) <- paste0("PCoA", 1:2)
dune_pcoa_result <- cbind(dune_pcoa_points, meta_tab)
head(dune_pcoa_result)


set.seed(1)
dune.div <- adonis2(otu_table ~ SampleType, data = meta_tab, permutations = 999, method="bray")

dune.div


dune_adonis <- paste0("R-squared: ",round(dune.div$R2,2), "; P-value: ", dune.div$`Pr(>F)`)

p= ggplot(dune_pcoa_result, aes(x=PCoA1, y=PCoA2, color=SampleType, group = SampleType)) +
  labs(x=paste("PCoA 1 (", eig_percent[1], "%)", sep=""),
       y=paste("PCoA 2 (", eig_percent[2], "%)", sep=""),
       title=dune_adonis) +
  geom_point(size=2,alpha=0.8) +scale_color_manual(values=c("Bulk_CK" = "#0000FF", "Bulk_Xs" = "#FFFF00",
                               "Rhizosphere_CK" = "#228B22", "Rhizosphere_Xs" = "#FF4040",
                               "Root_CK" = "#00FFFF", "Root_Xs" = "#FF6EB4",
                               "Stem_CK" = "#98FB98", "Stem_Xs" = "#FFFACD",
                               "Seed_CK" = "#00CED1", "Seed_Xs" = "#FFA54F"))+
  
  theme_bw() +stat_ellipse(level = 0.9)+
  theme(panel.grid=element_blank()) + theme(legend.position="right")+theme(legend.title = element_blank())+
  theme(legend.key.height = unit(5, "pt"),legend.key.width = unit(5, "pt"))+guides(color = guide_legend(override.aes = list(size = 1)))+theme(legend.text=element_text(size=11))
  

p
ggsave(paste("pcoa.pdf", sep=""), p, width = 6, height = 3.5)
ggsave(paste("pcoa.png", sep=""), p, width = 6, height = 3.5)

