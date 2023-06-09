#title: "Result2,Figure A,Deseq2"
#author: "Xing Wang"


rm(list = ls())
getwd()


mycounts<-read.csv("rc.csv",row.names = 1)
dim(mycounts)
mycount <- mycounts[rowMeans(mycounts)>1,] 
dim(mycount)
#字符串转因子
mymeta<-read.csv("rc_meta.csv",stringsAsFactors = T) 

colnames(mycount) == mymeta$id

library(DESeq2)
dds <- DESeqDataSetFromMatrix(countData=mycount, 
                              colData=mymeta, 
                              design=~dex)
dds <- DESeq(dds)
res <- results(dds,contrast = c("dex","Xs","CK"))
head(res)
class(res)
res_1<-data.frame(res)
class(res_1)
head(res_1)
write.csv(mycount,file = "rc_count.csv") 
write.csv(res_1,file = "rc_diffmycount.csv") 


df<-read.csv("rc_diffmycount.csv",header=T,stringsAsFactors = F)
head(df)
dim(df)
df$group<-ifelse(df$log2FoldChange>=2&df$pvalue<=0.05,"Enriched",
                 ifelse(df$log2FoldChange<=-2&df$pvalue<=0.05,
                        "Depleted","Not sig"))
table(df$group)

write.csv(df,file="rc_diff_summary.csv",quote = F)

library(ggplot2)
#install.packages("ggrepel")
library(ggrepel)
 ggplot(df,aes(x=log2FoldChange,y=-log10(pvalue)))+
   geom_point(aes(color=group),size=20)+
   scale_color_manual(values=c("#1E90FF","#FF6347","gray"))
df$pvalue_log10<-(-log10(df$pvalue))
df1<-df[df$pvalue_log10>=2,]
dim(df1)

p= ggplot(df,aes(x=log2FoldChange,y=-log10(pvalue)))+
   geom_point(aes(color=group),size=4,alpha=0.8)+
   scale_color_manual(values=c("#1E90FF","#FF6347","gray"))+
   geom_label_repel(data=df1,aes(x=log2FoldChange,y=-log10(pvalue),
                                label=Features,fosme=1000))+
   theme_bw()  + theme(legend.position = "none")+
  
   labs(y="-log10(pvalue)",x="log(Fold Change)",title="rc depleted:1,enriched:0")

p
ggsave(paste("rc.pdf", sep=""), p, width = 4.5, height = 5)
ggsave(paste("rc.png", sep=""), p, width = 4.5, height = 5)