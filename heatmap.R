# generate heatmap for selected samples
library(gplots)
library(tidyr)
library(RColorBrewer)
my_palette<-colorRampPalette(c("blue", "white", "red"))(n = 100)

generate_heatmap<-function(samples, var, whethernormal){

  df<-data.frame(pattern=NA, score=NA, sample=NA)
  i<-1
  for (i in 1:length(samples)){
    sample<-samples[i]
    temp.df<-cbind(pattern.summary.list[[sample]][["Pattern"]],
          pattern.summary.list[[sample]][["Enrichment..Sample.Frequency...Background.Frequency."]],
          sample)
    colnames(temp.df)<-c("pattern","score","sample")
    df<-rbind(df,temp.df)
  }
  df<-df[-1,]
  df$score<-as.numeric(df$score)
  if (var=="Enrichment Score"){
  temp.M<-spread(df, sample, score)
  temp.M[is.na(temp.M)]<-1
  M<-as.matrix(temp.M[,-1])
  rownames(M)=temp.M$pattern
  colnames(M)=colnames(temp.M)[-1]
  }
  if (var=="Quantitative intensity"){
    i<-1
    df$intensity<-rep(NA,nrow(df))
    for (i in 1:nrow(df)){
    sample<-df$sample[i]
    sum.pattern<-read.delim(paste0("Individual/",sample,"/c4_05_2_3f/summary/c4-05-2-3f_summary_table.txt"),header=TRUE,check.names = FALSE)[,-1]
    ind.row<-which(sum.pattern[,df$pattern[i]]==1)
    df$intensity[i]<-mean(sum.pattern[ind.row,ncol(sum.pattern)])
    }
    temp.M<-spread(df[,-2], sample, intensity)
    temp.M[is.na(temp.M)]<-0
    M<-as.matrix(temp.M[,-1])
    rownames(M)=temp.M$pattern
    colnames(M)=colnames(temp.M)[-1]
  }
  if (whethernormal==TRUE){flag.scale<-"row"}else{
    flag.scale<-"none"
  }
  heatmap.2(M,col=my_palette,trace="none",scale=flag.scale, cexCol = 1, cexRow = 1,margins=c(5,10), Colv=FALSE, density.info = "none")
}