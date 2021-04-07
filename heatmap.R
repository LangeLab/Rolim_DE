# generate heatmap for selected samples
library(ComplexHeatmap)
library(tidyr)
library(RColorBrewer)
my_palette<-colorRampPalette(c("blue", "white", "red"))(n = 100)

generate_heatmap<-function(path,pattern.summary.list, samples, var, whethernormal, quantdata=NULL){
  if (any(is.null(samples))){return()}
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
    temp.filename<-list.files(paste0(path,sample,"/c4_05_2_3f/summary/"))
    temp.filename<-temp.filename[grep("summary_table.txt",temp.filename)]
    sum.pattern<-read.delim(paste0(path,sample,"/c4_05_2_3f/summary/",temp.filename),header=TRUE,check.names = FALSE)[,-1]
    ind.row<-which(sum.pattern[,df$pattern[i]]==1)
    sub.quant<-quantdata[which((quantdata[,1]==sample)&(quantdata[,2] %in% sum.pattern[ind.row,1])),]
    df$intensity[i]<-mean(sub.quant[,3])
    }
    temp.M<-spread(na.omit(df[,-2]), sample, intensity)
    temp.M[,-1]<-log10(temp.M[,-1])
    temp.M[is.na(temp.M)]<-0
    M<-as.matrix(temp.M[,-1])
    rownames(M)=temp.M$pattern
    colnames(M)=colnames(temp.M)[-1]
  }
  if (whethernormal==TRUE){
    M<-t(scale(t(M)))
  }
  Heatmap(M,column_order=colnames(M),row_names_gp = gpar(fontsize = 8))
}
