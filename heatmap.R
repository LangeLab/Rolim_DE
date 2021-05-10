# generate heatmap for selected samples
library(pheatmap)
library(tidyr)
library(RColorBrewer)
my_palette<-colorRampPalette(c("blue", "white", "red"))(n = 100)

generate_heatmap<-function(path,pattern.summary.list, samples, var, quantdata, fileformat,whetherlog2){
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
    sum.pattern.list<-list()
    for (i in samples){
    if (fileformat=="Long format, with multiple samples"){path1<-paste0(path, i)}else{path1<-path}
    temp.filename<-list.files(paste0(path1,"/summary/"))
    temp.filename<-temp.filename[grep("summary_table.txt",temp.filename)]
    sum.pattern.list[[i]]<-read.delim(paste0(path1,"/summary/",temp.filename),header=TRUE,check.names = FALSE)[,-1]}
    i<-1
    df$intensity<-rep(NA,nrow(df))
    for (i in 1:nrow(df)){
    sample<-df$sample[i]
    sum.pattern<-sum.pattern.list[[sample]]
    ind.row<-which(sum.pattern[,df$pattern[i]]==1)
    sub.quant<-quantdata[which((quantdata[,1]==sample)&(quantdata[,2] %in% sum.pattern[ind.row,"aligned_sequence"])),]
    df$intensity[i]<-mean(sub.quant[,3], na.rm=TRUE)
    }
    temp.M<-spread(na.omit(df[,-2]), sample, intensity)
    temp.M[,-1]<-log10(temp.M[,-1])
    temp.M[is.na(temp.M)]<-0
    M<-as.matrix(temp.M[,-1])
    M<-10**M
    rownames(M)=temp.M$pattern
    colnames(M)=colnames(temp.M)[-1]
  }
  if(whetherlog2==TRUE){
    M<-log2(M)
  }
  g<-pheatmap(M,colors = my_palette)
  return(g)
}


genrate_heatmap_col<-function(quantdata,colIntensity,whethernormal,whetherlog2){
  patternsstring<-colnames(quantdata)[grep("\\.",colnames(quantdata))]
  M<-matrix(0,nrow=length(patternsstring),ncol=length(colIntensity))
  for (i in 1:nrow(M)){
    ind<-which(quantdata[,patternsstring[i]]==1)
    M[i,]<-apply(quantdata[ind, colIntensity],2, function(x) mean(x, na.rm=TRUE))
  }
  colnames(M)<-colIntensity
  rownames(M)<-patternsstring
  if(whetherlog2==TRUE){
    M<-log2(M)
  }
  g<-pheatmap(M,colors = my_palette)
  return(g)
}

