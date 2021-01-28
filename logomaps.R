library(ggplot2)
library(ggseqlogo)
library(stringr)
library(gridExtra)

# function for getting all patterns that in the form of sequence files
find_all_pattern<-function(samples){
pattern.seqfile<-list()
i=1
for(i in 1:length(samples)){
  temp.pattern<-read.csv(paste0("Individual/", samples[i], "/c4_05_2_3f/patterns/pattern_summary_table.csv"),header=TRUE,sep=",")[,2]
  temp.pattern<-as.character(temp.pattern)
  pattern.seqfile[[sample.list[i]]]<-temp.pattern
}
all.pattern<-unique(unname(unlist(pattern.seqfile)))
return(all.pattern)
}

# function for generating logomaps for each pattern
generate_logomap<-function(pattern, samples,methodin,ticktype){
  i=1
  seqlogo.ls<-list()
  pattern<-gsub("\\[|\\]", "", pattern)
  pattern<-gsub("\\.","x",pattern)
  if(ticktype=="1,2,3..."){
    ticks<-c(1:nchar(pattern))
  }
  if(ticktype=="...,P2,P1,P1',P2'...(for even length only)"){
    midpoint<-nchar(pattern)/2
    ticks<-rep(NA,nchar(pattern))
    ticks[midpoint:1]<-paste0("P",c(1:(nchar(pattern)/2)))
    ticks[(midpoint+1):nchar(pattern)]<-paste0("P",c(1:(nchar(pattern)/2)),"'")
  }
  if(ticktype=="...-1,0,1,...(for odd length only)"){
    midpoint<-floor(nchar(pattern)/2)
    ticks<-c((-midpoint):midpoint)
  }

  for (i in 1:length(samples)){
    sample<-samples[i]
    ls.seq<-list()
    if(file.exists(paste0("Individual/",sample,"/c4_05_2_3f/patterns/",pattern,"_sequences.txt"))){
      ls.seq[[pattern]]<-as.character(read.delim(paste0("Individual/",sample,"/c4_05_2_3f/patterns/",pattern,"_sequences.txt"),
                                                 header=FALSE,check.names = FALSE)[,1])
      pcharind<-c(1:nchar(pattern))[-as.integer(gregexpr("x", pattern)[[1]])]
      g<-ggseqlogo(data=ls.seq, method=methodin)
      seqlogo.ls[[sample]]<-g+ggtitle(sample)+
        scale_x_discrete(limits=ticks)+annotate('rect', xmin=(pcharind-0.5), xmax=(pcharind+0.5), ymin = -0.05, ymax = max(g[["layers"]][[1]][["data"]][["y"]]), alpha =0.15,fill='red')
     
    }
  }
  logomap.pattern<-grid.arrange(grobs=seqlogo.ls)
  return(logomap.pattern)
}

generate_summarytable<-function(pattern, samples){
  i=1
  tbl<-data.frame(Sample=NA, Sample.Frequency=NA, Foreground.Frequency=NA, 
                  Foreground.Size=NA, Enrichment.Score=NA)
  for (i in 1:length(samples)){
    sample<-samples[i]
    ind<-which(pattern.summary.list[[sample]]$Pattern==pattern)
    if(length(ind)!=0){
      tbl<-rbind(tbl,c(sample,unname(pattern.summary.list[[sample]][ind,-1])))}
  }
  tbl<-tbl[-1,]
  return(tbl)
}

