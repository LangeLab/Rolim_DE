sum.pattern<-read.delim("c4_05_2_3f/summary/c4-05-2-3f_summary_table.txt",header=TRUE,check.names = FALSE)[,-1]
pattern_summary_table<-read.csv("c4_05_2_3f/patterns/pattern_summary_table.csv",header=TRUE,sep=",")[,-1]

library(dplyr)
# get some fake log-2 quantitative data
raw_data<-data.frame(InputSequence=sum.pattern$input_sequence)
raw_data<-cbind(raw_data, sum.pattern[,5:ncol(sum.pattern)])
i<-1
for(i in 1:10){
  raw_data<-cbind(raw_data, rnorm(nrow(raw_data),0,1))
}
for(i in 11:20){
  raw_data<-cbind(raw_data, rnorm(nrow(raw_data),1,1))
}
colnames(raw_data)[10:29]<-paste0("sample",c(1:20))
DoE<-data.frame(name=paste0("sample",c(1:20)),
                group=c(rep("A",10),rep("B",10)))


# heatmap for quants aggregated by pattern
i<-1
df<-matrix(0,nrow=nrow(pattern_summary_table),ncol=20)
pattern_summary_table$Pattern<-as.character(pattern_summary_table$Pattern)

for (i in 1:nrow(pattern_summary_table)){
  ind.row<-which(raw_data[,pattern_summary_table$Pattern[i]]==1)
  df[i,]<-apply(raw_data[ind.row,10:29],2,mean)
}
colnames(df)<-paste0("sample",c(1:20))
df<-data.frame(df)
rownames(df)<-pattern_summary_table$Pattern
library(RColorBrewer)
my_palette<-colorRampPalette(c("blue", "white", "red"))(n = 100)
library(gplots)
heatmap.2(as.matrix(df),col=my_palette,trace="none", margins=c(7.5,7.5), Colv=FALSE,density.info = "none")

# logo map for sequence
library(ggseqlogo)

# input a list, with each slot contains all sequences follows this pattern
filenames<-list.files(path="c4_05_2_3f/patterns/", pattern="\\.txt$")
i<-1
ls.seq<-list()
for (i in 1:length(filenames)){
  seqname<-unlist(strsplit(filenames[i],split="_"))[1]
  ls.seq[[seqname]]<-as.character(read.delim(paste0("c4_05_2_3f/patterns/",filenames[i]),header=FALSE,check.names = FALSE)[,1])
}
ggseqlogo(data = ls.seq)
# plus output a summary for pattern summary table

# table comparison between different dataset
