# collect samples and patterns
sample.list<-list.files(path="Individual/")
pattern.summary.list<-list()
i=1
for(i in 1:length(sample.list)){
  temp.tab<-read.csv(paste0("Individual/", sample.list[i], "/c4_05_2_3f/patterns/pattern_summary_table.csv"),header=TRUE,sep=",")[,-1]
  temp.tab$Pattern<-as.character(temp.tab$Pattern)
  temp.tab$Enrichment..Sample.Frequency...Background.Frequency.<-as.numeric(temp.tab$Enrichment..Sample.Frequency...Background.Frequency.)
  pattern.summary.list[[sample.list[i]]]<-temp.tab
}
