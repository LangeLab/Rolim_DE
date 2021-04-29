# collect samples and patterns
library(dplyr)
library(reshape2)

match_checked_samples<-function(df, checked_samples){
  ind.col<-unlist(lapply(checked_samples,function(x) grep(x,colnames(df))))
  tempdf<-df[,ind.col]
  colnames(tempdf)<-colnames(df)[ind.col]
  rownames(tempdf)<-df[,1]
  return(tempdf)
}

transformdata<-function(quant_data_summary_table, checkSamples, colIntensity, bb){
  bb<-bb[which(bb$input_sample_name %in% checkSamples),grep("\\.",colnames(bb))]
  df<-quant_data_summary_table %>% filter(input_sample_name %in% checkSamples)
  df<-cbind(df[,c(1:2)],df[,colIntensity], bb)
  aa<-cbind(aggregate.data.frame(df[,colIntensity],by=list(df$aligned_sequence),function(x) mean(x, na.rm=TRUE)),
            aggregate.data.frame(df[,grep("\\.",colnames(df))],by=list(df$aligned_sequence),function(x) sum(x, na.rm=TRUE)>0)[,-1])
  return(na.omit(aa))
}
