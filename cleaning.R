# collect samples and patterns

match_checked_samples<-function(df, checked_samples){
  ind.col<-unlist(lapply(checked_samples,function(x) grep(x,colnames(df))))
  tempdf<-df[,ind.col]
  colnames(tempdf)<-colnames(df)[ind.col]
  rownames(tempdf)<-df[,1]
  return(tempdf)
}
