AddTagsToCSV <-function(csv, tags){
  l<-length(tags)
  if(l==0){return}
  for(x in 1:l){
    add<-tags[x]
    csv[1,1]<-csv[1,1]+1
    #Row Filled, add a new Row
    if(csv[1,1]%%100==0){
      v<-csv[1,]
      v[1,]<-0
      csv<-rbind(csv, v)
    }
    
    Row<-as.integer(csv[1,1]/100)+1
    Col<-(csv[1,1]%%100)+1
    csv[Row,Col]<-add
    if(csv[1,1]>100000){csv[1,1]<-0}
  }
  return(csv)
}