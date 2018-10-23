SaveTags<-function(board, csv){
  write.csv(x = csv, file = paste("IdentificationTags", board, ".csv", sep = ""), row.names = FALSE)
}