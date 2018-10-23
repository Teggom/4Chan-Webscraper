library("rvest")
board<-"wg"
url<-"http://boards.4chan.org/wg/"
Session<-html_session(url)
PostNums<-Session %>%
  html_nodes(".op .desktop a+ a") %>%
  html_text()

Threads<-c()
for(x in 1:length(PostNums)){
  Threads<-c(Threads, html_session(paste("boards.4chan.org/", board,"/thread/", PostNums[x], sep = "")))  
}

Clicked<-follow_link(Session, "Click here")

for(x in 65:70){
  c<-follow_link(Session, i = x)
}

#####################
tryCatch({
Thread<-html_session("http://boards.4chan.org/wg/thread/6670074/")
  PictureLinks<-Thread %>%
  html_nodes(".fileText a") %>%
  html_text()
FullNames<-Thread %>%
  html_nodes(".fileText") %>%
  html_text()
if(length(FullNames)!=length(PictureLinks)){print(paste("Fatal Error Encountered, PictureNameLength: ", length(PictureLinks), " |&| PictureHeaderTextLength: ", length(FullNames)));stop()}
for(x in 1:length(PictureLinks)){
  OldPicture<-FALSE
  picture<-follow_link(Thread, PictureLinks[x])
  nvurl<-picture$url
  newTagsToAdd<-c()
  ####
  ### Checks old pictures
  ####
  for(a in 1:ncol(Tags)){
    if(length(grep(gsub("\\D", "", nvurl), Tags[a]))>0){
      OldPicture<-TRUE
    }
  }
  
  ####
  ### Downloads the picture if it is unique
  ####
  if(!OldPicture){
    if(grepl(".png$", PictureLinks[x])){
      download.file(url = nvurl, destfile = paste(gsub("\\D", "", nvurl),".png", sep = ""), method = "internal", mode = "ab")
    }
    if(grepl(".jpg$", PictureLinks[x])){
      download.file(url = nvurl, destfile = paste(gsub("\\D", "", nvurl),".jpg", sep = ""), method = "internal", mode = "ab")
    }
    newTagsToAdd<-c(newTagsToAdd, gsub("\\D", "", nvurl))
  }
  
  
}
}, error = function(x){print(paste("Error while working in a thread, Pictures for Thread Failed!", sep = ""))})

#######ADDNVURL TO THE BAD LIST




