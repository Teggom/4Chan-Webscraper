#This is the code to run which will execute the rest of the 4Chan Code to scrape a board

################
###  To Do   ###
################
###  Set Up so it tryCatch fail to install Rvest
################
### </To Do> ###
################

Scrap_4chan <- function(board, startWithEndOfBoard, page){
  SubfolderName<-paste("Pictures", toupper(board), sep = "")
  
  if(length(setdiff("rvest", rownames(installed.packages())))>0){
    install.packages("rvest")
  }
  
  TimerStart<-proc.time()
  library("rvest")
  #######
  #######
    #Important: Set this to a folder directory to save your pictures
    #Manually change the \ to / or this will not work
    if(!tryCatch({dir_Path<-"C:/Users/Stephen Kozak/Documents/R Code/4Chan/4ChWorkFolder";setwd(dir_Path);TRUE},error = function(x){FALSE})){print("Please ensure that you have switched the \ to / in your path, and that your path is valid");stop()}
    setwd(dir_Path)
    if(!tryCatch({source("AddTags.R");TRUE}, error=function(x){FALSE})){print("Failed to source AddTags.R");stop()}
    if(!tryCatch({source("SaveTags.R");TRUE}, error=function(x){FALSE})){print("Failed to source SaveTags.R");stop()}
    if(!file.exists(paste("Pictures", board, sep = ""))){dir.create(paste("Pictures", board, sep = ""))}
    if(!file.exists(paste("IdentificationTags", board, ".csv", sep = ""))){write.csv(t(data.frame(1:100)), file = paste("IdentificationTags", board, ".csv", sep = ""), row.names = FALSE)}
    Tags<-read.csv(paste("IdentificationTags", board, ".csv", sep = ""), header = TRUE, row.names = NULL)
    Tags[1,]<-0
    #######
    #######
    ##############       ##############%$#%$#%$#%$#%$#%$#%$#%$#%$#%$#%$#%$#%$#%$#%$#%$#%$#%$#%$#%$#%$#%$#%$#%$#
    ##### SPECIFY DIMENSIONS HERE #####%$#%$#%$#%$#%$#%$#%$#%$#%$#%$#%$#%$#%$#%$#%$#%$#%$#%$#%$#%$#%$#%$#%$#%$#
    ##############       ##############%$#%$#%$#%$#%$#%$#%$#%$#%$#%$#%$#%$#%$#%$#%$#%$#%$#%$#%$#%$#%$#%$#%$#%$#
    # LowXDim<-0         #
    # HighXDim<-10000    #
    # LowYDim<-0         #
    # HighYDim<-10000    #
    ######################
    LowXDim<-1700
    HighXDim<-4900
    LowYDim<-900
    HighYDim<-3400
    
    
    
    
  #####
    ####
  #####
  url<-paste("http://boards.4chan.org/", board, "/", sep = "")
  urls<-c()
  ThreadsVisited<-c("")
  Threads<-c()
  qqf<-0
  ## ## ## ## ## ## Start of the loop
  setwd(paste("Pictures", board, sep = ""))
  CountSkippedPics<-0
  while(qqf<1000){
    if(CountSkippedPics>100){
      #if(page<=0){
      #  Sys.sleep(90)
      #}
      if(board == 'w'){ board = 'wg'; url <- paste("http://boards.4chan.org/", board, '/', sep = "")}
      else if(board == 'wg'){ board = 'w'; url <- paste("http://boards.4chan.org/", board, '/', sep = "");startWithEndOfBoard <- FALSE}
    }
    ####
    ### Open and retrieve threads
    ####
    if(startWithEndOfBoard){
      if(page>1){
        print(paste("Page", page, "about to load"))
      } else {
        print("Main Page Loading")
      }
      if(page>1){
        MainBoard<-html_session(paste(url,page, sep = ""))
        print(MainBoard$url)
        print(paste("Loading Page:", page))
      } else {
        MainBoard<-html_session(url)
        print("Back 10 Pages Examined, Refreshing Page 1!")
      }
      page<-page-1
    } else {
      MainBoard<-html_session(url)
      page<-0
      print("Default Connection to 1st Page Opened!")
    }
    ThreadNums<-MainBoard %>%
      html_nodes(".op .desktop a+ a") %>%
      html_text()
    
    ####
    ### removes Duplicate Posts
    ####
    #for(x in 1:length(ThreadNums)){
    #  if(length(grep(ThreadNums[x], ThreadsVisited))!=0){ThreadNums[x]<-0}
    #}
    ##Adds the new threads to the already visited
    #for(x in 1:length(ThreadNums)){
    #  if(ThreadNums[x]!=0){ThreadsVisited<-c(ThreadsVisited, ThreadNums[x])}
    #}
  #  
    ####
    ### Create URLs
    ### Opens New Sessions
    ####
    urls<-c()
    for(x in 1:length(ThreadNums)){
      if(ThreadNums[x]!=0){
        urls<-c(urls, paste("boards.4chan.org/", board, "/thread/", ThreadNums[x], sep = ""))
      }
    }
    
    ####
    ### Runs Though all Sessions, downloading pictures
    ####
    for(TNum in 1:length(ThreadNums)){
      #################################################################
      tryCatch({
        print(paste("Loading thread:", ThreadNums[TNum]))
        Thread<-html_session(urls[TNum])#refreshes the thread and opens it
        PictureLinks<-Thread %>%
          html_nodes(".fileText a") %>%
          html_text()
        FullNames<-Thread %>%
          html_nodes(".fileText") %>%
          html_text()
        ListOfThumbnails<-Thread %>%
          html_nodes("#delform img") %>%
          #html_nodes(".fileThumb img") %>%
          html_attr('src')
       if(length(FullNames)!=length(PictureLinks)||length(PictureLinks)!=length(ListOfThumbnails)){print(paste("Fatal Error Encountered, PictureNameLength: ", length(PictureLinks), " |&| PictureHeaderTextLength: ", length(FullNames), " |&| ListOfThumbNailsLength: ", length(ListOfThumbnails), sep = ""));stop()}
        for(x in 1:length(PictureLinks)){
          Sys.sleep(.25) #########################Removing This Will Speed up the Code a lot
          newTagsToAdd<-c()
          ####
          ### Checks old pictures
          ####
         # for(a in 1:ncol(Tags)){
         #   if(length(grep(gsub("\\D", "", nvurl), Tags[a]))>0){
         #     OldPicture<-TRUE
         #   }
         # }
          
          ####
          ### Downloads the picture if it is unique
          ####
          #picture<-follow_link(Thread, PictureLinks[x])
          #nvurl<-picture$url
          #print("Thumbnail # followed by nvurl #")
          #print(paste(gsub("\\D", "", ListOfThumbnails[x])))
          #print(paste(gsub("\\D", "", nvurl)))
          
          Dims<-gsub("^.*MB,|^.*KB,|\\)$|\\s", "", FullNames[x])
          picX<-as.numeric(gsub("x.*$", "", Dims))
          picY<-as.numeric(gsub("^.*x", "", Dims))
          
          tryCatch({
            if((picX>=3340&&picX<=3540&&picY>=1390&&picY<=1490)){
              if(grepl(".png$", PictureLinks[x])){
                if(!file.exists(paste("G:/Picture Dump Folder/", "MaciekWallpapers", "/", gsub("\\D", "", ListOfThumbnails[x]),".png", sep = ""))){
                  picture<-follow_link(Thread, PictureLinks[x])
                  nvurl<-picture$url
                  download.file(url = nvurl, destfile = paste("G:/Picture Dump Folder/", "MaciekWallpapers", "/", gsub("\\D", "", nvurl),".png", sep = ""), method = "internal", mode = "ab")
                  print(paste("Downloaded A picture for Maciek:", length(list.files(path = "G:/Picture Dump Folder/MaciekWallpapers")), "Downloaded total"))
                }
              }
              if(grepl(".jpg$", PictureLinks[x])){
                if(!file.exists(paste("G:/Picture Dump Folder/", "MaciekWallpapers", "/", gsub("\\D", "", ListOfThumbnails[x]),".jpg", sep = ""))){
                  picture<-follow_link(Thread, PictureLinks[x])
                  nvurl<-picture$url
                  download.file(url = nvurl, destfile = paste("G:/Picture Dump Folder/", "MaciekWallpapers", "/", gsub("\\D", "", nvurl),".jpg", sep = ""), method = "internal", mode = "ab")
                  print(paste("Downloaded A picture for Maciek:", length(list.files(path = "G:/Picture Dump Folder/MaciekWallpapers")), "Downloaded total"))
                  print(paste("G:/Picture Dump Folder/", "MaciekWallpapers", "/", gsub("\\D", "", nvurl),".jpg", sep = ""))
                }
              }
            } 
            if(picX/picY>2.2&&picX/picY<2.4){
              if(grepl(".png$", PictureLinks[x])){
                if(!file.exists(paste("G:/Picture Dump Folder/", "TWOneByNine", "/", gsub("\\D", "", ListOfThumbnails[x]),".png", sep = ""))){
                  picture<-follow_link(Thread, PictureLinks[x])
                  nvurl<-picture$url
                  download.file(url = nvurl, destfile = paste("G:/Picture Dump Folder/", "TWOneByNine", "/", gsub("\\D", "", nvurl),".png", sep = ""), method = "internal", mode = "ab")
                  print(paste("Downloaded a near 21:9 Picture:", length(list.files(path = "G:/Picture Dump Folder/TWOneByNine")), "Downloaded total"))
                }
              }
              if(grepl(".jpg$", PictureLinks[x])){
                if(!file.exists(paste("G:/Picture Dump Folder/", "TWOneByNine", "/", gsub("\\D", "", ListOfThumbnails[x]),".jpg", sep = ""))){
                  picture<-follow_link(Thread, PictureLinks[x])
                  nvurl<-picture$url
                  download.file(url = nvurl, destfile = paste("G:/Picture Dump Folder/", "TWOneByNine", "/", gsub("\\D", "", nvurl),".jpg", sep = ""), method = "internal", mode = "ab")
                  print(paste("Downloaded a near 21:9 Picture:", length(list.files(path = "G:/Picture Dump Folder/TWOneByNine")), "Downloaded total"))
                  print(paste("G:/Picture Dump Folder/", "TWOneByNine", "/", gsub("\\D", "", nvurl),".jpg", sep = ""))####
                }
              }
            }
            
          }, error=function(x){print("Error in Maciek's Gather Path")})
          
          
          
          
          
          if(picX>=LowXDim && picX<=HighXDim && picY>=LowYDim && picY<=HighYDim && picX>=picY ){
            if(grepl(".png$", PictureLinks[x])){
              if(!file.exists(paste("G:/Picture Dump Folder/", SubfolderName, "/", gsub("\\D", "", ListOfThumbnails[x]),".png", sep = ""))){
                picture<-follow_link(Thread, PictureLinks[x])
                nvurl<-picture$url
                download.file(url = nvurl, destfile = paste("G:/Picture Dump Folder/", SubfolderName, "/", gsub("\\D", "", nvurl),".png", sep = ""), method = "internal", mode = "ab")
                print(paste("Skipped", CountSkippedPics, "pictures between Downloads!"))
                CountSkippedPics<-0
              } else {CountSkippedPics<-CountSkippedPics+1}
            }
            else if(grepl(".jpg$", PictureLinks[x])){
              if(!file.exists(paste("G:/Picture Dump Folder/", SubfolderName, "/", gsub("\\D", "", ListOfThumbnails[x]),".jpg", sep = ""))){
                picture<-follow_link(Thread, PictureLinks[x])
                nvurl<-picture$url
                download.file(url = nvurl, destfile = paste("G:/Picture Dump Folder/", SubfolderName, "/", gsub("\\D", "", nvurl),".jpg", sep = ""), method = "internal", mode = "ab")
                print(paste("Skipped", CountSkippedPics, "pictures between Downloads!"))
                CountSkippedPics<-0
              } else {CountSkippedPics<-CountSkippedPics+1}
            } else {CountSkippedPics<-CountSkippedPics+1}
            newTagsToAdd<-c(newTagsToAdd, gsub("\\D", "", nvurl))
          } else {CountSkippedPics<-CountSkippedPics+1}
        }
      }, error = function(x){print(paste("failure in Thread: ", gsub("^4", "", gsub("\\D", "", urls[TNum]))))})
      ##############################################################################
    }
    qqf<-qqf+1
  }
  ## ## ## ## ## ## ## This is the end of the loop
  print(paste("Total Time for One Iteration is: ", paste(proc.time()-TimerStart), sep = ""))
}

