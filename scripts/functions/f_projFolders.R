## Project Folder Creation/Check Function
proj.Folders<-function(new=FALSE){
  
  if(new){
    cat("Type fullpath to directory you want create video: \n")
    ldir<-scan(what="character",nlines = 1) # enter full path
    setwd(ldir) # set working dir to path provided
    cat("Working Project Dir now: \n ", getwd(), "\n\n")
    basepath<-getwd() # set working directory as basepath
  } else {
    basepath<-getwd() # set working directory as basepath
  }
  
  # check if folders exist otherwise create
  if (dir.exists(paste0(basepath,"/output/composite"))) {
    cat("Folders exist and are in directory path already...Good to go! \n")
  } else {
    cat("Folders do not exist in directory - creating now... \n")
    dir.create(file.path(paste0(basepath,"/data")),showWarnings=TRUE)
    dir.create(file.path(paste0(basepath,"/output")),showWarnings=TRUE)
    dir.create(file.path(paste0(basepath,"/output/composite")), showWarnings=TRUE)
    dir.create(file.path(paste0(basepath,"/output/temp")), showWarnings=TRUE)
    dir.create(file.path(paste0(basepath,"/output/temp/thermo")), showWarnings=TRUE)
    dir.create(file.path(paste0(basepath,"/output/temp/hydro")), showWarnings=TRUE)
    dir.create(file.path(paste0(basepath,"/photos")),showWarnings=TRUE)
    #dir.create(file.path(paste0(basepath,"/scripts")),showWarnings=TRUE)
    cat("All folders created! \n
        \t data 
        \t output
        \t\t output/composite
        \t\t output/temp
        \t photos
        \t scripts")
  }
  assign(x = "basepath", value = basepath, envir = .GlobalEnv)
  }
