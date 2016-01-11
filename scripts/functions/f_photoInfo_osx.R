
## PHOTO INFO FUNCTION (FOR MAC)
photoInfo <- function(parallel = TRUE, cores = 2, 
                      test.subset = length(files), 
                      baro=TRUE){
  
  if(parallel == TRUE){
    print("Parallel processing")
    
    # Mac
    library(doMC)
    registerDoMC(cores)
    
    results <- foreach(i = files[1:test.subset], .packages = ("stringr"),.combine = rbind) %dopar%{
      text <- as.data.frame(system(paste("identify -verbose", i), intern = T), stringsAsFactors = F)
      colnames(text) <- "column"
      
      expos <- system(command = paste("convert", i, "-colorspace gray -resize 1x1  txt:-"), intern = T)
      baroin <- ifelse(baro==TRUE,
                       as.character(unlist(strsplit(x=text[substr(text$column, 1, 26) == "    exif:ImageDescription:",], split=": "))[2]),
                       NA)
      
      temp <- data.frame("photo" = str_sub(string = i,start = -12,end = -1),
                         "datetime" = as.character(unlist(strsplit(x = text[substr(text$column, 1, 18) == "    exif:DateTime:",], split = ": "))[2]),
                         "baro_in" = as.numeric(str_extract(baroin, pattern = "([1-9]{2})[.]([0-9]{2})")),
                         "brightness" = as.numeric(gsub("[\\(\\)]", "", 
                                                        regmatches(expos[2], gregexpr("\\([0-9]{2,3}\\)", expos[2])))), 
                         stringsAsFactors = F)
      
      temp$datetime <- lubridate::ymd_hms(temp$datetime)
      temp
    }
  }
  
  else{
    print("Looping through photos")
    
    results <- data.frame()
    
    for(i in files[1:test.subset]){
      print(i)
      text <- as.data.frame(system(paste("identify -verbose", i), intern = T), stringsAsFactors = F)
      colnames(text) <- "column"
      
      expos <- system(command = paste("convert", i, "-colorspace gray -resize 1x1  txt:-"), intern = T)
      baroin <- ifelse(baro==TRUE,
                       as.character(unlist(strsplit(x=text[substr(text$column, 1, 26) == "    exif:ImageDescription:",], split=": "))[2]),
                       NA)
      
      temp <- data.frame("photo" = str_sub(string = i,start = -12,end = -1), 
                         "datetime" = as.character(unlist(strsplit(x = text[substr(text$column, 1, 18) == "    exif:DateTime:",], split = ": "))[2]),
                         "baro_in" = as.numeric(str_extract(baroin, pattern = "([1-9]{2})[.]([0-9]{2})")),
                         "brightness" = as.numeric(gsub("[\\(\\)]", "", regmatches(expos[2], gregexpr("\\([0-9]{2,3}\\)", expos[2])))), stringsAsFactors = F)
      
      temp$datetime <- lubridate::ymd_hms(temp$datetime)
      results <- rbind(results,temp)
    }
  }
  return(results)
}
