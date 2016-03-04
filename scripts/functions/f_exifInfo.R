## USE EXIFTOOL TO PULL PHOTO METADATA
## 2016-02-26
## R. Peek

## This function requires "exiftool" in the system path.

## this line will pull photo name, creation date, and exposure/gamecam strip info
## exiftool -r -s -T -FILE:FileName -EXIF:CreateDate -EXIF:Software -EXIF:ImageDescription . > photoinfo/exifinfo.txt"

## Other commands:
## exiftool -r -w -common
## exiftool -r -s -G
## exiftool -r -T '-*Baro*' PICT0001.JPG 
## exiftool -T -r -g1 -IFD0:Image\\Description
## exiftool -r -s -G -EXIF:ModifyDate PICT0001.JPG

### Exposure
## exiftool -r -T '-*ExposureLuma2' PICT0001.JPG # on old moultries
## exiftool -r -s -G -Composite:LightValue . # new moultrie

# NEW MOULTRIE EXIF FUNCTION ----------------------------------------------

## need to provide a date the card/cam was checked and 
## a location for the photos (photo path)
# date checked/downloaded

exifInfo<-function( site, # location
                    datecheck, # date camera checked/downloaded
                    path.to.photos, 
                    saveInfo = TRUE) { # quoted folder path
  path.to.photos = path.to.photos
  datecheck = datecheck
  site = site

  # packages  
  library(readr)
  library(lubridate)
  library(tidyr)
  
  # internal R call for new MOULTRIE, get info and write to local dataframe in R
  exifinfo<-system(command = paste0('exiftool -r -s -T -FILE:FileName -EXIF:CreateDate -EXIF:MakerNoteUnknownText -Composite:LightValue ',path.to.photos),intern=T)
  # str(exifinfo) # view data
  text <- as.data.frame(row.names = NULL, x = exifinfo, stringsAsFactors = F) # convert to text dataframe with column name
  
  # split out data into columns
  df<-separate(data=text, col = exifinfo, into = c("photo", "datetime", "infostrip","exposure"), sep = "\t") # split by tab
  
  # exposure helps filter night/low light shots (anything >=4 is good on new cams)
  df$exposure <- as.numeric(df$exposure)
  df$baro_inHg <- as.numeric(stringr::str_extract(string=df$infostrip,pattern="([0-9]{1,2})[.]([0-9]{1,2})")) # get baroHg
  df$air_C <- as.numeric(sapply(strsplit(df$infostrip, ":", fixed = T), "[", 2))
  df$site <- site
  df$datetime<-ymd_hms(df$datetime) # convert datetime to POSIXct
  
  df<-df[,c(7,1:2,4:6)] # pull cols of interest

  print(head(df))
  Pause()
  
  print(summary(df))
  Pause()
  
  df.name<-paste0(site, "_exif_",datecheck) # set name for assigning to dataframe
  
  assign(df.name, df, envir=.GlobalEnv) # assign dataframe
  
  # save data to data folder in project
  # write_rds(assign(df.name, df), path = paste0("./data/", site, "_exif_",datecheck, ".rds")) # save to rds
  
  if(saveInfo){
    write_rds(assign(df.name, df), path = paste0("./data/", site, "_exif_",datecheck, ".rds")) # save to rds
    print(paste(df.name, ".rds saved here: ", getwd(), "/data/", sep=""))
  } else {
    cat("No exifinfo saved as .rds")
  }
  
}


