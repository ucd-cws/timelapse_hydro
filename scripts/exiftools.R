## USE EXIFTOOL TO PULL PHOTO METADATA
## 2016-02-17
## R. Peek

## Some notes on using exiftool
## this line will pull photo name, creation date, and exposure/gamecam strip info
## It's fast and requires "exiftools"
##@ exiftool -r -s -T -FILE:FileName -EXIF:CreateDate -EXIF:Software -EXIF:ImageDescription . > photoinfo/exifinfo.txt"

## Other commands
## exiftool -list -EXIF:All
## exiftool -r -w -common
## exiftool -r -s -G
## exiftool -r -T '-*Baro*' PICT0001.JPG 
## exiftool -r -T -IFD0:Image\\ Description PICT0001.JPG
## exiftool -T -r -g1 -IFD0:Image\\Description
## exiftool -r -T '-*ExposureLuma2' PICT0001.JPG
## exiftool -r -s -G -EXIF:ModifyDate PICT0001.JPG


# LIBRARIES ---------------------------------------------------------------

library(readr)
library(lubridate)
library(tidyr)

# OLD MOULTRIE ------------------------------------------------------------

path.to.photos<-"/gamecam/"
path.to.photos<-"/Volumes/projects/Long_Term_Monitoring/American_Yuba_Rivers/photos/NFYuba/TimeLapse/20140512/" # server
site <- "NFY"

# internal R call for old MOULTRIE
exifinfo<-system(command = paste0('exiftool -r -s -T -FILE:FileName -EXIF:CreateDate -EXIF:Software -EXIF:ImageDescription ',path.to.photos),intern=T)

# make a text file from metadata
text <- as.data.frame(row.names = NULL, x = exifinfo, stringsAsFactors = F) # convert to text dataframe with column name

# split out data into columns by tab
df<-separate(data=text, col = exifinfo, into = c("photo", "datetime", "exposure","infostrip"), sep = "\t") # split by tab

# now format columns
df$exposure <- as.numeric(stringr::str_sub(df[,"exposure"], -3, -1)) # get just exposure
df$datetime<-ymd_hms(df$datetime) # convert datetime to POSIXct
df$baro_inHg <- as.numeric(stringr::str_extract(df[,"infostrip"],pattern="([1-9]{2})[.]([0-9]{2})"))
df$air_C <- as.numeric(stri_sub(sapply(strsplit(df$infostrip, ".", fixed = T), "[", 4),from = 1, -4))
#df$air_C <- as.numeric(stringr::str_extract(df[,'infostrip'],pattern='(?:inHg.\\s.)([0-9]{1,2})[.C]'))
df$site <- site
df<-df[,c(7,1:3,5,6)] # pull cols of interest
str(df)

df.name<-paste0(site, "_exif") # set name for assigning to dataframe
assign(df.name, df) # assign dataframe

# save data
#save(NFA_exif, file = paste0(root, "PROJECTS/timelapse_hydro/data/", site, "_exif.rda")) # save to rda
write_rds(assign(df.name, df), path = paste0("./data/", site, "_exif.rds")) # save to rds
write_rds(assign(df.name, df), path = paste0("./data/", site, "_exif_20140512.rds")) # for specific date

# read in rds
#nfa_exif<-read_rds(path = paste0(root, "PROJECTS/timelapse_hydro/data/", site, "_exif.rds"))

# read in rda
#load(file = paste0(root, "PROJECTS/timelapse_hydro/data/", site, "_exif.rda"))

# NEW MOULTRIE ------------------------------------------------------------

# photo location and site info
path.to.photos<-"I:/DCIM/100MFCAM" # SD Card
path.to.photos<-"/Volumes/projects/Long_Term_Monitoring/American_Yuba_Rivers/photos/NFYuba/TimeLapse/20140512/" # server
site <- "NFY"

# internal R call for new MOULTRIE, get info and write to local dataframe in R
exifinfo<-system(command = paste0('exiftool -r -s -T -FILE:FileName -EXIF:CreateDate -EXIF:MakerNoteUnknownText ',path.to.photos),intern=T)
# str(exifinfo) # view data
text <- as.data.frame(row.names = NULL, x = exifinfo, stringsAsFactors = F) # convert to text dataframe with column name

# split out data into columns
df<-separate(data=text, col = exifinfo, into = c("photo", "datetime", "infostrip"), sep = "\t") # split by tab

# new cameras set to only day, don't really need to filter by exposure...though need to find approp field if so
df$baro_inHg <- as.numeric(stringr::str_extract(string=df$infostrip,pattern="([0-9]{1,2})[.]([0-9]{1,2})")) # get baroHg
df$air_C <- as.numeric(sapply(strsplit(df$infostrip, ":", fixed = T), "[", 2))
df$site <- site
df$datetime<-ymd_hms(df$datetime) # convert datetime to POSIXct

df<-df[,c(6,1:2,4,5)] # pull cols of interest

h(df)

df.name<-paste0(site, "_exif") # set name for assigning to dataframe
assign(df.name, df) # assign dataframe

# save data
write_rds(assign(df.name, df), path = paste0(root, "/data/", site, "_exif.rds")) # save to rds

# clear workspace
rm(list=ls())

# read in rds
#exif<-read_rds(path = paste0(root, "/data/", site, "_exif.rds"))  # or use base package readRDS


