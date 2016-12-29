# Timelapse Hydrography image compositer script: WINDOWS ----------------

## Authors: Ryan Peek, Eric Holmes, and Nick Santos
## Organization: UC Davis Center for Watershed Sciences
## Date: 2016/12/28

## Note: Script requires installation of imagemagick and exiftools, and both must be on the system path for 
## command calls to work, similarly ffmpeg is required to stitch into mp4 file.

# 1A. LOAD LIBRARIES & FUNCTIONS ---------------------------------------------

library(ggplot2)
library(scales)
library(lubridate)
library(gridExtra)
library(foreach)
library(doParallel)
library(stringr)
library(stringi)
library(dplyr)
library(magrittr)
library(readr)
library(tidyr)

# source functions
source("./scripts/functions/f_projFolders.R")
source("./scripts/functions/f_photoInfo.R")
source("./scripts/functions/f_exifInfo.R")
source("./scripts/functions/f_thermohydro.R")
source("./scripts/functions/f_hydrographs.R")
source("./scripts/functions/f_photoComposite.R")

# 1B. CREATE FOLDERS ----------------------------------------------

## Only need to do this once, check if all proj folders are created in current 
## working directory. Best to work from a project, and navigate to root dir. 
## i.e., setwd("./PROJECTS/timelapse_hydro")

## run folder function
proj.Folders(new=F) # new=FALSE will just check, new=TRUE will create

# 2. SET GLOBAL INFO: On Photo Folder & Site ------------------------------------

## Select sitename (should be same name as folder the raw photos are in)
site<-"NFA"

## Set working directory to photos
path.to.photos<-"X:/Sierra/Long_Term_Monitoring/American_Yuba_Rivers/photos/NFAmerican/Timelapse/20161228_download"

## Date photos checked/downloaded
datecheck = "2016-12-28"

## 2A. If photo information has already been gathered, pre-load the RData files here:

## Load photolist and photolist_sub data to environment
# load(file = paste0("./data/",site, "_photolist_", datecheck, ".rda"))

# 3. CREATE PHOTOLIST: Get Photo Info ----------------------------------------

## This section will collect the datetime and brightness/exposure from each photo. 
## Make sure to check the date, site, and path above.
    ## newMoultrie=TRUE if using M-990i Gen 2
    ## newMoultrie=FALSE if using MFH-M65.

## Generate Photolist and Save to File
exifInfo(site=site, path.to.photos = path.to.photos, newMoultrie = TRUE, datecheck = datecheck, saveInfo = T)

## Load the photolist here (renames to "photolist" instead of 'SITE_exif_datecheck')
photolist<-read_rds(path = paste0("./data/", site, "_exif_", datecheck, ".rds")) # from exifinfo

## Match timelapse interval times here (in minutes), e.g., for hourly = 60, 15 minutes = 15
interval = 15 ## set interval in minutes
photolist$timeround <- as.POSIXct(round(as.double(photolist$datetime)/
                                          (interval*60))*(interval*60),
                                  origin=(as.POSIXct(tz="GMT",'1970-01-01')))

head(photolist) # check the data

# make sure all are distinct (i.e., number here matches total rows in df)
ifelse(nrow(photolist)==dplyr::n_distinct(photolist$timeround), "All rows are distinct", "STOP, duplicates in data") 

## Subset to photos to daytime images using exposure/brightness as proxy
photolist_sub <- photolist[photolist$exposure >= 4, ] # NEW CAMERAS (see notes line 53-56)

## Subset to photos to daytime images using exposure/brightness as proxy
photolist_sub <- photolist[photolist$exposure > 40, ] # OLD CAMERAS (see notes line 53-56)

## Save to .Rdata file so you don't have to do this again and wait
save(photolist, photolist_sub, file = paste0("./data/",site, "_photolist_", datecheck, ".rda"))

# 4. SUBSET to Window of Interest ------------------------------------------

## If subsetting to certain date window use these lines
start.date<-ymd_hms("2016-02-01 07:00:00")
end.date<-ymd_hms("2016-05-04 07:00:00")

photolist_sub %<>% filter(datetime >= start.date & datetime <= end.date)
summary(photolist_sub)

# 5A. GET SOLINST LOGGER DATA ------------------------------

## get the logger file name of interest
logger.name <- "./data/loggers/2016_CLA_solinst_06_20.csv"
interval <- 15 # set the interval to minutes
skiplines <- 11

## read in data
level <- read.csv(logger.name, stringsAsFactors = F, skip = skiplines) # check lines to skip

## Make a datetime column

## check for YMD vs. MDY
# YMD
# level$datetime <- as.POSIXct(paste(level$Date, level$Time), format = "%Y/%m/%d %H:%M:%S") # check to make sure YMD or MDY
# MDY
level$datetime <- as.POSIXct(paste(level$Date, level$Time), format = "%m/%d/%Y %H:%M:%S")

## Round to interval (set above)
level$timeround <- as.POSIXct(round(as.double(level$datetime)/(interval*60))*(interval*60),origin=(as.POSIXct(tz="GMT",'1970-01-01')))

head(level)

# make Level/Temperature columns ALL CAPS
colnames(level)[c(4:5)]<- toupper(colnames(level)[c(4:5)])

# 5B. AGGREGATE TO HOURLY & COMPENSATE -----------------------------------------

## subset to the photo list 
levsub <- na.omit(level[level$timeround >= range(photolist_sub$timeround)[1] & level$timeround <= range(photolist_sub$timeround)[2],])

## Make Hourly dataset to match photos
dfhr<- levsub %>%
  mutate("year"=year(timeround),
         "month"=month(timeround),
         "yday"=yday(timeround),
         "hour"=hour(timeround)) %>% 
  group_by(year, month, yday, hour)%>%
  summarize(
    "lev.avg"=mean(LEVEL,na.rm=TRUE),
    "lev.min"=min(LEVEL,na.rm=TRUE),
    "lev.max"=max(LEVEL,na.rm=TRUE),
    "temp.avg"=mean(TEMPERATURE,na.rm=TRUE),
    "temp.min"=min(TEMPERATURE,na.rm=TRUE),
    "temp.max"=max(TEMPERATURE,na.rm=TRUE))%>%
  mutate("datetime"=ymd_hms(strptime(paste0(year,"-", month,"-", yday, " ",
                                            hour,":00"),format = "%Y-%m-%j %H:%M"))) %>%
  select(datetime,year,month,yday,lev.avg:temp.max) %>%
  as.data.frame()

names(dfhr)
names(photolist_sub)

# merge with photolist_sub to make into one dataframe for everything
dff<-merge(dfhr, photolist_sub[ ,c(7,1:2,4:6) ], by.x="datetime", by.y="timeround", all = F)

summary(dff) # the final merged data

### COMPENSATION WITH GAME CAMERA BARO
dff$baro_m<-convertBaro(x = dff$baro_inHg, type = "in", adj = 0) # converts inHg to m (and subtracts 9)

# now subtract compensated baro from level column
dff$lev.avg.comp <- dff$lev.avg - dff$baro_m

# test plot
ggplot(dff) + geom_line(aes(datetime, lev.avg-10), color="red") + 
  geom_line(aes(datetime, lev.avg.comp), color="maroon", size=1.6, alpha=0.4)

# 5C. WRITE PHOTO/LOGGER DATA TO RDA FILE -------------------------------------

## Save to .Rdata file so you don't have to do this again and wait

save(dff, file = paste0("./data/",site, "_combined_loggerphotos_", str_sub(floor_date(start.date, unit = "day"),end = -1),"_", str_sub(floor_date(end.date, unit = "day"),start=-5,end = -1), ".rda"))

# 6A. CREATE LOGGER THERMOHYDROGRAPH ------------------------------------------

load(file = paste0("./data/",site, "_combined_loggerphotos_", str_sub(floor_date(start.date, unit = "day"),end = -1),"_", str_sub(floor_date(end.date, unit = "day"),start=-5,end = -1), ".rda"))

source("./scripts/functions/f_thermohydro.R")

#p <- proc.time()

fig.Thermohydro()

fig.Thermohydro(test.subset=10)

#runtime <- proc.time() - p
#print(paste("finished in", format(runtime[3]/60, digits = 4), "minutes"))

# 6B. CREATE LOGGER HYDROGRAPH --------------------------------------------

source("./scripts/functions/f_hydrographs.R")

## SOLINST version: NEED lev.avg, flow_cfs, or flow_cms columns

fig.Hydro(test.subset=10,stage = F, cms = F, yvar = "flow_cfs") # short test
fig.Hydro(stage = F, cms = F, yvar = "flow_cfs") # short test

# 7A. GET USGS DATA from USGS station --------------------------------------

# might need to install packages: caTools, data.table, readr

source("./scripts/functions/f_USGS_15min.R") # make sure data.table installed

get.USGS(gage = 11427000, river = "NFA", sdate = "2011-01-01", edate = "2016-01-01", saveHrly = T, save15 = F) # 11727000 USGS for NFA, #11413000 is for NFY

usgs<-readr::read_csv("data/usgs/NFA_2014-06-01_hourly_USGS.csv")

## subset to the photo list 
Qsub <- na.omit(usgs[usgs$datetime >= range(photolist_sub$timeround)[1] & usgs$datetime <= range(photolist_sub$timeround)[2],])

## merge with photolist_sub to make into one dataframe for everything
dff<-merge(Qsub, photolist_sub[,c(1,3:5)], by.x="datetime", by.y="timeround", all = F)

## merge with photolist_sub to make into one dataframe for everything (new camera?)
dff<-merge(Qsub[,c(1:3,5,11:12)], photolist_sub[,c(7,2,5:6)], by.x="datetime", by.y="timeround", all = F)

head(dff)

# 7B. CREATE USGS HYDROGRAPH ---------------------------------------------------------

## USGS version: NEED flow_cms & flow_cfs columns
source("./scripts/functions/f_hydrographs_usgs.R")

fig.Hydro.usgs(cms = F, test.subset = 20) # test a subset
fig.Hydro.usgs(cms = F) # to run all

# 7C. CREATE USGS HYDROGRAPH & AIR ------------------------------------------------------

## USGS version: NEED flow_cms & flow_cfs columns
source("./scripts/functions/f_hydrographs_air_usgs.R")

fig.Hydroair.usgs(air = T, test.subset = 20)
fig.Hydroair.usgs(air = T)

# 7D. CREATE USGS AIR TEMP GRAPH ------------------------------------------

## USGS version
source("./scripts/functions/f_hydrographs_air_usgs.R")

fig.Hydroair.usgs(air = T, test.subset = 100)
fig.Hydroair.usgs(air = F)

# Make Test Thermohydro Plots --------------------------------------------------------------

## SET COLORS AND BREAKS FOR WATER TEMPERATURES
breaksair<-(seq(0,36,4)) # best for air
breaksh20<-(seq(0,27,3)) # best for water

palette<-c("black","midnightblue","blue","deepskyblue2",
           "green4","green","yellow","orange","red","darkviolet") # orange orangered brown4

# USGS plot with air as thermoscale

## plot stage or flow
water<-"flow_cms" #  "stage_m"
usgslab<-"Q (cms)" # "Stage (m)"
minusgs<-min(dff$flow_cms) # or stage_m

grid.arrange(
  ggplot() + # can switch flow_cms with stage_m and use air for color
    geom_line(data = dff, aes_string(x = "datetime", y = water), color = "grey50", size = 1, alpha=0.8) +
    scale_colour_gradientn(name=expression(paste("AirTemp(",degree,"C)")),colours=palette(palette), values=breaksair,
                           rescaler = function(x, ...) x, oob = identity,limits=range(breaksair), breaks=breaksair, space="Lab") +
    geom_ribbon(data = dff[dff$datetime <= dff$datetime[nrow(dff)],], 
                aes_string(x = "datetime", ymax = water, ymin = minusgs), fill = "gray80", alpha = .5) +
    geom_line(data = dff[dff$datetime <= dff$datetime[nrow(dff)],], 
              aes_string(x = "datetime", y = water, color="air_C"), size = 1) +
    geom_point(data = dff[dff$datetime == dff$datetime[nrow(dff)],], 
               aes_string(x = "datetime", y = water), pch=21, fill="gray10", col="white",size = 8) + 
    labs(list(x="",y = usgslab)) + theme_bw() +
    theme(axis.text.x = element_text(face="bold", size=12),
          axis.text.y = element_text(face="bold", size=12),
          axis.title.y = element_text(face="bold", size=12),
          panel.background = element_rect(fill = "transparent",colour = NA),
          panel.grid.minor = element_blank(), 
          panel.grid.major = element_blank(),
          plot.background = element_rect(fill = "transparent",colour = NA))
)

# USGS plot with air as separate

grid.arrange(
  ggplot() + # can switch flow_cms with stage_m and use air for color
    geom_line(data = dff, aes_string(x = "datetime", y = water), color = "blue2", size = 1, alpha=0.8) +
    scale_colour_gradientn(name=expression(paste("Air\nTemp(",degree,"C)")),colours=palette(palette), values=breaksair,
                           rescaler = function(x, ...) x, oob = identity,limits=range(breaksair), breaks=breaksair, space="Lab") +
    geom_ribbon(data = dff[dff$datetime <= dff$datetime[nrow(dff)],], 
                aes_string(x = "datetime", ymax = water, ymin = minusgs), fill = "gray80", alpha = .5) +
    geom_line(data = dff[dff$datetime <= dff$datetime[nrow(dff)],], 
              aes_string(x = "datetime", y = "air_C", color="air_C"), size = 1) +
    geom_point(data = dff[dff$datetime == dff$datetime[nrow(dff)],], 
               aes_string(x = "datetime", y = "air_C"), pch=21, fill="gray60", col="white",size = 6) + 
    geom_point(data = dff[dff$datetime == dff$datetime[nrow(dff)],], 
               aes_string(x = "datetime", y = water), pch=21, fill="gray10", col="white",size = 7) + 
    labs(list(x="",y = usgslab)) + theme_bw() +
    theme(axis.text.x = element_text(face="bold", size=12),
          axis.text.y = element_text(face="bold", size=12),
          axis.title.y = element_text(face="bold", size=12),
          panel.background = element_rect(fill = "transparent",colour = NA),
          panel.grid.minor = element_blank(), 
          panel.grid.major = element_blank(),
          plot.background = element_rect(fill = "transparent",colour = NA))
)


# SOLINST plot to set colors
grid.arrange(
  ggplot() + 
    geom_line(data = dff, aes(x = datetime, y = lev.avg), color = "grey50", size = 1, alpha=0.8) +
    scale_colour_gradientn(name=expression(paste("Water \nTemp (",degree,"C)")),colours=palette(palette), values=breaksh20, 
                           rescaler = function(x, ...) x, oob = identity,limits=range(breaksh20), breaks=breaks, space="Lab") +
    geom_ribbon(data = dff[dff$datetime <= dff$datetime[nrow(dff)],], 
                aes(x = datetime, ymax = lev.avg, ymin = min(dff$lev.avg)), fill = "gray30", alpha = .5) +
    
    geom_line(data = dff[dff$datetime <= dff$datetime[nrow(dff)],], 
              aes(x = datetime, y = lev.avg, color=temp.avg), size = 1) +
    geom_point(data = dff[dff$datetime == dff$datetime[nrow(dff)],], 
               aes(x = datetime, y = lev.avg),pch=21, fill="gray10", col="white",size = 8) + 
    labs(list(x="",y = "Stage (m)")) + theme_bw() +
    theme(axis.text.x = element_text(face="bold", size=12),
          axis.text.y = element_text(face="bold", size=12),
          axis.title.y = element_text(face="bold", size=12),
          panel.background = element_rect(fill = "transparent",colour = NA),
          panel.grid.minor = element_blank(), 
          panel.grid.major = element_blank(),
          plot.background = element_rect(fill = "transparent",colour = NA))
)


# USGS hydro+air POINT
grid.arrange(
  ggplot(dff, aes(x = datetime, y = flow_cms)) + geom_line(color = "grey30", size = 1) + theme_bw() + 
    geom_ribbon(data = dff[dff$datetime <= photolist_sub$timeround[nrow(dff)],], 
                aes(ymax = flow_cms, ymin = min(dff$flow_cms)), fill = "blue", alpha = .5) +
    scale_colour_gradientn(name=expression(paste("AirTemp(",degree,"C)")),colours=palette(palette), values=breaksair, 
                           rescaler = function(x, ...) x, oob = identity,limits=range(breaksair), breaks=breaksair, space="Lab") +
    geom_point(data = dff[dff$datetime <= photolist_sub$timeround[nrow(dff)],], 
               aes(x = datetime, y = flow_cms, color=air_C), size = 1.3) +
    geom_point(data = dff[dff$datetime == photolist_sub$timeround[nrow(dff)],], 
               aes(x = datetime, y = flow_cms), pch=21, fill="skyblue1", size = 8) +
    #ylim(c(0,200))+
    labs(list(x = "", y = "Discharge (cms)")) +
    theme(axis.text.x = element_text(face="bold", size=12),
          axis.text.y = element_text(face="bold", size=12),
          axis.title.y = element_text(face="bold", size=12),
          panel.background = element_rect(fill = "transparent",colour = NA),
          panel.grid.minor = element_blank(), 
          panel.grid.major = element_blank(),
          plot.background = element_rect(fill = "transparent",colour = NA))
)

# 8. OVERLAY IMAGES with imagemagick -------------------------------------

## make composite of plot and photos with composite command.
## make sure you are in the root folder of your timelapse project folders

p <- proc.time()

# Run photoComposite function
photoComposite(parallel = T, cores=3, thermo=T, site="MFY", plotlocation = "northwest",
               alpha = 0.6,
               plotwidth = 700,
               plotheight = 500,
               gap = 12,
               infobarheight = 125)

runtime <- proc.time() - p
print(paste("finished in", format(runtime[3]/60, digits = 4), "minutes"))

# 9. RENAME FILES --------------------------------------------------------

# Should do this in order to make the imagemagick call work to stitch into video

## PC: RENAME FILES
dir_w_files<-"./output/composite"
oldfiles<-list.files(dir_w_files,pattern = ".JPG",full.names = T)
newfilenumbers<-seq(from = 1,to = length(oldfiles),by = 1)
newfilenames<-vector()
for(i in 1:length(oldfiles)){
  newfilenames[i]<-paste0(sprintf("PICT%04d", i),".JPG")
}

# now rename in numerical 4 digit order
file.rename(from = file.path(list.files(dir_w_files,pattern='.JPG',full.names = T)), 
            to = file.path(dir_w_files, newfilenames))

## MACOSX: RENAME FILES

## pictures need to be renamed in order so create dummy folder in composite folder
system(command =  paste("mkdir ./output/composite_ordered")) # make a dir to rename photos 
system(command = paste("cp ./output/composite/*JPG ./output/composite_ordered/")) # copy over
system(command = paste('x=1; for i in output/composite_ordered/*JPG; do counter=$(printf %04d $x); ln "$i" output/composite_ordered/WORK"$counter".JPG; x=$(($x+1)); done'))

# 10. CREATE mp4 USING FFMPEG -------------------------------------------------

## move to dir with composite photos
setwd("./output/composite")

## MAKE SURE THE ONLY THING IN THE COMPOSITE FOLDER IS JPGs, no other files or ffmpeg will crash

## now make movie mp4 (images need to be numerically ordered, default ~25 frames per sec)
shell(cmd = paste('ffmpeg -f image2 -i  PICT%04d.JPG -s 800x600 tuolapse_2015_short.mp4'))

## make slower (longer exposure per image, frames per second)
shell(cmd = paste('ffmpeg -f image2 -r 12 -i PICT%04d.JPG -s 800x600 tuolapse_2015_short.mp4'))
