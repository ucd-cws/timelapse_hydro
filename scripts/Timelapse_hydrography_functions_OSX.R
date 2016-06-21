## Timelapse Hydrography image compositer script: OSX ---------------------

## Author: Ryan Peek
## Organization: UC Davis Center for Watershed Sciences
## Date: 2016-05-21

## Note: Script requires **imagemagick** to be in the system path for command calls 
## Similarly **ffmpeg** is required to be installed for stitching into mp4 file.

# LOAD LIBRARIES ----------------------------------------------------------

library(ggplot2)
library(scales)
library(lubridate)
library(gridExtra)
library(foreach)
library(doParallel)
library(stringr)
library(dplyr)
library(magrittr)
library(doMC)
library(readr)
library(tidyr)

# SOURCE FUNCTIONS --------------------------------------------------------

source("./scripts/functions/f_projFolders.R")
source("./scripts/functions/f_photoInfo_osx.R")
source("./scripts/functions/f_photoComposite_osx.R")

# CREATE FOLDERS FOR PROJECT ----------------------------------------------

## check if all proj folders are created in current working dir.
## best if you navigate to root dir of interest first
#setwd("./PROJECTS/timelapse_hydro")

## run folder function
proj.Folders(new=F)

# OLD MOULTRIE: GATHER PHOTO INFO (DATETIME, BARO, BRIGHTNESS) ------------

# Select sitename (should be same name as folder the raw photos are in)
site<-"MFY"

## list all photo files in the dir
files <- list.files(path = paste0(getwd(),"/photos/",site), pattern = ".jpg",ignore.case = TRUE, full.names = T)
#photo_numbers<-list.files(path = paste0(getwd(),"/photos/NFA"), pattern = ".jpg",ignore.case = TRUE, full.names = F) # photos only

## Run photoInfo function
photolist <- photoInfo(parallel=F, test.subset = 10) # this is for testing

photolist <- photoInfo(cores = 3) # almost 40 min for ~3700 photos

## round to whole hours
photolist$timeround <- floor_date(x = photolist$datetime, unit = "hour")

## Subset to photos to daytime images using exposure/brightness as proxy
photolist_sub <- photolist[photolist$brightness > 40, ]

## Save to .RData file so you don't have to do this again and wait
# save(photolist, photolist_sub, file = "./data/nfa_20150805_photolist.rda")
# load("./data/nfa_20150805_photolist.rda")

# NEW MOULTRIE: Gather Photoinfo using exiftools --------------------------

source("./scripts/functions/f_exifInfo.R")

# if running function:
exifInfo(site="NFA",path.to.photos = "~/Desktop/gamecam/nfa/", datecheck = "2016-02-25", saveInfo = T)

# if just loading data
photolist<-readr::read_rds("./data/NFA_exif_2016-02-25.rds")

# need to match times with 15 or hourly intervals: do that here
interval = 15 ## set interval in minutes
photolist$timeround <- as.POSIXct(round(as.double(photolist$datetime)/
                                          (interval*60))*(interval*60),
                                  origin=(as.POSIXct(tz="GMT",'1970-01-01')))
head(photolist)

# make sure all are distinct (i.e., number here matches total rows in df)
ifelse(nrow(photolist)==dplyr::n_distinct(photolist$timeround), "All rows are distinct", "STOP, duplicates in data") 

## Subset to photos to daytime images using exposure/brightness as proxy
photolist_sub <- photolist[photolist$exposure >= 4, ]

# SUBSET DATE TO WINDOW OF INTEREST ---------------------------------------

## If subsetting to certain date window use these lines
start.date<-ymd_hms("2015-11-01 09:00:00")
end.date<-ymd_hms("2015-12-17 09:00:00")

## use magrittr to filter by dates
photolist_sub %<>% filter(datetime >= start.date & datetime <= end.date)

# GET DATA FROM USGS and SUBSET TO PHOTOLIST ------------------------------

source("./scripts/functions/f_USGS_15min.R")

# get.USGS(11427000, "NFA", sdate = "2016-02-01", save15 = T)

usgs<-readr::read_csv("data/NFA_2016-02-01_15min_USGS.csv")

## subset to the photo list 
Qsub <- na.omit(usgs[usgs$datetime >= range(photolist_sub$timeround)[1] & usgs$datetime <= range(photolist_sub$timeround)[2],])

## merge with photolist_sub to make into one dataframe for everything
dff<-merge(Qsub[,c(1:3,5,11:12)], photolist_sub[,c(7,2,5:6)], by.x="datetime", by.y="timeround", all = F)

head(dff)

# GET LOGGER DATA FROM CSV AND SUBSET TO PHOTOS ---------------------------

## get the logger file name of interest
logger.name <- "2015_NFA_solinst_08_05.csv"

## set interval in minutes
interval = 15

## read in csv
level <- read.csv(paste(getwd(), "/data/", logger.name, sep = ""), stringsAsFactors = F, skip = 13) # check lines to skip
level$datetime <- as.POSIXct(paste(level$Date, level$Time), format = "%Y/%m/%d %H:%M:%S")
level$timeround <- as.POSIXct(round(as.double(level$datetime)/(interval*60))*(interval*60),origin=(as.POSIXct(tz="GMT",'1970-01-01')))
colnames(level)[4:5]<-toupper(colnames(level)[4:5])

## subset to the photo list 
levsub <- na.omit(level[level$timeround >= range(photolist$timeround)[1] & level$timeround <= range(photolist$timeround)[2],])

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
h(dfhr)

## merge with photolist_sub to make into one dataframe for everything
dff<-merge(dfhr, photolist_sub[,c(1,3:5)], by.x="datetime", by.y="timeround", all = F)

head(dff)

# LOAD EXISTING DATA ------------------------------------------------------

load("./data/mfy_2016_photolist.rda") # photo list
load("./data/mfy_2016_gage_data.rda") # flow/logger data


# TEST PLOTS --------------------------------------------------------------

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

# THERMOHYDROGRAPH --------------------------------------------------------

source("./scripts/functions/f_thermohydro.R")

fig.Thermohydro()

fig.Thermohydro(test.subset=10)

# USGS HYDROGRAPH ONLY ----------------------------------------------------

## USGS version: NEED flow_cms & flow_cfs columns

source("./scripts/functions/f_hydrographs_usgs.R")

fig.Hydro.usgs(cms = F, test.subset = 20)
fig.Hydro.usgs(cms = F)

# USGS HYDROGRAPH ONLY ----------------------------------------------------

## USGS version: NEED flow_cms & flow_cfs columns

source("./scripts/functions/f_hydrographs_air_usgs.R")
fig.Hydroair.usgs(air = T, test.subset = 20)
fig.Hydroair.usgs(air = T)

# SOLINST HYDROGRAPH ONLY -------------------------------------------------

source("./scripts/functions/f_hydrographs.R")

## SOLINST version: NEED lev.avg, flow_cfs, or flow_cms columns

fig.Hydro(test.subset=10,stage = F, cms = F, yvar = "flow_cfs") # short test

fig.Hydro(stage = F, cms = F, yvar = "flow_cfs") # short test

# AIR TEMP ----------------------------------------------------------------

## USGS version
source("./scripts/functions/f_hydrographs_air_usgs.R")

fig.Hydroair.usgs(air = T, test.subset = 100)
fig.Hydroair.usgs(air = F)

# OVERLAY IMAGES USING IMAGEMAGICK -composite COMMAND ---------------------

## make sure you are in the root folder of your timelapse project folders
## and photos are copied into folder in your photos folder within projects
## i.e., "timelapse_hydro/photos/*SITE*/[images]"

p <- proc.time()

# Run photoComposite function
photoComposite(parallel = T, cores=3, thermo=F, site="MFY", plotlocation = "southeast")

runtime <- proc.time() - p
print(paste("finished in", format(runtime[3]/60, digits = 4), "minutes"))

# RENAME OF FILES ------------------------------------------------

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



# CREATE mp4 USING FFMPEG -------------------------------------------------

## move to dir with composite photos
setwd("./output/composite/")

## now make movie mp4 (images need to be numerically ordered, default ~25 frames per sec)
system(command = paste('ffmpeg -f image2 -i  PICT%04d.JPG -s 800x600 mfylapse_2016_short.mp4'))

## make slower (longer exposure per image, '-r ##' frames per second)
system(command = paste('ffmpeg -f image2 -r 12 -i PICT%04d.JPG -s 800x600 ../videos/mfyHHtimelapse_2015_short.mp4'))

