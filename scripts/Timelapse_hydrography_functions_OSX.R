## Timelapse Hydrography image compositer script: OSX ---------------------

## Author: Eric Holmes, Ryan Peek, and Nick Santos
## Organization: UC Davis Center for Watershed Sciences
## Date: 2016/01/05

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

# SOURCE FUNCTIONS --------------------------------------------------------

source("./scripts/functions/f_projFolders.R")
source("./scripts/functions/f_photoInfo_osx.R")
source("./scripts/functions/f_thermohydro.R")
source("./scripts/functions/f_hydrographs.R")
source("./scripts/functions/f_photoComposite_osx.R")

# CREATE FOLDERS FOR PROJECT ----------------------------------------------

## check if all proj folders are created in current working dir.
## best if you navigate to root dir of interest first
#setwd("./PROJECTS/timelapse_hydro")

## run folder function
proj.Folders(new=FALSE)

# GATHER PHOTO INFO (DATETIME, BARO, BRIGHTNESS) ------------------------

p <- proc.time()

## list all photo files in the dir
files <- list.files(path = paste0(getwd(),"/photos"), pattern = ".jpg",ignore.case = TRUE, full.names = T)
photo_numbers<-list.files(path = paste0(getwd(),"/photos"), pattern = ".jpg",ignore.case = TRUE, full.names = F) # photos only

## Run photoInfo function
photolist <- photoInfo(parallel=T,cores = 3, test.subset = 10) # this is for testing
photolist <- photoInfo(cores = 4) # almost 40 min for ~3700 photos

## round to whole hours
photolist$timeround <- floor_date(x = photolist$datetime, unit = "hour")

## Subset to photos to daytime images using exposure/brightness as proxy
photolist_sub <- photolist[photolist$brightness > 40, ]

## check runtime
runtime <- proc.time() - p
print(paste("finished in", format(runtime[3]/60, digits = 4), "minutes"))

## Save to .RData file so you don't have to do this again and wait
# save(photolist, photolist_sub, file = "./data/tuo_20151217_photolist.Rda")
load("./data/tuo_20151217_photolist.Rda")

# SUBSET DATE TO WINDOW OF INTEREST ---------------------------------------

## If subsetting to certain date window use these lines
start.date<-ymd_hms("2015-11-01 09:00:00")
end.date<-ymd_hms("2015-12-17 09:00:00")

## use magrittr to filter by dates
photolist_sub %<>% filter(datetime >= start.date & datetime <= end.date)

# GET LOGGER DATA FROM CSV AND SUBSET TO PHOTOS ---------------------------

## get the logger file name of interest
logger.name <- "2015_TUO_solinst_12_17.csv"

## set interval in minutes
interval = 15

## read in csv
level <- read.csv(paste(getwd(), "/data/", logger.name, sep = ""), stringsAsFactors = F, skip = 11) # check lines to skip
level$datetime <- as.POSIXct(paste(level$Date, level$Time), format = "%Y/%m/%d %H:%M:%S")
level$datetimeround <- as.POSIXct(round(as.double(level$datetime)/(interval*60))*(interval*60),origin=(as.POSIXct(tz="GMT",'1970-01-01')))

## subset to the photo list 
levsub <- na.omit(level[level$datetimeround >= range(photolist$timeround)[1] & level$datetimeround <= range(photolist$timeround)[2],])

## Make Hourly dataset to match photos
dfhr<- levsub %>%
  mutate("year"=year(datetimeround),
         "month"=month(datetimeround),
         "yday"=yday(datetimeround),
         "hour"=hour(datetimeround)) %>% 
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

## merge with photolist_sub to make into one dataframe for everything
dff<-merge(dfhr, photolist_sub[,c(1,3:5)], by.x="datetime", by.y="timeround", all = F)

# head(dff)

# TEST PLOTS --------------------------------------------------------------

## SET COLORS AND BREAKS FOR WATER TEMPERATURES
breaks<-(c(0,3,6,9,12,15,18,21,24,27)) 
palette<-c("black","midnightblue","blue","deepskyblue2",
           "green4","green","yellow","orange","red","darkviolet") # orange orangered brown4

# test plot to set colors
grid.arrange(
  ggplot() + 
    geom_line(data = dff, aes(x = datetime, y = lev.avg), color = "grey50", size = 1, alpha=0.8) +
    scale_colour_gradientn(name=expression(paste("Water \nTemp (",degree,"C)")),colours=palette(palette), values=breaks, 
                           rescaler = function(x, ...) x, oob = identity,limits=range(breaks), breaks=breaks, space="Lab") +
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

# THERMOHYDROGRAPH --------------------------------------------------------

#p <- proc.time()

fig.Thermo()

#fig.Thermo(test.subset=3)

#runtime <- proc.time() - p
#print(paste("finished in", format(runtime[3]/60, digits = 4), "minutes"))

# HYDROGRAPH ONLY ---------------------------------------------------------

#p <- proc.time()

fig.Hydro()

#fig.Hydro(test.subset=3) # short test

#runtime <- proc.time() - p
#print(paste("finished in", format(runtime[3]/60, digits = 4), "minutes"))

# OVERLAY IMAGES USING IMAGEMAGICK -composite COMMAND ---------------------

## make sure you are in the root folder of your timelapse project folders

p <- proc.time()

# Run photoComposite function
photoComposite(parallel = T, cores=3, thermo=T)
photoComposite(parallel = F, thermo=T)

runtime <- proc.time() - p
print(paste("finished in", format(runtime[3]/60, digits = 4), "minutes"))

# OPTIONAL RENAME OF FILES ------------------------------------------------

## if you want to rename lots of files
dir_w_files<-"./output/composite/hydro"
oldfiles<-list.files(dir_w_files,pattern = ".png",full.names = T)
newfilenumbers<-seq(from = 1,to = length(oldfiles),by = 1)
newfilenames<-vector()
for(i in 1:length(oldfiles)){
  newfilenames[i]<-paste0(sprintf("PICT%04d", i),".JPG")
}

file.rename(from = file.path(list.files(dir_w_files,pattern='.png',full.names = T)), 
            to = file.path(dir_w_files, newfilenames))

# CREATE mp4 USING FFMPEG -------------------------------------------------

## move to dir with composite folders
setwd("./output/composite/hydro")

## pictures need to be renamed in order so create dummy folder in composite folder
system(command =  paste("mkdir img_in_order")) # make a dir to rename photos 

## now rename photos (if more than 9999 photos add digit) # only works on mac
system(command = paste('x=1; for i in *png; do counter=$(printf %04d $x); ln "$i" img_in_order/img"$counter".png; x=$(($x+1)); done'))
system(command = paste('x=1; for i in *JPG; do counter=$(printf %04d $x); ln "$i" img_in_order/img"$counter".png; x=$(($x+1)); done'))

# now make movie mp4 (images need to be numerically ordered in "img_in_order" folder)
system(command = paste('ffmpeg -f image2 -i img_in_order/img%04d.png -s 800x600 tuolapse_2015_fall.mp4'))

## make slower (longer exposure per image)
system(command = paste('ffmpeg -f image2 -r 1.0 -i img_in_order/img%04d.png -s 800x600 tuolapse_2015_short.avi'))
system(command = paste('ffmpeg -f image2 -r 1.0 -i img_in_order/img%04d.png -s 800x600 tuolapse_2015_short.mp4'))

## make movie mp4 (with raw name of images i.e., "JPG.png")
system(command = paste('ffmpeg -f image2 -i output/composite/hydro/PICT%04d.JPG.png -s 800x600 tuolapse_2015_short.mp4'))


## additional ffmpeg info
# ffmpeg -y -loop 1 -f image2 -r 0.5 -i image-%03d.jpg -s:v 1280x720 -b:v 1M \
#  -i soundtrack.mp3 -t 01:05:00 -map 0:0 -map 1:0 out.avi

## -loop_input – loops the images
## -r 0.5 – sets the framerate to 0.5, which means that each image will be shown for 2 seconds. Just take the inverse, 
##  for example if you want each image to last for 3 seconds, set it to 0.33
## -i soundtrack.mp3 # soundtrack file you can add whatever
## -t 01:05:00 #set the output length in hh:mm:ss format







