## Timelapse Hydrography image compositer script: WINDOWS ----------------

## Author: Eric Holmes, Ryan Peek, and Nick Santos
## Organization: UC Davis Center for Watershed Sciences
## Date: 2016/01/05

## Note: Script requires installation of imagemagick and for imagemagick to be on the system path for 
## the command calls to work, similarly ffmpeg is required to stitch into mp4 file.

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

# Source Functions --------------------------------------------------------

source("./scripts/functions/f_projFolders.R")
source("./scripts/functions/f_photoInfo.R")
source("./scripts/functions/f_thermohydro.R")
source("./scripts/functions/f_hydrographs.R")
source("./scripts/functions/f_photoComposite.R")

# CREATE FOLDERS FOR PROJECT ----------------------------------------------

## check if all proj folders are created in current working dir.
## best if you navigate to root dir of interest first
setwd("./PROJECTS/timelapse_hydro")

# load pre-run data (photolist)
load("./data/tuo_20151217_photolist.Rda")

## run folder function (new=FALSE means don't create new folders set, just check)

proj.Folders(new=FALSE)

# Gather datetime and brightness from timelapse photos --------------------

p <- proc.time()

## list all photo files in the dir
files <- list.files(path = paste0(getwd(),"/photos"), pattern = ".jpg",ignore.case = TRUE, full.names = T)

## make a vector of just the photo_numbers
photo_numbers<-basename(files) # ".jpg" only

## Run photoInfo function
photolist <- photoInfo(parallel=T, test.subset = 20) # this is for testing
photolist <- photoInfo(cores = 2) # almost 40 min for ~3700 photos

## check runtime
runtime <- proc.time() - p
print(paste("finished in", format(runtime[3]/60, digits = 4), "minutes"))

## round to whole hours
photolist$timeround <- floor_date(x = photolist$datetime, unit = "hour")

## Subset to photos to daytime images using exposure/brightness as proxy
photolist_sub <- photolist[photolist$brightness > 40, ]

## Save to .Rdata file so you don't have to do this again and wait
# save(photolist, photolist_sub, file = "./data/tuo_20151217_photolist.Rda")

# SUBSET DATE WINDOW OF INTEREST ------------------------------------------

## If subsetting to certain date window use these lines
start.date<-ymd_hms("2015-12-01 09:00:00")
end.date<-ymd_hms("2015-12-15 09:00:00")

photolist_sub %<>% filter(datetime >= start.date & datetime <= end.date)
s(photolist_sub)

# Create moving hydrographs from logger data ------------------------------

## get the logger file name of interest
logger.name <- "2015_TUO_solinst_12_17.csv"
interval = 15

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

# merge with photolist_sub to make into one dataframe for everything
dff<-merge(dfhr, photolist_sub[,c(1,3:5)], by.x="datetime", by.y="timeround", all = F)

s(dff)

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

fig.Thermohydro()

fig.Thermohydro(test.subset=10)

#runtime <- proc.time() - p
#print(paste("finished in", format(runtime[3]/60, digits = 4), "minutes"))

# HYDROGRAPH ONLY ---------------------------------------------------------

#p <- proc.time()

fig.Hydro()

fig.Hydro(test.subset=10) # short test

#runtime <- proc.time() - p
#print(paste("finished in", format(runtime[3]/60, digits = 4), "minutes"))

# Overlay images using imagemagick -composite command ---------------------

## make sure you are in the root folder of your timelapse project folders

p <- proc.time()

# Run photoComposite function
photoComposite(parallel = T, cores=3,thermo = FALSE)

runtime <- proc.time() - p
print(paste("finished in", format(runtime[3]/60, digits = 4), "minutes"))

# OPTIONAL RENAME OF FILES ------------------------------------------------

# if you want to rename lots of files
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

# CREATE mp4 USING FFMPEG -------------------------------------------------
setwd("./output/composite")


## MAKE SURE THE ONLY THING IN THE COMPOSITE FOLDER IS JPGs, no other files or ffmpeg will crash

## now make movie mp4 (images need to be numerically ordered, default ~25 frames per sec)
shell(cmd = paste('ffmpeg -f image2 -i  PICT%04d.JPG -s 800x600 tuolapse_2015_short.mp4'))

## make slower (longer exposure per image, frames per second)
shell(cmd = paste('ffmpeg -f image2 -r 12 -i PICT%04d.JPG -s 800x600 tuolapse_2015_short.mp4'))


## additional ffmpeg info
#  -i soundtrack.mp3 -t 01:05:00 -map 0:0 -map 1:0 out.avi
# -loop_input – loops the images
# -r 0.5 – sets the framerate to 0.5, which means that each image will be shown for 2 seconds. Just take the inverse, 
##  for example if you want each image to last for 3 seconds, set it to 0.33
# -i soundtrack.mp3 # soundtrack file you can add whatever
## -t 01:05:00 #set the output length in hh:mm:ss format







