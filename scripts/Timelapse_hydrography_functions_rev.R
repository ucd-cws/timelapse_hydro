## Timelapse Hydrography image compositer script -------------------------
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
library(doMC)

# CREATE FOLDERS FOR PROJECT ----------------------------------------------

## check if all proj folders are created in current working dir.
## best if you navigate to root dir of interest first
setwd("./PROJECTS/timelapse_hydro")

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
    dir.create(file.path(paste0(basepath,"/photos")),showWarnings=TRUE)
    dir.create(file.path(paste0(basepath,"/scripts")),showWarnings=TRUE)
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

## run folder function
proj.Folders(new=FALSE)

## if you want to pick a separate location for photos
# photoDir<-setwd(choose.dir()) # for PC
# photoDir<-setwd("path") # need to hard code for MAC

# Gather datetime and brightness from timelapse photos --------------------

p <- proc.time()

## list all photo files in the dir
files <- list.files(path = paste0(getwd(),"/photos"), pattern = ".jpg",ignore.case = TRUE, full.names = T)
photo_numbers<-list.files(path = paste0(getwd(),"/photos"), pattern = ".jpg",ignore.case = TRUE, full.names = F) # photos only

## PHOTO INFO FUNCTION (FOR WINDOWS)
photoInfo <- function(parallel = TRUE, cores = 3, 
                      test.subset = length(files), 
                      baro=TRUE){
  
  if(parallel == TRUE){
    print("Parallel processing")
    
    # clu <- makeCluster(cores)
    # registerDoParallel(clu, cores = cores)
    
    library(doMC)
    registerDoMC(cores)
    
    results <- foreach(i = files[1:test.subset], .packages = ("stringr"),.combine = rbind) %dopar%{
      text <- as.data.frame(system(paste("identify -verbose", i), intern = T), stringsAsFactors = F)
      colnames(text) <- "column"
      
      expos <- system(command = paste("convert", i, "-colorspace gray -resize 1x1  txt:-"), intern = T)
      #expos <- shell(cmd = paste("convert", i, "-colorspace gray -resize 1x1  txt:-"), intern = T)
      baroin <- ifelse(baro==TRUE,
                       as.character(unlist(strsplit(x=text[substr(text$column, 1, 26) == "    exif:ImageDescription:",], split=": "))[2]),
                       NA)
      
      temp <- data.frame("photo" = str_sub(string = i,start = -12,end = -1),
                         "datetime" = as.character(unlist(strsplit(x = text[substr(text$column, 1, 18) == "    exif:DateTime:",], split = ": "))[2]),
                         "baro_in" = as.numeric(str_extract(baroin, pattern = "([1-9]{2})[.]([0-9]{2})")),
                         #"brightness" = as.numeric(unlist(strsplit(expos[2], split = ","))[3]), stringsAsFactors = F)
                         "brightness" = as.numeric(gsub("[\\(\\)]", "", regmatches(expos[2], gregexpr("\\([0-9]{2,3}\\)", expos[2])))), stringsAsFactors = F)
      
      temp$datetime <- lubridate::ymd_hms(temp$datetime)
      temp
    }
    # stopCluster(clu)
  }
  
  else{
    print("Looping through photos")
    
    results <- data.frame()
    
    for(i in files[1:test.subset]){
      print(i)
      text <- as.data.frame(system(paste("identify -verbose", i), intern = T), stringsAsFactors = F)
      colnames(text) <- "column"
      
      expos <- system(command = paste("convert", i, "-colorspace gray -resize 1x1  txt:-"), intern = T) # on Mac
      #expos <- shell(cmd = paste("convert", i, "-colorspace gray -resize 1x1  txt:-"), intern = T)
      baroin <- ifelse(baro==TRUE,
                       as.character(unlist(strsplit(x=text[substr(text$column, 1, 26) == "    exif:ImageDescription:",], split=": "))[2]),
                       NA)
      
      temp <- data.frame("photo" = str_sub(string = i,start = -12,end = -1), 
                         "datetime" = as.character(unlist(strsplit(x = text[substr(text$column, 1, 18) == "    exif:DateTime:",], split = ": "))[2]),
                         "baro_in" = as.numeric(str_extract(baroin, pattern = "([1-9]{2})[.]([0-9]{2})")),
                         "brightness" = as.numeric(gsub("[\\(\\)]", "", regmatches(expos[2], gregexpr("\\([0-9]{2,3}\\)", expos[2])))), stringsAsFactors = F)
                         #"brightness" = as.numeric(unlist(strsplit(expos[2], split = ","))[3]), stringsAsFactors = F)
      
      temp$datetime <- lubridate::ymd_hms(temp$datetime)
      results <- rbind(results,temp)
    }
  }
  return(results)
}

## Run photoInfo function
photolist <- photoInfo(parallel=F, test.subset = 100) # this is for testing
photolist <- photoInfo(cores = 4) # almost 40 min for ~3700 photos

## round to whole hours
photolist$timeround <- floor_date(x = photolist$datetime, unit = "hour")

## Subset to photos to daytime images using exposure/brightness as proxy
photolist_sub <- photolist[photolist$brightness > 40, ]

## Save to .Rdata file so you don't have to do this again and wait
# save(photolist, photolist_sub, file = "./data/tuo_20151217_photolist.Rda")
load("./data/tuo_20151217_photolist.Rda")

## check runtime
runtime <- proc.time() - p
print(paste("finished in", format(runtime[3]/60, digits = 4), "minutes"))

# Create moving hydrographs from logger data ------------------------------

setwd(basepath) # move back to basepath

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

head(dff)

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

fig.Thermo <- function(test.subset = nrow(photolist_sub)){
  
  ## SET COLORS AND BREAKS FOR WATER TEMPERATURES
  breaks<-(c(0,3,6,9,12,15,18,21,24,27)) 
  palette<-c("black","midnightblue","blue","deepskyblue2",
             "green4","green","yellow","orange","red","darkviolet") # orange orangered brown4
  
  print("Making thermohydrographs")
  
  for(j in 1:nrow(photolist_sub[c(1:test.subset),])){
    
    print(paste("making graph", j, "of", nrow(photolist_sub)))
    
    png(paste(getwd(), "/output/temp/thermo/",photolist_sub[j,"photo"],".png", sep = ""), 
        height=4, width=6, units="in", res=500, family="sans", bg = "transparent")
     
    grid.arrange(
      ggplot() + 
        geom_line(data = dff, aes(x = datetime, y = lev.avg), color = "grey50", size = 1, alpha=0.8) +
        scale_colour_gradientn(name=expression(paste("WaterTemp(",degree,"C)")),colours=palette(palette), values=breaks, 
                               rescaler = function(x, ...) x, oob = identity,limits=range(breaks), breaks=breaks, space="Lab") +
        geom_ribbon(data = dff[dff$datetime <= dff$datetime[j],], 
                    aes(x = datetime, ymax = lev.avg, ymin = min(dff$lev.avg)), fill = "gray30", alpha = .5) +
        geom_line(data = dff[dff$datetime <= dff$datetime[j],], 
                  aes(x = datetime, y = lev.avg, color=temp.avg), size = 1) +
        #scale_x_datetime(limits = c(as.POSIXct(min(dff$datetime)),as.POSIXct(max(dff$datetime))), labels = date_format("%b-%Y"))+
        geom_point(data = dff[dff$datetime == dff$datetime[j],], 
                   aes(x = datetime, y = lev.avg),pch=21, fill="gray10", col="white",size = 8) + 
        labs(list(x="",y = "Stage (m)")) + theme_bw() +
        theme(axis.text.x = element_text(face="bold", size=12),
              axis.text.y = element_text(face="bold", size=12),
              axis.title.y = element_text(face="bold", size=12),
              legend.title = element_text(face="bold", size=7),
              legend.text = element_text(size=6),
              panel.background = element_rect(fill = "transparent",colour = NA),
              panel.grid.minor = element_blank(), 
              panel.grid.major = element_blank(),
              plot.background = element_rect(fill = "transparent",colour = NA))
    )
    dev.off()
  }
}

fig.Thermo()

#fig.Thermo(test.subset=3)


#runtime <- proc.time() - p
#print(paste("finished in", format(runtime[3]/60, digits = 4), "minutes"))

# HYDROGRAPH ONLY ---------------------------------------------------------

#p <- proc.time()

fig.Hydro <- function(test.subset = nrow(photolist)){
  
  print("Making Hydrographs only")
  for(j in 1:nrow(photolist_sub[c(1:test.subset),])){
    print(paste("making graph", j, "of", test.subset))
    
    png(paste(getwd(), "/output/temp/hydro/", photolist_sub[j,"photo"], ".png", sep = ""), 
        height=4, width=6, units="in", res=500, family="sans", bg = "transparent")
    
    grid.arrange(
      ggplot(dff, aes(x = datetime, y = lev.avg)) + geom_line(color = "grey30", size = 1) + theme_bw() + 
        geom_ribbon(data = dff[dff$datetime <= photolist_sub$timeround[j],], 
                    aes(ymax = lev.avg, ymin = min(dff$lev.avg)), fill = "blue", alpha = .5) +
        geom_line(data = dff[dff$datetime <= photolist_sub$timeround[j],], 
                  aes(x = datetime, y = lev.avg), size = 2) +
        geom_point(data = dff[dff$datetime == photolist_sub$timeround[j],], 
                   aes(x = datetime, y = lev.avg), pch=21, fill="skyblue1", size = 8) +
        labs(list(x = "", y = "Stage (m)")) +
        theme(axis.text.x = element_text(face="bold", size=12),
              axis.text.y = element_text(face="bold", size=12),
              axis.title.y = element_text(face="bold", size=12),
              panel.background = element_rect(fill = "transparent",colour = NA),
              panel.grid.minor = element_blank(), 
              panel.grid.major = element_blank(),
              plot.background = element_rect(fill = "transparent",colour = NA))
    )
    dev.off()
  }
}

fig.Hydro()

#fig.Hydro(test.subset=3) # short test

#runtime <- proc.time() - p
#print(paste("finished in", format(runtime[3]/60, digits = 4), "minutes"))

# Overlay images using imagemagick -composite command ---------------------

## make sure you are in the root folder of your timelapse project folders

p <- proc.time()

## The Function
photoComposite <- function(bgcol = "white",
                           alpha = 0.5,
                           plotwidth = 600,
                           plotheight = 400,
                           gap = 5,
                           infobarheight = 105,
                           plotlocation = "southeast",
                           thermo=TRUE,
                           parallel = TRUE, 
                           cores = 2){
  
  path <- gsub(pattern = "/", replacement = "\\\\", x = getwd())
  if(thermo){
    photonames<-gsub(pattern=".JPG",replacement = "_thermo", x = photolist_sub[,"photo"],ignore.case = TRUE)
  } else {
    photonames<-gsub(pattern=".JPG",replacement = "_hydro", x = photolist_sub[,"photo"],ignore.case = TRUE)
  }
  print("Overlay images")
  
  # PC
  clu <- makeCluster(cores)
  registerDoParallel(clu, cores = cores)
  
  # MAC
  #require(doMC)
  #registerDoMC(cores)
  
  if(parallel == TRUE){
    print("Using Parallelization")
    foreach(k = photolist_sub$photo) %dopar%{
      if(thermo){
        figfold<-"thermo"
      } else {
        figfold<-"hydro"
      }
      
      shell(cmd = paste("convert -size 2048x1536", " ", path, '\\photos\\', k, # copied photos to proj dir
      #system(command = paste("convert -size 2048x1536", " ", path, '\\photos\\', k, # copied photos to proj dir
                        ##Plot and background rectangle placement
                        ' -draw', ' " fill ', 
                        "rgba(" , col2rgb(bgcol)[1], ",", col2rgb(bgcol)[2], ",", col2rgb(bgcol)[3], ",", alpha, ")",
                        
                        if(plotlocation == "northwest"){paste(' rectangle ', gap, ",", gap, " ", plotwidth, ",", plotheight, '"',  
                                                              " ", path, '\\output\\temp\\', figfold,'\\', k,".png",
                                                              " -gravity northwest -geometry ", plotwidth, "x", plotheight, "+", 10, "+15 -composite",sep = "")
                        },
                        
                        if(plotlocation == "northeast"){paste(' rectangle ', 2048 - plotwidth, ",", plotheight + gap, " ", 2048 - gap, ",", gap, '"', 
                                                              " ", path, '\\output\\temp\\', figfold,'\\', k,".png",
                                                              " -gravity northeast -geometry ", plotwidth, "x", plotheight, "+", 10, "+15 -composite",sep = "")
                        },
                        
                        if(plotlocation == "southwest"){paste(' rectangle ', gap, ",", 1536 - (plotheight + infobarheight), " ", 
                                                              plotwidth + gap, ",", 1536 + - (gap + infobarheight), '"', 
                                                              " ", path, '\\output\\temp\\', figfold,'\\', k,".png",
                                                              " -gravity southwest -geometry ", plotwidth, "x", plotheight, "+", gap, "+", infobarheight + gap, " -composite",sep = "")
                        },
                        
                        if(plotlocation == "southeast"){paste(' rectangle ', 2048 - plotwidth, ",", 1536 - (plotheight + infobarheight), " ",
                                                              2048 - gap, ",", 1536 + - (gap + infobarheight), '"', 
                                                              " ", path, '\\output\\temp\\', figfold,'\\', k,".png",
                                                              " -gravity southeast -geometry ", plotwidth, "x", plotheight, "+", gap, "+", infobarheight + gap, " -composite",sep = "")
                        },
                        
                        ##Output composite image  
                        " ", path, '\\output\\composite\\', k,".png", sep = ""))
    }
    stopCluster(clu)
  }
  
  if(parallel == FALSE){
    print("Looping")
    for(k in photolist_sub$photo){
      print(paste("overlay graph on:", k))
      if(thermo){
        figfold<-"thermo"
      } else {
        figfold<-"hydro"
      }
      
      shell(cmd = paste("convert -size 2048x1536", " ", path, '\\photos\\', k,
      #system(command = paste("convert -size 2048x1536", " ", path, '\\photos\\', k,                  
                        ##Plot and background rectangle placement
                        ' -draw', ' " fill ', 
                        "rgba(" , col2rgb(bgcol)[1], ",", col2rgb(bgcol)[2], ",", col2rgb(bgcol)[3], ",", alpha, ")",
                        
                        if(plotlocation == "northwest"){paste(' rectangle ', gap, ",", gap, " ", plotwidth, ",", plotheight, '"',  
                                                              " ", path, '\\output\\temp\\',figfold,'\\', k,".png",
                                                              " -gravity northwest -geometry ", plotwidth, "x", plotheight, "+", 10, "+15 -composite",sep = "")
                        },
                        
                        if(plotlocation == "northeast"){paste(' rectangle ', 2048 - plotwidth, ",", plotheight + gap, " ", 2048 - gap, ",", gap, '"', 
                                                              " ", path, '\\output\\temp\\', figfold,'\\', k,".png",
                                                              " -gravity northeast -geometry ", plotwidth, "x", plotheight, "+", 10, "+15 -composite",sep = "")
                        },
                        
                        if(plotlocation == "southwest"){paste(' rectangle ', gap, ",", 1536 - (plotheight + infobarheight), " ", 
                                                              plotwidth + gap, ",", 1536 + - (gap + infobarheight), '"', 
                                                              " ", path, '\\output\\temp\\', figfold,'\\', k,".png",
                                                              " -gravity southwest -geometry ", plotwidth, "x", plotheight, "+", gap, "+", infobarheight + gap, " -composite",sep = "")
                        },
                        
                        if(plotlocation == "southeast"){paste(' rectangle ', 2048 - plotwidth, ",", 1536 - (plotheight + infobarheight), " ",
                                                              2048 - gap, ",", 1536 + - (gap + infobarheight), '"', 
                                                              " ", path, '\\output\\temp\\', figfold,'\\', k, ".png",
                                                              " -gravity southeast -geometry ", plotwidth, "x", plotheight, "+", gap, "+", infobarheight + gap, " -composite",sep = "")
                        },
                        
                        ##Output composite image  
                        " ", path, '\\output\\composite\\', k,".png", sep = ""))
    }
  }
}

# Run photoComposite function
photoComposite(parallel = T, cores=6)

runtime <- proc.time() - p
print(paste("finished in", format(runtime[3]/60, digits = 4), "minutes"))



# OPTIONAL RENAME OF FILES ------------------------------------------------

# if you want to rename lots of files
dir_w_files<-"./output/composite"
oldfiles<-list.files(dir_w_files,full.names = T)
newfilenames<-vector()
for(i in 1:length(oldfiles)){
  newfilenames[i]<-paste0("PICT",i,".JPG")
}

file.rename(from = file.path(list.files(dir_w_files,full.names = T)), 
            to = file.path(dir_w_files, newfilenames))


# CREATE mp4 USING FFMPEG -------------------------------------------------
setwd("./output/composite")
## ffmpeg needs to be installed and linked in your path
## pictures need to be renamed in order so create dummy folder in composite folder
system(command =  paste("mkdir img_in_order")) # make a dir to rename photos 

# now rename photos (if more than 9999 photos add digit)
system(command = paste('x=1; for i in *png; do counter=$(printf %04d $x); ln "$i" img_in_order/img"$counter".png; x=$(($x+1)); done'))

# now make movie mp4
system(command = paste('ffmpeg -f image2 -i img_in_order/img%04d.png -s 800x600 2tstlapse.mp4'))
system(command = paste('ffmpeg -f image2 -i PICT%2d.png -s 800x600 2tstlapse.mp4'))
# add colorspace format
# -pix_fmt yuv420p

