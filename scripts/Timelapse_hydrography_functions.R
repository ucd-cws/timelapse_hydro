###################################################################################################
##Timelapse Hydrography image compositer script
##Author: Eric Holmes, Ryan Peek, and Nick Santos
##Organization: UC Davis Center for Watershed Sciences
##Date: 2015/12/22
###################################################################################################

##Note: Script requires installation of imagemagick and for imagemagick to be on the system path for 
##      the command calls to work

##Load libraries
library(ggplot2)
library(gridExtra)
library(foreach)
library(doParallel)

###################################################################################################
##Gather datetime and brightness from timelapse photos

print("Gather datetimes and brightness of photos")

setwd("C:/Users/ejholmes/Desktop/Timelapse_hydrography")
files <- list.files(paste(getwd(),"/photos", sep = ""), full.names = T)

p <- proc.time()

photoInfo <- function(parallel = TRUE, cores = 3, test.subset = length(files)){
  
  if(parallel == TRUE){
    print("Parallel processing")
    
    clu <- makeCluster(cores)
    registerDoParallel(clu, cores = cores)
    
    results <- foreach(i = files[1:test.subset], .combine = rbind) %dopar%{
      print(i)
      text <- as.data.frame(system(paste("identify -verbose", i), intern = T), stringsAsFactors = F)
      colnames(text) <- "column"
      
      expos <- shell(cmd = paste("convert", i, "-colorspace gray -resize 1x1  txt:-"), intern = T)
      
      temp <- data.frame("photo" = as.character(substr(i, nchar(i)- 11, nchar(i))), 
                         "datetime" = as.character(unlist(strsplit(x = text[substr(text$column, 1,18) == "    exif:DateTime:",], split = ": "))[2]),
                         "brightness" = as.numeric(unlist(strsplit(expos[2], split = ","))[3]), stringsAsFactors = F)
      
      temp$datetime <- as.POSIXlt(temp$datetime, format = "%Y:%m:%d %H:%M:%S")
      temp
    }
    stopCluster(clu)
  }
  
  else{
    print("for loop")
    
    results <- data.frame()
    
    for(i in files[1:test.subset]){
      print(i)
      text <- as.data.frame(system(paste("identify -verbose", i), intern = T), stringsAsFactors = F)
      colnames(text) <- "column"
      
      expos <- shell(cmd = paste("convert", i, "-colorspace gray -resize 1x1  txt:-"), intern = T)
      
      temp <- data.frame("photo" = as.character(substr(i, nchar(i)- 11, nchar(i))), 
                         "datetime" = as.character(unlist(strsplit(x = text[substr(text$column, 1,18) == "    exif:DateTime:",], split = ": "))[2]),
                         "brightness" = as.numeric(unlist(strsplit(expos[2], split = ","))[3]), stringsAsFactors = F)
      
      temp$datetime <- as.POSIXlt(temp$datetime, format = "%Y:%m:%d %H:%M:%S")
      results <- rbind(results,temp)
    }
  }
  return(results)
}

##Run photoInfo function
photolist <- photoInfo(test.subset = 20)

##round to whole hours
photolist$timeround <- round.POSIXt(x = photolist$datetime, units = "mins")

##Subset to photos to daytime images
photolist <- photolist[photolist$brightness > 40, ]

###################################################################################################
##Create moving hydrographs from logger data

logger.name <- "2015_TUO_solinst_12_17.csv"
interval = 15

level <- read.csv(paste(getwd(), "/data/", logger.name, sep = ""), stringsAsFactors = F, skip = 11)
level$datetime <- as.POSIXct(paste(level$Date, level$Time), format = "%Y/%m/%d %H:%M:%S")
level$datetimeround <- as.POSIXlt(round(as.double(level$datetime)/(interval*60))*(interval*60),origin=(as.POSIXct(tz="GMT",'1970-01-01')))

levsub <- na.omit(level[level$datetimeround >= range(photolist$timeround)[1] & level$datetimeround <= range(photolist$timeround)[2], ])
  
for(j in 1:nrow(photolist)){
  print(paste("making graph", j, "of", nrow(photolist)))
  
  png(paste(getwd(), "/output/temp/", photolist[j,"photo"],".png", sep = ""), 
      height=4, width=6, units="in", res=500, pointsize=12, family="serif", bg = "transparent")
  
  grid.arrange(
    ggplot(levsub, aes(x = datetimeround, y = LEVEL)) + geom_line(color = "grey30", size = 1) + theme_bw() + 
      geom_ribbon(data = levsub[levsub$datetimeround <= photolist$timeround[j],], aes(ymax = LEVEL, ymin = min(levsub$LEVEL)), fill = "blue", alpha = .5) +
      geom_line(data = levsub[levsub$datetimeround <= photolist$timeround[j],], aes(x = datetimeround, y = LEVEL), size = 2) +
      geom_point(data = levsub[levsub$datetimeround == photolist$timeround[j],], aes(x = datetimeround, y = LEVEL), size = 8) + 
      labs(list(x = "\nDate", y = "Stage")) +
      theme(panel.background = element_rect(fill = "transparent",colour = NA),
            panel.grid.minor = element_blank(), 
            panel.grid.major = element_blank(),
            plot.background = element_rect(fill = "transparent",colour = NA))
  )
  
  dev.off()
}

###################################################################################################
##Overlay images using imagemagick -composite command

photoComposite <- function(bgcol = "white",
                           alpha = 0.5,
                           plotwidth = 600,
                           plotheight = 400,
                           gap = 5,
                           infobarheight = 105,
                           plotlocation = "southeast",
                           parallel = TRUE, 
                           cores = 3){
  
  path <- gsub(pattern = "/", replacement = "\\\\", x = getwd())
  
  print("Overlay images")
  
  clu <- makeCluster(cores)
  registerDoParallel(clu, cores = cores)
  
  if(parallel == TRUE){
    foreach(k = photolist$photo) %dopar%{
      
      print(paste("overlay graph on:", k))
      shell(cmd = paste("convert -size 2048x1536", " ", path, '\\photos\\', k,
                        
                        ##Plot and background rectangle placement
                        ' -draw', ' " fill ', 
                        "rgba(" , col2rgb(bgcol)[1], ",", col2rgb(bgcol)[2], ",", col2rgb(bgcol)[3], ",", alpha, ")",
                        
                        if(plotlocation == "northwest"){paste(' rectangle ', gap, ",", gap, " ", plotwidth, ",", plotheight, '"',  
                                                              " ", path, '\\output\\temp\\', k,".png",
                                                              " -gravity northwest -geometry ", plotwidth, "x", plotheight, "+", 10, "+15 -composite",sep = "")
                        },
                        
                        if(plotlocation == "northeast"){paste(' rectangle ', 2048 - plotwidth, ",", plotheight + gap, " ", 2048 - gap, ",", gap, '"', 
                                                              " ", path, '\\output\\temp\\', k,".png",
                                                              " -gravity northeast -geometry ", plotwidth, "x", plotheight, "+", 10, "+15 -composite",sep = "")
                        },
                        
                        if(plotlocation == "southwest"){paste(' rectangle ', gap, ",", 1536 - (plotheight + infobarheight), " ", 
                                                              plotwidth + gap, ",", 1536 + - (gap + infobarheight), '"', 
                                                              " ", path, '\\output\\temp\\', k,".png",
                                                              " -gravity southwest -geometry ", plotwidth, "x", plotheight, "+", gap, "+", infobarheight + gap, " -composite",sep = "")
                        },
                        
                        if(plotlocation == "southeast"){paste(' rectangle ', 2048 - plotwidth, ",", 1536 - (plotheight + infobarheight), " ",
                                                              2048 - gap, ",", 1536 + - (gap + infobarheight), '"', 
                                                              " ", path, '\\output\\temp\\', k,".png",
                                                              " -gravity southeast -geometry ", plotwidth, "x", plotheight, "+", gap, "+", infobarheight + gap, " -composite",sep = "")
                        },
                        
                        ##Output composite image  
                        " ", path, '\\output\\composite\\', k,".png", sep = ""))
    }
    stopCluster(clu)
  }
  
  if(parallel == FALSE){
    for(k in photolist$photo){
      print(paste("overlay graph on:", k))
      shell(cmd = paste("convert -size 2048x1536", " ", path, '\\photos\\', k,
                        
                        ##Plot and background rectangle placement
                        ' -draw', ' " fill ', 
                        "rgba(" , col2rgb(bgcol)[1], ",", col2rgb(bgcol)[2], ",", col2rgb(bgcol)[3], ",", alpha, ")",
                        
                        if(plotlocation == "northwest"){paste(' rectangle ', gap, ",", gap, " ", plotwidth, ",", plotheight, '"',  
                                                              " ", path, '\\output\\temp\\', k,".png",
                                                              " -gravity northwest -geometry ", plotwidth, "x", plotheight, "+", 10, "+15 -composite",sep = "")
                        },
                        
                        if(plotlocation == "northeast"){paste(' rectangle ', 2048 - plotwidth, ",", plotheight + gap, " ", 2048 - gap, ",", gap, '"', 
                                                              " ", path, '\\output\\temp\\', k,".png",
                                                              " -gravity northeast -geometry ", plotwidth, "x", plotheight, "+", 10, "+15 -composite",sep = "")
                        },
                        
                        if(plotlocation == "southwest"){paste(' rectangle ', gap, ",", 1536 - (plotheight + infobarheight), " ", 
                                                              plotwidth + gap, ",", 1536 + - (gap + infobarheight), '"', 
                                                              " ", path, '\\output\\temp\\', k,".png",
                                                              " -gravity southwest -geometry ", plotwidth, "x", plotheight, "+", gap, "+", infobarheight + gap, " -composite",sep = "")
                        },
                        
                        if(plotlocation == "southeast"){paste(' rectangle ', 2048 - plotwidth, ",", 1536 - (plotheight + infobarheight), " ",
                                                              2048 - gap, ",", 1536 + - (gap + infobarheight), '"', 
                                                              " ", path, '\\output\\temp\\', k,".png",
                                                              " -gravity southeast -geometry ", plotwidth, "x", plotheight, "+", gap, "+", infobarheight + gap, " -composite",sep = "")
                        },
                        
                        ##Output composite image  
                        " ", path, '\\output\\composite\\', k,".png", sep = ""))
    }
  }
}

##Run photoComposite function
photoComposite(parallel = TRUE)

runtime <- proc.time() - p

print(paste("finished in", format(runtime[3]/60, digits = 4), "minutes"))
