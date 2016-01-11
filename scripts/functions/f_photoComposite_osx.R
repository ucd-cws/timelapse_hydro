## The Composite Photo Function

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
  
  #path <- gsub(pattern = "/", replacement = "\\\\", x = getwd())
  path <- getwd()
  
  if(thermo){
    photonames<-gsub(pattern=".JPG",replacement = "_thermo", x = photolist_sub[,"photo"],ignore.case = TRUE)
  } else {
    photonames<-gsub(pattern=".JPG",replacement = "_hydro", x = photolist_sub[,"photo"],ignore.case = TRUE)
  }
  print("Overlay images")
  
  # MAC
  require(doMC)
  registerDoMC(cores)
  
  if(parallel == TRUE){
    print("Using Parallelization")
    foreach(k = photolist_sub$photo) %dopar%{
      if(thermo){
        figfold<-"thermo"
      } else {
        figfold<-"hydro"
      }
      
      system(command = paste("convert -size 2048x1536", " ", path, '//photos//', k, # copied photos to proj dir
                             ## Plot and background rectangle placement
                             ' -draw', ' " fill ', 
                             "rgba(" , col2rgb(bgcol)[1], ",", col2rgb(bgcol)[2], ",", col2rgb(bgcol)[3], ",", alpha, ")",
                             
                             if(plotlocation == "northwest"){paste(' rectangle ', gap, ",", gap, " ", plotwidth, ",", plotheight, '"',  
                                                                   " ", path, '//output//temp//', figfold,'//', k,".png",
                                                                   " -gravity northwest -geometry ", plotwidth, "x", plotheight, "+", 10, "+15 -composite",sep = "")
                             },
                             
                             if(plotlocation == "northeast"){paste(' rectangle ', 2048 - plotwidth, ",", plotheight + gap, " ", 2048 - gap, ",", gap, '"', 
                                                                   " ", path, '//output//temp//', figfold,'//', k,".png",
                                                                   " -gravity northeast -geometry ", plotwidth, "x", plotheight, "+", 10, "+15 -composite",sep = "")
                             },
                             
                             if(plotlocation == "southwest"){paste(' rectangle ', gap, ",", 1536 - (plotheight + infobarheight), " ", 
                                                                   plotwidth + gap, ",", 1536 + - (gap + infobarheight), '"', 
                                                                   " ", path, '//output//temp//', figfold,'//', k,".png",
                                                                   " -gravity southwest -geometry ", plotwidth, "x", plotheight, "+", gap, "+", infobarheight + gap, " -composite",sep = "")
                             },
                             
                             if(plotlocation == "southeast"){paste(' rectangle ', 2048 - plotwidth, ",", 1536 - (plotheight + infobarheight), " ",
                                                                   2048 - gap, ",", 1536 + - (gap + infobarheight), '"', 
                                                                   " ", path, '//output//temp//', figfold,'//', k,".png",
                                                                   " -gravity southeast -geometry ", plotwidth, "x", plotheight, "+", gap, "+", infobarheight + gap, " -composite",sep = "")
                             },
                             
                             ## Output composite image  
                             " ", path, '//output//composite//', k,".png", sep = ""))
    }
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
      
      system(command = paste("convert -size 2048x1536", " ", path, '//photos//', k,                  
                             ## Plot and background rectangle placement
                             ' -draw', ' " fill ', 
                             "rgba(" , col2rgb(bgcol)[1], ",", col2rgb(bgcol)[2], ",", col2rgb(bgcol)[3], ",", alpha, ")",
                             
                             if(plotlocation == "northwest"){paste(' rectangle ', gap, ",", gap, " ", plotwidth, ",", plotheight, '"',  
                                                                   " ", path, '//output//temp//',figfold,'//', k,".png",
                                                                   " -gravity northwest -geometry ", plotwidth, "x", plotheight, "+", 10, "+15 -composite",sep = "")
                             },
                             
                             if(plotlocation == "northeast"){paste(' rectangle ', 2048 - plotwidth, ",", plotheight + gap, " ", 2048 - gap, ",", gap, '"', 
                                                                   " ", path, '//output//temp//', figfold,'//', k,".png",
                                                                   " -gravity northeast -geometry ", plotwidth, "x", plotheight, "+", 10, "+15 -composite",sep = "")
                             },
                             
                             if(plotlocation == "southwest"){paste(' rectangle ', gap, ",", 1536 - (plotheight + infobarheight), " ", 
                                                                   plotwidth + gap, ",", 1536 + - (gap + infobarheight), '"', 
                                                                   " ", path, '//output//temp//', figfold,'//', k,".png",
                                                                   " -gravity southwest -geometry ", plotwidth, "x", plotheight, "+", gap, "+", infobarheight + gap, " -composite",sep = "")
                             },
                             
                             if(plotlocation == "southeast"){paste(' rectangle ', 2048 - plotwidth, ",", 1536 - (plotheight + infobarheight), " ",
                                                                   2048 - gap, ",", 1536 + - (gap + infobarheight), '"', 
                                                                   " ", path, '//output//temp//', figfold,'//', k, ".png",
                                                                   " -gravity southeast -geometry ", plotwidth, "x", plotheight, "+", gap, "+", infobarheight + gap, " -composite",sep = "")
                             },
                             
                             ## Output composite image  
                             " ", path, '//output//composite//', k,".png", sep = ""))
    }
  }
}