## The Composite Photo Function

photoComposite_osx <- function(bgcol = "white",
                           alpha = 0.5,
                           plotwidth = 800,
                           plotheight = 600,
                           gap = 5,
                           infobarheight = 100,
                           plotlocation = "southeast",
                           site="",
                           thermo=TRUE,
                           parallel = TRUE, 
                           cores = 2){
  
  path <- getwd()
  
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
      system(command = paste("convert -size 2848x2136", " ", path, '//photos//',site,'//', k, #
      #system(command = paste("convert -size 2048x1536", " ", path, '//photos//',site,'//', k, # copied photos to proj dir
                             ## Plot and background rectangle placement
                             ' -draw', ' " fill ', 
                             "rgba(" , col2rgb(bgcol)[1], ",", col2rgb(bgcol)[2], ",", col2rgb(bgcol)[3], ",", alpha, ")",
                             
                             if(plotlocation == "northwest"){paste(' rectangle ', gap, ",", gap, " ", plotwidth, ",", plotheight, '"',  
                                                                   " ", path, '//output//temp//', figfold,'//', k,
                                                                   " -gravity northwest -geometry ", plotwidth, "x", plotheight, "+", 10, "+15 -composite",sep = "")
                             },
                             
                             if(plotlocation == "northeast"){paste(' rectangle ', 2048 - plotwidth, ",", plotheight + gap, " ", 2048 - gap, ",", gap, '"', 
                                                                   " ", path, '//output//temp//', figfold,'//', k,
                                                                   " -gravity northeast -geometry ", plotwidth, "x", plotheight, "+", 10, "+15 -composite",sep = "")
                             },
                             
                             if(plotlocation == "southwest"){paste(' rectangle ', gap, ",", 1536 - (plotheight + infobarheight), " ", 
                                                                   plotwidth + gap, ",", 1536 + - (gap + infobarheight), '"', 
                                                                   " ", path, '//output//temp//', figfold,'//', k,
                                                                   " -gravity southwest -geometry ", plotwidth, "x", plotheight, "+", gap, "+", infobarheight + gap, " -composite",sep = "")
                             },
                             
                             if(plotlocation == "southeast"){paste(' rectangle ', 2848 - infobarheight, ",", 1946, " ",
                                                                   1948, ",", 1436, '"', 
                                                                   " ", path, '//output//temp//', figfold,'//', k,
                                                                   " -gravity southeast -geometry ", plotwidth, "x", plotheight, "+", infobarheight, "+", infobarheight, " -composite",sep = "")
                             },
                             
                             ## Output composite image  
                             " ", path, '//output//composite//', k, sep = ""))
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
      
      system(command = paste("convert -size 2048x1536", " ", path, '//photos//',site,'//', k,                  
                             ## Plot and background rectangle placement
                             ' -draw', ' " fill ', 
                             "rgba(" , col2rgb(bgcol)[1], ",", col2rgb(bgcol)[2], ",", col2rgb(bgcol)[3], ",", alpha, ")",
                             
                             if(plotlocation == "northwest"){paste(' rectangle ', gap, ",", gap, " ", plotwidth, ",", plotheight, '"',  
                                                                   " ", path, '//output//temp//',figfold,'//', k,
                                                                   " -gravity northwest -geometry ", plotwidth, "x", plotheight, "+", 10, "+15 -composite",sep = "")
                             },
                             
                             if(plotlocation == "northeast"){paste(' rectangle ', 2048 - plotwidth, ",", plotheight + gap, " ", 2048 - gap, ",", gap, '"', 
                                                                   " ", path, '//output//temp//', figfold,'//', k,
                                                                   " -gravity northeast -geometry ", plotwidth, "x", plotheight, "+", 10, "+15 -composite",sep = "")
                             },
                             
                             if(plotlocation == "southwest"){paste(' rectangle ', gap, ",", 1536 - (plotheight + infobarheight), " ", 
                                                                   plotwidth + gap, ",", 1536 + - (gap + infobarheight), '"', 
                                                                   " ", path, '//output//temp//', figfold,'//', k,
                                                                   " -gravity southwest -geometry ", plotwidth, "x", plotheight, "+", gap, "+", infobarheight + gap, " -composite",sep = "")
                             },
                             
                             if(plotlocation == "southeast"){paste(' rectangle ', 2048 - plotwidth, ",", 1536 - infobarheight, " ",
                                                                   2048 - (plotwidth + infobarheight), ",", 1536 - (plotheight + infobarheight), '"', 
                                                                   " ", path, '//output//temp//', figfold,'//', k,
                                                                   " -gravity southeast -geometry ", plotwidth, "x", plotheight, "+", gap, "+", infobarheight + gap, " -composite",sep = "")
                             },
                             
                             ## Output composite image  
                             " ", path, '//output//composite//', k, sep = ""))
    }
  }
}
