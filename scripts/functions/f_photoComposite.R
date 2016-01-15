## The Composite Photo Function
photoComposite <- function(bgcol = "white",
                           alpha = 0.5,
                           plotwidth = 600,
                           plotheight = 400,
                           gap = 5,
                           infobarheight = 105,
                           plotlocation = "southeast",
                           thermo=TRUE,
                           site="",
                           parallel = TRUE, 
                           cores = 2){
  
  path <- gsub(pattern = "/", replacement = "\\\\", x = getwd())

  print("Overlay images")
  
  # PC
  clu <- makeCluster(cores)
  registerDoParallel(clu, cores = cores)
  
  if(parallel == TRUE){
    print("Using Parallelization")
    foreach(k = photolist_sub$photo) %dopar%{
      if(thermo){
        figfold<-"thermo"
      } else {
        figfold<-"hydro"
      }
      
      shell(cmd = paste("convert -size 2048x1536", " ", path, '\\photos\\',site,'\\', k, # copied photos to proj dir
                        
                        ## Plot and background rectangle placement
                        ' -draw', ' " fill ', 
                        "rgba(" , col2rgb(bgcol)[1], ",", col2rgb(bgcol)[2], ",", col2rgb(bgcol)[3], ",", alpha, ")",
                        
                        if(plotlocation == "northwest"){paste(' rectangle ', gap, ",", gap, " ", plotwidth, ",", plotheight, '"',  
                                                              " ", path, '\\output\\temp\\', figfold,'\\', k,
                                                              " -gravity northwest -geometry ", plotwidth, "x", plotheight, "+", 10, "+15 -composite",sep = "")
                        },
                        
                        if(plotlocation == "northeast"){paste(' rectangle ', 2048 - plotwidth, ",", plotheight + gap, " ", 2048 - gap, ",", gap, '"', 
                                                              " ", path, '\\output\\temp\\', figfold,'\\', k,
                                                              " -gravity northeast -geometry ", plotwidth, "x", plotheight, "+", 10, "+15 -composite",sep = "")
                        },
                        
                        if(plotlocation == "southwest"){paste(' rectangle ', gap, ",", 1536 - (plotheight + infobarheight), " ", 
                                                              plotwidth + gap, ",", 1536 + - (gap + infobarheight), '"', 
                                                              " ", path, '\\output\\temp\\', figfold,'\\', k,
                                                              " -gravity southwest -geometry ", plotwidth, "x", plotheight, "+", gap, "+", infobarheight + gap, " -composite",sep = "")
                        },
                        
                        if(plotlocation == "southeast"){paste(' rectangle ', 2048 - plotwidth, ",", 1536 - (plotheight + infobarheight), " ",
                                                              2048 - gap, ",", 1536 + - (gap + infobarheight), '"', 
                                                              " ", path, '\\output\\temp\\', figfold,'\\', k,
                                                              " -gravity southeast -geometry ", plotwidth, "x", plotheight, "+", gap, "+", infobarheight + gap, " -composite",sep = "")
                        },
                        
                        ## Output composite image  
                        " ", path, '\\output\\composite\\', k, sep = ""))
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
      
      shell(cmd = paste("convert -size 2048x1536", " ", path, '\\photos\\',site,'\\', k,
                        ##Plot and background rectangle placement
                        ' -draw', ' " fill ', 
                        "rgba(" , col2rgb(bgcol)[1], ",", col2rgb(bgcol)[2], ",", col2rgb(bgcol)[3], ",", alpha, ")",
                        
                        if(plotlocation == "northwest"){paste(' rectangle ', gap, ",", gap, " ", plotwidth, ",", plotheight, '"',  
                                                              " ", path, '\\output\\temp\\',figfold,'\\', k,
                                                              " -gravity northwest -geometry ", plotwidth, "x", plotheight, "+", 10, "+15 -composite",sep = "")
                        },
                        
                        if(plotlocation == "northeast"){paste(' rectangle ', 2048 - plotwidth, ",", plotheight + gap, " ", 2048 - gap, ",", gap, '"', 
                                                              " ", path, '\\output\\temp\\', figfold,'\\', k,
                                                              " -gravity northeast -geometry ", plotwidth, "x", plotheight, "+", 10, "+15 -composite",sep = "")
                        },
                        
                        if(plotlocation == "southwest"){paste(' rectangle ', gap, ",", 1536 - (plotheight + infobarheight), " ", 
                                                              plotwidth + gap, ",", 1536 + - (gap + infobarheight), '"', 
                                                              " ", path, '\\output\\temp\\', figfold,'\\', k,
                                                              " -gravity southwest -geometry ", plotwidth, "x", plotheight, "+", gap, "+", infobarheight + gap, " -composite",sep = "")
                        },
                        
                        if(plotlocation == "southeast"){paste(' rectangle ', 2048 - plotwidth, ",", 1536 - (plotheight + infobarheight), " ",
                                                              2048 - gap, ",", 1536 + - (gap + infobarheight), '"', 
                                                              " ", path, '\\output\\temp\\', figfold,'\\', k,
                                                              " -gravity southeast -geometry ", plotwidth, "x", plotheight, "+", gap, "+", infobarheight + gap, " -composite",sep = "")
                        },
                        
                        ##Output composite image  
                        " ", path, '\\output\\composite\\', k, sep = ""))
    }
  }
}
