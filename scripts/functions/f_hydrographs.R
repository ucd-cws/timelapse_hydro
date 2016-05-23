## make hydrographs

fig.Hydro <- function(test.subset = nrow(photolist_sub), stage=TRUE, cms=FALSE, yvar="lev.avg"){
  
  if(stage){
    print("Making Stage Hydrographs (m)")
    for(j in 1:nrow(photolist_sub[c(1:test.subset),])){
      
      print(paste("making graph", j, "of", nrow(photolist_sub)))
      
      png(paste(getwd(), "/output/temp/hydro/", photolist_sub[j,"photo"], sep = ""), 
          height=4, width=6, units="in", res=500, family="sans", bg = "transparent")
      
      grid.arrange(
        ggplot(dff, aes_string(x = "datetime", y = yvar)) + geom_line(color = "grey30", size = 1) + theme_bw() + 
          geom_ribbon(data = dff[dff$datetime <= photolist_sub$timeround[j],], 
                      aes_string(ymax = yvar, ymin = min(dff[[yvar]])), fill = "blue", alpha = .5) +
          geom_line(data = dff[dff$datetime <= photolist_sub$timeround[j],], 
                    aes_string(x = "datetime", y = yvar), size = 2) +
          geom_point(data = dff[dff$datetime == photolist_sub$timeround[j],], 
                     aes_string(x = "datetime", y = yvar), pch=21, fill="skyblue1", size = 8) +
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
  } else if(cms==FALSE & stage==FALSE) {
    print("Making Flow Hydrographs (cfs)")
    for(j in 1:nrow(photolist_sub[c(1:test.subset),])){
      
      print(paste("making graph", j, "of", nrow(photolist_sub)))
      
      png(paste(getwd(), "/output/temp/hydro/", photolist_sub[j,"photo"], sep = ""), 
          height=4, width=6, units="in", res=500, family="sans", bg = "transparent")
      
      grid.arrange(
        ggplot(dff, aes_string(x = "datetime", y = yvar)) + geom_line(color = "grey30", size = 1) + theme_bw() + 
          geom_ribbon(data = dff[dff$datetime <= photolist_sub$timeround[j],], 
                      aes_string(ymax = yvar, ymin = min(dff[[yvar]])), fill = "blue", alpha = .5) +
          geom_line(data = dff[dff$datetime <= photolist_sub$timeround[j],], 
                    aes_string(x = "datetime", y = yvar), size = 2) +
          geom_point(data = dff[dff$datetime == photolist_sub$timeround[j],], 
                     aes_string(x = "datetime", y = yvar), pch=21, fill="skyblue1", size = 8) +
          labs(list(x = "", y = "Discharge (cfs)")) +
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
  } else if(cms==TRUE & stage==FALSE) {
    print("Making Flow Hydrographs (cms)")
    for(j in 1:nrow(photolist_sub[c(1:test.subset),])){
      
      print(paste("making graph", j, "of", nrow(photolist_sub)))
      
      png(paste(getwd(), "/output/temp/hydro/", photolist_sub[j,"photo"], sep = ""), 
          height=4, width=6, units="in", res=500, family="sans", bg = "transparent")
      
      grid.arrange(
        ggplot(dff, aes_string(x = "datetime", y = yvar)) + geom_line(color = "grey30", size = 1) + theme_bw() + 
          geom_ribbon(data = dff[dff$datetime <= photolist_sub$timeround[j],], 
                      aes_string(ymax = yvar, ymin = min(dff[[yvar]])), fill = "blue", alpha = .5) +
          geom_line(data = dff[dff$datetime <= photolist_sub$timeround[j],], 
                    aes_string(x = "datetime", y = yvar), size = 2) +
          geom_point(data = dff[dff$datetime == photolist_sub$timeround[j],], 
                     aes_string(x = "datetime", y = yvar), pch=21, fill="skyblue1", size = 8) +
          labs(list(x = "", y = "Discharge (cms)")) +
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
}