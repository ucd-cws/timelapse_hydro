## make USGS hydrographs with flow and air

fig.Hydroair.usgs <- function(test.subset = nrow(photolist_sub), air=T){
  
  ## SET COLORS AND BREAKS FOR AIR TEMPERATURES
  breaks<-(seq(0,36,4)) # best for air
  # breaks<-(seq(0,27,3)) # best for water
  palette<-c("black","midnightblue","blue","deepskyblue2", "green4", 
             "green","yellow","orange","red","darkviolet") # orange orangered brown4
  
  if(air){

    print("Making AIR Thermohydrographs with USGS flow (cms)...")
    for(j in 1:nrow(photolist_sub[c(1:test.subset),])){
      
      print(paste("making graph", j, "of", nrow(photolist_sub)))

      png(paste(getwd(), "/output/temp/thermo/", photolist_sub[j,"photo"], sep = ""), 
          height=4, width=6, units="in", res=500, family="sans", bg = "transparent")
      
      grid.arrange(
        ggplot(dff, aes(x = datetime, y = flow_cms)) + geom_line(color = "grey30", size = 1) + theme_bw() + 
          geom_ribbon(data = dff[dff$datetime <= photolist_sub$timeround[j],], 
                      aes(ymax = flow_cms, ymin = min(dff$flow_cms)), fill = "blue", alpha = .5) +
          scale_colour_gradientn(name=expression(paste("AirTemp(",degree,"C)")),colours=palette(palette), values=breaks, 
                                 rescaler = function(x, ...) x, oob = identity,limits=range(breaks), breaks=breaks, space="Lab") +
          geom_point(data = dff[dff$datetime <= photolist_sub$timeround[j],], 
                    aes(x = datetime, y = flow_cms, color=air_C), size = 1.3) +
          geom_point(data = dff[dff$datetime == photolist_sub$timeround[j],], 
                     aes(x = datetime, y = flow_cms), pch=21, fill="skyblue1", size = 8) +
          #ylim(c(0,5000))+
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
  } else {
    
    print("Making Flow Hydrographs (cfs)")
    for(j in 1:nrow(photolist_sub[c(1:test.subset),])){
      
      print(paste("making graph", j, "of", nrow(photolist_sub)))
      
      png(paste(getwd(), "/output/temp/hydro/", photolist_sub[j,"photo"], sep = ""), 
          height=4, width=6, units="in", res=500, family="sans", bg = "transparent")
      
      grid.arrange(
        ggplot(dff, aes(x = datetime, y = flow_cfs)) + geom_line(color = "grey30", size = 1) + theme_bw() + 
          geom_ribbon(data = dff[dff$datetime <= photolist_sub$timeround[j],], 
                      aes(ymax = flow_cfs, ymin = min(dff$flow_cfs)), fill = "blue", alpha = .5) +
          geom_line(data = dff[dff$datetime <= photolist_sub$timeround[j],], 
                    aes(x = datetime, y = flow_cfs), size = 2) +
          geom_point(data = dff[dff$datetime == photolist_sub$timeround[j],], 
                     aes(x = datetime, y = flow_cfs), pch=21, fill="skyblue1", size = 8) +
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
  }
}
