## make plot with air

fig.camgraph <- function(test.subset = nrow(photolist_sub), air=T){
  
  ## SET COLORS AND BREAKS FOR AIR TEMPERATURES
  breaks<-(seq(0,36,4)) # best for air
  # breaks<-(seq(0,27,3)) # best for water
  palette<-c("black","midnightblue","blue","deepskyblue2", "green4", 
             "green","yellow","orange","red","darkviolet") # orange orangered brown4
  
  if(air){

    print("Making AIR graphs with Moultrie Camera info (C)...")
    for(j in 1:nrow(photolist_sub[c(1:test.subset),])){
      
      print(paste("making graph", j, "of", nrow(photolist_sub)))

      png(paste(getwd(), "/output/temp/air/", photolist_sub[j,"photo"], sep = ""), 
          height=4, width=6, units="in", res=500, family="sans", bg = "transparent")
      
      grid.arrange(
        ggplot(photolist_sub, aes(x = datetime, y = air_C)) + geom_line(color = "grey30", size = 1) + theme_bw() + 
          geom_ribbon(data = photolist_sub[photolist_sub$datetime <= photolist_sub$timeround[j],], 
                      aes(ymax = air_C, ymin = min(photolist_sub$air_C)), fill = "blue", alpha = .5) +
          scale_colour_gradientn(name=expression(paste("AirTemp(",degree,"C)")),colours=palette(palette), values=breaks, 
                                 rescaler = function(x, ...) x, oob = identity,limits=range(breaks), breaks=breaks, space="Lab") +
          geom_line(data = photolist_sub[photolist_sub$datetime <= photolist_sub$datetime[j],], 
                    aes(x = datetime, y = air_C, color=air_C), size = 1) +
          geom_point(data = photolist_sub[photolist_sub$datetime == photolist_sub$datetime[j],], 
                     aes(x = datetime, y = air_C),pch=21, fill="gray10", col="white",size = 8) +
          labs(list(x = "", y = "Air Temperature (C)")) +
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
    
    print("Making Barograph (inHg)")
    for(j in 1:nrow(photolist_sub[c(1:test.subset),])){
      
      print(paste("making graph", j, "of", nrow(photolist_sub)))
      
      png(paste(getwd(), "/output/temp/air/", photolist_sub[j,"photo"], sep = ""), 
          height=4, width=6, units="in", res=500, family="sans", bg = "transparent")
      
      grid.arrange(
        ggplot(photolist_sub, aes(x = datetime, y = baro_inHg)) + geom_line(color = "grey30", size = 1) + theme_bw() + 
          geom_ribbon(data = photolist_sub[photolist_sub$datetime <= photolist_sub$timeround[j],], 
                      aes(ymax = baro_inHg, ymin = min(photolist_sub$flow_cfs)), fill = "blue", alpha = .5) +
          geom_line(data = photolist_sub[photolist_sub$datetime <= photolist_sub$timeround[j],], 
                    aes(x = datetime, y = baro_inHg), size = 2) +
          geom_point(data = photolist_sub[photolist_sub$datetime == photolist_sub$timeround[j],], 
                     aes(x = datetime, y = baro_inHg), pch=21, fill="skyblue1", size = 8) +
          labs(list(x = "", y = "Barometric Pressure (in Hg)")) +
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
