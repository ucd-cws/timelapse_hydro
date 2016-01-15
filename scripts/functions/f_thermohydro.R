## make Thermohydrographs

fig.Thermohydro <- function(test.subset = nrow(photolist_sub)){
  
  ## SET COLORS AND BREAKS FOR WATER TEMPERATURES
  breaks<-(c(0,3,6,9,12,15,18,21,24,27)) 
  palette<-c("black","midnightblue","blue","deepskyblue2",
             "green4","green","yellow","orange","red","darkviolet") # orange orangered brown4
  
  print("Making thermohydrographs")
  
  for(j in 1:nrow(photolist_sub[c(1:test.subset),])){
    
    print(paste("making graph", j, "of", nrow(photolist_sub)))
    
    png(paste(getwd(), "/output/temp/thermo/",photolist_sub[j,"photo"],sep = ""), 
        height=4, width=6, units="in", res=500, family="sans", bg = "transparent")
    
    grid.arrange(
      ggplot() + 
        geom_line(data = dff, aes(x = datetime, y = lev.avg), color = "black", size = 1, alpha=0.8) +
        scale_colour_gradientn(name=expression(paste("WaterTemp(",degree,"C)")),colours=palette(palette), values=breaks, 
                               rescaler = function(x, ...) x, oob = identity,limits=range(breaks), breaks=breaks, space="Lab") +
        geom_ribbon(data = dff[dff$datetime <= dff$datetime[j],], 
                    aes(x = datetime, ymax = lev.avg, ymin = min(dff$lev.avg)), fill = "gray30", alpha = 0.5) +
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
