## make hydrographs

fig.Hydro <- function(test.subset = nrow(photolist_sub)){
  
  print("Making Hydrographs only")
  for(j in 1:nrow(photolist_sub[c(1:test.subset),])){
    
    print(paste("making graph", j, "of", nrow(photolist_sub)))
    #print(paste("making graph", j, "of", test.subset))
    
    png(paste(getwd(), "/output/temp/hydro/", photolist_sub[j,"photo"], sep = ""), 
        height=4, width=6, units="in", res=500, family="sans", bg = "transparent")
    
    grid.arrange(
      ggplot(dff, aes(x = datetime, y = lev.avg)) + geom_line(color = "grey30", size = 1) + theme_bw() + 
        geom_ribbon(data = dff[dff$datetime <= photolist_sub$datetimeround[j],], 
                    aes(ymax = lev.avg, ymin = min(dff$lev.avg)), fill = "blue", alpha = .5) +
        geom_line(data = dff[dff$datetime <= photolist_sub$datetimeround[j],], 
                  aes(x = datetime, y = lev.avg), size = 2) +
        geom_point(data = dff[dff$datetime == photolist_sub$datetimeround[j],], 
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
