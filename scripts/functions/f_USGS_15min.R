### Download 15 min gage data from USGS and plot graphs
### by R. Peek 7/26/15

#  All data from USGS NWIS website, in RDB format

 # For 15 min data: 
  #  http://waterservices.usgs.gov/nwis/iv/?format=rdb&sites=11427000&startDT=2011-10-01&endDT=2012-08-23&parameterCd=00060,00065,00010 ##

 # For for multiple gages in one call "&sites=01646500,06306300"
	#  http://waterservices.usgs.gov/nwis/iv/?format=rdb&sites=11413000,11427000&startDT=2009-10-01&endDT=2015-07-26&parameterCd=00060,00065,00010

# INSTANTANEOUS -----------------------------------------------------------

get.USGS<-function(gage, river, sdate="2010-10-01", edate=Sys.Date(),
                   save15=FALSE, 
                   saveHrly=FALSE,
                   saveDaily=FALSE,
                   saveHrPlot=FALSE){
  ##  SITES
  ##  11413000 NF Yuba,  Goodyears Bar
  ##  11427000 NF American,  Clementine Dam
  ##  11285500 Tuo R at Wards Ferry
  
  ## These may not work...
  ##  11210100 SF Kaweah,   Three Rivers
  ##  11264500 Merced, 	Happy Isles Bridge
  ##  11276600 Tuolumne, Early Intake
  ##  11335000 Cosumnes,  Michigan Bar  
  ##  11401500 Feather,  Indian Creek nr Crescent Mill

  # load packages
  if(!require(lubridate)) { install.packages("lubridate"); require(lubridate)}
  if(!require(data.table)) { install.packages("data.table"); require(data.table)}
  
  # set basic variables
  gage = gage; sdate = sdate; river = river; edate=edate
  
  # read raw instantaneous data (15 min) 
  rawdat<-fread(input = paste("http://waterservices.usgs.gov/nwis/iv/?format=rdb&sites=",
                          gage,"&startDT=",sdate,"&endDT=",edate,
                          "&parameterCd=00060,00065",sep=""), sep="\t", skip = 26) 
  
  
  # test line: paste("http://waterservices.usgs.gov/nwis/iv/?format=rdb&sites=",11427000,"&startDT=","2010-10-01","&endDT=","2015-07-30","&parameterCd=00060,00065",sep="")
  
  riv.df <- as.data.frame(rawdat) # read data
  
  # adjust/format the datetime to POSIXct (good for plotting w ggplot)
  # to check time zones, use:   grep("^America", OlsonNames(),value=TRUE)
  riv.df$datetime<-ymd_hm(paste(riv.df[[3]]," ",riv.df[[4]],sep=""),tz="America/Los_Angeles")
  
  riv <-data.frame(riv.df[,c(2,9,5:8)]) # subset to columns of interest
  colnames(riv)<- c("gage","datetime","flow_cfs","cd_00060","stage_ft", "cd_00065") # rename cols
  riv[,4]<-as.factor(riv[,4])
  riv[,6]<-as.factor(riv[,6])
  print(summary(riv))
  print(head(riv))
  
  # MAKE HOURLY -------------------------------------------------------------

  # load packages
  if(!require(dplyr)) { install.packages("dplyr"); require(dplyr)}

  # Add other columns for processing
  riv$year<-year(riv$datetime)
  riv$mon<-month(riv$datetime)
  riv$yday<-yday(riv$datetime)
  riv$hour<-hour(riv$datetime)
  riv$flow_cms<-riv$flow_cfs*0.028316847
  riv$stage_m<-riv$stage_ft*0.3048
  
  # Make Hourly dataset
  riv.hr<- riv %>%
    group_by(gage,year, mon, yday, hour)%>%
    dplyr::summarize(
      "flow_cfs"=mean(flow_cfs,na.rm=TRUE),
      "flow_cms"=mean(flow_cms,na.rm=TRUE),
      "stage_ft"=mean(stage_ft,na.rm=TRUE),
      "stage_m"=mean(stage_m,na.rm=TRUE)
      )%>%
    mutate("datetime"=ymd_hms(strptime(paste0(year,"-", mon,"-", yday, " ",
                                              hour,":00"),format = "%Y-%m-%j %H:%M"))) %>%
    select(datetime,year,mon,yday,flow_cfs:stage_m) %>%
    as.data.frame()
  h(riv.hr)
  
  if(!require(ggplot2)) { install.packages("ggplot2"); require(ggplot2)}
  
  # Plot the Hourly Flow
  print(flowsHr<-ggplot(data=riv.hr,aes(datetime,flow_cfs))+geom_line(col="red")+
          xlab("") + ylab("Flow (cfs)") + theme_bw()+
          ggtitle(paste0(river, " (USGS: ",gage, "): Avg Hourly Discharge (cfs)")))

  Pause()
  
  print(flowsHr_cms<-ggplot(data=riv.hr,aes(datetime,flow_cms))+geom_line(col="maroon")+
          xlab("") + ylab("Flow (cms)") + theme_bw()+
          ggtitle(paste0(river, " (USGS: ",gage, "): Avg Hourly Discharge (cms)")))
  
  Pause()
  
  # Plot the Hourly Stage
  print(stageHr_m<-ggplot(data=riv.hr,aes(datetime,stage_m))+geom_line(col="blue4")+
          theme_bw()+ ylab("Stage (m)") + xlab("") +
          ggtitle(paste0(river, " (USGS: ",gage, "): Avg Hourly Stage (m)")))
  
  Pause()
  
  # GET DAILY DATA  -----------------------------------------------------
  
  # use the waterData package
  #   if(!require(waterData)) { install.packages("waterData"); require(waterData)}
  
  #   nfy<-importDVs("11413000", code="00060", 
  #   stat="00003", sdate="2011-10-01", edate="2015-07-26")
  
  # OR FOR MANUAL DPLYR ROUTE WITH MOVING AVERAGES FROM 15 min raw data: 

  if(!require(caTools)) { install.packages("caTools"); require(caTools)}
  riv.dy<-riv %>%
    group_by(gage, year, mon, yday)%>%
    dplyr::summarize("flow_cfs"=mean(flow_cfs,na.rm=TRUE),
                     "flow_cms"=mean(flow_cms,na.rm=TRUE),
                     "stage_ft"=mean(stage_ft,na.rm=TRUE),
                     "stage_m"=mean(stage_m,na.rm=TRUE)) %>%
    mutate("flow.7.avg"= runmean(flow_cms, k=7, endrule="mean",align="center"),
           "flow.7.min"= runmin(flow_cms, k=7, align="center"),
           "flow.7.max"= runmax(flow_cms, k=7, align="center"),
           "stage.7.avg"= runmean(stage_m, k=7, endrule="mean",align="center"),
           "stage.7.min"= runmin(stage_m, k=7, align="center"),
           "stage.7.max"= runmax(stage_m, k=7, align="center")) %>%
    mutate("datetime"=ymd(strptime(paste0(year,"-", mon,"-", yday),
                                   format = "%Y-%m-%j"))) %>%
    select(datetime,year,mon,yday,flow_cfs:stage.7.max) %>%  
    as.data.frame()
  
  # Plot the Daily Temperature  
  print(flow7cms<-ggplot()+
          geom_ribbon(data=riv.dy, aes(datetime,ymax=flow.7.max,ymin=flow.7.min),
                      fill="gray70",colour="gray60",lty=2,size=0.7,alpha=0.4)+
          geom_line(data=riv.dy,aes(datetime,flow.7.avg), col="maroon",size=1)+
          theme_bw() + xlab("") + ylab("Flow (cms)") +
          ggtitle(paste0(river, " (USGS: ",gage, "): 7 Day Avg Flow (cms)")))
  
  # Save data ---------------------------------------------------------------   
  
  if(!require(readr)) { install.packages("readr"); require(readr)}

  if(save15){
    write_csv(riv, path=paste0(river,"_",sdate,"_15min_USGS.csv"))
    print(paste("15 min data saved here: ",getwd(),sep=""))
  } else {
    cat("USGS 15-min data not saved.\n")
  }
  
  if(saveHrly){
    write_csv(riv.hr, path=paste0(river,"_",sdate,"_hourly_USGS.csv"))
    print(paste("Hourly data saved here: ",getwd(),sep=""))
  } else {
    cat("USGS hourly data not saved.\n")
  }
  
  if(saveDaily){
    write_csv(riv.dy, path=paste0(river,"_",sdate,"_daily_USGS.csv"))
    print(paste("Daily data saved here: ",getwd(),sep=""))
  } else {
    cat("USGS daily data not saved.\n")
  }

  if(saveHrPlot){
    png(filename=paste0(river,"_",sdate,"_hourly_Qcms_USGS.png"),width=11,height=8.5,units="in",res=72)
    print(flowsHr_cms)
    dev.off()
    print(paste("Hourly flow cms plot saved here: ",getwd(),sep=""))
  } else {
    cat("No plot saved")
  }
  
  # save data to workspace/environment
  
  river1<-paste0(river,"_15")
  assign(river1,riv, envir=.GlobalEnv) # print to workspace
  
  river2<-paste0(river,"_hr")
  assign(river2,riv.hr, envir=.GlobalEnv) # print to workspace
  
  river3<-paste0(river,"_dy")
  assign(river3,riv.dy, envir=.GlobalEnv) # print to workspace
  
  cat("All finished... data in current environment \n")
}  
