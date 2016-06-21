# Pull MFY Data

# LOAD LIBRARIES ----------------------------------------------------------

library(ggplot2)
library(lubridate)
library(dplyr)
library(magrittr)
library(readr)
library(tidyr)

## get the logger file name of interest
logger.name <- "MY15min_data.csv"
level <- read_csv(paste(getwd(), "/data/", logger.name, sep = "")) %>% as.data.frame()
summary(level)
## read in csv
level <- read_csv(paste(getwd(), "/data/", logger.name, sep = "")) %>%
  replace_na(list(MYR_BLW_OHD_YC5_cfs=0, LOHMANRIDGE_YC4_cfs=0)) %>% 
  mutate(datetime=dmy_hm(Datetime),
         hour=hour(datetime),
         month=month(datetime),
         yday=yday(datetime),
         year=year(datetime),
         FLOW=LOHMANRIDGE_YC4_cfs + MYR_BLW_OHD_YC5_cfs) %>%
  select(datetime, hour:FLOW)
summary(level)

## now make hourly to match timelapse photos
dfhr<-level %>% 
  group_by(year, month, yday, hour) %>% 
  summarize(
    "flow_cfs"=mean(FLOW,na.rm=TRUE),
    "flow_cms"=flow_cfs*0.028316847) %>%
  mutate("datetime"=ymd_hms(strptime(paste0(year,"-", month,"-", yday, " ",
                                            hour,":00"),format = "%Y-%m-%j %H:%M"))) %>%
  select(datetime,year,month,yday,flow_cfs, flow_cms) %>%
  as.data.frame()

summary(dfhr)


# MERGE WITH MFY PHOTOLIST ------------------------------------------------

load("./data/mfy_2016_photolist.rda")

# merge with photolist_sub to make into one dataframe for everything
dff<-merge(dfhr, photolist_sub[ ,c(7,1:2,4:6) ], by.x="datetime", by.y="timeround", all = F)

save(dff, file = "./data/mfy_2016_gage_data.rda")

summary(dff)

# plots
dfhr %>% ggplot(aes(x=datetime, y=flow_cms)) + geom_line(color="blue")
dfhr %$% plot(x=datetime, y=flow_cms, col="blue", typ="l")


# LOOP THROUGH PLOTS ------------------------------------------------------

# for (var in c("dep_delay", "arr_delay")) {
#   flights %>%
#     ggplot(aes_string(x=var)) +
#     geom_histogram()
#   # ggsave(paste(var, "_hist.png"))  # you can save them within the loop
# }

# OLD WAY OF ROUNDING -----------------------------------------------------

# ## round to interval (set in minutes)
# level$datetime <- as.POSIXct(level$Datetime, format = "%d %b %y, %R")
# interval = 60 # set interval
# level$datetimeround <- as.POSIXct(round(as.double(level$datetime)/(interval*60))*(interval*60),origin=(as.POSIXct(tz="",'1970-01-01')))

