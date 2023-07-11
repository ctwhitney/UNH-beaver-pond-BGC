## CC and CCBP (and maybe PD) metabolism estimates

library(tidyverse)

rm(list = ls())

## CC discharge/temperature from HOBO loggers
## Already QA/QC'd for 2021 LTER Update
CC_Q<-do.call(bind_rows, lapply(list.files(path=("C:/Users/ctw1/OneDrive - USNH/Data/Analysis/LTER_Update_2021/Data/CC/Discharge/"),
                                           pattern="*.csv",full=TRUE),read.csv, skip=0, header=TRUE, stringsAsFactors=FALSE))
CC_Q$DateTime<-as.POSIXct(strptime(paste(CC_Q$Date,CC_Q$Time),format="%m/%d/%Y %H:%M", tz = "EST"))
CC_Q$Date<-as.Date(strptime(CC_Q$Date,"%m/%d/%Y"))
CC_Q$Year<-as.numeric(format(CC_Q$DateTime,"%Y"))
CC_Q$Date2<-lubridate::floor_date(CC_Q$Date,"month")
CC_Q$Date3<-lubridate::floor_date(CC_Q$Date,"day")
CC_Q$Discharge.m3s <- CC_Q$Discharge.Ls / 1000

CCdat1 <- CC_Q %>% select(DateTime, DO.mgL, Temp.C, Discharge.Ls, HOBO_Depth.m) %>% as.data.frame()


# ## Truncate CCdat to remove data after DO.mgL is NA
# CCdat2 <- CCdat %>% filter(as.numeric(format(DateTime, "%Y")) < 2005)
# CCdat2$Diff <- c(NA,diff(CCdat2$DateTime))
# CCdat2$Diff <- CCdat2$Diff/60/24

CCdat1 <- CCdat1 %>% filter(complete.cases(CCdat1))
# CCdat3$Diff <- c(NA,diff(CCdat3$DateTime))
# CCdat3$Diff <- CCdat3$Diff/60/24


## Later CC DO data
CCDO <- do.call(bind_rows, lapply(list.files(path=("C:/Users/ctw1/OneDrive - USNH/Data/Analysis/LTER_Update_2021/Data/CC/DO/"),
                                             pattern="CC_DO_20.",full=TRUE),read.csv, skip=0, header=TRUE, stringsAsFactors=FALSE))
CCDO$DateTime <- as.POSIXct(strptime(paste(CCDO$Date, CCDO$Time), format = "%m/%d/%Y %H:%M", tz = "EST"))

CCdat2 <- CCDO %>% select(c(DateTime, DO.mgL, Temp.C))
CCdat2 <- CCdat2 %>% left_join(., select(filter(CC_Q, Year >= 2016), DateTime, Discharge.Ls, HOBO_Depth.m), by = "DateTime")
CCdat2 <- CCdat2 %>% filter(complete.cases(CCdat2))




# CCdat2 <- CCdat %>% left_join(select(CCDO, Year, DateTime, DO.mgL), by = "DateTime") %>% mutate(DO.obs = coalesce(DO.mgL.x, DO.mgL.y)) %>% select(-c(DO.mgL.x, DO.mgL.y))
# CCdat <- CCdat %>% mutate(light = )
# CCdat <- CCdat %>% select(DateTime, DO.obs, temp.water = Temp.C, pressure.air,)




## Meteorological data for air pressure
Met<-do.call(bind_rows, lapply(list.files(path=("C:/Users/ctw1/OneDrive - USNH/Data/Analysis/LTER_Update_2021/Data/Meteorological/PIE_LTER"),
                                           pattern="*.csv",full=TRUE),read.csv, dec=".", header=TRUE, stringsAsFactors=FALSE))
Met$DateTime<-as.POSIXct(strptime(paste(Met$Date,Met$Time),format="%d-%b-%Y %H:%M", tz = "EST"))
Met <- Met %>% mutate(pressure.air = BAR * 0.1)
#Met$Date<-as.Date(strptime(Met$Date,"%d-%b-%Y"))
#Met$Time2<-lubridate::hm(Met$Time)
#Met$Year<-as.numeric(format(Met$DateTime,"%Y"))
#Met$Date2<-lubridate::floor_date(Met$Date,"month")
#Met <- Met %>% select(DateTime, pressure.air)

CCdat1 <- CCdat1 %>% left_join(., select(Met, DateTime, BAR), by = "DateTime")
CCdat2 <- CCdat2 %>% left_join(., select(Met, DateTime, BAR), by = "DateTime")

#CCdat <- merge.data.table(setDT(CCdat), setDT(Met), by = "DateTime", allow.cartesian = TRUE)
#CCdat2 <- Met %>% select(DateTime, pressure.air) %>% right_join(., CCdat, by = "DateTime")

# Create continuous DateTime and merge with CCdat
CCdatTimeStamps1 <- data.frame(DateTime = seq(CCdat1$DateTime[1], CCdat1[nrow(CCdat1),]$DateTime, by = 15*60))
CCdatTimeStamps2 <- data.frame(DateTime = seq(CCdat2$DateTime[1], CCdat2[nrow(CCdat2),]$DateTime, by = 15*60))


CCdat1 <- CCdatTimeStamps1 %>% left_join(CCdat1, by = "DateTime")
CCdat2 <- CCdatTimeStamps2 %>% left_join(CCdat2, by = "DateTime")


                            
ggplot()+
  geom_line(data = CCdat1, aes(x = DateTime, y = DO.mgL, color = "Temp.C"))+
  geom_line(data = CCdat2, aes(x = DateTime, y = DO.mgL, color = "DO.mgL"))+
  #geom_line(data = CCdat2, aes(x = DateTime, y = DO.Pct, color = "DO.Pct"))+
  #geom_line(data = CCdat2, aes(x = DateTime, y = Discharge.Ls/1000, color = "Q"))+
  #geom_line(data = CCdat2, aes(x = DateTime, y = HOBO_Depth.m, color = "Depth"))+
  #geom_line(data = CCDO, aes(x = DateTime, y = DO.mgL, color = "CCDO"))+
  theme_bw()






