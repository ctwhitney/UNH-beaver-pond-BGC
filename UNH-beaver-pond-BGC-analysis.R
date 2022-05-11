## UNH-beaver-pond-BGC-analysis script

##rgdal for working with jp2,sid, etc. files
##raster can handle tiff
library(tmap)
library(,,someething els emap related here>>)
library(ggmap)
library(tidyverse)


################################################################################
## Import data sets
################################################################################


#####Read in Grab sample data#####
#GRABS<-read.csv(file="C:/Users/ctw1/Box/Data/Analysis/Dams/Data/Reservoir_Grab_2022-05-09.csv",header=TRUE,stringsAsFactors=FALSE)
GRABS<-read.csv(file="Data/Reservoir_Grab_2022-05-09.csv",header=TRUE,stringsAsFactors=FALSE)
GRABS$DateTime<-as.POSIXct(strptime(paste(GRABS$Date,GRABS$Time),format="%m/%d/%y %H:%M",tz="America/New_York"))
attributes(GRABS$DateTime)$tzone<-"EST"
GRABS$Date<-as.POSIXct(strptime(GRABS$Date,format="%m/%d/%Y"))
GRABS$Year<-as.numeric(format(GRABS$Date,"%Y"))
GRABS$DateTime15<-as.POSIXct(round(as.numeric(GRABS$DateTime)/900)*900,origin='1970-01-01',tz="EST")

## Remove Location = NA and Storm data from GRABS and IP04
GRABS<-GRABS[!is.na(GRABS$Location),]
GRABS<-GRABS[GRABS$Site=="CCBP" | GRABS$Site=="DBBP",]

# Calculate N2:Ar disequilibrium
GRABS$NArDisEq<-GRABS$NArcalc.mean-GRABS$NAr_sat

# Calculate DOC/DON ratio
GRABS$DOCtoDON<-GRABS$DOC.mgL/GRABS$DON.mgL
# Calculate DOC/TDN ratio
GRABS$DOCtoTDN<-GRABS$DOC.mgL/GRABS$DON.mgL


## Import CCBP internal sampling data


#Internal <- read.csv(file="C:/Users/ctw1/Box/Data/Analysis/CCBP_CH4/Data/CCBP_CH4_Data_2022-05-09.csv", header = TRUE, stringsAsFactors = FALSE)
Internal <- read.csv(file="Data/CCBP_CH4_Data_2022-05-09.csv", header = TRUE, stringsAsFactors = FALSE)
Internal$DateTime <- as.POSIXct(strptime(paste(Internal$Date, Internal$Time), format="%m/%d/%Y %H:%M", tz = "America/New_York"))
attributes(Internal$DateTime)$tzone <- "EST"
Internal$Date <- as.Date(Internal$Date, format = "%m/%d/%Y")
Internal$Year <- as.numeric(format(Internal$Date, "%Y"))
Internal$DateTime15 <- as.POSIXct(round(as.numeric(Internal$DateTime)/900)*900, origin = '1970-01-01', tz = "EST")


## Import CCBP synoptic data

Synoptic <- read.csv(file="Data/Input_Data_2022-05-06.csv", header = TRUE, stringsAsFactors = FALSE)
Synoptic$DateTime <- as.POSIXct(strptime(paste(Synoptic$Date, Synoptic$Time), format="%m/%d/%Y %H:%M", tz = "America/New_York"))
attributes(Synoptic$DateTime)$tzone <- "EST"
Synoptic$Date <- as.Date(Synoptic$Date, format  ="%m/%d/%Y")
Synoptic$Year <- as.numeric(format(Synoptic$Date, "%Y"))
Synoptic$DateTime15 <- as.POSIXct(round(as.numeric(Synoptic$DateTime)/900)*900, origin = '1970-01-01', tz = "EST")


##Import discharge data
## Can read in csv of data that has already been processed
BPQ <- read.csv("Data/BP_Q_2022-05-09.csv", header = TRUE, stringsAsFactors = FALSE)
BPQ$Date <- as.Date(BPQ$Date, format = "%Y-%m-%d")



# This was done already and the resulting dataframe was stored in the 'Data' directory#####
# ## Import CC discharge data
# CCQ <- read.csv(file="C:/Users/ctw1/Box/Data/Analysis/CCBP_CH4/Data/CC_Q_Daily_2022-05-09.csv", header=TRUE, stringsAsFactors = FALSE)
# CCQ$Date <- as.Date(CCQ$Date,format="%Y-%m-%d")
# 
# ## Scale to individual sampling locations
# CCBPQ <- data.frame(Date = CCQ$Date, Discharge.m3s = (0.078275/4.63) * (CCQ$Discharge.Ls/1000), Site  ="CCBP", Sample = "CCBP_UP_3")
# CCBPQ <- rbind(CCBPQ, data.frame(Date = CCQ$Date, Discharge.m3s = (0.554225/4.63) * (CCQ$Discharge.Ls/1000), Site  ="CCBP", Sample = "CCBP_UP_2"))
# CCBPQ <- rbind(CCBPQ, data.frame(Date = CCQ$Date, Discharge.m3s = (0.91245/4.63) * (CCQ$Discharge.Ls/1000), Site  ="CCBP", Sample = "CCBP_UP_1"))
# CCBPQ <- rbind(CCBPQ, data.frame(Date = CCQ$Date, Discharge.m3s = (1.72665/4.63) * (CCQ$Discharge.Ls/1000), Site  ="CCBP", Sample = "CCBP_OUT"))
# CCBPQ <- rbind(CCBPQ, data.frame(Date = CCQ$Date, Discharge.m3s = (3.962682/4.63) * (CCQ$Discharge.Ls/1000), Site  ="CCBP", Sample = "CCBP_Side"))
# CCBPQ <- rbind(CCBPQ, data.frame(Date = CCQ$Date, Discharge.m3s = (CCQ$Discharge.Ls/1000), Site  ="CCBP", Sample = "CC"))
# 
# 
# 
# ## Import Oyster discharge data
# OYSQ <- read.csv(file="C:/Users/ctw1/Box/Data/Analysis/CCBP_CH4/Data/Oyster_Q_Daily_2022-05-09.csv", header=TRUE, stringsAsFactors = FALSE)
# OYSQ$Date <- as.Date(OYSQ$Date,format="%Y-%m-%d")
# 
# ## Scale to individual sampling locations
# DBBPQ <- data.frame(Date = OYSQ$Date, Discharge.m3s = (2.58999/31.33886) * OYSQ$Discharge.m3s, Site  ="DBBP", Sample = "DBBP_UP")
# DBBPQ <- rbind(DBBPQ, data.frame(Date = OYSQ$Date, Discharge.m3s = (3.263385/31.33886) * OYSQ$Discharge.m3s, Site  ="DBBP", Sample = "DBBP_OUT"))
# 
# BPQ <- bind_rows(CCBPQ, DBBPQ)
# 
# write.csv(Q_Daily, file="Data/BPQ_2022-05-09.csv",row.names=FALSE)
# 
# rm(CCBPQ, CCQ, OYSQ, DBBPQ)
#####

################################################################################
## Processing of data
################################################################################

############## GRAB SAMPLE DATA

GRABS$SArea.km2<-NA
GRABS[GRABS$Site == "CCBP",]$SArea.km2 <- 6900/1e+6
GRABS[GRABS$Site == "DBBP",]$SArea.km2 <- 16732.8/1e+6

## Weight nutrient concentrations by catchment area for calculating removal
GRABS <- GRABS %>% select(-c(CO2.ppm, UNH_ID, Gas_Flag, NAr_sat:NAr_calc.C, N2.mM.A:N2.mM.C)) %>%
  mutate(across(Cond.uScm:N2.mM.mean, .fns = ~.x * CArea.km2, .names="{.col}CA")) %>%
  rename_with(~str_remove(., "\\.mgL|\\.ugL|\\.C|\\.uScm|\\.mM"), .cols = (c(Cond.uScmCA:Temp.CCA, pHCA:N2.mM.meanCA)))
  
## Merge Discharge data with GRABS dataframe
GRABS <- GRABS %>% left_join(., BPQ, by = c("Site", "Sample", "Date"))

## Extract "UP" samples for each reservoir into a single input for each variable
Removal<-GRABS[grep("UP",GRABS$Location),]

## Aggregate multiple "UP" samples into a single "UP" 

Removal <- Removal %>%  select(c(2, 4, 6:25, 27:37, 42, 44, 47:66, 68:81)) %>% 
  group_by(across(all_of(c("Site", "Date")))) %>% summarize_all(sum) %>%
  left_join(., filter(GRABS, Location == "OUT"), by=c("Date", "Site"), suffix = c(".UP",".OUT"))

## Add DOC:DON into Removal df
Removal$DOCtoDON.UP <- Removal$DOCCA.UP / Removal$DONCA.UP
Removal$DOCtoDON.OUT <- Removal$DOCCA.OUT / Removal$DONCA.OUT
Removal$DOCtoTDN.UP <- Removal$DOCCA.UP / Removal$TDNCA.UP
Removal$DOCtoTDN.OUT <- Removal$DOCCA.OUT / Removal$TDNCA.OUT



## Need to fix the below abomination...

## Removal dataframe
Removal$RSpCond<-with(Removal,((Sp_CondCA.UP-Sp_CondCA.OUT)/Sp_CondCA.UP))
Removal$RTemp<-with(Removal,((TempCA.UP-TempCA.OUT)/TempCA.UP))
Removal$RDO.mgL<-with(Removal,((DO.mgLCA.UP-DO.mgLCA.OUT)/DO.mgLCA.UP))
Removal$RDO.pct<-with(Removal,((DO.pctCA.UP-DO.pctCA.OUT)/DO.pctCA.UP))
Removal$RTSS<-with(Removal,((TSSCA.UP-TSSCA.OUT)/TSSCA.UP))
Removal$RChl<-with(Removal,((ChlCA.UP-ChlCA.OUT)/ChlCA.UP))
Removal$RPO4<-with(Removal,((PO4CA.UP-PO4CA.OUT)/PO4CA.UP))
Removal$RNH4<-with(Removal,((NH4CA.UP-NH4CA.OUT)/NH4CA.UP))
Removal$RCl<-with(Removal,((ClCA.UP-ClCA.OUT)/ClCA.UP))
Removal$RNO3<-with(Removal,((NO3CA.UP-NO3CA.OUT)/NO3CA.UP))
Removal$RSO4<-with(Removal,((SO4CA.UP-SO4CA.OUT)/SO4CA.UP))
Removal$RBr<-with(Removal,((BrCA.UP-BrCA.OUT)/BrCA.UP))
Removal$RDOC<-with(Removal,((DOCCA.UP-DOCCA.OUT)/DOCCA.UP))
Removal$RTDN<-with(Removal,((TDNCA.UP-TDNCA.OUT)/TDNCA.UP))
Removal$RDON<-with(Removal,((DONCA.UP-DONCA.OUT)/DONCA.UP))
Removal$RDIN<-with(Removal,((DINCA.UP-DINCA.OUT)/DINCA.UP))
Removal$RTDP<-with(Removal,((TDPCA.UP-TDPCA.OUT)/TDPCA.UP))
Removal$RN2O<-with(Removal,((N2OCA.UP-N2OCA.OUT)/N2OCA.UP))
Removal$RCH4<-with(Removal,((CH4CA.UP-CH4CA.OUT)/CH4CA.UP))
#Removal$RAFDM<-with(Removal,((AFDM.mgLCA.UP-AFDM.mgLCA.OUT)/AFDM.mgLCA.UP))
#Removal$RAsh<-with(Removal,((Ash.mgLCA.UP-Ash.mgLCA.OUT)/Ash.mgLCA.UP))
Removal$RPC<-with(Removal,((PCCA.UP-PCCA.OUT)/PCCA.UP))
Removal$RPN<-with(Removal,((PNCA.UP-PNCA.OUT)/PNCA.UP))
Removal$RTN<-with(Removal,((TNCA.UP-TNCA.OUT)/TNCA.UP))
Removal$RNAr<-with(Removal,((NArcalc.meanCA.UP-NArcalc.meanCA.OUT)/NArcalc.meanCA.UP))
Removal$RN2<-with(Removal,((N2.meanCA.UP-N2.meanCA.OUT)/N2.meanCA.UP))
Removal$RDOCtoDON<-with(Removal,((DOCtoDON.UP-DOCtoDON.OUT)/DOCtoDON.UP))
Removal$RDOCtoTDN<-with(Removal,((DOCtoTDN.UP-DOCtoTDN.OUT)/DOCtoTDN.UP))


## Create flag for Cl values using original idea of 10% per input
Removal$RFlag<-ifelse(Removal$RCl > 0.3 | Removal$RCl < -0.3, 1, 0)
## Create second RFlag set at ±20% (standard for reservoir sites)
Removal$RFlag2<-ifelse((Removal$RCl > (0.2) | Removal$RCl < (-0.2)), 1, 0)

## Merge discharge data with Removal datafreame
#Removal <- Removal %>% left_join(., select(BPQ, -Sample), by = c("Site", "Date"))

## Calculate hydraulic load for each site/date ##
Removal$HL.myr<-(Removal$Discharge.m3s.OUT/(Removal$SArea.km2.OUT*1e+6))*86400*365


## Add some other grouping variables to the data frame
# Month
Removal$Month<-format(Removal$Date,"%B")

# Season
for(i in 1:nrow(Removal)){
  Removal$Season[as.numeric(format(Removal$Date,"%m"))%in%c(1,2,12)]<-"Winter"
  Removal$Season[as.numeric(format(Removal$Date,"%m"))%in%c(3:5)]<-"Spring"
  Removal$Season[as.numeric(format(Removal$Date,"%m"))%in%c(6:8)]<-"Summer"
  Removal$Season[as.numeric(format(Removal$Date,"%m"))%in%c(9:11)]<-"Autumn"
}

# Day of year (numeric)
Removal$DOY<-as.POSIXlt(Removal$Date)$yday+1


################### INTERNAL SAMPLING DATA

Means <- Internal %>% group_by(Sample) %>% summarize(across(c(Cond.uScm:DON.mgL), ~mean(., na.rm=TRUE)))




ggplot(data=Means)+
  geom_col(aes(x=Sample,y=TDN.mgL))+
  #facet_wrap(~Sample)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust=1))


bbox<-c(-70.925, 42.7675, -70.915149, 42.7750)
#CCBPmap<-get_stamenmap(bbox, zoom=15, crop=TRUE)
CCBPmap<-get_map(bbox, zoom=15, crop=TRUE)
ggmap(CCBPmap)

ggmap(CCBPmap)+
  geom_point(data=Means, aes(x=Longitude, y=Latitude, color = DIN.mgL), size=4)+
  #coord_fixed(xlim=c(-70.945093,-70.915149),ylim=c(42.75073,42.779325),ratio=1)+
  scale_color_gradient2(expression(bold(DIN~(mgL^-1))),low="green",mid="yellow",high="red",midpoint=0.2)+
  labs(x=NULL,y=NULL)+
  theme_bw()





################################################################################
## Create plots
################################################################################
















