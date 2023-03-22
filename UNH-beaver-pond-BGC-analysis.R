## UNH-beaver-pond-BGC-analysis script

##rgdal for working with jp2,sid, etc. files
##raster can handle tiff (plotRGB)
#library(tmap) # might not need this one
#library(raster)
#library(rgdal)
#library(basemaps)
library(st)
library(sf)
library(sp)
#library(rnaturalearth)
#library(rnaturalearthdata)
#library(stars)
library(ggmap)
library(rgdal)
#library(gganimate)
#library(gifski)
library(tidyverse)

## Register Google API key if not already registered

register_google("AIzaSyCAUmkKYVIMYg8rTYw2ex4tgpe-Qbr7F3E")


################################################################################
## Import data sets
################################################################################


#####Read in Grab sample data#####
#GRABS<-read.csv(file="C:/Users/ctw1/Box/Data/Analysis/Dams/Data/Reservoir_Grab_2022-05-09.csv",header=TRUE,stringsAsFactors=FALSE)
GRABS<-read.csv(file="Data/Reservoir_Grab_2022-09-14.csv",header=TRUE,stringsAsFactors=FALSE)
GRABS$DateTime<-as.POSIXct(strptime(paste(GRABS$Date,GRABS$Time),format="%m/%d/%Y %H:%M",tz="America/New_York"))
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

GRABS$NH4.mgL <- GRABS$NH4.ugL/1000


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

## Add factoring variable
Synoptic$WBType <- ifelse(Synoptic$WBType == "Channel", "Channel", "Pond")
Synoptic$WBType <- factor(Synoptic$WBType, levels = c("Channel", "Pond"))


##Import discharge data
## Can read in csv of data that has already been processed
BPQ <- read.csv("Data/BP_Q_2022-05-09.csv", header = TRUE, stringsAsFactors = FALSE)
BPQ$Date <- as.Date(BPQ$Date, format = "%Y-%m-%d")


## SIGMA Timeseries
SIGMA<-read.csv(file="C:/Users/ctw1/Box/Data/Analysis/LTER_Update_2021/Data/WAT-YSI-SIGMA-timeseries_2022-01-19.csv",header=TRUE,stringsAsFactors=FALSE)
SIGMA$Date<-as.Date(strptime(SIGMA$Date,format="%Y-%m-%d"))
SIGMA$Year<-as.numeric(format(SIGMA$Date,"%Y"))
#SIGMA<-SIGMA %>% mutate(DateTime=as.POSIXct(strptime(paste(SIGMA$Date,"12:00"),format="%Y-%m-%d %H:%M"),tz="EST"),)
SIGMA$DateTime<-as.POSIXct(strptime(paste(SIGMA$Date,"12:00"),format="%Y-%m-%d %H:%M"),tz="EST")
SIGMA$Date2<-lubridate::floor_date(SIGMA$Date,"month")

## Calculate DIN (not in data set) as sum of NO3, NO2, NH4

SIGMA <- SIGMA %>% as.data.frame() %>% mutate(DIN=select(., c(NO2, NO3, NH4)) %>% rowSums(na.rm=TRUE))


## Calculate DON if not in dataset
SIGMA$DON<-ifelse(is.na(SIGMA$DON),SIGMA$TDN - SIGMA$DIN, SIGMA$DON)

## Filter out all but CC data
SIGMA <- SIGMA %>% filter(Permanent_ID == "YSI-CC")
SIGMA$Site <- "CCBP"

## Convert from uM to mg/L
SIGMA$NO3.mgL<-SIGMA$NO3*14.0067/1000
SIGMA$NH4.mgL<-SIGMA$NH4*14.0067/1000
SIGMA$DIN.mgL<-SIGMA$DIN*14.0067/1000
SIGMA$PO4.mgL<-SIGMA$PO4*30.973762/1000
SIGMA$Cl.mgL<-SIGMA$Cl*35.453/1000
SIGMA$SO4.mgL<-SIGMA$SO4*32.065/1000
SIGMA$Br.mgL<-SIGMA$Br*159.808/1000
SIGMA$TDN.mgL<-SIGMA$TDN*14.0067/1000
SIGMA$DON.mgL<-SIGMA$DON*14.0067/1000
SIGMA$DOC.mgL<-SIGMA$DOC*12.0107/1000
SIGMA$TDP.mgL<-SIGMA$TDP*30.973762/1000
SIGMA$TSS.mgL<-SIGMA$TSS

## Select only certain columns
SIGMA <- SIGMA %>% select(c(4,20:22, 24:34))


## Replace negative concentrations with zero (don't know detection limit)
GRABS[,c(26:37)][GRABS[,c(26:37)] < 0] <- 0



################################################################################
## Processing of data
################################################################################

############## GRAB SAMPLE DATA

GRABS$SArea.km2<-NA
GRABS[GRABS$Site == "CCBP",]$SArea.km2 <- 6900/1e+6
GRABS[GRABS$Site == "DBBP",]$SArea.km2 <- 16732.8/1e+6

## Merge Discharge data with GRABS dataframe
GRABS <- GRABS %>% left_join(., BPQ, by = c("Site", "Sample", "Date"))


## Weight nutrient concentrations by catchment area for calculating removal
GRABS <- GRABS %>% select(-c(CO2.ppm,UNH_ID, Gas_Flag, NAr_sat:NAr_calc.C, N2.mM.A:N2.mM.C)) %>% #Selects variables I want
  mutate(across(Cond.uScm:N2.mM.mean, .fns = ~.x * CArea.km2, .names="{.col}CA")) %>% #Weight by CArea
  rename_with(~str_remove(., "\\.mgL|\\.ugL|\\.C|\\.uScm|\\.mM"), .cols = (c(Cond.uScmCA:Temp.CCA, pHCA:N2.mM.meanCA))) #Remove units from weighted names
## Weight nutrient concentrations by discharge for calculating removal  
GRABS <-  GRABS %>% mutate(across(Cond.uScm:N2.mM.mean, .fns = ~.x * (Discharge.m3s * 1000) * 86400 * 365 / 1e+6 / CArea.km2, .names="{.col}.kgkm2Day")) %>%
  rename_with(~str_remove(., "\\.mgL|\\.ugL|\\.C|\\.uScm|\\.mM"), .cols = (c(Cond.uScm.kgkm2Day:Temp.C.kgkm2Day, pH.kgkm2Day:N2.mM.mean.kgkm2Day)))

# ## Weight nutrient concentrations by discharge for calculating removal
# GRABS <- GRABS %>% select(-c(CO2.ppm, UNH_ID, Gas_Flag, NAr_sat:NAr_calc.C, N2.mM.A:N2.mM.C)) %>%
#   mutate(across(Cond.uScm:N2.mM.mean, .fns = ~.x * CArea.km2, .names="{.col}CA")) %>%
#   rename_with(~str_remove(., "\\.mgL|\\.ugL|\\.C|\\.uScm|\\.mM"), .cols = (c(Cond.uScmCA:Temp.CCA, pHCA:N2.mM.meanCA)))

  

## Extract "UP" samples for each reservoir into a single input for each variable
Removal<-GRABS[grep("UP",GRABS$Location),]

## Aggregate multiple "UP" samples into a single "UP" 

Removal <- Removal %>%  select(c(2, 4, 6:25, 27:37, 42, 44, 47:81, 83:115)) %>% # was: 47:66, 68:81
  group_by(across(all_of(c("Site", "Date")))) %>% summarize_all(sum) %>%
  left_join(., filter(GRABS, Location == "OUT"), by=c("Date", "Site"), suffix = c(".UP",".OUT")) %>% as.data.frame()

## Add DOC:DON into Removal df
Removal$DOCtoDON.UP <- Removal$DOCCA.UP / Removal$DONCA.UP
Removal$DOCtoDON.OUT <- Removal$DOCCA.OUT / Removal$DONCA.OUT
Removal$DOCtoTDN.UP <- Removal$DOCCA.UP / Removal$TDNCA.UP
Removal$DOCtoTDN.OUT <- Removal$DOCCA.OUT / Removal$TDNCA.OUT

##### Trying to fix this shit

Removal$RNO3 <- with(Removal, (NO3.kgkm2Day.UP - NO3.kgkm2Day.OUT)/NO3.kgkm2Day.UP)
Removal$RNO3 <- with(Removal, (NO3CA.UP - NO3CA.OUT)/NO3CA.UP)
Removal$RNO3 <- with(Removal,((NO3.mgL.UP * CArea.km2.UP)-NO3CA.OUT)/(NO3.mgL.UP * CArea.km2.UP))

tmpNO3 <- Removal %>% select(c(Site, Date)) %>% as.data.frame()
tmpNO3$RNO3_1 <- with(Removal, (NO3.kgkm2Day.UP - NO3.kgkm2Day.OUT)/NO3.kgkm2Day.UP)
tmpNO3$RNO3_2 <- with(Removal, (NO3CA.UP - NO3CA.OUT)/NO3CA.UP)
tmpNO3$RNO3_3 <- with(Removal, ((NO3.mgL.UP * CArea.km2.UP)-NO3CA.OUT)/(NO3.mgL.UP * CArea.km2.UP))

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


Removal$RNO32 <- with(Removal,(((NO3.mgL.UP*CArea.km2.UP)-NO3CA.OUT)/(NO3.mgL.UP*CArea.km2.UP)))


## Removal dataframe
Removal$RSpCond<-with(Removal,((Sp_Cond.uScm.UP * CArea.km2.UP)-Sp_CondCA.OUT)/(Sp_Cond.uScm.UP * CArea.km2.UP))
Removal$RTemp<-with(Removal,((TempCA.UP * CArea.km2.UP)-TempCA.OUT)/(Temp.C.UP * CArea.km2.UP))
Removal$RDO.mgL<-with(Removal,((DO.mgL.UP * CArea.km2.UP)-DO.mgLCA.OUT)/(DO.mgL.UP * CArea.km2.UP))
Removal$RDO.pct<-with(Removal,((DO.pct.UP * CArea.km2.UP)-DO.pctCA.OUT)/(DO.pct.UP * CArea.km2.UP))
Removal$RTSS<-with(Removal,((TSS.mgL.UP * CArea.km2.UP)-TSSCA.OUT)/(TSS.mgL.UP * CArea.km2.UP))
Removal$RChl<-with(Removal,((Chl.mgL.UP * CArea.km2.UP)-ChlCA.OUT)/(Chl.mgL.UP * CArea.km2.UP))
Removal$RPO4<-with(Removal,((PO4.ugL.UP * CArea.km2.UP)-PO4CA.OUT)/(PO4.ugL.UP * CArea.km2.UP))
Removal$RNH4<-with(Removal,((NH4.ugL.UP * CArea.km2.UP)-NH4CA.OUT)/(NH4.ugL.UP * CArea.km2.UP))
Removal$RCl<-with(Removal,((Cl.mgL.UP * CArea.km2.UP)-ClCA.OUT)/(Cl.mgL.UP * CArea.km2.UP))
Removal$RNO3<-with(Removal,((NO3.mgL.UP * CArea.km2.UP)-NO3CA.OUT)/(NO3.mgL.UP * CArea.km2.UP))
Removal$RSO4<-with(Removal,((SO4.mgL.UP * CArea.km2.UP)-SO4CA.OUT)/(SO4.mgL.UP * CArea.km2.UP))
Removal$RBr<-with(Removal,((Br.mgL.UP * CArea.km2.UP)-BrCA.OUT)/(Br.mgL.UP * CArea.km2.UP))
Removal$RDOC<-with(Removal,((DOC.mgL.UP * CArea.km2.UP)-DOCCA.OUT)/(DOC.mgL.UP * CArea.km2.UP))
Removal$RTDN<-with(Removal,((TDN.mgL.UP * CArea.km2.UP)-TDNCA.OUT)/(TDNCA.UP * CArea.km2.UP))
Removal$RDON<-with(Removal,((DON.mgL.UP * CArea.km2.UP)-DONCA.OUT)/(DONCA.UP * CArea.km2.UP))
Removal$RDIN<-with(Removal,((DIN.mgL.UP * CArea.km2.UP)-DINCA.OUT)/(DINCA.UP * CArea.km2.UP))
Removal$RTDP<-with(Removal,((TDP.mgL.UP * CArea.km2.UP)-TDPCA.OUT)/(TDPCA.UP * CArea.km2.UP))
Removal$RN2O<-with(Removal,((N2O.mM.UP * CArea.km2.UP)-N2OCA.OUT)/(N2OCA.UP * CArea.km2.UP))
Removal$RCH4<-with(Removal,((CH4.mM.UP * CArea.km2.UP)-CH4CA.OUT)/(CH4CA.UP * CArea.km2.UP))
#Removal$RAFDM<-with(Removal,((AFDM.mgLCA.UP-AFDM.mgLCA.OUT)/AFDM.mgLCA.UP))
#Removal$RAsh<-with(Removal,((Ash.mgLCA.UP-Ash.mgLCA.OUT)/Ash.mgLCA.UP))
Removal$RPC<-with(Removal,((PC.mgL.UP * CArea.km2.UP)-PCCA.OUT)/(PCCA.UP * CArea.km2.UP))
Removal$RPN<-with(Removal,((PN.mgL.UP * CArea.km2.UP)-PNCA.OUT)/(PNCA.UP * CArea.km2.UP))
Removal$RTN<-with(Removal,((TN.mgL.UP * CArea.km2.UP)-TNCA.OUT)/(TNCA.UP * CArea.km2.UP))
Removal$RNAr<-with(Removal,((NArcalc.mean.UP * CArea.km2.UP)-NArcalc.meanCA.OUT)/(NArcalc.meanCA.UP * CArea.km2.UP))
Removal$RN2<-with(Removal,((N2.mM.mean.UP * CArea.km2.UP)-N2.meanCA.OUT)/(N2.meanCA.UP * CArea.km2.UP))
Removal$RDOCtoDON<-with(Removal,((DOCtoDON.UP-DOCtoDON.OUT)/DOCtoDON.UP))
Removal$RDOCtoTDN<-with(Removal,((DOCtoTDN.UP-DOCtoTDN.OUT)/DOCtoTDN.UP))

with(Removal, (NO3.kgkm2Day.UP - NO3.kgkm2Day.OUT)/NO3.kgkm2Day.UP)

## Removal dataframe
Removal$RSpCond<-with(Removal,((Sp_Cond.kgkm2Day.UP-Sp_Cond.kgkm2Day.OUT)/Sp_Cond.kgkm2Day.UP))
Removal$RTemp<-with(Removal,((Temp.kgkm2Day.UP-Temp.kgkm2Day.OUT)/Temp.kgkm2Day.UP))
Removal$RDO.mgL<-with(Removal,((DO.mgL.kgkm2Day.UP-DO.mgL.kgkm2Day.OUT)/DO.mgL.kgkm2Day.UP))
Removal$RDO.pct<-with(Removal,((DO.pct.kgkm2Day.UP-DO.pct.kgkm2Day.OUT)/DO.pct.kgkm2Day.UP))
Removal$RTSS<-with(Removal,((TSS.kgkm2Day.UP-TSS.kgkm2Day.OUT)/TSS.kgkm2Day.UP))
Removal$RChl<-with(Removal,((Chl.kgkm2Day.UP-Chl.kgkm2Day.OUT)/Chl.kgkm2Day.UP))
Removal$RPO4<-with(Removal,((PO4.kgkm2Day.UP-PO4.kgkm2Day.OUT)/PO4.kgkm2Day.UP))
Removal$RNH4<-with(Removal,((NH4.kgkm2Day.UP-NH4.kgkm2Day.OUT)/NH4.kgkm2Day.UP))
Removal$RCl<-with(Removal,((Cl.kgkm2Day.UP-Cl.kgkm2Day.OUT)/Cl.kgkm2Day.UP))
Removal$RNO3<-with(Removal,((NO3.kgkm2Day.UP-NO3.kgkm2Day.OUT)/NO3.kgkm2Day.UP))
Removal$RSO4<-with(Removal,((SO4.kgkm2Day.UP-SO4.kgkm2Day.OUT)/SO4.kgkm2Day.UP))
Removal$RBr<-with(Removal,((Br.kgkm2Day.UP-Br.kgkm2Day.OUT)/Br.kgkm2Day.UP))
Removal$RDOC<-with(Removal,((DOC.kgkm2Day.UP-DOC.kgkm2Day.OUT)/DOC.kgkm2Day.UP))
Removal$RTDN<-with(Removal,((TDN.kgkm2Day.UP-TDN.kgkm2Day.OUT)/TDN.kgkm2Day.UP))
Removal$RDON<-with(Removal,((DON.kgkm2Day.UP-DON.kgkm2Day.OUT)/DON.kgkm2Day.UP))
Removal$RDIN<-with(Removal,((DIN.kgkm2Day.UP-DIN.kgkm2Day.OUT)/DIN.kgkm2Day.UP))
Removal$RTDP<-with(Removal,((TDP.kgkm2Day.UP-TDP.kgkm2Day.OUT)/TDP.kgkm2Day.UP))
Removal$RN2O<-with(Removal,((N2O.kgkm2Day.UP-N2O.kgkm2Day.OUT)/N2O.kgkm2Day.UP))
Removal$RCH4<-with(Removal,((CH4.kgkm2Day.UP-CH4.kgkm2Day.OUT)/CH4.kgkm2Day.UP))
#Removal$RAFDM<-with(Removal,((AFDM.mgL.kgkm2Day.UP-AFDM.mgL.kgkm2Day.OUT)/AFDM.mgL.kgkm2Day.UP))
#Removal$RAsh<-with(Removal,((Ash.mgL.kgkm2Day.UP-Ash.mgL.kgkm2Day.OUT)/Ash.mgL.kgkm2Day.UP))
Removal$RPC<-with(Removal,((PC.kgkm2Day.UP-PC.kgkm2Day.OUT)/PC.kgkm2Day.UP))
Removal$RPN<-with(Removal,((PN.kgkm2Day.UP-PN.kgkm2Day.OUT)/PN.kgkm2Day.UP))
Removal$RTN<-with(Removal,((TN.kgkm2Day.UP-TN.kgkm2Day.OUT)/TN.kgkm2Day.UP))
Removal$RNAr<-with(Removal,((NAr.kgkm2Daylc.mean.kgkm2Day.UP-NAr.kgkm2Daylc.mean.kgkm2Day.OUT)/NAr.kgkm2Daylc.mean.kgkm2Day.UP))
Removal$RN2<-with(Removal,((N2.mean.kgkm2Day.UP-N2.mean.kgkm2Day.OUT)/N2.mean.kgkm2Day.UP))
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

## Calculate means by sample location and reduce

Means <- Internal %>% group_by(Sample) %>% summarize(across(c(Cond.uScm:DON.mgL), ~mean(., na.rm=TRUE)))



## ggmap heatmap

CCBPmapInternal <- get_googlemap(center=c(lon = -70.9203, lat = 42.772258), 
                                 maptype = "satellite", zoom = 17) #18

ggmap(CCBPmapInternal)

# InternalNO3 <- ggmap(CCBPmapInternal)+
#   stat_density2d(data = Internal, aes(x = Longitude, y = Latitude, fill = ..level.., alpha = ..level..),
#                  geom = "polygon", bins = 16, size = 0.01)+
#   scale_fill_gradient(low = "green", high = "red")+
#   scale_alpha(range = c(0, 0.3))

## Interpolate NO3

## Akima intepolation is kind of sloppy. Need a different method that works better for such a small scale
NO3interp <- with(Means[!Means$Sample == "CC" & !Means$Sample == "CC_UPPER",], 
            akima::interp(x = Longitude, y = Latitude, z = NO3.mgL, 
                          xo = seq(min(Means$Longitude), max(Means$Longitude), length = 200), 
                          yo = seq(min(Means$Latitude), max(Means$Latitude), length = 200), linear = FALSE))

NO3df <- reshape2::melt(NO3interp$z, na.rm = TRUE)
names(NO3df) <- c("x", "y", "NO3.mgL")
NO3df$lon <- NO3interp$x[NO3df$x]
NO3df$lat <- NO3interp$y[NO3df$y]


## Import CCBP outline

CCBPOutline <- readOGR("Data/Spatial/CCBP_Polygon/CCBP_poly.shp")
CCBPOutline <- readOGR("Data/Spatial/CCBP_poly_simple/CCBP_poly_simple.shp")
CCBPOutline <- read_sf("Data/Spatial/CCBP_poly_simple/CCBP_poly_simple.shp")
CCBPOutline4326 <- st_transform(CCBPOutline, "EPSG:4326")

coordinates(Means) = ~Longitude + Latitude
Means4326 <- spTransform(Means, "EPSG:4326")

InternalNO3 <- ggmap(CCBPmapInternal)+
  coord_sf(crs=4326,xlim=c(-7897580, -7893501 ), ylim=c(5273954, 5278369))+
  geom_sf(data=CCBPOutline4326, inherit.aes=FALSE, color="blue", size=1)+
  #geom_point(data=Means,aes(x=Longitude,y=Latitude, fill = NO3.mgL),size=4)+
  geom_tile(data = NO3df, aes(x = lon, y = lat, fill = NO3.mgL), alpha = 0.8)+
  #stat_contour(data = NO3df, aes(x = lon, y = lat, z = NO3.mgL))+
  scale_fill_continuous(low = "green", high = "red")
  #coord_map()

InternalNO3


## Using spatstat
## https://stackoverflow.com/questions/35617548/confining-an-idw-interpolation-to-a-polygon-in-rspatstat

## Using subset of original Means data

NO3dfSS <- Means %>% select(c("Latitude", "Longitude", "NO3.mgL"))

NO3dfSS <- ppp(NO3dfSS$Longitude, NO3dfSS$Latitude, poly = NO3dfSS)

marks(NO3dfSS) <- NO3dfSS$NO3.mgL


## Using gstat and sp
## https://rspatial.org/raster/analysis/4-interpolation.html

MeansSP <- SpatialPoints(Means[,11:10], proj4string = CRS("+proj=longlat +datum=NAD83"))
MeansSP <-  SpatialPointsDataFrame(MeansSP, Means)

Means4326 <- st_transform(Means, "EPSG:4326")
CCBPOutline4326 <- st_transform(CCBPOutline, "EPSG:4326")

CCBPOutline4326 <- spTransform(CCBPOutline, "EPSG:4326")

spplot(MeansSP, 'NO3.mgL')


r <- raster(CCBPOutline4326, res = 10000)
gs <- gstat(formula = NO3.mgL~1, locations = Means4326)
idw <- interpolate(r, gs)

## Using gstat and sp
## https://stackoverflow.com/questions/39591240/spatial-interpolation-error-using-idw/39623736#39623736

## Using original 'Means' data

x.range <- range(Means[!Means$Sample == "CC" & !Means$Sample == "CC_UPPER",]$Longitude)
y.range <- range(Means[!Means$Sample == "CC" & !Means$Sample == "CC_UPPER",]$Latitude)

x.range <- c(min(Means[!Means$Sample == "CC" & !Means$Sample == "CC_UPPER",]$Longitude),
             -70.919)
y.range <- c(min(Means[!Means$Sample == "CC" & !Means$Sample == "CC_UPPER",]$Latitude),
             42.773)

#x.range = range(CCBPOutline$)

x<-seq(x.range[1], x.range[2], length.out=200)
y<-seq(y.range[1], y.range[2], length.out=200)
grd<-expand.grid(x,y)

#coordinates(Means) = ~Longitude+Latitude
coordinates(grd) <- ~ Var1+Var2
gridded(grd) <- TRUE

#proj4string(Means) <- CRS("+proj=longlat +datum=NAD83")
proj4string(grd) <- CRS("+proj=longlat +datum=NAD83")

plot(grd, cex=1.5)

NO3.idw <- idw(formula=NO3.mgL ~ 1, locations = Means, newdata = grd, idp = 2.0)
NH4.idw <- idw(formula=NH4.ugL ~ 1, locations = Means, newdata = grd, idp = 2.0)
DIN.idw <- idw(formula=DIN.mgL ~ 1, locations = Means, newdata = grd, idp = 2.0)
DON.idw <- idw(formula=DON.mgL ~ 1, locations = Means, newdata = grd, idp = 2.0)
TDN.idw <- idw(formula=TDN.mgL ~ 1, locations = Means, newdata = grd, idp = 2.0)
DOpct.idw <- idw(formula=DO.pct ~ 1, locations = Means, newdata = grd, idp = 2.0)
DOmgL.idw <- idw(formula=DO.mgL ~ 1, locations = Means, newdata = grd, idp = 2.0)
ClmgL.idw <- idw(formula=Cl.mgL ~ 1, locations = Means, newdata = grd, idp = 2.0)



## Convert dat.idw to raster
r.NO3.idw <- raster(NO3.idw)
r.NH4.idw <- raster(NH4.idw)
r.DIN.idw <- raster(DIN.idw)
r.DON.idw <- raster(DON.idw)
r.TDN.idw <- raster(TDN.idw)
r.DOpct.idw <- raster(DOpct.idw)
r.DOmgL.idw <- raster(DOmgL.idw)
r.ClmgL.idw <- raster(ClmgL.idw)


## Convert to same CRS
#r.NO3.idw <- r.NO3.idw %>% st_transform(st_crs(CCBPOutline))

sr <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs";

## Reproject raster in EPSG:4326

r.NO3.idw.RP <- projectRaster(r.NO3.idw, crs = sr)
r.NH4.idw.RP <- projectRaster(r.NH4.idw, crs = sr)
r.DIN.idw.RP <- projectRaster(r.DIN.idw, crs = sr)
r.DON.idw.RP <- projectRaster(r.DON.idw, crs = sr)
r.TDN.idw.RP <- projectRaster(r.TDN.idw, crs = sr)
r.DOpct.idw.RP <- projectRaster(r.DOpct.idw, crs = sr)
r.DOmgL.idw.RP <- projectRaster(r.DOmgL.idw, crs = sr)
r.ClmgL.idw.RP <- projectRaster(r.ClmgL.idw, crs = sr)



r.NO3.crop <- crop(r.NO3.idw.RP, extent(CCBPOutline4326))
r.NH4.crop <- crop(r.NH4.idw.RP, extent(CCBPOutline4326))
r.DIN.crop <- crop(r.DIN.idw.RP, extent(CCBPOutline4326))
r.DON.crop <- crop(r.DON.idw.RP, extent(CCBPOutline4326))
r.TDN.crop <- crop(r.TDN.idw.RP, extent(CCBPOutline4326))
r.DOpct.crop <- crop(r.DOpct.idw.RP, extent(CCBPOutline4326))
r.DOmgL.crop <- crop(r.DOmgL.idw.RP, extent(CCBPOutline4326))
r.ClmgL.crop <- crop(r.ClmgL.idw.RP, extent(CCBPOutline4326))


r.NO3.mask <- mask(r.NO3.crop, CCBPOutline4326)
r.NH4.mask <- mask(r.NH4.crop, CCBPOutline4326)
r.DIN.mask <- mask(r.DIN.crop, CCBPOutline4326)
r.DON.mask <- mask(r.DON.crop, CCBPOutline4326)
r.TDN.mask <- mask(r.TDN.crop, CCBPOutline4326)
r.DOpct.mask <- mask(r.DOpct.crop, CCBPOutline4326)
r.DOmgL.mask <- mask(r.DOmgL.crop, CCBPOutline4326)
r.ClmgL.mask <- mask(r.ClmgL.crop, CCBPOutline4326)


NO3.idw <- as.data.frame(rasterToPoints(r.NO3.mask))
NH4.idw <- as.data.frame(rasterToPoints(r.NH4.mask))
DIN.idw <- as.data.frame(rasterToPoints(r.DIN.mask))
DON.idw <- as.data.frame(rasterToPoints(r.DON.mask))
TDN.idw <- as.data.frame(rasterToPoints(r.TDN.mask))
DOpct.idw <- as.data.frame(rasterToPoints(r.DOpct.mask))
DOmgL.idw <- as.data.frame(rasterToPoints(r.DOmgL.mask))
ClmgL.idw <- as.data.frame(rasterToPoints(r.ClmgL.mask))

NH4.idw$var1.pred <- NH4.idw$var1.pred/1000

mid <- function(x) {
 MP <- (min(x) + max(x))/2
 return(MP)
}

NO3idw<- ggmap(CCBPmapInternal)+
  geom_sf(data=CChydroArc4326, inherit.aes=FALSE, color="blue", size=1)+
  geom_sf(data=CChydroPoly4326, inherit.aes=FALSE, color="blue",fill="blue")+
  geom_point(data=NO3.idw, aes(x=x, y=y, color=var1.pred))+
  #geom_point(data=Means, aes(x=Longitude, y=Latitude, color = NO3.mgL), size=4)+
  #geom_sf(data=CCBPOutline4326, inherit.aes=FALSE, color="blue",fill = NA, size=1, alpha = 0.8)+
  scale_color_gradient2(name=expression(bold(NO[3]~(mg~L^-1))),low="darkblue",mid="green",high="red",midpoint=0.01, n.breaks = 3)+
  theme_bw()+
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = "right",
        legend.direction = "vertical",
        #legend.title = element_blank())+
        #legend.position = c(0.89,0.5),
        #legend.background = element_rect(fill = "white", color = "black"),
        legend.title = element_text(size = 8),
        legend.margin = margin(0, 0, 0, 0, "pt"),
        plot.margin = margin(1,0,-10,0, "pt"),
        legend.text = element_text(size = 8, angle = 0, hjust = 1))+
  guides(color = guide_colorbar(label.hjust = 0.75, title.position = "top", title.hjust = 0.5))

NO3idw


NH4idw<- ggmap(CCBPmapInternal)+
  geom_sf(data=CChydroArc4326, inherit.aes=FALSE, color="blue", size=1)+
  geom_sf(data=CChydroPoly4326, inherit.aes=FALSE, color="blue",fill="blue")+
  geom_point(data=NH4.idw, aes(x=x, y=y, color=var1.pred))+
  #geom_point(data=Means, aes(x=Longitude, y=Latitude, color = NO3.mgL), size=4)+
  #geom_sf(data=CCBPOutline4326, inherit.aes=FALSE, color="blue",fill = NA, size=1, alpha = 0.8)+
  scale_color_gradient2(name=expression(bold(NH[4]~(mu*g~L^-1))),low="darkblue",mid="green",high="red",midpoint=0.12, breaks = c(0.025, 0.12, 0.22))+
  theme_bw()+
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = "right",
        legend.direction = "vertical",
        #legend.title = element_blank())+
        #legend.position = c(0.89,0.5),
        #legend.background = element_rect(fill = "white", color = "black"),
        legend.title = element_text(size = 8),
        legend.margin = margin(0, 0, 0, 0, "pt"),
        #plot.margin = margin(1,0,0,0, "pt"),
        legend.text = element_text(size = 8, angle = 0, hjust = 1))+
  guides(color = guide_colorbar(label.hjust = 0.75, title.position = "top", title.hjust = 0.5))

NH4idw

ggsave(file = paste0("Plots/IDW_NH4_", Sys.Date(), ".png"), NH4idw)

DINidw<- ggmap(CCBPmapInternal)+
  geom_sf(data=CChydroArc4326, inherit.aes=FALSE, color="blue", size=1)+
  geom_sf(data=CChydroPoly4326, inherit.aes=FALSE, color="blue",fill="blue")+
  geom_point(data=DIN.idw, aes(x=x, y=y, color=var1.pred))+
  #geom_point(data=Means, aes(x=Longitude, y=Latitude, color = NO3.mgL), size=4)+
  #geom_sf(data=CCBPOutline4326, inherit.aes=FALSE, color="blue",fill = NA, size=1, alpha = 0.8)+
  scale_color_gradient2(name=expression(bold(DIN~(mg~L^-1))),low="darkblue",mid="green",high="red",midpoint=0.18, breaks = c(0.07, 0.18, 0.30))+
  theme_bw()+
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = "right",
        legend.direction = "vertical",
        #legend.title = element_blank())+
        #legend.position = c(0.89,0.5),
        #legend.background = element_rect(fill = "white", color = "black"),
        legend.title = element_text(size = 8),
        legend.margin = margin(0, 0, 0, 0, "pt"),
        #plot.margin = margin(1,0,0,0, "pt"),
        legend.text = element_text(size = 8, angle = 0, hjust = 1))+
  guides(color = guide_colorbar(label.hjust = 0.75, title.position = "top", title.hjust = 0.5))

DINidw


DONidw<- ggmap(CCBPmapInternal)+
  geom_sf(data=CChydroArc4326, inherit.aes=FALSE, color="blue", size=1)+
  geom_sf(data=CChydroPoly4326, inherit.aes=FALSE, color="blue",fill="blue")+
  geom_point(data=DON.idw, aes(x=x, y=y, color=var1.pred))+
  #geom_point(data=Means, aes(x=Longitude, y=Latitude, color = NO3.mgL), size=4)+
  #geom_sf(data=CCBPOutline4326, inherit.aes=FALSE, color="blue",fill = NA, size=1, alpha = 0.8)+
  scale_color_gradient2(name=expression(bold(DON~(mg~L^-1))),low="darkblue",mid="green",high="red",midpoint=0.31, breaks = c(0.27, 0.31, 0.36))+
  theme_bw()+
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = "right",
        legend.direction = "vertical",
        #legend.title = element_blank())+
        #legend.position = c(0.89,0.5),
        #legend.background = element_rect(fill = "white", color = "black"),
        legend.title = element_text(size = 8),
        legend.margin = margin(0, 0, 0, 0, "pt"),
        #plot.margin = margin(0,0,0,0, "pt"),
        legend.text = element_text(size = 8, angle = 0, hjust = 1))+
  guides(color = guide_colorbar(label.hjust = 0.75, title.position = "top", title.hjust = 0.5))

DONidw


TDNidw<- ggmap(CCBPmapInternal)+
  geom_sf(data=CChydroArc4326, inherit.aes=FALSE, color="blue", size=1)+
  geom_sf(data=CChydroPoly4326, inherit.aes=FALSE, color="blue",fill="blue")+
  geom_point(data=TDN.idw, aes(x=x, y=y, color=var1.pred))+
  #geom_point(data=Means, aes(x=Longitude, y=Latitude, color = NO3.mgL), size=4)+
  #geom_sf(data=CCBPOutline4326, inherit.aes=FALSE, color="blue",fill = NA, size=1, alpha = 0.8)+
  scale_color_gradient2(name=expression(bold(TDN~(mg~L^-1))),low="darkblue",mid="green",high="red",midpoint=0.56, breaks = c(0.35, 0.56, 0.80))+
  theme_bw()+
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = "right",
        legend.direction = "vertical",
        #legend.title = element_blank())+
        #legend.position = c(0.89,0.5),
        #legend.background = element_rect(fill = "white", color = "black"),
        legend.title = element_text(size = 8),
        legend.margin = margin(0, 0, 0, 0, "pt"),
        #plot.margin = margin(0,0,0,0, "pt"),
        legend.text = element_text(size = 8, angle = 0, hjust = 1))+
  guides(color = guide_colorbar(label.hjust = 0.75, title.position = "top", title.hjust = 0.5))

TDNidw



DOpctidw<- ggmap(CCBPmapInternal)+
  geom_sf(data=CChydroArc4326, inherit.aes=FALSE, color="blue", size=1)+
  geom_sf(data=CChydroPoly4326, inherit.aes=FALSE, color="blue",fill="blue")+
  geom_point(data=DOpct.idw, aes(x=x, y=y, color=var1.pred))+
  #geom_point(data=Means, aes(x=Longitude, y=Latitude, color = NO3.mgL), size=4)+
  #geom_sf(data=CCBPOutline4326, inherit.aes=FALSE, color="blue",fill = NA, size=1, alpha = 0.8)+
  scale_color_gradient2(name=expression(bold(DO~("%"))),low="darkblue",mid="green",high="red",midpoint=30.7, breaks = c(20.0, 30.0, 40.0))+
  theme_bw()+
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = "right",
        legend.direction = "vertical",
        #legend.title = element_blank())+
        #legend.position = c(0.89,0.5),
        #legend.background = element_rect(fill = "white", color = "black"),
        legend.title = element_text(size = 8),
        legend.margin = margin(0, 0, 0, 0, "pt"),
        #plot.margin = margin(0,0,0,0, "pt"),
        legend.text = element_text(size = 8, angle = 0, hjust = 1))+
  guides(color = guide_colorbar(label.hjust = 0.75, title.position = "top", title.hjust = 0.5))

DOpctidw


DOmgLidw<- ggmap(CCBPmapInternal)+
  geom_sf(data=CChydroArc4326, inherit.aes=FALSE, color="blue", size=1)+
  geom_sf(data=CChydroPoly4326, inherit.aes=FALSE, color="blue",fill="blue")+
  geom_point(data=DOmgL.idw, aes(x=x, y=y, color=var1.pred))+
  #geom_point(data=Means, aes(x=Longitude, y=Latitude, color = NO3.mgL), size=4)+
  #geom_sf(data=CCBPOutline4326, inherit.aes=FALSE, color="blue",fill = NA, size=1, alpha = 0.8)+
  scale_color_gradient2(name=expression(bold(DO~("%"))),low="darkblue",mid="green",high="red",midpoint= 2.8, breaks = c(1.0, 2.8, 3.3))+
  theme_bw()+
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal",
        #legend.title = element_blank())+
        #legend.position = c(0.89,0.5),
        #legend.background = element_rect(fill = "white", color = "black"),
        legend.title = element_text(size = 8),
        legend.margin = margin(0, 0, 0, 0, "pt"))+
  guides(color = guide_colorbar(label.hjust = 0.75, title.position = "top", title.hjust = 0.5))

DOmgLidw


ClmgLidw<- ggmap(CCBPmapInternal)+
  geom_sf(data=CChydroArc4326, inherit.aes=FALSE, color="blue", size=1)+
  geom_sf(data=CChydroPoly4326, inherit.aes=FALSE, color="blue",fill="blue")+
  geom_point(data=ClmgL.idw, aes(x=x, y=y, color=var1.pred))+
  #geom_point(data=Means, aes(x=Longitude, y=Latitude, color = NO3.mgL), size=4)+
  #geom_sf(data=CCBPOutline4326, inherit.aes=FALSE, color="blue",fill = NA, size=1, alpha = 0.8)+
  scale_color_gradient2(name=expression(bold(Cl~(mg~L^-1))),low="darkblue",mid="green",high="red",midpoint=99.00251, n.breaks = 3)+
  theme_bw()+
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal",
        #legend.title = element_blank())+
        #legend.position = c(0.89,0.5),
        #legend.background = element_rect(fill = "white", color = "black"),
        legend.title = element_text(size = 8),
        legend.margin = margin(0, 0, 0, 0, "pt"))+
  guides(color = guide_colorbar(label.hjust = 0.75, title.position = "top", title.hjust = 0.5))

ClmgLidw


## Plot all species together

ALLidw <- cowplot::plot_grid(NO3idw, NH4idw, DINidw,DONidw, TDNidw, DOpctidw) #, align = "h"

ggsave(file = paste0("Plots/IDW_All_", Sys.Date(), ".png"), ALLidw)


egg::ggarrange(NO3idw, NH4idw, DINidw, DONidw, TDNidw, DOpctidw)

## Another way to plot all:

NO3.idw <- NO3.idw %>% mutate(Species = "NO3")
NH4.idw <- NH4.idw %>% mutate(Species = "NH4")
DIN.idw <- DIN.idw %>% mutate(Species = "DIN")
DON.idw <- DON.idw %>% mutate(Species = "DON")
TDN.idw <- TDN.idw %>% mutate(Species = "TDN")
DOpct.idw <- DOpct.idw %>% mutate(Species = "DO")
DOmgL.idw <- DOpct.idw %>% mutate(Species = "DO")
#DOC.idw <- DOC.idw %>% mutate(Species = "DOC")

## Normalize individual vectors

## Feature Scaling
NO3.idw <- NO3.idw %>% mutate(valueFS = var1.pred/max(var1.pred))
NH4.idw <- NH4.idw %>% mutate(valueFS = var1.pred/max(var1.pred))
DIN.idw <- DIN.idw %>% mutate(valueFS = var1.pred/max(var1.pred))
DON.idw <- DON.idw %>% mutate(valueFS = var1.pred/max(var1.pred))
TDN.idw <- TDN.idw %>% mutate(valueFS = var1.pred/max(var1.pred))
DOpct.idw <- DOpct.idw %>% mutate(valueFS = var1.pred/max(var1.pred))
DOmgL.idw <- DOmgL.idw %>% mutate(valueFS = var1.pred/max(var1.pred))

## Range scaling
NO3.idw <- NO3.idw %>% mutate(valueMM = (var1.pred - min(var1.pred))/max(var1.pred - min(var1.pred)))
NH4.idw <- NH4.idw %>% mutate(valueMM = (var1.pred - min(var1.pred))/max(var1.pred - min(var1.pred)))
DIN.idw <- DIN.idw %>% mutate(valueMM = (var1.pred - min(var1.pred))/max(var1.pred - min(var1.pred)))
DON.idw <- DON.idw %>% mutate(valueMM = (var1.pred - min(var1.pred))/max(var1.pred - min(var1.pred)))
TDN.idw <- TDN.idw %>% mutate(valueMM = (var1.pred - min(var1.pred))/max(var1.pred - min(var1.pred)))
DOpct.idw <- DOpct.idw %>% mutate(valueMM = (var1.pred - min(var1.pred))/max(var1.pred - min(var1.pred)))
DOmgL.idw <- DOmgL.idw %>% mutate(valueMM = (var1.pred - min(var1.pred))/max(var1.pred - min(var1.pred)))


idwAll <- rbind(NO3.idw, NH4.idw, DIN.idw, DON.idw, TDN.idw, DOmgL.idw)

names(idwAll)[1:3] <- c("lon", "lat", "value")

idwAll$Species_f <- factor(idwAll$Species, levels = c("NO3", "NH4", "DIN", "DON", "TDN", "DO"))

RemovalLong$Season_f <- factor(RemovalLong$Season, levels=c("Spring", "Summer", "Autumn", "Winter"))



Allidw<- ggmap(CCBPmapInternal)+
  geom_sf(data=CChydroArc4326, inherit.aes=FALSE, color="blue", size=1)+
  geom_sf(data=CChydroPoly4326, inherit.aes=FALSE, color="blue",fill="blue")+
  geom_point(data=idwAll, aes(x=lon, y=lat, color=valueMM))+
  facet_wrap(~Species_f)+
  #geom_point(data=Means, aes(x=Longitude, y=Latitude, color = NO3.mgL), size=4)+
  #geom_sf(data=CCBPOutline4326, inherit.aes=FALSE, color="blue",fill = NA, size=1, alpha = 0.8)+
  scale_color_gradient2(name=expression(bold(Conc.)),low="darkblue",mid="green",high="red",midpoint=0.5)+
  theme_bw()+
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 12),
        plot.margin = margin(-1,-1,-1,-1, "pt"),
        panel.border = element_rect(color="black",fill=NA,size=1))
        #legend.position = "bottom",
        #legend.direction = "horizontal",
        #legend.title = element_blank())+
        #legend.position = c(0.89,0.5),
        #legend.background = element_rect(fill = "white", color = "black"),
        #legend.title = element_text(size = 8),
        #legend.margin = margin(0, 0, 0, 0, "pt"))+

Allidw

ggsave(file = paste0("Plots/All_idw_2_", Sys.Date(), ".png"), Allidw)




## None of this really did what I wanted to do

# NO3idwSF <- NO3idwSF %>% st_transform(st_crs(CCBPOutline))
# 
# 
# 
# ## Clip to CCBPOutline
# 
# ## Convert idw data to Spatial Points Data Frame
# NO3idwDF <- data.frame(dat.idw)
# coordinates(NO3idwSPDF) = ~ Var1+Var2
# proj4string(NO3idwSPDF) <- "+proj=longlat +datum=NAD83"
# NO3idwSF <- as(NO3idwSPDF, "sf")
# 
# dat.idwSPoly <- as(dat.idw, "SpatialPolygons")
# 
# ## Convert CCBPOutline shapefile to raster
# CCBPOutlineraster <- terra::rast(CCBPOutline)
# CCBPOutlineraster <- stars::st_rasterize(CCBPOutline)
# CCBPOutlineRaster <- raster(CCBPOutlineraster)
# projection(CCBPOutlineraster) <- "+proj=longlat +datum=NAD83"
# crs(CCBPOutlineraster) <- "+proj=longlat +datum=NAD83"
# 
# 
# NO3idwSF <- NO3idwSF %>% st_transform(st_crs(CCBPOutline))
# 
# dat.idw2 <- as(dat.idw, "sf") %>% st_transform(st_crs(CCBPOutline))
# 
# tmp <- st_crop(dat.idw2, CCBPOutline)
# 
# ## Clip
# tmpcrop <- crop(NO3idwSPDF, CCBPOutlineraster, inverse = FALSE)
# 
# ggplot()
# geom_point(data=NO3idwSPDF, aes(x=))
# 
# 
# NO3tmp <- ggplot()+
#   geom_point(data=NO3idwSPDF, aes(x=Var1,y=Var2, color = var1.pred))+
#   #geom_tile(data = idwDF, aes(x = Var1, y = Var2, fill = var1.pred), alpha = 0.8)+
#   geom_sf(data=CCBPOutline4326, inherit.aes=FALSE, color="blue",fill = NA, size=1, alpha = 0.8)+
#   scale_color_gradient2(name=expression(bold(Temp~(degree~C))),low="darkblue",mid="green",high="red",midpoint=0.015)
#   #scale_color_continuous(low = "green", high = "red")
# NO3tmp
#   
# ggsave(file = paste0("Plots/NO3_IDW_", Sys.Date(), ".png"), NO3tmp)  
#   
#   
# 
# 
# ## Clip to extent of CCBP
# 
# #tmp <- intersect(dat.idw, CCBPOutline)


################################################################################
## Create plots
################################################################################


## Internal sampling plots


bboxInternal<-c(-70.925, 42.7675, -70.915149, 42.7750)
#CCBPmap<-get_stamenmap(bbox, zoom=15, crop=TRUE)
CCBPmapInternal<-get_map(bboxInternal, zoom=15, crop=TRUE, maptype = "satellite", source = "google")
ggmap(CCBPmapInternal)

ggplot(data=Means)+
  geom_col(aes(x=Sample,y=TDN.mgL))+
  #facet_wrap(~Sample)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust=1))

DINInternal <- ggmap(CCBPmapInternal)+
  geom_point(data=Means, aes(x=Longitude, y=Latitude, color = DIN.mgL), size=4)+
  #coord_fixed(xlim=c(-70.945093,-70.915149),ylim=c(42.75073,42.779325),ratio=1)+
  scale_color_gradient2(expression(bold(DIN~(mgL^-1))),low="green",mid="yellow",high="red",midpoint=0.2)+
  labs(x=NULL,y=NULL)+
  theme_bw()
DINInternal

ggsave(file="Plots/DIN_Internal_2022-05-11.png",DINInternal)


DONInternal <- ggmap(CCBPmapInternal)+
  geom_point(data=Means, aes(x=Longitude, y=Latitude, color = DON.mgL), size=4)+
  #coord_fixed(xlim=c(-70.945093,-70.915149),ylim=c(42.75073,42.779325),ratio=1)+
  scale_color_gradient2(expression(bold(DON~(mgL^-1))),low="green",mid="yellow",high="red",midpoint=0.3)+
  labs(x=NULL,y=NULL)+
  theme_bw()
DONInternal

ggsave(file="Plots/DON_Internal_2022-05-11.png",DINInternal)

TDNInternal <- ggmap(CCBPmapInternal)+
  geom_point(data=Means, aes(x=Longitude, y=Latitude, color = TDN.mgL), size=4)+
  #coord_fixed(xlim=c(-70.945093,-70.915149),ylim=c(42.75073,42.779325),ratio=1)+
  scale_color_gradient2(expression(bold(TDN~(mgL^-1))),low="green",mid="yellow",high="red",midpoint=0.55)+
  labs(x=NULL,y=NULL)+
  theme_bw()
TDNInternal

ggsave(file="Plots/DON_Internal_2022-05-11.png",TDNInternal)


DONan <- ggplot()+
  geom_line(data=Internal[Internal$Sample == "CCBP_SIDE",], aes(x=Date, y=DON.mgL))+
  #facet_wrap(~Sample)+
  transition_reveal(Date)

tmpan <- ggplot()+
  geom_line(data=Removal, aes(x=Date, y=-RDIN*100, color="RDIN"))+
  geom_line(data=Removal, aes(x=Date, y=-RDON*100, color="RDON"))+
  theme_bw()+
  transition_reveal(Date)


ggmap(CCBPmap)+
  geom_point(data=Means,(aes(x=Longitude, y=Latitude, color=DIN.mgL)))


## Long-term Cart Creek data

## Some plots to show trends in N species from long-term LTER data at CC

SIGMAlong <- SIGMA %>% pivot_longer(cols=c(NO3.mgL:DIN.mgL,TDN.mgL:DOC.mgL)) %>% as.data.frame()

SIGMAlong$name_f <- NA

SIGMAlong[SIGMAlong$name == c("NO3.mgL"),]$name_f <- "NO3"
SIGMAlong[SIGMAlong$name == c("DIN.mgL"),]$name_f <- "DIN"
SIGMAlong[SIGMAlong$name == c("DON.mgL"),]$name_f <- "DON"
SIGMAlong[SIGMAlong$name == c("TDN.mgL"),]$name_f <- "TDN"
SIGMAlong[SIGMAlong$name == c("NH4.mgL"),]$name_f <- "NH4"
SIGMAlong[SIGMAlong$name == c("DOC.mgL"),]$name_f <- "DOC"

SIGMAlong$name_f <- factor(SIGMAlong$name_f, levels = c("NO3", "DIN", "DON","TDN","NH4", "DOC"),
                           labels=c("NO3" = "bold(NO[3])", "DIN" = "bold(DIN)","DON" = "bold(DON)",
                                    "TDN" = "bold(TDN)", "NH4" = "bold(NH[4])", "DOC" = "bold(DOC)"))


CCN <- ggplot()+
  geom_point(data=SIGMAlong, aes(x=Date, y=value))+
  geom_smooth(data=SIGMAlong, aes(x=Date, y=value), formula = 'y~x', method="lm")+
  labs(x="", y=expression(bold(Concentration~(mgL^-1))))+
  facet_wrap(~name_f, nrow=3, ncol=2, scales="free_y", labeller = label_parsed)+
  theme_bw()+
  theme(panel.border = element_rect(color="black",fill=NA,size=1), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text=element_text(size=12,color="black"),
        axis.title=element_text(face="bold",size=13), axis.title.y=element_text(vjust=2),
        axis.title.x=element_text(vjust=0),legend.title = element_blank(),legend.key=element_blank(),
        plot.title=element_text(size=13),legend.text=element_text(size=9),legend.position="bottom",
        strip.background = element_blank(),strip.text.x = element_text(face="bold",size=12))#,
        #plot.margin=unit(c(5.5,5.5,0,5.5),"pt"),legend.box.margin=margin(t=-0.3,unit='cm'))
CCN

ggsave(file=paste0("Plots/CCN_lm_", Sys.Date(), ".png"), CCN)

## Linear models for N vs Date figure
summary(lm(NO3.mgL~Date, SIGMA)) #p = 0.24, sloep = -3.48e-06
summary(lm(DIN.mgL~Date, SIGMA)) #p = 0.09, slope = -5.8e-06
summary(lm(DON.mgL~Date, SIGMA)) #p = 0.29, slope = 6.77e-06
summary(lm(TDN.mgL~Date, SIGMA)) #p = 0.13, slope = 1.08e-06
summary(lm(NH4.mgL~Date, SIGMA)) #p = 0.51, slope = -1.45e-06
summary(lm(DOC.mgL~Date, SIGMA)) #p = 0.29, slope = 1.53e-04



## Combo CC and CCBP_OUT

ggplot()+
  geom_point(data=GRABS[GRABS$Site=="CCBP",],aes(x=as.Date(Date),y=DON.mgL, color="CCBP"))+
  geom_point(data=SIGMA,aes(x=Date,y=DON.mgL, color="CC"))+
  geom_smooth(data=GRABS[GRABS$Site=="CCBP",],aes(x=as.Date(Date),y=DON.mgL, color="CCBP"), formula = 'y~x', method="lm", fullrange=F)+
  geom_smooth(data=SIGMA,aes(x=Date,y=DON.mgL, color="CC"), formula = 'y~x', method="lm")+
  theme_bw()

NO3GRABS <- lm(NO3.mgL~Date,GRABS[GRABS$Site=="CCBP",])
NO3SIGMA <- lm(NO3.mgL~Date,SIGMA)
               
               
               
               
################### CCBP long-term mass balance #############
#GRABS$NH4.mgL <- GRABS$NH4.ugL/1000

GRABSlong <- GRABS %>% filter(Site == "CCBP") %>% select(c(Sample:Date,DO.pct, NH4.mgL, NO3.mgL, DOC.mgL, TDN.mgL, DIN.mgL,DON.mgL,
                                                           DO.pct.kgkm2Day, NH4.kgkm2Day, NO3.kgkm2Day, DOC.kgkm2Day, TDN.kgkm2Day,
                                                           DIN.kgkm2Day, DON.kgkm2Day)) %>% 
  pivot_longer(cols=c(DO.pct:DON.kgkm2Day)) %>% as.data.frame()


GRABSlong$name_f <- NA

GRABSlong[GRABSlong$name == c("NO3.mgL", "NO3.kgkm2Day"),]$name_f <- "NO3"
GRABSlong[GRABSlong$name == c("DIN.kgkm2Day","DIN.mgL"),]$name_f <- "DIN" #, "DIN.kgkm2Day"
GRABSlong[GRABSlong$name == c("DON.mgL", "DON.kgkm2Day"),]$name_f <- "DON"
GRABSlong[GRABSlong$name == c("TDN.mgL", "TDN.kgkm2Day"),]$name_f <- "TDN"
GRABSlong[GRABSlong$name == c("NH4.kgkm2Day","NH4.mgL"),]$name_f <- "NH4"
GRABSlong[GRABSlong$name == c("DOC.kgkm2Day","DOC.mgL"),]$name_f <- "DOC"
GRABSlong[GRABSlong$name == c("DO.pct", "DO.pct.kgkm2Day"),]$name_f <- "DO"

GRABSlong$name_f <- factor(GRABSlong$name_f, levels = c("NO3", "DIN", "DON","TDN","NH4", "DOC", "DO"),
                           labels=c("NO3" = "bold(NO[3])", "DIN" = "bold(DIN)","DON" = "bold(DON)",
                                    "TDN" = "bold(TDN)", "NH4" = "bold(NH[4])", "DOC" = "bold(DOC)",
                                    "DO" = "bold(DO)"))

GRABSlong$Location_f <- factor(GRABSlong$Location, levels=c("UP_1", "UP_2", "UP_3", "SIDE", "OUT"),
                               labels = c("UP_1" = "Input 1", "UP_2" = "Input 2", "UP_3" = "Input 3",
                                          "SIDE" = "Sidepool", "OUT" = "Output"))

GRABSlong[grep("kgkm2Day",GRABSlong$name),]
GRABSlong[grep("mgL",GRABSlong$name),]

GRABSlongconc <- GRABSlong[grep("mgL", GRABSlong$name),]
GRABSlong


## Row 154 is fucked in 'GRABS'. Low Cl values and high N values...

CCBP_Cout <- ggplot()+
  geom_point(data=GRABSlongconc[!GRABSlongconc$name == "DO.pct" & GRABSlongconc$Location == "OUT",], aes(x=Date, y=value))+
  geom_smooth(data=GRABSlongconc[!GRABSlongconc$name == "DO.pct" & GRABSlongconc$Location == "OUT",], aes(x=Date, y=value), formula = 'y~x', method="lm")+
  labs(x="", y=expression(bold(Concentration~(mgL^-1))))+
  facet_wrap(~name_f, nrow=2, ncol=3, scales="free_y", labeller = label_parsed)+
  lims(y=c(0,NA))+
  theme_bw()+
  theme(panel.border = element_rect(color="black",fill=NA,size=1), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text=element_text(size=12,color="black"),
        axis.title=element_text(face="bold",size=13), axis.title.y=element_text(vjust=2),
        axis.title.x=element_text(vjust=0),legend.title = element_blank(),legend.key=element_blank(),
        plot.title=element_text(size=13),legend.text=element_text(size=9),legend.position="bottom",
        strip.background = element_blank(),strip.text.x = element_text(face="bold",size=12))#,
CCBP_Cout

ggsave(file=paste0("Plots/CCBP_cOUT_lm_", Sys.Date(), ".png"), CCBP_Cout)

##Input/Output boxplot

NO3HSD <- as.data.frame(agricolae::HSD.test(aov(NO3.mgL~Location,GRABS[!GRABS$Location == "DOWN",]),trt="Location",group=TRUE)$groups)

print(agricolae::HSD.test(aov(NO3.mgL~Location,GRABS[!GRABS$Location == "DOWN" & !GRABS$Location=="UP",]),trt="Location",group=TRUE))
print(agricolae::HSD.test(aov(DIN.mgL~Location,GRABS[!GRABS$Location == "DOWN" & !GRABS$Location=="UP",]),trt="Location",group=TRUE))
print(agricolae::HSD.test(aov(DON.mgL~Location,GRABS[!GRABS$Location == "DOWN" & !GRABS$Location=="UP",]),trt="Location",group=TRUE))
print(agricolae::HSD.test(aov(TDN.mgL~Location,GRABS[!GRABS$Location == "DOWN" & !GRABS$Location=="UP",]),trt="Location",group=TRUE))
print(agricolae::HSD.test(aov(NH4.mgL~Location,GRABS[!GRABS$Location == "DOWN" & !GRABS$Location=="UP",]),trt="Location",group=TRUE))
print(agricolae::HSD.test(aov(DOC.mgL~Location,GRABS[!GRABS$Location == "DOWN" & !GRABS$Location=="UP",]),trt="Location",group=TRUE))



ann_text<-data.frame(
  label=c("a","a","a","a","a",
          "b","ab","a","ab","ab",
          "b","ab","a","a","ab",
          "b","a","a","ab","ab",
          "b","ab","a","b","b",
          "b","a","ab","ab","ab"),
  name_f=factor(c("bold(NO[3])","bold(NO[3])","bold(NO[3])","bold(NO[3])","bold(NO[3])",
           "bold(DIN)","bold(DIN)","bold(DIN)","bold(DIN)","bold(DIN)",
            "bold(DON)","bold(DON)","bold(DON)","bold(DON)","bold(DON)",
            "bold(TDN)","bold(TDN)","bold(TDN)","bold(TDN)","bold(TDN)",
            "bold(NH[4])","bold(NH[4])","bold(NH[4])","bold(NH[4])","bold(NH[4])",
            "bold(DOC)","bold(DOC)","bold(DOC)","bold(DOC)","bold(DOC)"), levels=c("bold(NO[3])","bold(DIN)","bold(DON)","bold(TDN)",
                                                                                   "bold(NH[4])", "bold(DOC)")),
  x=c(1,2,3,4,5,
      1,2,3,4,5,
      1,2,3,4,5,
      1,2,3,4,5,
      1,2,3,4,5,
      1,2,3,4,5),
  y=c(0.3,0.4,0.5,1.3,1.75,
      0.6,0.85,0.9,1.4,0.5,
      0.6,0.75,0.75,0.8,0.75,
      1.2,1.25,1.4,1.7,1.0,
      0.5,0.75,0.8,0.3,0.4,
      18,26.5,21,18.5,21.5)
)

### Do the same with fluxes

GRABSlongFlux <- GRABSlong[grep("kgkm2Day",GRABSlong$name),]
GRABSlongFlux <- GRABSlongFlux[!GRABSlongFlux$Location == "DOWN" & !GRABSlongFlux$Location == "SIDE",]
GRABSlongFlux <- GRABSlongFlux[GRABSlongFlux$value < 900 & !is.na(GRABSlongFlux$value),]

head(GRABS %>% filter(Location -c("DOWN", "SIDE", "UP")))

print(agricolae::HSD.test(aov(NO3.kgkm2Day~Location,GRABS[!GRABS$Location == "DOWN" & !GRABS$Location=="UP" & !GRABS$Location == "SIDE" & GRABS$NO3.kgkm2Day < 5000,]),trt="Location",group=TRUE))
print(agricolae::HSD.test(aov(DIN.kgkm2Day~Location,GRABS[!GRABS$Location == "DOWN" & !GRABS$Location=="UP" & !GRABS$Location == "SIDE" & GRABS$DIN.kgkm2Day < 900,]),trt="Location",group=TRUE))
print(agricolae::HSD.test(aov(DON.kgkm2Day~Location,GRABS[!GRABS$Location == "DOWN" & !GRABS$Location=="UP" & !GRABS$Location == "SIDE",]),trt="Location",group=TRUE))
print(agricolae::HSD.test(aov(TDN.kgkm2Day~Location,GRABS[!GRABS$Location == "DOWN" & !GRABS$Location=="UP" & !GRABS$Location == "SIDE",]),trt="Location",group=TRUE))
print(agricolae::HSD.test(aov(NH4.kgkm2Day~Location,GRABS[!GRABS$Location == "DOWN" & !GRABS$Location=="UP" & !GRABS$Location == "SIDE" & GRABS$NH4.kgkm2Day < 800,]),trt="Location",group=TRUE))
print(agricolae::HSD.test(aov(DOC.kgkm2Day~Location,GRABS[!GRABS$Location == "DOWN" & !GRABS$Location=="UP" & !GRABS$Location == "SIDE" & GRABS$DOC.kgkm2Day < 800,]),trt="Location",group=TRUE))

summary(lm(NO3.kgkm2Day~Location,GRABS[!GRABS$Location == "DOWN" & !GRABS$Location=="UP" & !GRABS$Location == "SIDE" & GRABS$NO3.kgkm2Day < 5000,]))
summary(lm(DIN.kgkm2Day~Location,GRABS[!GRABS$Location == "DOWN" & !GRABS$Location=="UP" & !GRABS$Location == "SIDE" & GRABS$NO3.kgkm2Day < 900,]))
summary(lm(DON.kgkm2Day~Location,GRABS[!GRABS$Location == "DOWN" & !GRABS$Location=="UP" & !GRABS$Location == "SIDE",]))
summary(lm(TDN.kgkm2Day~Location,GRABS[!GRABS$Location == "DOWN" & !GRABS$Location=="UP" & !GRABS$Location == "SIDE",]))
summary(lm(NH4.kgkm2Day~Location,GRABS[!GRABS$Location == "DOWN" & !GRABS$Location=="UP" & !GRABS$Location == "SIDE" & GRABS$NO3.kgkm2Day < 800,]))
summary(lm(DOC.kgkm2Day~Location,GRABS[!GRABS$Location == "DOWN" & !GRABS$Location=="UP" & !GRABS$Location == "SIDE" & GRABS$NO3.kgkm2Day < 800,]))




CCBPconc <- ggplot()+
  #geom_boxplot(data=GRABSlong[!GRABSlong$name == "DO.pct" & !GRABSlong$Location == "DOWN",],aes(x=Location_f,y=value))+
  geom_boxplot(data=GRABSlongFlux[!GRABSlongFlux$name == "DO.pct.kgkm2Day" & !GRABSlongFlux$Location == "DOWN",],aes(x=Location_f,y=value))+
  #geom_text(data=ann_text,mapping=aes(x=x,y=y,label=label))+
  facet_wrap(~name_f,scales="free_y", nrow=3, label=label_parsed)+
  scale_x_discrete("")+
  scale_y_continuous(expression(bold(Concentration~(kg~km^"-2"~Day^"-1"))))+
  theme_bw()+
  theme(panel.border = element_rect(color="black",fill=NA,size=1), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text=element_text(size=12,color="black"), axis.text.x = element_text(angle=45, hjust=1),
        axis.title=element_text(face="bold",size=13), axis.title.y=element_text(vjust=2),
        axis.title.x=element_text(vjust=0),legend.title = element_blank(),legend.key=element_blank(),
        plot.title=element_text(size=13),legend.text=element_text(size=9),legend.position="bottom",
        strip.background = element_blank(),strip.text.x = element_text(face="bold",size=12))

CCBPconc

ggsave(file=paste0("Plots/CCBP_conc_",Sys.Date(),".png"),CCBPconc)



## Means at different CCBP locations
mean(GRABS[GRABS$Sample=="CCBP_UP_1",]$NO3.mgL,na.rm=TRUE)
mean(GRABS[GRABS$Sample=="CCBP_UP_2",]$NO3.mgL,na.rm=TRUE)
mean(GRABS[GRABS$Sample=="CCBP_UP_3",]$NO3.mgL,na.rm=TRUE)
mean(GRABS[GRABS$Sample=="CCBP_SIDE",]$NO3.mgL,na.rm=TRUE)
mean(GRABS[GRABS$Sample=="CCBP_OUT",]$NO3.mgL,na.rm=TRUE)
mean(GRABS[GRABS$Sample=="CC",]$NO3.mgL,na.rm=TRUE)


## CCBP Removal
RemovalLong <- Removal %>% filter(Site == "CCBP" & RTDN > -24 & RFlag2 == 0) %>% select(c(Date, HL.myr, Discharge.m3s.OUT, Season, Temp.C.OUT, RNH4, RNO3, RDOC, RTDN, RDON, RDIN)) %>%
  pivot_longer(cols=c(RNH4:RDIN)) %>% as.data.frame()


RemovalLong$name_f <- NA

RemovalLong$name_f <- factor(RemovalLong$name, levels = c("RNO3", "RDIN", "RDON","RTDN","RNH4", "RDOC"),
                           labels=c("NO3" = "bold(NO[3])", "DIN" = "bold(DIN)","DON" = "bold(DON)",
                                    "TDN" = "bold(TDN)", "NH4" = "bold(NH[4])", "DOC" = "bold(DOC)"))
RemovalLong$Season_f <- factor(RemovalLong$Season, levels=c("Spring", "Summer", "Autumn", "Winter"))


CCBPR <- ggplot()+
  geom_point(data=RemovalLong, aes(x=HL.myr, y=-value*100, shape=Season_f), size=1.5, stroke=1.5)+
  #geom_smooth(data=RemovalLong[RemovalLong$name == "RDOC",], aes(x=HL.myr, y=-value*100), formula='y~x', method="lm", color = "black")+
  geom_hline(yintercept = 0)+
  scale_x_continuous(limits=c(NA, 300),trans="log10")+
  labs(x=expression(bold(HL~(m~yr^-1))), y=expression(bold(Delta~Concentration~("%"))))+
  facet_wrap(~name_f, nrow=3, ncol=2, scales="free_y", labeller = label_parsed)+
  #lims(x=c(NA,300))+
  theme_bw()+
  theme(panel.border = element_rect(color="black",fill=NA,size=1), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text=element_text(size=12,color="black"),
        axis.title=element_text(face="bold",size=13), axis.title.y=element_text(vjust=2),
        axis.title.x=element_text(vjust=0),legend.title = element_blank(),legend.key=element_blank(),
        plot.title=element_text(size=13),legend.text=element_text(size=9),legend.position="bottom",
        strip.background = element_blank(),strip.text.x = element_text(face="bold",size=12))
CCBPR

ggsave(file=paste0("Plots/CCBPR_HL_", Sys.Date(), ".png"), CCBPR)


CCBPRNO3tmp <- ggplot()+
  geom_point(data=Removal[Removal$RFlag2==0,],aes(x=HL.myr,y=-RNO3*100))+
  scale_x_continuous(trans = "log10")+
  labs(x=expression(bold(HL~(m~yr^-1))), y=expression(bold(Delta~NO3~("%"))))
CCBPRNO3tmp

CCBPRNO3tmp2 <- ggplot()+
  geom_point(data=Removal[Removal$RFlag2==0,],aes(x=HL.myr,y=-RNO32*100))+
  scale_x_continuous(trans = "log10")+
  labs(x=expression(bold(HL~(m~yr^-1))), y=expression(bold(Delta~NO3~("%"))))
CCBPRNO3tmp2

CCBPRNO3 <- cowplot::plot_grid(CCBPRNO3tmp, CCBPRNO3tmp2)
ggsave(file = paste0("Plots/CCBPRNO3_",Sys.Date(),".png"), CCBPRNO3)

CCBPRCl <- ggplot()+
  geom_point(data=Removal[Removal$RFlag2==0 & !is.na(Removal$Season),],aes(x=HL.myr, y=-RCl*100), size=1.5, stroke=1.5, color="red")+
  geom_point(data=Removal[!Removal$RFlag2==0 & !is.na(Removal$Season),],aes(x=HL.myr, y=-RCl*100), size=1.5, stroke=1.5, color="black")+
  geom_hline(yintercept=c(-20,0,20))+
  scale_x_continuous(limits=c(NA,300), trans="log10")+
  labs(x=expression(bold(HL~(m~yr^-1))), y=expression(bold(Delta*Cl~("%"))))+
  theme_bw()+
  theme(panel.border = element_rect(color="black",fill=NA,size=1), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text=element_text(size=12,color="black"),
        axis.title=element_text(face="bold",size=13), axis.title.y=element_text(vjust=2),
        axis.title.x=element_text(vjust=0),legend.title = element_blank(),legend.key=element_blank(),
        plot.title=element_text(size=13),legend.text=element_text(size=9),legend.position="bottom",
        strip.background = element_blank(),strip.text.x = element_text(face="bold",size=12))
CCBPRCl

ggsave(file=paste0("Plots/CCBPRCl_HL_", Sys.Date(), ".png"), CCBPRCl)


#### Input/output concentrations at CCBP
Removal$NH4.mgL.OUT <- Removal$NH4.ugL.OUT/1000
Removal$NH4.mgL.UP <- Removal$NH4.ugL.UP/1000

RemovalLongConc <- Removal %>% filter(Site == "CCBP") %>% select(c(Date, HL.myr, Season, NO3.mgL.UP, DIN.mgL.UP, DON.mgL.UP,
                                                                   TDN.mgL.UP, NH4.mgL.UP, DOC.mgL.UP, NO3.mgL.OUT, DIN.mgL.OUT,
                                                                   DON.mgL.OUT, TDN.mgL.OUT, NH4.mgL.OUT, DOC.mgL.OUT)) %>%
  pivot_longer(cols=c(NO3.mgL.UP:DOC.mgL.OUT)) %>% as.data.frame()

## Add 'Location' factor variable
RemovalLongConc <- RemovalLongConc %>% mutate(Location = ifelse(grepl(".OUT",RemovalLongConc$name), "OUTPUT", "INPUT"))
RemovalLongConc$Location <- factor(RemovalLongConc$Location, levels=c("INPUT", "OUTPUT"))

## Add 'Species' factor variable
RemovalLongConc <- RemovalLongConc %>% mutate(Species = stringr::str_extract(name, "[^.]+"))
RemovalLongConc$Species <- factor(RemovalLongConc$Species, levels=c("NO3", "DIN", "DON", "TDN", "NH4", "DOC"),
                                  labels = c("NO3" = "bold(NO[3])", "DIN" = "bold(DIN)", "DON" = "bold(DON)",
                                             "TDN" = "bold(TDN)", "NH4" = "bold(NH[4])", "DOC" = "bold(DOC)"))

ggplot()+
  geom_boxplot(data=RemovalLongConc, aes(x=Location, y=value))+
  facet_wrap(~Species, nrow = 3,scales="free_y", labeller = label_parsed)+
  theme_bw()


######## Same as above but using 'GRABS' data

GRABSlong



################### SYNOPTIC SAMPLING DATA

## 

# bboxSynoptic<-c(-70.945093, 42.750739, -70.915149, 42.779325)
# #CCBPmap<-get_stamenmap(bbox, zoom=15, crop=TRUE)
# CCBPmapSynoptic<-get_map(bboxSynoptic, zoom=15, crop=TRUE)# maptype="terrain")
# ggmap(CCBPmapSynoptic)


CCBPmapSynoptic <- get_googlemap(center=c(lon = -70.935, lat = 42.767741), maptype = "satellite", zoom = 14)
ggmap(CCBPmapSynoptic)



ggmap(CCBPmapSynoptic)+
  geom_point(data=Synoptic,aes(x=Longitude, y=Latitude, color=NO3.mgL), size=4)


CCOutline <- st_read("Data/Spatial/CCBP_Outline/CCBP_Outline.shp")
CCOutlineSZ <- st_read("Data/Spatial/CCBP_Outline_SZ/CCBP_Outline_SZ.shp")
CChydroArc <- st_read("Data/Spatial/CCBP_Hydro_Arc2/CCBP_Hydro_Arc.shp")
CChydroPoly <- st_read("Data/Spatial/CCBP_Hydro_Poly2/CCBP_Hydro_Poly.shp")

ParkerhydroArc <- st_read("Data/Spatial/Parker_Hydro_Arc/Parker_Hydro.shp")

#CChydroArc3857 <- st_transform(CChydroArc, 3857)[1]

CCOutline4326 <- st_transform(CCOutline, "EPSG:4326")
CCOutlineSZ4326 <- st_transform(CCOutlineSZ, "EPSG:4326")
CChydroArc4326 <- st_transform(CChydroArc, "EPSG:4326")
CChydroPoly4326 <- st_transform(CChydroPoly, "EPSG:4326")

ParkerhydroArc4326 <- st_transform(ParkerhydroArc, "EPSG:4326")


# attr(CCBPmapSynoptic, "bb")$ll.lat <- st_bbox(CChydroArc3857)["ymin"]
# attr(CCBPmapSynoptic, "bb")$ll.lon <- st_bbox(CChydroArc3857)["xmin"]
# attr(CCBPmapSynoptic, "bb")$ur.lat <- st_bbox(CChydroArc3857)["ymax"]
# attr(CCBPmapSynoptic, "bb")$ur.lon <- st_bbox(CChydroArc3857)["xmax"]


## Left off on this...somewhere around joining the atts to the 2021 summary............

SynopticMeans <- Synoptic %>%
  group_by(Site) %>%
  summarize(across(c(Latitude:DO.pct, DistFromMouth.m,narsat:DON.mgL), ~mean(., na.rm=TRUE))) %>%
  left_join(., unique(Synoptic[,c(1,16,17)]), by="Site")
  
SynopticMeans$WBType <- factor(SynopticMeans$WBType, levels=c("Pond_Outflow", "Channel"), labels=c("Pond_Outflow" = "Pond", "Channel" = "Channel"))

SynopticSites <- ggmap(CCBPmapSynoptic)+
  coord_sf(crs=4326,xlim=c(-7897580, -7893501 ), ylim=c(5273954, 5278369))+
  geom_sf(data=CChydroArc4326, inherit.aes=FALSE, color="blue", size=1)+
  geom_sf(data=CChydroPoly4326, inherit.aes=FALSE, color="blue",fill="blue")+
  #geom_sf(data=CCOutline4326, inherit.aes=FALSE, color="yellow", fill=NA, size=1)+
  geom_sf(data=CCOutlineSZ4326, inherit.aes=FALSE, color="red", fill=NA, size=1)+
  geom_point(data=SynopticMeans[!is.na(SynopticMeans$NH4.ugL),],aes(x=Longitude,y=Latitude, shape = WBType), color = "green",size=4)+
  #coord_sf(crs=3857,xlim=c(-7897580, -7893501 ), ylim=c(5273954, 5278369))+
  #coord_fixed(xlim=c(-70.945093,-70.908),ylim=c(42.75,42.779325),ratio=1)+
  scale_color_gradient2(expression(bold(NH[4]~(mgL^-1))),low="green",mid="yellow",high="red",midpoint=0.4)+
  scale_shape_manual("", values=c("Channel" = 17, "Pond" = 19))+
  
  labs(x=NULL,y=NULL)+
  theme_bw()+
  guides(color = guide_legend(order = 1), shape = guide_legend(order = 2))
#theme(axis.text=element_blank())
SynopticSites

ggsave(file=paste0("Plots/SynopticSites_",Sys.Date(),".png"),SynopticSites)


NH4Synoptic <- ggmap(CCBPmapSynoptic)+
  coord_sf(crs=4326,xlim=c(-7897580, -7893501 ), ylim=c(5273954, 5278369))+
  geom_sf(data=CChydroArc4326, inherit.aes=FALSE, color="blue", size=1)+
  geom_sf(data=CChydroPoly4326, inherit.aes=FALSE, color="blue",fill="blue")+
  #geom_sf(data=CCOutline4326, inherit.aes=FALSE, color="yellow", fill=NA, size=1)+
  geom_sf(data=CCOutlineSZ4326, inherit.aes=FALSE, color="red", fill=NA, size=1)+
  geom_point(data=SynopticMeans[!is.na(SynopticMeans$NH4.ugL),],aes(x=Longitude,y=Latitude,color= NH4.ugL/1000, shape = WBType),size=4)+
  #coord_sf(crs=3857,xlim=c(-7897580, -7893501 ), ylim=c(5273954, 5278369))+
  #coord_fixed(xlim=c(-70.945093,-70.908),ylim=c(42.75,42.779325),ratio=1)+
  scale_color_gradient2(expression(bold(NH[4]~(mgL^-1))),low="green",mid="yellow",high="red",midpoint=0.4)+
  scale_shape_manual("", values=c("Channel" = 17, "Pond" = 19))+
  
  labs(x=NULL,y=NULL)+
  theme_bw()+
  guides(color = guide_colorbar(order = 1), shape = guide_legend(order = 2))
  #theme(axis.text=element_blank())
NH4Synoptic

ggsave(file=paste0("Plots/NH4_Synoptic_",Sys.Date(),".png"),NH4Synoptic)


NO3Synoptic <- ggmap(CCBPmapSynoptic)+
  coord_sf(crs=4326,xlim=c(-7897580, -7893501 ), ylim=c(5273954, 5278369))+
  geom_sf(data=CChydroArc4326, inherit.aes=FALSE, color="blue", size=1)+
  geom_sf(data=CChydroPoly4326, inherit.aes=FALSE, color="blue",fill="blue")+
  #geom_sf(data=CCOutline4326, inherit.aes=FALSE, color="yellow", fill=NA, size=1)+
  geom_sf(data=CCOutlineSZ4326, inherit.aes=FALSE, color="red", fill=NA, size=1)+
  geom_point(data=SynopticMeans[!is.na(SynopticMeans$NO3.mgL),],aes(x=Longitude,y=Latitude,color= NO3.mgL, shape=WBType),size=4)+
  #coord_fixed(xlim=c(-70.945093,-70.915149),ylim=c(42.75073,42.779325),ratio=1)+
  scale_color_gradient2(expression(bold(NO[3]~(mgL^-1))),low="green",mid="yellow",high="red",midpoint=0.25)+
  scale_shape_manual("", values=c("Channel" = 17, "Pond" = 19))+
  labs(x=NULL,y=NULL)+
  theme_bw()+
  #theme(legend.position = "bottom", legend.title = element_text(vjust = 1, hjust = 0.5))
  theme(axis.text=element_blank())+
  guides(color = guide_colorbar(order = 1), shape = guide_legend(order = 2))
NO3Synoptic

ggsave(file=paste0("Plots/NO3_Synoptic_", Sys.Date(),".png"),NO3Synoptic)

DONSynoptic <- ggmap(CCBPmapSynoptic)+
  coord_sf(crs=4326,xlim=c(-7897580, -7893501 ), ylim=c(5273954, 5278369))+
  geom_sf(data=CChydroArc4326, inherit.aes=FALSE, color="blue", size=1)+
  geom_sf(data=CChydroPoly4326, inherit.aes=FALSE, color="blue",fill="blue")+
  #geom_sf(data=CCOutline4326, inherit.aes=FALSE, color="yellow", fill=NA, size=1)+
  geom_sf(data=CCOutlineSZ4326, inherit.aes=FALSE, color="red", fill=NA, size=1)+
  geom_point(data=SynopticMeans[!is.na(SynopticMeans$DON.mgL),],aes(x=Longitude,y=Latitude,color= DON.mgL, shape=WBType),size=4)+
  #coord_fixed(xlim=c(-70.945093,-70.915149),ylim=c(42.75073,42.779325),ratio=1)+
  scale_color_gradient2(expression(bold(DON~(mgL^-1))),low="green",mid="yellow",high="red",midpoint=0.4)+ #0.4
  scale_shape_manual("", values=c("Channel" = 17, "Pond" = 19))+
  labs(x=NULL,y=NULL)+
  theme_bw()+
  #theme(legend.position = "bottom", legend.title = element_text(vjust = 1, hjust = 0.5))
  theme(axis.text=element_blank())+
  guides(color = guide_colorbar(order = 1), shape = guide_legend(order = 2))
DONSynoptic

ggsave(file=paste0("Plots/DON_Synoptic_", Sys.Date(),".png"),DONSynoptic)


TDNSynoptic <- ggmap(CCBPmapSynoptic)+
  coord_sf(crs=4326,xlim=c(-7897580, -7893501 ), ylim=c(5273954, 5278369))+
  geom_sf(data=CChydroArc4326, inherit.aes=FALSE, color="blue", size=1)+
  geom_sf(data=CChydroPoly4326, inherit.aes=FALSE, color="blue",fill="blue")+
  #geom_sf(data=CCOutline4326, inherit.aes=FALSE, color="yellow", fill=NA, size=1)+
  geom_sf(data=CCOutlineSZ4326, inherit.aes=FALSE, color="red", fill=NA, size=1)+
  geom_point(data=SynopticMeans[!is.na(SynopticMeans$TDN.mgL),],aes(x=Longitude,y=Latitude,color= TDN.mgL, shape=WBType),size=4)+
  #coord_fixed(xlim=c(-70.945093,-70.915149),ylim=c(42.75073,42.779325),ratio=1)+
  scale_color_gradient2(expression(bold(TDN~(mgL^-1))),low="green",mid="yellow",high="red",midpoint=0.6)+
  scale_shape_manual("", values=c("Channel" = 17, "Pond" = 19))+
  labs(x=NULL,y=NULL)+
  theme_bw()+
  #theme(legend.position = "bottom", legend.title = element_text(vjust = 1, hjust = 0.5))
  theme(axis.text=element_blank())+
  guides(color = guide_colorbar(order = 1), shape = guide_legend(order = 2))
TDNSynoptic

ggsave(file=paste0("Plots/TDN_Synoptic_", Sys.Date(),".png"),TDNSynoptic)


NArdisSynoptic <- ggmap(CCBPmapSynoptic)+
  coord_sf(crs=4326,xlim=c(-7897580, -7893501 ), ylim=c(5273954, 5278369))+
  geom_sf(data=CChydroArc4326, inherit.aes=FALSE, color="blue", size=1)+
  geom_sf(data=CChydroPoly4326, inherit.aes=FALSE, color="blue",fill="blue")+
  #geom_sf(data=CCOutline4326, inherit.aes=FALSE, color="yellow", fill=NA, size=1)+
  geom_sf(data=CCOutlineSZ4326, inherit.aes=FALSE, color="red", fill=NA, size=1)+
  geom_point(data=SynopticMeans[!is.na(SynopticMeans$nardiseq),],aes(x=Longitude,y=Latitude,color= nardiseq, shape=WBType),size=4)+
  #coord_fixed(xlim=c(-70.945093,-70.915149),ylim=c(42.75073,42.779325),ratio=1)+
  scale_color_gradient2(expression(bold(N["2"]*":"*Ar~Disequilibrium)),low="green",mid="yellow",high="red",midpoint=0)+
  scale_shape_manual("", values=c("Channel" = 17, "Pond" = 19))+
  labs(x=NULL,y=NULL)+
  theme_bw()+
  #theme(legend.position = "bottom", legend.title = element_text(vjust = 1, hjust = 0.5))
  theme(axis.text=element_blank())+
  guides(color = guide_colorbar(order = 1), shape = guide_legend(order = 2))
NArdisSynoptic

ggsave(file=paste0("Plots/NArdis_Synoptic_", Sys.Date(),".png"),NArdisSynoptic)









TDNSynoptic <- ggmap(CCBPmapSynoptic)+
  coord_sf(crs=4326,xlim=c(-7897580, -7893501 ), ylim=c(5273954, 5278369))+
  geom_sf(data=CChydroArc4326, inherit.aes=FALSE, color="blue", size=1)+
  geom_sf(data=CChydroPoly4326, inherit.aes=FALSE, color="blue",fill="blue")+
  geom_point(data=Synoptic[Synoptic$Campaign == 1 & !is.na(Synoptic$TDN.mgL),],aes(x=Longitude,y=Latitude,color= TDN.mgL, shape=WBType),size=4)+
  #geom_sf(data=Synoptic[Synoptic$Campaign == 1 & !is.na(Synoptic$NH4.ugL),],aes(color= NH4.ugL/1000),size=4)+
  coord_fixed(xlim=c(-70.945093,-70.908),ylim=c(42.75073,42.779325),ratio=1)+
  scale_color_gradient2(expression(bold(TDN~(mgL^-1))),low="green",mid="yellow",high="red",midpoint=0.75)+
  labs(x=NULL,y=NULL)+
  theme_bw()
#theme(axis.text=element_blank())
TDNSynoptic

DONSynoptic <- ggmap(CCBPmapSynoptic)+
  coord_sf(crs=4326,xlim=c(-7897580, -7893501 ), ylim=c(5273954, 5278369))+
  geom_sf(data=CChydroArc4326, inherit.aes=FALSE, color="blue", size=1)+
  geom_sf(data=CChydroPoly4326, inherit.aes=FALSE, color="blue",fill="blue")+
  geom_point(data=Synoptic[Synoptic$Campaign == 1 & !is.na(Synoptic$DON.mgL),],aes(x=Longitude,y=Latitude,color= DON.mgL, shape=WBType),size=4)+
  #geom_sf(data=Synoptic[Synoptic$Campaign == 1 & !is.na(Synoptic$NH4.ugL),],aes(color= NH4.ugL/1000),size=4)+
  #coord_fixed(xlim=c(-70.945093,-70.915149),ylim=c(42.75073,42.779325),ratio=1)+
  scale_color_gradient2(expression(bold(DON~(mgL^-1))),low="green",mid="yellow",high="red",midpoint=0.4)+
  labs(x=NULL,y=NULL)+
  theme_bw()+
  theme(axis.text=element_blank())
DONSynoptic

##Physicochemical parameters vs WBType

## DO vs WBType
DOWB <- ggplot()+
  geom_boxplot(data=SynopticMeans, aes(x = WBType, y=DO.pct))+
  scale_x_discrete("")+
  scale_y_continuous(expression(bold(DO~("%"))))+
  theme_bw()+
  theme(panel.border = element_rect(color="black",fill=NA,size=1), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text=element_text(size=12,color="black", face = "bold"),
        axis.title=element_text(face="bold",size=13), axis.title.y=element_text(vjust=2),
        axis.title.x=element_text(vjust=0),legend.title = element_blank(),legend.key=element_blank(),
        plot.title=element_text(size=13),legend.text=element_text(size=9),legend.position="bottom",
        strip.background = element_blank(),strip.text.x = element_text(face="bold",size=12))
DOWB

ggsave(file=paste0("Plots/NO3WB_", Sys.Date(), ".png"),NO3WB)

t.test(Synoptic$DO.pct~Synoptic$WBType)

## NArdiseq
NArdisWB <- ggplot()+
  geom_boxplot(data=SynopticMeans, aes(x = WBType, y=nardiseq))+
  geom_hline(yintercept=0)+
  scale_x_discrete("")+
  scale_y_continuous(expression(bold(N["2"]*":"*Ar~Diseq.)))+
  theme_bw()+
  theme(panel.border = element_rect(color="black",fill=NA,size=1), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text=element_text(size=12,color="black", face = "bold"),
        axis.title=element_text(face="bold",size=13), axis.title.y=element_text(vjust=1),
        axis.title.x=element_text(vjust=0),legend.title = element_blank(),legend.key=element_blank(),
        plot.title=element_text(size=13),legend.text=element_text(size=9),legend.position="bottom",
        strip.background = element_blank(),strip.text.x = element_text(face="bold",size=12))
NArdisWB
## Positive = denitrification, negative = fixation

ggsave(file=paste0("Plots/NArdisWB_", Sys.Date(), ".png"),NArdisWB)

t.test(Synoptic$nardiseq~Synoptic$WBType)


## NArdiseq
SO4WB <- ggplot()+
  geom_boxplot(data=SynopticMeans, aes(x = WBType, y=SO4.mgL))+
  #geom_hline(yintercept=0)+
  scale_x_discrete("")+
  scale_y_continuous(expression(bold(SO["4"]~(mgL^"-1"))))+
  theme_bw()+
  theme(panel.border = element_rect(color="black",fill=NA,size=1), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text=element_text(size=12,color="black", face = "bold"),
        axis.title=element_text(face="bold",size=13), axis.title.y=element_text(vjust=2),
        axis.title.x=element_text(vjust=0),legend.title = element_blank(),legend.key=element_blank(),
        plot.title=element_text(size=13),legend.text=element_text(size=9),legend.position="bottom",
        strip.background = element_blank(),strip.text.x = element_text(face="bold",size=12))
SO4WB
## Positive = denitrification, negative = fixation

ggsave(file=paste0("Plots/NArdisWB_", Sys.Date(), ".png"),NArdisWB)

t.test(Synoptic$nardiseq~Synoptic$WBType)


PCWB <- cowplot::plot_grid(DOWB+theme(axis.text.x = element_blank()), NArdisWB+theme(axis.text.x = element_blank()), 
                           DOCWB, SO4WB, align="hv")

ggsave(file=paste0("Plots/PCWB_",Sys.Date(),".png"),PCWB)


t.test(Synoptic$DO.pct~Synoptic$WBType)
t.test(Synoptic$nardiseq~Synoptic$WBType)
t.test(Synoptic$DOC.mgL~Synoptic$WBType)
t.test(Synoptic$SO4.mgL~Synoptic$WBType)

## Temp.C vs WBType
TempWB <- ggplot()+
  geom_boxplot(data=SynopticMeans, aes(x = WBType, y=Temp.C))+
  scale_x_discrete("")+
  scale_y_continuous(expression(bold(Temp~(degree*C))))+
  theme_bw()+
  theme(panel.border = element_rect(color="black",fill=NA,size=1), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text=element_text(size=12,color="black", face = "bold"),
        axis.title=element_text(face="bold",size=13), axis.title.y=element_text(vjust=2),
        axis.title.x=element_text(vjust=0),legend.title = element_blank(),legend.key=element_blank(),
        plot.title=element_text(size=13),legend.text=element_text(size=9),legend.position="bottom",
        strip.background = element_blank(),strip.text.x = element_text(face="bold",size=12))
TempWB

ggsave(file=paste0("Plots/NO3WB_", Sys.Date(), ".png"),NO3WB)

t.test(Synoptic$Temp.C~Synoptic$WBType)



## NO3 vs WBType
NO3WB <- ggplot()+
  geom_boxplot(data=SynopticMeans, aes(x = WBType, y=NO3.mgL))+
  scale_x_discrete("")+
  scale_y_continuous(expression(bold(NO["3"]~(mgL^"-1"))))+
  theme_bw()+
  theme(panel.border = element_rect(color="black",fill=NA,size=1), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text=element_text(size=12,color="black", face = "bold"),
        axis.title=element_text(face="bold",size=13), axis.title.y=element_text(vjust=2),
        axis.title.x=element_text(vjust=0),legend.title = element_blank(),legend.key=element_blank(),
        plot.title=element_text(size=13),legend.text=element_text(size=9),legend.position="bottom",
        strip.background = element_blank(),strip.text.x = element_text(face="bold",size=12))
NO3WB

ggsave(file=paste0("Plots/NO3WB_", Sys.Date(), ".png"),NO3WB)

t.test(Synoptic$NO3.mgL~Synoptic$WBType)


NH4WB <- ggplot()+
  geom_boxplot(data=SynopticMeans, aes(x = WBType, y=NH4.ugL/1000))+
  scale_x_discrete("")+
  scale_y_continuous(expression(bold(NH["4"]~(mgL^"-1"))))+
  theme_bw()+
  theme(panel.border = element_rect(color="black",fill=NA,size=1), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text=element_text(size=12,color="black", face = "bold"),
        axis.title=element_text(face="bold",size=13), axis.title.y=element_text(vjust=2),
        axis.title.x=element_text(vjust=0),legend.title = element_blank(),legend.key=element_blank(),
        plot.title=element_text(size=13),legend.text=element_text(size=9),legend.position="bottom",
        strip.background = element_blank(),strip.text.x = element_text(face="bold",size=12))
NH4WB

ggsave(file=paste0("Plots/NO3WB_", Sys.Date(), ".png"),NO3WB)

t.test(Synoptic$NH4.ugL/1000~Synoptic$WBType)


DINWB <- ggplot()+
  geom_boxplot(data=SynopticMeans, aes(x = WBType, y=DIN.mgL))+
  scale_x_discrete("")+
  scale_y_continuous(expression(bold(DIN~(mgL^"-1"))))+
  theme_bw()+
  theme(panel.border = element_rect(color="black",fill=NA,size=1), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text=element_text(size=12,color="black", face = "bold"),
        axis.title=element_text(face="bold",size=13), axis.title.y=element_text(vjust=2),
        axis.title.x=element_text(vjust=0),legend.title = element_blank(),legend.key=element_blank(),
        plot.title=element_text(size=13),legend.text=element_text(size=9),legend.position="bottom",
        strip.background = element_blank(),strip.text.x = element_text(face="bold",size=12))
DINWB

ggsave(file=paste0("Plots/DINWB_", Sys.Date(), ".png"),NO3WB)

t.test(Synoptic$DIN.mgL~Synoptic$WBType)


DONWB <- ggplot()+
  geom_boxplot(data=SynopticMeans, aes(x = WBType, y=DON.mgL))+
  scale_x_discrete("")+
  scale_y_continuous(expression(bold(DON~(mgL^"-1"))))+
  theme_bw()+
  theme(panel.border = element_rect(color="black",fill=NA,size=1), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text=element_text(size=12,color="black", face = "bold"),
        axis.title=element_text(face="bold",size=13), axis.title.y=element_text(vjust=2),
        axis.title.x=element_text(vjust=0),legend.title = element_blank(),legend.key=element_blank(),
        plot.title=element_text(size=13),legend.text=element_text(size=9),legend.position="bottom",
        strip.background = element_blank(),strip.text.x = element_text(face="bold",size=12))
DONWB

ggsave(file=paste0("Plots/DONWB_", Sys.Date(), ".png"),NO3WB)

t.test(Synoptic$DON.mgL~Synoptic$WBType)

TDNWB <- ggplot()+
  geom_boxplot(data=SynopticMeans, aes(x = WBType, y=TDN.mgL))+
  scale_x_discrete("")+
  scale_y_continuous(expression(bold(TDN~(mgL^"-1"))))+
  theme_bw()+
  theme(panel.border = element_rect(color="black",fill=NA,size=1), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text=element_text(size=12,color="black", face = "bold"),
        axis.title=element_text(face="bold",size=13), axis.title.y=element_text(vjust=2),
        axis.title.x=element_text(vjust=0),legend.title = element_blank(),legend.key=element_blank(),
        plot.title=element_text(size=13),legend.text=element_text(size=9),legend.position="bottom",
        strip.background = element_blank(),strip.text.x = element_text(face="bold",size=12))
TDNWB

ggsave(file=paste0("Plots/DONWB_", Sys.Date(), ".png"),NO3WB)

t.test(Synoptic$TDN.mgL~Synoptic$WBType)


DOCWB <- ggplot()+
  geom_boxplot(data=SynopticMeans, aes(x = WBType, y=DOC.mgL))+
  scale_x_discrete("")+
  scale_y_continuous(expression(bold(DOC~(mgL^"-1"))))+
  theme_bw()+
  theme(panel.border = element_rect(color="black",fill=NA,size=1), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text=element_text(size=12,color="black", face = "bold"),
        axis.title=element_text(face="bold",size=13), axis.title.y=element_text(vjust=2),
        axis.title.x=element_text(vjust=0),legend.title = element_blank(),legend.key=element_blank(),
        plot.title=element_text(size=13),legend.text=element_text(size=9),legend.position="bottom",
        strip.background = element_blank(),strip.text.x = element_text(face="bold",size=12))
DOCWB

ggsave(file=paste0("Plots/DONWB_", Sys.Date(), ".png"),NO3WB)

t.test(Synoptic$DOC.mgL~Synoptic$WBType)



WBall <- cowplot::plot_grid(NO3WB, DINWB, DONWB, TDNWB,NH4WB,DOCWB, DOWB, NArdisWB, nrow=4)

## 2 columns and 4 rows
WBall2x4 <- cowplot::plot_grid(NO3WB+theme(axis.text.x = element_blank()), DINWB+theme(axis.text.x = element_blank()),
                            DONWB+theme(axis.text.x = element_blank()), TDNWB+theme(axis.text.x = element_blank()),
                            NH4WB+theme(axis.text.x = element_blank()), DOCWB+theme(axis.text.x = element_blank()),
                            DOWB, NArdisWB, nrow=4)

## 4 columns and 2 rows
WBall4x2 <- cowplot::plot_grid(NO3WB+theme(axis.text.x = element_blank()), DINWB+theme(axis.text.x = element_blank()),
                            DONWB+theme(axis.text.x = element_blank()), TDNWB+theme(axis.text.x = element_blank()),
                            NH4WB+theme(axis.text.x = element_text(angle = 45, hjust = 1)), 
                            DOCWB+theme(axis.text.x = element_text(angle = 45, hjust = 1)), 
                            DOWB+theme(axis.text.x = element_text(angle = 45, hjust = 1)), 
                            NArdisWB+theme(axis.text.x = element_text(angle = 45, hjust = 1)), nrow=2, rel_heights = c(1,1.2))
WBall4x2

ggsave(file=paste0("Plots/WBall_",Sys.Date(),".png"),WBall4x2)


##WB t-tests
t.test(Synoptic$NO3.mgL~Synoptic$WBType)
t.test(Synoptic$DIN.mgL~Synoptic$WBType)
t.test(Synoptic$DON.mgL~Synoptic$WBType)
t.test(Synoptic$TDN.mgL~Synoptic$WBType)
t.test(Synoptic$NH4.ugL/1000~Synoptic$WBType)
t.test(Synoptic$DOC.mgL~Synoptic$WBType)
t.test(Synoptic$DO.pct~Synoptic$WBType)
t.test(Synoptic$nardiseq~Synoptic$WBType)


NArdisSynoptic <- ggmap(CCBPmapSynoptic)+
  coord_sf(crs=4326,xlim=c(-7897580, -7893501 ), ylim=c(5273954, 5278369))+
  geom_sf(data=CChydroArc4326, inherit.aes=FALSE, color="blue", size=1)+
  geom_sf(data=CChydroPoly4326, inherit.aes=FALSE, color="blue",fill="blue")+
  #geom_sf(data=CCOutline4326, inherit.aes=FALSE, color="yellow", fill=NA, size=1)+
  geom_sf(data=CCOutlineSZ4326, inherit.aes=FALSE, color="red", fill=NA, size=1)+
  geom_point(data=SynopticMeans[!is.na(SynopticMeans$nardiseq),],aes(x=Longitude,y=Latitude,color= nardiseq, shape=WBType),size=4)+
  #coord_fixed(xlim=c(-70.945093,-70.915149),ylim=c(42.75073,42.779325),ratio=1)+
  scale_color_gradient2(expression(bold(N["2"]*":"*Ar~Disequilibrium)),low="green",mid="yellow",high="red",midpoint=0)+
  scale_shape_manual("", values=c("Channel" = 17, "Pond" = 19))+
  labs(x=NULL,y=NULL)+
  theme_bw()+
  #theme(legend.position = "bottom", legend.title = element_text(vjust = 1, hjust = 0.5))
  theme(axis.text=element_blank())
NArdisSynoptic
## Positive = denitrification, negative = fixation

ggsave(file=paste0("Plots/NArdisSynoptic_", Sys.Date(),".png"),NArdisSynoptic)



DINSynoptic <- ggmap(CCBPmapSynoptic)+
  coord_sf(crs=4326,xlim=c(-7897580, -7893501 ), ylim=c(5273954, 5278369))+
  geom_sf(data=CChydroArc4326, inherit.aes=FALSE, color="blue", size=1)+
  geom_sf(data=CChydroPoly4326, inherit.aes=FALSE, color="blue",fill="blue")+
  geom_point(data=Synoptic[Synoptic$Campaign == 1 & !is.na(Synoptic$DIN.mgL),],aes(x=Longitude,y=Latitude,color= DIN.mgL, shape=WBType),size=4)+
  #geom_sf(data=Synoptic[Synoptic$Campaign == 1 & !is.na(Synoptic$NH4.ugL),],aes(color= NH4.ugL/1000),size=4)+
  #coord_fixed(xlim=c(-70.945093,-70.915149),ylim=c(42.75073,42.779325),ratio=1)+
  scale_color_gradient2(expression(bold(NO[3]~(mgL^-1))),low="green",mid="yellow",high="red",midpoint=0.4)+
  labs(x=NULL,y=NULL)+
  theme_bw()+
  theme(axis.text=element_blank())
DINSynoptic


# xlim=c(-70.945093,-70.912) works here. Exploring xlims to see if CC -> PR can fit in

TDNSynoptic <- ggmap(CCBPmapSynoptic)+
  coord_sf(crs=4326,xlim=c(-7897580, -7893501 ), ylim=c(5273954, 5278369))+
  geom_sf(data=CChydroArc4326, inherit.aes=FALSE, color="blue", size=1)+
  geom_sf(data=CChydroPoly4326, inherit.aes=FALSE, color="blue",fill="blue")+
  geom_point(data=Synoptic[Synoptic$Campaign == 1 & !is.na(Synoptic$TDN.mgL),],aes(x=Longitude,y=Latitude,color= TDN.mgL, shape=WBType),size=4)+
  #geom_sf(data=Synoptic[Synoptic$Campaign == 1 & !is.na(Synoptic$NH4.ugL),],aes(color= NH4.ugL/1000),size=4)+
  coord_fixed(xlim=c(-70.945093,-70.908),ylim=c(42.75073,42.779325),ratio=1)+
  scale_color_gradient2(expression(bold(TDN~(mgL^-1))),low="green",mid="yellow",high="red",midpoint=0.75)+
  labs(x=NULL,y=NULL)+
  theme_bw()
  #theme(axis.text=element_blank())
TDNSynoptic

DONSynoptic <- ggmap(CCBPmapSynoptic)+
  coord_sf(crs=4326,xlim=c(-7897580, -7893501 ), ylim=c(5273954, 5278369))+
  geom_sf(data=CChydroArc4326, inherit.aes=FALSE, color="blue", size=1)+
  geom_sf(data=CChydroPoly4326, inherit.aes=FALSE, color="blue",fill="blue")+
  geom_point(data=Synoptic[Synoptic$Campaign == 1 & !is.na(Synoptic$DON.mgL),],aes(x=Longitude,y=Latitude,color= DON.mgL, shape=WBType),size=4)+
  #geom_sf(data=Synoptic[Synoptic$Campaign == 1 & !is.na(Synoptic$NH4.ugL),],aes(color= NH4.ugL/1000),size=4)+
  #coord_fixed(xlim=c(-70.945093,-70.915149),ylim=c(42.75073,42.779325),ratio=1)+
  scale_color_gradient2(expression(bold(DON~(mgL^-1))),low="green",mid="yellow",high="red",midpoint=0.4)+
  labs(x=NULL,y=NULL)+
  theme_bw()+
  theme(axis.text=element_blank())
DONSynoptic

DOCSynoptic <- ggmap(CCBPmapSynoptic)+
  coord_sf(crs=4326,xlim=c(-7897580, -7893501 ), ylim=c(5273954, 5278369))+
  geom_sf(data=CChydroArc4326, inherit.aes=FALSE, color="blue", size=1)+
  geom_sf(data=CChydroPoly4326, inherit.aes=FALSE, color="blue",fill="blue")+
  geom_point(data=Synoptic[Synoptic$Campaign == 1 & !is.na(Synoptic$DOC.mgL),],aes(x=Longitude,y=Latitude,color= DOC.mgL, shape=WBType),size=4)+
  #geom_sf(data=Synoptic[Synoptic$Campaign == 1 & !is.na(Synoptic$NH4.ugL),],aes(color= NH4.ugL/1000),size=4)+
  #coord_fixed(xlim=c(-70.945093,-70.908),ylim=c(42.75073,42.779325),ratio=1)+
  scale_color_gradient2(expression(bold(TDN~(mgL^-1))),low="green",mid="yellow",high="red",midpoint=8)+
  labs(x=NULL,y=NULL)+
  theme_bw()
#theme(axis.text=element_blank())
DOCSynoptic


DOSynoptic <- ggmap(CCBPmapSynoptic)+
  coord_sf(crs=4326,xlim=c(-7897580, -7893501 ), ylim=c(5273954, 5278369))+
  geom_sf(data=CChydroArc4326, inherit.aes=FALSE, color="blue", size=1)+
  geom_sf(data=CChydroPoly4326, inherit.aes=FALSE, color="blue",fill="blue")+
  #geom_sf(data=CCOutline4326, inherit.aes=FALSE, color="yellow", fill=NA, size=1)+
  geom_sf(data=CCOutlineSZ4326, inherit.aes=FALSE, color="red", fill=NA, size=1)+
  geom_point(data=SynopticMeans[!is.na(SynopticMeans$DO.pct),],aes(x=Longitude,y=Latitude,color= DO.pct, shape = WBType),size=4)+
  #coord_sf(crs=3857,xlim=c(-7897580, -7893501 ), ylim=c(5273954, 5278369))+
  #coord_fixed(xlim=c(-70.945093,-70.908),ylim=c(42.75,42.779325),ratio=1)+
  scale_color_gradient2(expression(bold(DO~("%"~Sat))),low="red",mid="yellow",high="green",midpoint=40)+
  scale_shape_manual("", values=c("Channel" = 17, "Pond" = 19))+
  
  labs(x=NULL,y=NULL)+
  theme_bw()
#theme(axis.text=element_blank())
DOSynoptic

ggsave(file="Plots/NH4_Synoptic_2022-05-11.png",DOSynoptic)


# TDN channel = 0.6537392, calc = 0.6519052
# TDN pond = 0.6472778, calc = 0.6472772
# DIN channel = 0.18012520
# DIN pond = 0.06566798
# DON channel = 0.4717800
# DON pond = 0.5816092







NO3SynopticDFM <- ggplot()+
  geom_point(data=Synoptic[!is.na(Synoptic$NO3.mgL),],aes(x=DistFromMouth.m, y=NO3.mgL, color=Flowpath, shape = WBType), size=3)+
  geom_smooth(data=Synoptic[!is.na(Synoptic$NO3.mgL),],aes(x=DistFromMouth.m, y=NO3.mgL, color=Flowpath), method="loess")+
  facet_wrap(~Campaign, nrow=2)+
  theme_bw()
NO3SynopticDFM

ggsave(file="Plots/NO3_Synoptic_DFM_2022-05-11.png",NO3SynopticDFM)

NH4SynopticDFM <- ggplot()+
  geom_point(data=Synoptic[!is.na(Synoptic$NH4.ugL),],aes(x=DistFromMouth.m, y=NH4.ugL/1000, color=Flowpath, shape = WBType), size=3)+
  geom_smooth(data=Synoptic[!is.na(Synoptic$NH4.ugL),],aes(x=DistFromMouth.m, y=NH4.ugL/1000, color=Flowpath), method="loess")+
  facet_wrap(~Campaign, nrow=2)+
  theme_bw()
NH4SynopticDFM

ggsave(file="Plots/NH4_Synoptic_DFM_2022-05-11.png",NH4SynopticDFM)

N2ArdisSynopticDFM <- ggplot()+
  geom_point(data=Synoptic[!is.na(Synoptic$nardiseq),],aes(x=DistFromMouth.m, y=nardiseq, color=Flowpath, shape = WBType), size=3)+
  geom_smooth(data=Synoptic[!is.na(Synoptic$nardiseq),],aes(x=DistFromMouth.m, y=nardiseq, color=Flowpath), method="loess")+
  facet_wrap(~Campaign, nrow=2)+
  theme_bw()
N2ArdisSynopticDFM

ggsave(file="Plots/N2Ardis_Synoptic_DFM_2022-05-11.png",N2ArdisSynopticDFM)

DONSynopticDFM <- ggplot()+
  geom_point(data=Synoptic[!is.na(Synoptic$DON.mgL),],aes(x=DistFromMouth.m, y=DON.mgL, color=Flowpath, shape = WBType), size=3)+
  geom_smooth(data=Synoptic[!is.na(Synoptic$DON.mgL),],aes(x=DistFromMouth.m, y=DON.mgL, color=Flowpath), method="loess")+
  scale_x_reverse()+
  facet_wrap(~Campaign, nrow=2)+
  theme_bw()
DONSynopticDFM

ggsave(file="Plots/DON_Synoptic_DFM_2022-05-11.png",DONSynopticDFM)


###########################################################
## Internal Sampling
###########################################################

CCBPmapInternal<- get_googlemap(center=c(-70.935, 42.767741), maptype = "satellite", zoom = 14)
ggmap(CCBPmapSynopticInternal)


################################################################################
## Compile CC discharge data and scale to CCBP
################################################################################


# This was done already and the resulting dataframe was stored in the 'Data' directory#####
# Code stgored here for future analysis/re-analysis

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
## Various code that didn't really work but may with some more time
################################################################################


# ## Try LANDSAT data
# ## Wound up being far too coarse to see anything of value
# 
# BMblue <- raster("Data/Basemap/LC08_L1TP_012030_20220511_20220511_02_RT_B2.tif")
# BMgreen <- raster("Data/Basemap/LC08_L1TP_012030_20220511_20220511_02_RT_B3.tif")
# BMred <- raster("Data/Basemap/LC08_L1TP_012030_20220511_20220511_02_RT_B4.tif")
# BMrgb <- stack(BMred, BMgreen, BMblue)
# 
# 
# bboxCCSynoptic <- raster(ymn=42.750739, ymx=42.779325, xmn=-70.945093, xmx=-70.915149)
# bboxCCSynoptic <- projectExtent(bboxCCSynoptic, BMrgb@crs)
# CCBM <- crop(BMrgb, bboxCCSynoptic)


# ## None of this shit really did the trick...reverting back to using ggmap
# 
# BMSynoptic <- basemap(ext=c(xmin=42.750739, xmax=42.779325, ymin=-70.945093, ymax=-70.915149))
# 
# ext=sf::st_bbox(c(xmin=-70.945093, xmax=-70.915149,ymin=42.750739, ymax=42.779325),crs=sf::st_crs('EPSG:4326'))
# set_defaults(map_service="esri", map_type="world_imagery")#, map_token = "pk.eyJ1IjoiY2hyaXN3aGl0bmV5IiwiYSI6ImNsMzM1M292ZTJjYXYzam81Z2FhajZvNDMifQ.DQ6KzcGKP4zTSqKeamblJQ")
# tmp<-basemap(ext)
# 
# 
# tmp <- readGDAL("C:/Users/ctw1/Downloads/19TCH420355/19TCH420355.jp2") ## This file is fucking HUGE! R can't process on this comp
# 
# map <- basemap(ext)
# 
# map2 <- as.data.frame(map)
# 



# tmpraster<-rasterFromXYZ(tmpdfwide[,c(1,2,6)])
# 
# 
# SynopticSF <- Synoptic %>% st_as_sf(coords=c("Longitude","Latitude"), crs=4326, remove=FALSE)
# SynopticSF <- st_transform(SynopticSF, st_crs(tmp))
# 
# ggplot()+
#   geom_stars(data=tmp,aes(fill=))+
#   #scale_fill_gradient(low="black",high="white")+
#   scale_fill_gradientn(colors=c("black","white"),n.breaks=255)+
#   geom_sf(data=SynopticSF,aes(color=NO3.mgL))+
#   scale_color_gradient2(expression(bold(NO[3]~(mgL^-1))),low="green",mid="yellow",high="red",midpoint=0.3)
# 
# tmpdf <- raster::as.data.frame(tmp)
# 
# ##Trying something else here
# tmpdf2<-tmpdf
# tmpdf2$band = c("red", "blue", "green")[tmpdf2$band]
# r <- stars::st_as_stars(tmpdf2, dims = c("x", "y", "band"))
# 
# tmpdfwide <- tmpdf %>% pivot_wider(names_from=band,values_from=basemap_20220512162927.tif) %>% as.data.frame()
# 
# tmpdfwide$palette <- rgb(tmpdfwide[,3:5],maxColorValue=255)
# 
# ramp <- rgb(tmpdfwide[,3:5], maxColorValue = 255)
# 
# ggplot()+
#   geom_raster(data=tmp,aes(x=x,y=y))+
#   scale_fill_gradientn(colors=c("black","white"))
# #scale_color_identity()
# 
# 
# tmpblue <- raster(tmpdfwide[,c(1,2,3)])
# tmpgreen <- raster(tmpdfwide[,c(1,2,4)])
# tmpred <- raster(tmpdfwide[,c(1,2,5)])
# 
# tmprgb <- stack(tmpred,tmpgreen,tmpblue)
# 
# plotRGB(tmprgb)
# 
# ggplot()+
#   geom_stars(data=r,aes(x=x,y=y,fill=basemap_20220512111424.tif))+
#   #geom_sf(data=SynopticSF,aes(color=NO3.mgL))+
#   #scale_fill_manual(values=terrain.colors(255))
#   #scale_fill_gradientn(n.breaks=255)
#   scale_fill_gradientn(colors=c("yellow","red"))
# #scale_color_gradient2(expression(bold(NO[3]~(mgL^-1))),low="green",mid="yellow",high="red",midpoint=0.3)
# 
# ggmap(basemap_ggplot(ext))+
#   #basemap_gglayer(ext)+
#   theme_bw()
# 
# #geom_tile(aes(x=x,y=y, fill=as.factor(band)),map2)+ #basemap_20220512110548.tif
# #geom_sf(aes(y=Longitude, x=Latitude, color = NO3.mgL),data=Synoptic)
# #scale_fill_manual("",values=c("1"="red","2"="green","3"="blue"))




# 
# BM1 <- raster("C:/Users/ctw1/Downloads/19TCH420355.tif")
# BM1rescale <- raster::aggregate(BM1, fact = 100)
# BM1df <- raster::as.data.frame(BM1rescale, xy = TRUE)
# 
# BM2 <- readGDAL("C:/Users/ctw1/Downloads/19TCH420355.tif")
# BM2rescale <- raster::aggregate(BM2, fact=200, FUN = mean)
# 
# ggplot()+
#   geom_raster(data=BM1df, aes(x=x, y=y, fill=X19TCH420355))




#   #merge(., unique(Synoptic[,c(1,16,17)]),by="Site")
# 
# 
# SynopticMeanstest2 <- Synoptic %>% group_by(Site) %>% 
#   summarize(across(c(Latitude:DO.pct,DistFromMouth.m,narsat:DON.mgL), ~mean(., na.rm=TRUE))) %>%
#   as.data.frame() %>%
# 
# SynopticMeans2020 <- Synoptic %>%
#   filter(Campaign == "2020") %>%
#   group_by(Site) %>%
#   summarize(across(c(Latitude:DO.pct,DistFromMouth.m,narsat:DON.mgL), ~mean(., na.rm=TRUE))) %>%
#   left_join(filter(Synoptic,Campaign == "2020") %>%
#               select(c(Site,DistFromMouth.m:WBType)), by="Site") %>%
#   full_join(., Synoptic %>% filter(Campaign == "2021") %>%
#               group_by(Site) %>%
#               summarize(across(c(Latitude:DO.pct,DistFromMouth.m,narsat:DON.mgL), ~mean(., na.rm=TRUE))) %>%
#               left_join(filter(Synoptic,Campaign == "2021") %>%
#                           select(c(Site,DistFromMouth.m:WBType)), by="Site"))
# 
# 
# Synoptic2021 <- Synoptic %>% filter(Campaign == "2021") %>%
#             group_by(Site) %>%
#             summarize(across(c(Latitude:DO.pct,DistFromMouth.m,narsat:DON.mgL), ~mean(., na.rm=TRUE))) %>%
#             left_join(filter(Synoptic,Campaign == "2021") %>%
#                         select(c(Site,DistFromMouth.m:WBType)), by="Site")
# 
# 
# 
#               group_by(Site) %>%
#               summarize(across(c(Latitude:DO.pct,DistFromMouth.m,narsat:DON.mgL), ~mean(., na.rm=TRUE))) %>%
#               left_join(filter(Synoptic, Campaign == "2021") %>%
#                           (c(Site, Flowpath:WBType))) %>%
#   full_join(., filter(Synoptic, Campaign == "2021") %>%
#               summarize(across(c(Latitude:DO.pct,DistFromMouth.m,narsat:DON.mgL), ~mean(., na.rm=TRUE))) %>%
#               left_join(filter(Synoptic, Campaign == "2021") %>%
#                           select(c(Site, Flowpath:WBType))))
# 
# 
# #%>%
# #  left_join(., select(Synoptic, c(Site, Flowpath:WBType), Site))
#   
#   
#   
# 
# SynopticMeanstest2 <- Synoptic %>% group_by(Site) %>% 
#   summarize(across(c(Latitude:DO.pct,DistFromMouth.m,narsat:DON.mgL), ~mean(., na.rm=TRUE))) %>%
#   as.data.frame() %>%
#   left_join(., select(Synoptic, c(Site, Flowpath:WBType)), by = "Site")



