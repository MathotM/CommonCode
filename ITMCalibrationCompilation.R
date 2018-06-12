#Compilation of Raw Data for ITM calibration


# Setting working directory as this file location
library(rstudioapi)
library(ggplot2)
library(lubridate)

rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# #path to files
PathJournal<-c("./../GHGData/Information/Chambers")#c("./../../../GHGData/Information/Chambers")

# Files Name
JournalName<-c("Journal_Mes_Chambers.csv")  
ITMCalibCompiledName<-c("ITMCalibCompiled.csv")

#Parameters
TimeZone<-c("Europe/brussels")

# Opening of journal and save comiled ITMcalibation file

print("Opening of Journal Data")

Journal.df<-try(read.csv(file=paste(PathJournal,JournalName,sep=c("/")),sep=c(";"),dec=c(","),header=TRUE,stringsAsFactors = FALSE),silent = TRUE)
ITMCalibJournal.df<-Journal.df[Journal.df$Activity==c("ITM_Calibration"),]
ITMCalibJournal.df$Time<-paste(ITMCalibJournal.df$Year,"/",ITMCalibJournal.df$Month,"/",ITMCalibJournal.df$Day," ",ITMCalibJournal.df$Hour,":",ITMCalibJournal.df$Minute,":",ITMCalibJournal.df$Second,sep=c(""))
ITMCalibJournal.df$Time<-as_datetime(ymd_hms(ITMCalibJournal.df$Time,tz=TimeZone),tz=TimeZone)
colnames(ITMCalibJournal.df)

ITMCalibJournal.df<-ITMCalibJournal.df[,c("Site","Position","Activity","Stage","Comment","Synchronisation","ToNotUse","Time")]
write.table(ITMCalibJournal.df,paste(PathJournal,ITMCalibCompiledName,sep=c("/")),row.names = FALSE,sep=c(";"),dec=(","))
