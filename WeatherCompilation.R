#Compilation of weather Data from Raw Weather Data. Produced compiles fildes is store with raw Data
#Open Libraries

setwd("Y:/GES/GlobalDAQ/CommonCode")

library(ggplot2)
library(lubridate)
library(reshape2)

#Parameters
TimeZone<-c("Europe/brussels")

#Produced file names
WeatherCompiledName<-c("WeatherCompiled.csv")
PathRawDataWeather<-c("./../WeatherData/RawData")

#################################Compilation Weatherrological Data
RawDataList_Weather<-list.files(PathRawDataWeather) # File list of weather data
RawDataList_Weather<-RawDataList_Weather[!(RawDataList_Weather%in%c(WeatherCompiledName))] # Avoid selection of previously compiled files

NBRawDataFiles_Weather<-length(RawDataList_Weather)

#Meta<-read.table(paste(PathMetaFiles,DataMeta,sep=c("/")),sep=c(";"),dec=",",header=T,stringsAsFactors = F)
#MetaDate<-as_datetime(dmy(c(Meta$Valeur[Meta$Item==c("Debut")],Meta$Valeur[Meta$Item==c("Fin")])),tz=TimeZone)
#ToBeSelected<-min(ReadFile$Time)>MetaDate[1]&max(ReadFile$Time)<MetaDate[2]

for (i in 1:NBRawDataFiles_Weather){
  ReadFile<-read.table(paste(PathRawDataWeather,RawDataList_Weather[i],sep=c("/")),header=TRUE,sep=c(','),dec=".",stringsAsFactors = FALSE)
  ReadFile$Time<-as_datetime(dmy_hms(ReadFile$timestamp,tz=TimeZone),tz=TimeZone)
  ReadFile$Name<-RawDataList_Weather[i]
  if(i==1){CompiledRaw_Weather<-ReadFile}
  if(i>1){CompiledRaw_Weather<-rbind(CompiledRaw_Weather,ReadFile)}
  print(paste("Weather_file:",i,"of",NBRawDataFiles_Weather,sep=" "))
}
summary(CompiledRaw_Weather)
View(CompiledRaw_Weather[is.na(CompiledRaw_Weather$Time),])
WeatherDataCompiled<-unique(CompiledRaw_Weather[!is.na(CompiledRaw_Weather$Time),!colnames(CompiledRaw_Weather)==c("Name")])
summary(WeatherDataCompiled)

summary(WeatherDataCompiled)

write.table(WeatherDataCompiled,file=paste(PathRawDataWeather,WeatherCompiledName,sep=c("/")),row.names = FALSE,dec=c(","),sep=c(";"))


