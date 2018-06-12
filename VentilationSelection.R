# # function for selection in ventilation compiled file and plot of data seleced
VentilationSelection<-function(
  NameCompiledFile=NameCompiledFile,
  PathRawDataVentilation=PathRawDataVentilation,
  StartData=StartData,
  EndData=EndData,
  DataMeta=DataMeta,
  TimeZone=TimeZone
){
library(lubridate)

Ventilation.df<-read.csv(file=paste(PathRawDataVentilation,NameCompiledFile,sep=c("/")),sep=c(";"),dec=c(","),header=TRUE,stringsAsFactors = FALSE)
Ventilation.df$Time<-as_datetime(ymd_hms(Ventilation.df$Time,tz=TimeZone),tz=TimeZone)
if(class(StartData)==c("character")){StartData<-as_datetime(ymd_hms(StartData,tz=TimeZone),tz=TimeZone)}
if(class(EndData)==c("character")){EndData<-as_datetime(ymd_hms(EndData,tz=TimeZone),tz=TimeZone)}

Ventilation.df<-Ventilation.df[Ventilation.df$Time>StartData&Ventilation.df$Time<EndData,]
return(Ventilation.df)
}