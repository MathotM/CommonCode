#ManureStoreTemperature

getwd()

library(lubridate)
library(ggplot2)

ManureStoreTemperatureData<-read.table(file="./../GHGData/RawData/ManureStoreTemperature/2017_11_28_14_18_40.CSV",header=T,sep=",",dec=".")
colnames(ManureStoreTemperatureData)<-c("Time","Position","Item","Temperature_Mean","Temperature_SD")
ManureStoreTemperatureData$Time<-as_datetime(ymd_hms(ManureStoreTemperatureData$Time))

summary(ManureStoreTemperatureData)
Selection<-ManureStoreTemperatureData$Time<as_datetime(ymd_hms(c("2017/11/28_22_00_00")))
sum(Selection)
ggplot(ManureStoreTemperatureData[Selection,],aes(x=Time,y=Temperature_Mean,color=as.factor(Item)))+
  geom_point()+
  facet_grid(Position~.,scale="free")+
  theme_bw()
