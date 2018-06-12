# # function for compilation of data from manure in barn and storage and saving of compiled data and figure in main folder
# no empty file to be imported
# Manure Amount, temperature and analysis

#Libraries
  library(rstudioapi)
  library(ggplot2)
  library(lubridate)
  library(reshape2)

#removal of object in memory
  rm(list=ls())

#definition of working folder
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# Path to files

PathRawDataManureAmount<-c("./../Manure/RawData/Amount")
PathRawDataManureTemperature<-c("./../Manure/RawData/Temperature")
PathRawDataManureAnalysis<-c("./../Manure/RawData/Analysis")


# Parameter

TimeZone<-c("Europe/brussels")
ToCompile<-c("ManureAmount","ManureTemperature","ManureAnalysis")[c(TRUE,TRUE,TRUE)]# modify the logicla vector in function of files  to compile

#Compiled file definition
ManureAmountCompiledData<-c("ManureAmountCompiled.csv") #Data
ManureAmountCompiledFig<-c("ManureAmountCompiled.png") #figure

ManureTemperatureCompiledData<-c("ManureTemperatureCompiled.csv") #Data
ManureTemperatureCompiledFig<-c("ManureTemperatureCompiled.png") #figure

ManureAnalysisCompiledData<-c("ManureAnalysisCompiled.csv") #Data
ManureAnalysisCompiledFig<-c("ManureAnalysisCompiled.png") #figure

# File list selection
RawDataList_ManureAmount<-list.files(PathRawDataManureAmount) 
RawDataList_ManureAmount<-RawDataList_ManureAmount[!(RawDataList_ManureAmount%in%c(ManureAmountCompiledData,ManureAmountCompiledFig))] # Avoid selection of previously compiled files
NBRawDataList_ManureAmount<-length(RawDataList_ManureAmount)

RawDataList_ManureTemperature<-list.files(PathRawDataManureTemperature) 
RawDataList_ManureTemperature<-RawDataList_ManureTemperature[!(RawDataList_ManureTemperature%in%c(ManureTemperatureCompiledData,ManureTemperatureCompiledFig))] # Avoid selection of previously compiled files
NBRawDataList_ManureTemperature<-length(RawDataList_ManureTemperature)


RawDataList_ManureAnalysis<-list.files(PathRawDataManureAnalysis) 
RawDataList_ManureAnalysis<-RawDataList_ManureAnalysis[!(RawDataList_ManureAnalysis%in%c(ManureAnalysisCompiledData,ManureAnalysisCompiledFig))] # Avoid selection of previously compiled files
NBRawDataList_ManureAnalysis<-length(RawDataList_ManureAnalysis)

# Liste and Selection of charactristics to compile
RawDataTypePath.list<-list(ManureAmount=PathRawDataManureAmount,ManureTemperature=PathRawDataManureTemperature,ManureAnalysis=PathRawDataManureAnalysis)[ToCompile]
RawDataTypeList.list<-list(ManureAmount=RawDataList_ManureAmount,ManureTemperature=RawDataList_ManureTemperature,ManureAnalysis=RawDataList_ManureAnalysis)[ToCompile]
RawDataTypeNB.list<-list(ManureAmount=NBRawDataList_ManureAmount,ManureTemperature=NBRawDataList_ManureTemperature,ManureAnalysis=NBRawDataList_ManureAnalysis)[ToCompile]


NBCharacteristics<-length(RawDataTypeList.list)



print("Compilation of data ")
#Compilation of data  
  ManureCompiledData.list<-RawDataTypeNB.list
  for (i in 1:NBCharacteristics){
    
    CharacteristicDataName<-names(RawDataTypeList.list)[i]
    characteristicDataPath<-RawDataTypePath.list[[CharacteristicDataName]]
    characteristicDataList<-RawDataTypeList.list[[CharacteristicDataName]]
    characteristicDataNB<-RawDataTypeNB.list[[CharacteristicDataName]]
    if(characteristicDataNB>0){
      for (j in 1:characteristicDataNB){
        FileName<-characteristicDataList[j]
        FilePath<-paste(characteristicDataPath,FileName,sep=c("/"))
        ReadFile<-try(read.table(file=FilePath,header=TRUE,sep=c(";"),dec=c(","),stringsAsFactors = F))
        if(dim(ReadFile)[2]==1){
          ReadFile<-try(read.table(file=FilePath,header=TRUE,sep=c(","),dec=c("."),stringsAsFactors = F))
        }
        ReadFile$Name<-FileName
      if(j==1){CompiledData<-ReadFile}
      if(j>1){CompiledData<-rbind(CompiledData,ReadFile)}
      }
    }
    ManureCompiledData.list[[CharacteristicDataName]]<-CompiledData
    
  }

ManureAmountCompiledData.df<-ManureCompiledData.list$ManureAmount
ManureTemperatureCompiledData.df<-ManureCompiledData.list$ManureTemperature
ManureAnalysisCompiledData.df<-ManureCompiledData.list$ManureAnalysis

#Setting Time Variable
ManureAmountCompiledData.df$Time<-as_datetime(ymd_hm(paste(paste(ManureAmountCompiledData.df$Year,ManureAmountCompiledData.df$Month,ManureAmountCompiledData.df$Day,sep=c("/")),paste(ManureAmountCompiledData.df$Hour,ManureAmountCompiledData.df$Minute,sep=c(":")),sep=c(" ")),tz=TimeZone),tz=TimeZone)
ManureTemperatureCompiledData.df$Time<-as_datetime(ymd_hms(ManureTemperatureCompiledData.df$Datetime,tz=TimeZone),tz=TimeZone)
ManureAnalysisCompiledData.df$Time<-as_datetime(dmy(ManureAnalysisCompiledData.df$SamplingDate,tz=TimeZone),tz=TimeZone)

#Site definition in Temperature Data 
ManureTemperatureCompiledData.df$Site<-""
ManureTemperatureCompiledData.df$Site[grepl(c("cell"),ManureTemperatureCompiledData.df$Name)]<-c("Storage")

#Position definition in Temperrature Data. Remove the 6 character of the file name for position determination
ManureTemperatureCompiledData.df$Position<-""
ManureTemperatureCompiledData.df$Position[ManureTemperatureCompiledData.df$Site==c("Storage")]<-substr(ManureTemperatureCompiledData.df$Name[ManureTemperatureCompiledData.df$Site==c("Storage")],6,6)


#Summary and plot of main data

summary(ManureAmountCompiledData.df) 
ggplot(data=ManureAmountCompiledData.df,aes(x=ManureStage,y=WeightKg,color=Treatment))+
   stat_summary(fun.y = sum, fun.ymin = sum, fun.ymax = sum)+
   facet_grid(ManureType+Repetition~FeedingStage)+#,scales = "free"
   theme_bw()+
   theme(axis.text.x  = element_text(angle=90))
ggsave(file=paste(PathRawDataManureAmount,ManureAmountCompiledFig,sep=c("/")))  

summary(ManureTemperatureCompiledData.df) 
ggplot(data=ManureTemperatureCompiledData.df[,],aes(x=trunc(Time,c("days")),y=T,color=as.factor(probe)))+
  facet_grid(Position~.)+
  stat_summary(geom=c("pointrange"))+
  stat_summary(geom=c("line"))+
  theme_bw()+
  theme(axis.text.x  = element_text(angle=90))
ggsave(file=paste(PathRawDataManureTemperature,ManureTemperatureCompiledFig,sep=c("/")))  

summary(ManureAnalysisCompiledData.df) 
ggplot(data=ManureAnalysisCompiledData.df,aes(x=SamplingDate,y=DryMatterPCFresh,color=Treatment))+
  stat_summary(fun.y=mean,fun.ymin=min,fun.ymax=max)+
  facet_grid(ManureType~Site,scales = "free")+
  theme_bw()+
  theme(axis.text.x  = element_text(angle=90))
ggsave(file=paste(PathRawDataManureAnalysis,ManureAnalysisCompiledFig,sep=c("/")))  



print("Saving of compiled files in raw data files")
write.table(ManureAmountCompiledData.df,file=paste(PathRawDataManureAmount,ManureAmountCompiledData,sep=c("/")),dec=c(","),row.name=F,sep=c(";"))
write.table(ManureTemperatureCompiledData.df,file=paste(PathRawDataManureTemperature,ManureTemperatureCompiledData,sep=c("/")),dec=c(","),row.name=F,sep=c(";"))
write.table(ManureAnalysisCompiledData.df,file=paste(PathRawDataManureAnalysis,ManureAnalysisCompiledData,sep=c("/")),dec=c(","),row.name=F,sep=c(";"))

