# # function for compilation of data from ventilation system in barn and storage Save compiled data and figure in main folder
# no empty file to be imported
# Removal of unrealistic values in last lignes of the script


#Libraries
library(ggplot2)
library(lubridate)
library(reshape2)

setwd("Y:/GES/GlobalDAQ/CommonCode")

# Path to files

PathRawDataVentilation<-c("./../GHGData/RawData/Ventilation")
TimeZone<-c("Europe/brussels")

#Compiled file definition
VentilationCompiledData<-c("VentilationCompiled.csv") #Data
VentilationCompiledFig<-c("VentilationCompiled.png") #figure
                       
# File list selection
RawDataList_Ventilation<-list.files(PathRawDataVentilation) # Enregistrement de la liste des fichiers dans donnees brutes de flux d'air
RawDataList_Ventilation<-RawDataList_Ventilation[!(RawDataList_Ventilation%in%c(VentilationCompiledData,VentilationCompiledFig))] # Avoid selection of previously compiled files

NBRawDataList_Ventilation<-length(RawDataList_Ventilation)
#DataImport<-data.frame(Name=RawDataList_Ventilation,LineNb=NA,SeparatorSign="SemiComa")

countFlowData<-0
countTempData<-0

for (i in 1:NBRawDataList_Ventilation){
  FileName<-RawDataList_Ventilation[i]
  FilePath<-paste(PathRawDataVentilation,FileName,sep=c("/"))
  if(!is.na(as.numeric(substr(FileName,1,1)))){
    ReadFile<-try(read.table(file=FilePath,header=TRUE,sep=c("\t"),dec=c(","),stringsAsFactors = F))
    ReadFile$Site<-c("Barn","Storage")[substr(FileName,8,8)==c("E","S")]
    ReadFile$Name<-FileName
    ReadFile<-ReadFile[!(is.na(ReadFile$Date)|is.na(ReadFile$Time)),]
    if(mean(nchar(ReadFile$Date)==8)==1){ReadFile$Date<-paste(substr(ReadFile$Date,1,6),(2000+as.numeric(substr(ReadFile$Date,7,8))),sep=(""))}
    ReadFile$Minute=c(":00")
    
    ReadFile$Time<-paste(ReadFile$Time,ReadFile$Minute,sep=c(""))
    #tryCatch (ReadFile$Time<-as_datetime(dmy_hms(paste(ReadFile$Date,ReadFile$Time,sep=" "),tz=TimeZone),tz=TimeZone),warning = function(war) {return(FileName)})
    ReadFile$Time<-as_datetime(dmy_hms(paste(ReadFile$Date,ReadFile$Time,sep=" "),tz=TimeZone),tz=TimeZone)
    ReadFile[!is.na(ReadFile$Time),]
    colnames(ReadFile)<-gsub(".D","",colnames(ReadFile))
    colnames(ReadFile)<-gsub(" D","",colnames(ReadFile))
    #ReadFile<-ReadFile[,-c("X")]
    if((sum(colnames(ReadFile)=="PC5")!=1)&(substr(FileName,8,8)==c("S"))){ReadFile$PC5<-NA} # Assign NA if missing D5 coloumns
    if((sum(colnames(ReadFile)=="Temp5")!=1)&(substr(FileName,8,8)==c("S"))){ReadFile$Temp5<-NA}# Assign NA if missing Temp5 coloumns
    
    try(colnames(ReadFile)[colnames(ReadFile)==c("Temp5")]<-c("TempExt"))
    try(colnames(ReadFile)[colnames(ReadFile)==c("TempEXT")]<-c("TempExt"))
    if(substr(FileName,7,7)==c("T")){
      countTempData<-countTempData+1
      if (countTempData==1){DataTemperature<-ReadFile[,c("Name","Time","Site","Temp1","Temp2","Temp3","Temp4","TempExt")]}else{DataTemperature<-rbind(DataTemperature,ReadFile[,c("Name","Time","Site","Temp1","Temp2","Temp3","Temp4","TempExt")])}  
    }
    
    if(substr(FileName,7,7)==c("P")){
      countFlowData<-countFlowData+1
      if (countFlowData==1){DataFlow<-ReadFile[,c("Name","Time","Site","PC1","PC2","PC3","PC4","PC5")]}else{DataFlow<-rbind(DataFlow,ReadFile[,c("Name","Time","Site","PC1","PC2","PC3","PC4","PC5")])}  
    }
  
  }
  
  if(sum(substr(FileName,1,1)%in%c("P","T"))>0){
    ReadFile<-try(read.table(file=FilePath,skip=0,header=TRUE,sep=c(","),dec=c("."),stringsAsFactors = F))
    colnames(ReadFile)[colnames(ReadFile)==c("Datetime")]<-c("Time")
    ReadFile<-ReadFile[!(ReadFile$Time==""),]
    ReadFile$Time<-as_datetime(ymd_hms(ReadFile$Time,tz=TimeZone),tz=TimeZone)
    ReadFile$Site<-c("Storage")
    ReadFile$Name<-FileName
    if(substr(FileName,1,1)==c("T")){
      countTempData<-countTempData+1
      if (countTempData==1){DataTemperature<-ReadFile[,c("Name","Time","Site","Temp1","Temp2","Temp3","Temp4","TempExt")]}else{DataTemperature<-rbind(DataTemperature,ReadFile[,c("Name","Time","Site","Temp1","Temp2","Temp3","Temp4","TempExt")])}  
    }
    if(substr(FileName,1,1)==c("P")){ 
      countFlowData<-countFlowData+1
      if (countFlowData==1){DataFlow<-ReadFile[,c("Name","Time","Site","PC1","PC2","PC3","PC4","PC5")]}else{DataFlow<-rbind(DataFlow,ReadFile[,c("Name","Time","Site","PC1","PC2","PC3","PC4","PC5")])}  
    }
  }
  #print(paste(i,colnames(ReadFile)),sep=" ")
  print(paste(i,FileName),sep=" ")
}

summary(DataTemperature)
View(DataTemperature[is.na(DataTemperature$Time),])
DataTemperature<-DataTemperature[!is.na(DataTemperature$Time),]
DataTemperature<-DataTemperature[,!colnames(DataTemperature)==("Name")]
DataTemperature<-unique(DataTemperature)
DataTemperature[,c("Temp1","Temp2","Temp3","Temp4","TempExt")]<-apply(DataTemperature[,c("Temp1","Temp2","Temp3","Temp4","TempExt")],2,as.numeric)
summary(DataTemperature)

View(DataFlow[is.na(DataFlow$Time),])
summary(DataFlow)
DataFlow<-DataFlow[!is.na(DataFlow$Time),]
DataFlow<-DataFlow[,!colnames(DataFlow)==("Name")]
DataFlow<-unique(DataFlow)
DataFlow[,c("PC1","PC2","PC3","PC4","PC5")]<-apply(DataFlow[,c("PC1","PC2","PC3","PC4","PC5")],2,as.numeric)
summary(DataFlow)


Flow<-melt(DataFlow,measure.vars=c("PC1","PC2","PC3","PC4","PC5"),id.vars=c("Time","Site"),fun=length,variable.name=c("Parameter"),value.name = "Value")
Temperature<-melt(DataTemperature,measure.vars=c("Temp1","Temp2","Temp3","Temp4","TempExt"),id.vars=c("Time","Site"),fun=length,variable.name=c("Parameter"),value.name = "Value")
Ventilation<-rbind(Flow,Temperature)
Ventilation$Position<-gsub("PC","",gsub("Temp","",Ventilation$Parameter))
Ventilation$Parameter<-as.character(Ventilation$Parameter)
Ventilation$Parameter[grepl("PC",Ventilation$Parameter)]<-c("Flow_in_PC")
Ventilation$Parameter[grepl("Temp",Ventilation$Parameter)]<-c("Temp_in_degCels")
Ventilation<-Ventilation[!is.na(Ventilation$Value),]
Ventilation$Parameter<-as.factor(Ventilation$Parameter)

Outlier1<-!(Ventilation$Parameter==c("Temp_in_degCels")&Ventilation$Value>90) # Temperature lower than 90 in principe 100 = not connected
Outlier2<-!(Ventilation$Parameter==c("Temp_in_degCels")&Ventilation$Value<(-50))# Temperature higher than 50 because not relistic
Outlier3<-!(Ventilation$Parameter==c("Flow_in_PC")&Ventilation$Value<0)# PC higher than 0 because not realistic
Outlier4<-!(Ventilation$Parameter==c("Flow_in_PC")&Ventilation$Value>(150))# PC lower than 150 because not realistic

Ventilation<-Ventilation[(Outlier1&Outlier2&Outlier3&Outlier4),]


####################################Correction of data
#################Wrong assignation of temperature sensor at barn before the 27/04/2018 at 16h00

Selection<-Ventilation$Site==c("Barn")&Ventilation$Time>=as_datetime(ymd_hms(c("2012/10/01 00:00:00"),tz=TimeZone),tz=TimeZone)&Ventilation$Time<=as_datetime(ymd_hms(c("2018/04/27 16:00:00"),tz=TimeZone),tz=TimeZone)&Ventilation$Position!=c("Ext")&Ventilation$Parameter==c("Temp_in_degCels")
#Ventilation$PositionMes<-Ventilation$Position
Ventilation$Position[Selection]<-as.numeric(Ventilation$Position[Selection])%%4+1

#ggplot(Ventilation,aes(x=as.numeric(PositionMes),y=Position))+
  #geom_point()+
  #facet_grid(Parameter~Site)

#################Lack of ventilation data, adjusted according to supposed ventilation flow before and after the problematic section of data

summary(Ventilation)

# ggplot(data=Ventilation[,],aes(x=Time,y=Value,color=Position))+
#   geom_point()+
#   facet_grid(Parameter~Site,scales = "free")+
#   theme_bw()
# ggsave(file=paste(PathRawDataVentilation,VentilationCompiledFig,sep=c("/")))

write.table(Ventilation,file=paste(PathRawDataVentilation,VentilationCompiledData,sep=c("/")),dec=c(","),row.name=F,sep=c(";"))
