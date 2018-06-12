# Pre treatment of Data from Manure
# # function for pretreatment of Manure Data

ManurePretreatment<-function(
  PathManureSelected=PathManureSelected,
  ManureAmountSelectedName=ManureAmountSelectedName,
  ManureTemperatureSelectedName=ManureTemperatureSelectedName,
  ManuraAnalysisSelectedName=ManuraAnalysisSelectedName,
  TimeZone=TimeZone
)
{
  
  #Libraries
  library(ggplot2)
  library(lubridate)
  library(reshape2)
  library(splitstackshape)
  
  
  
  # Sub function definition
  # Opnening of files
  
  OpenData.fct<-function(Path=Path,Filename=Filename){read.csv(file=paste(Path,Filename,sep=c("/")),header=T,sep=c(";"),dec=c(","),stringsAsFactors = F)}
  DT.fct<-function(TimeVariable)  {as_datetime(ymd_hms(TimeVariable,tz=TimeZone),tz=TimeZone)}      
  D.fct<-function(TimeVariable)  {as_datetime(ymd(TimeVariable,tz=TimeZone),tz=TimeZone)}      
  
  
  #Opening of Data
  ManureAmountPretreated.df<-OpenData.fct(PathManureSelected,ManureAmountSelectedName)
  ManureTemperaturePretreated.df<-OpenData.fct(PathManureSelected,ManureTemperatureSelectedName)
  ManureAnalysisPretreated.df<-OpenData.fct(PathManureSelected,ManureAnalysisSelectedName)
  
  #Manure Volume Calculation
  ManureAmountPretreated.df$ManureVolumeBarnM3<-ManureAmountPretreated.df$Length_Barn*ManureAmountPretreated.df$Width_Barn*ManureAmountPretreated.df$Heigth_Barn
  ManureAmountPretreated.df$ManureVolumeStorageM3<-ManureAmountPretreated.df$Length_Storage*ManureAmountPretreated.df$Width_Storage*ManureAmountPretreated.df$Heigth_Storage
  
  #Time format setting
  ManureAmountPretreated.df$Time<-DT.fct(ManureAmountPretreated.df$Time)
  ManureTemperaturePretreated.df$Time<-DT.fct(ManureTemperaturePretreated.df$Time)
  ManureAnalysisPretreated.df$Time<-D.fct(ManureAnalysisPretreated.df$Time)
  
  
  ManurePretreatedData.list<-list(ManureAmount=ManureAmountPretreated.df,ManureTemperature=ManureTemperaturePretreated.df,ManureAnalysis=ManureAnalysisPretreated.df)
  
  return(ManurePretreatedData.list)
}
