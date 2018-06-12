# # function for selection in Manure Data and selection on time basis
ManureSelection<-function(
                          PathRawDataManureAmount=PathRawDataManureAmount,
                          PathRawDataTemperature=PathRawDataTemperature,
                          PathRawDataAnalysis=PathRawDataAnalysis,
                          ManureAmountCompiledDataName=ManureAmountCompiledDataName,
                          ManureTemperatureCompiledDataName=ManureTemperatureCompiledDataName,
                          ManureAnalysisCompiledDataName=ManureAnalysisCompiledDataName,
                          StartData=StartData,
                          EndData=EndData,
                          TimeZone=TimeZone
                          ){
  
    library(lubridate)
    ManureAmountSelectedData.df<-try(read.csv(file=paste(PathRawDataManureAmount,ManureAmountCompiledDataName,sep=c("/")),sep=c(";"),dec=c(","),header=TRUE,stringsAsFactors = FALSE),silent = TRUE)
    ManureTemperatureSelectedData.df<-try(read.csv(file=paste(PathRawDataTemperature,ManureTemperatureCompiledDataName,sep=c("/")),sep=c(";"),dec=c(","),header=TRUE,stringsAsFactors = FALSE),silent = TRUE)
    ManureAnalysisSelectedData.df<-try(read.csv(file=paste(PathRawDataAnalysis,ManureAnalysisCompiledDataName,sep=c("/")),sep=c(";"),dec=c(","),header=TRUE,stringsAsFactors = FALSE),silent = TRUE)

    #Time Setting
    if(class(ManureAmountSelectedData.df)!=c("try-error")){ManureAmountSelectedData.df$Time<-as_datetime(ymd_hms(ManureAmountSelectedData.df$Time,tz=TimeZone),tz=TimeZone)}
    if(class(ManureTemperatureSelectedData.df)!=c("try-error")){ManureTemperatureSelectedData.df$Time<-as_datetime(ymd_hms(ManureTemperatureSelectedData.df$Time,tz=TimeZone),tz=TimeZone)}
    if(class(ManureAnalysisSelectedData.df)!=c("try-error")){ManureAnalysisSelectedData.df$Time<-as_datetime(ymd(ManureAnalysisSelectedData.df$Time,tz=TimeZone),tz=TimeZone)}
  
    #Time boundaries
    if(class(StartData)==c("character")){StartData<-as_datetime(ymd_hms(StartData,tz=TimeZone),tz=TimeZone)}
    if(class(EndData)==c("character")){EndData<-as_datetime(ymd_hms(EndData,tz=TimeZone),tz=TimeZone)}
    
    #Selection on time
    if(class(ManureAmountSelectedData.df)!=c("try-error")){ManureAmountSelectedData.df<-ManureAmountSelectedData.df[ManureAmountSelectedData.df$Time>StartData&ManureAmountSelectedData.df$Time<EndData,]}
    if(class(ManureTemperatureSelectedData.df)!=c("try-error")){ManureTemperatureSelectedData.df<-ManureTemperatureSelectedData.df[ManureTemperatureSelectedData.df$Time>StartData&ManureTemperatureSelectedData.df$Time<EndData,]}
    if(class(ManureAnalysisSelectedData.df)!=c("try-error")){ManureAnalysisSelectedData.df<-ManureAnalysisSelectedData.df[ManureAnalysisSelectedData.df$Time>StartData&ManureAnalysisSelectedData.df$Time<EndData,]}
    
    #Replacement of error DF by 0
    if(class(ManureAmountSelectedData.df)==c("try-error")){ManureAmountSelectedData.df<-NA}
    if(class(ManureTemperatureSelectedData.df)==c("try-error")){ManureTemperatureSelectedData.df<-NA}
    if(class(ManureAnalysisSelectedData.df)==c("try-error")){ManureAnalysisSelectedData.df<-NA}
    
    
    
    ManureSelectedData.list<-list(ManureAmount=ManureAmountSelectedData.df,ManureTemperature=ManureTemperatureSelectedData.df,ManureAnalysis=ManureAnalysisSelectedData.df)
    
    
    return(ManureSelectedData.list)
}


