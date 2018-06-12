# # function for selection in Manure Data and selection on time basis
InnovaAnalyserSelectionFct<-function(
                          PathRawDataInnovaAnalyser=PathRawDataInnovaAnalyser,
                          InnovaAnalyserCompiledDataName=InnovaAnalyserCompiledDataName, # c("InnovaAnalyserCompiled.csv")
                          StartData=StartData,
                          EndData=EndData,
                          TimeZone=TimeZone
                          ){
  
    library(lubridate)
    print("Opening of data compiled")
    InnovaAnalyserSelectedData.df<-try(read.csv(file=paste(PathRawDataInnovaAnalyser,InnovaAnalyserCompiledDataName,sep=c("/")),sep=c(";"),dec=c(","),header=TRUE,stringsAsFactors = FALSE),silent = TRUE)
    #Time Setting
    if(class(InnovaAnalyserSelectedData.df)!=c("try-error")){InnovaAnalyserSelectedData.df$Time<-as_datetime(ymd_hms(InnovaAnalyserSelectedData.df$Time,tz=TimeZone),tz=TimeZone)}
  
    #Time boundaries
    if(class(StartData)==c("character")){StartData<-as_datetime(ymd_hms(StartData,tz=TimeZone),tz=TimeZone)}
    if(class(EndData)==c("character")){EndData<-as_datetime(ymd_hms(EndData,tz=TimeZone),tz=TimeZone)}
    
    print("Format Time of data compiled")
    #Selection on time
    if(class(InnovaAnalyserSelectedData.df)!=c("try-error")){InnovaAnalyserSelectedData.df<-InnovaAnalyserSelectedData.df[InnovaAnalyserSelectedData.df$Time>StartData&InnovaAnalyserSelectedData.df$Time<EndData,]}

    #Replacement of error DF by 0
    if(class(InnovaAnalyserSelectedData.df)==c("try-error")){InnovaAnalyserSelectedData.df<-NA}

    return(InnovaAnalyserSelectedData.df)
}


