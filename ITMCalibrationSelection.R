ITMCalibibrationSelectionFct<-function(
  PathITMCalibJournalCompiled=PathITMCalibJournalCompiled,
  ITMCalibCompiledJournalName=ITMCalibCompiledJournalName,
  TimeZone=TimeZone,
  FigType=FigType,
  StartData=StartData,
  EndData=EndData
){
  
  library(lubridate)
  print("Opening of Journal Data")
  
  ITMCalibJournal.df<-try(read.csv(file=paste(PathITMCalibJournalCompiled,ITMCalibCompiledJournalName,sep=c("/")),sep=c(";"),dec=c(","),header=TRUE,stringsAsFactors = FALSE),silent = TRUE)
  
  #Time Setting
  if(class(ITMCalibJournal.df)!=c("try-error")){ITMCalibJournal.df$Time<-as_datetime(ymd_hms(ITMCalibJournal.df$Time,tz=TimeZone),tz=TimeZone)}
  
  #Time boundaries
  if(class(StartData)==c("character")){StartData<-as_datetime(ymd_hms(StartData,tz=TimeZone),tz=TimeZone)}
  if(class(EndData)==c("character")){EndData<-as_datetime(ymd_hms(EndData,tz=TimeZone),tz=TimeZone)}
  
  print("Format Time of data compiled")
  #Selection on time
  if(class(ITMCalibJournal.df)!=c("try-error")){ITMCalibJournalSelectedData.df<-ITMCalibJournal.df[ITMCalibJournal.df$Time>StartData&ITMCalibJournal.df$Time<EndData,]}
  
  #Replacement of error DF by 0
  if(class(ITMCalibJournal.df)==c("try-error")){ITMCalibJournalSelectedData.df<-NA}
  
  return(ITMCalibJournalSelectedData.df)
}