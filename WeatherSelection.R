# # function for selection in ventilation compiled file and plot of data seleced
WeatherSelection<-function(
                          NameCompiledFile=NameCompiledFile,
                          PathRawDataWeather=PathRawDataWeather,
                          StartData=StartData,
                          EndData=EndData,
                          TimeZone=TimeZone
    ){
    library(lubridate)
    Weather.df<-read.csv(file=paste(PathRawDataWeather,NameCompiledFile,sep=c("/")),sep=c(";"),dec=c(","),header=TRUE,stringsAsFactors = FALSE)
    Weather.df$Time<-as_datetime(ymd_hms(Weather.df$Time,tz=TimeZone),tz=TimeZone)
    if(class(StartData)==c("character")){StartData<-as_datetime(ymd_hms(StartData,tz=TimeZone),tz=TimeZone)}
    if(class(EndData)==c("character")){EndData<-as_datetime(ymd_hms(EndData,tz=TimeZone),tz=TimeZone)}
    
    Weather.df<-Weather.df[Weather.df$Time>StartData&Weather.df$Time<EndData,]
    return(Weather.df)
  }