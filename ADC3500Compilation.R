# Script compilation of data from INNOVa, save selected Data and source files and plot it with compilation of errors in log file

  library(rstudioapi)
  rm(list=ls())
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  
  
  PathCommonPlotParameters<-c("./CommonPlotParameters")
  PathRawData<-c("./../GHGData/RawData/ADCMGA3500/LOGS/LOG1") 
  PathPlot<-c("./../GHGData/RawData/ADCMGA3500") 
  
# File name definition
  CommonLibrName<-c("CommonLibraries.R")
  CommonPlotParametersName<-c("CommonPlotParam01.R")
  
  CompiledFilesListName<-c("CompiledFilesList.csv")
  ExistingCompiledDataName<-c("ADCMGA3500Compiled.csv")
  LogProbFileName<-c("LogProb.csv")
  CompiledName<-c("ADCMGA3500CompiledProb.csv")
  CompiledPlot<-c("ADCMGA3500Compiled.png")
  
  
#Parameters
  TimeZone<-c("Europe/brussels")
  Gas <-c("CH4","N2O","CO2","NH3","H2O") # Gas list definition 
  
#Libraries
  library(ggplot2)
  library(lubridate)
  library(reshape2)

# Sourcing of graphical parameters
  source(paste(PathCommonPlotParameters,CommonPlotParametersName,sep=c("/")) )
  CommonTheme<-CommonPlotParam01()

# Sub function definition
  # for plot
    PlotFigure<-function(DataFig){
      ggplot(DataFig,aes(x=Time,y=Concentration_ppm))+
        geom_point()+
        facet_grid(Gas~.,scale="free")+
        CommonTheme
      }
  # for data transformation  
    FormatFig<-function(PourTrans){melt(PourTrans,id=c("Date","Time","Type","Status","Inlet","Source"),variable.name=c("Gas"),value.name=c("Concentration_ppm"))}

  # names definitions function
    NameFileFct<-function(FilePath,FileName){paste(FilePath,FileName,sep=c("/"))} #Create file names
    NameFigFct<-function(FigPath,FigName){paste(FigPath,FigName,sep=c("/"))} #create fig Names

  # Data opening
   OpenCSVFct<-function(TabToOpen){read.table(file=TabToOpen,sep=c(","),dec=c("."), header=T,stringsAsFactors = F)}# Open CSV type files
   OpenCSV2Fct<-function(TabToOpen){read.table(file=TabToOpen,sep=c(";"),dec=c(","), header=T,stringsAsFactors = F)}# Open CSV type files
   
# raw data formating and Compilation

  print("raw data formating and Compilation")
  
  RawDataList_ADCMGA3500<-list.files(PathRawData) # creation of a list of present data in raw data file
  RawDataList_ADCMGA3500<- RawDataList_ADCMGA3500[!( RawDataList_ADCMGA3500%in%c(CompiledFilesListName,ExistingCompiledDataName,LogProbFileName,CompiledName,CompiledPlot))]#remove compiled files
  RawDataList_ADCMGA3500<- RawDataList_ADCMGA3500[grepl(c(".CSV"),RawDataList_ADCMGA3500)]# Selection of only .CSV files

  
  
  AllreadyCompiled<-try( OpenCSVFct(NameFileFct(PathRawData,CompiledFilesListName)), silent = TRUE) # Check Werther allready compiled files 
  Compiles<-data.frame(Consulte=RawDataList_ADCMGA3500,Compile=c(""),stringsAsFactors = F)


  if(class(AllreadyCompiled)!=c("try-error")){# Create a list of new file to compile according to already compiled files and list of file to compile
    FileListToCompil<-RawDataList_ADCMGA3500[!RawDataList_ADCMGA3500 %in% t(as.vector(AllreadyCompiled))]
    Datacompil<-OpenCSV2Fct(NameFileFct(PathRawData,ExistingCompiledDataName))
    Datacompil$Time<-as_datetime(ymd_hms(Datacompil$Time,tz=TimeZone),tz=TimeZone)
  }else{
    FileListToCompil<-RawDataList_ADCMGA3500
  }
  
  
  FileNumber<-length(FileListToCompil)
  LogProb<-data.frame(File="",Line="", Column="", Gas=c(""),Unit=c(""),stringsAsFactors = F) #File recording information
  
  if(FileNumber==0){print(c("No file to compile"))}
  if(FileNumber>0){
    LogProb<-data.frame(File=FileListToCompil,Line="", Column="", Gas=c(""),Unit=c(""),stringsAsFactors = F) #Fichier d'enregistrement des remarques en cas de bug
    
    #Loop for file concentration
    for (i in 1:FileNumber){
      #file identification
      SourceFile<-FileListToCompil[i]
      print(paste(SourceFile,": ", i," of ",FileNumber,"files"))
      
      #file opening
      Data01<-read.table(file=NameFileFct(PathRawData,SourceFile),sep=c(","),header=T,dec=c("."),stringsAsFactors =FALSE)
      Data01$Time<-as_datetime(ymd_hms(paste(Data01$Date,Data01$Time),tz=TimeZone),tz=TimeZone)
      #Data01$Time<-as_datetime(dmy_hm(Data01$Time,tz=TimeZone),tz=TimeZone)
      Compiles$Compile[Compiles$Consulte==SourceFile]<-c("x")
      # Check that dataframe are not null
      if(dim(Data01)[1]==0|dim(Data01)[2]==0){ #
        LogProb$Line[i]<-dim(Data01)[1]
        LogProb$Column[i]<-dim(Data01)[2]
      }
      Data01$Source<-SourceFile

      # si premier fichier Initial
      if (i==1){
          Data<-Data01
      }
      if (i!=1){
        Data<- rbind(Data,Data01)
      }
        
    }
      
  }
    
  if(class(AllreadyCompiled)!=c("try-error")&FileNumber!=0){Datacompil<- rbind(Datacompil,Data)}else{Datacompil<- Data}
  print(summary(Datacompil))
  
  colnames(Datacompil)[colnames(Datacompil)%in%c(paste("Gas",1:4,sep=c("")))]<-Gas[1:4]
  Datacompil$H2O<-as.numeric(gsub(",","",Datacompil$H2O))

#Plot all data   
 print("Plot all data ")  
 if(LogProb[1,1]!=""){ 
   DataFig<-FormatFig(Datacompil)
      PlotFigure(DataFig)
      ggsave(NameFigFct(PathPlot,CompiledPlot))
 }

 
  DataFig<-FormatFig(Datacompil)
 #Tout CO2 et CH4 Recpetion 


 Start<-as_datetime(ymd_hms(c("2018/03/05 08:05:00"),tz=TimeZone),tz=TimeZone)
 End<-as_datetime(ymd_hms(c("2018/03/10 14:20:00"),tz=TimeZone),tz=TimeZone) 
 
 PlotFigure(DataFig[DataFig$Time>Start&DataFig$Time<End,])
 
 Start<-as_datetime(ymd_hms(c("2018/03/10 08:05:00"),tz=TimeZone),tz=TimeZone)
 End<-as_datetime(ymd_hms(c("2018/03/30 14:20:00"),tz=TimeZone),tz=TimeZone) 
 
 PlotFigure(DataFig[DataFig$Time>Start&DataFig$Time<End,])
 
 
 
 
 #Tout sauf NH3 Calib 1 
 Time<-as_datetime(ymd_hms(c("2018/04/10 13:07:00","2018/04/10 13:20:00","2018/04/10 13:30:00","2018/04/10 13:40:00","2018/04/10 13:53:00","2018/04/10 14:00:00"),tz=TimeZone),tz=TimeZone)
 Duration<-300 #Seconds
 N2O<-c(0, 0.78195, 1.58395, 2.34585, 3.210005, 4.03406)
 NH3<-c(rep(0,6))
 CH4<-c(0,9.75,19.75,29.25,40.025,50.3)
 CO2<-c(0,390, 790, 1170, 1601, 2012)
 
 Calibration.df<-data.frame(Time,Duration,N2O,NH3,CH4,CO2)
 Calibration.df<-melt( Calibration.df,id=c("Time","Duration"),variable.name=c("Gas"),value.name=c("Concentration_ppm"))
 
  Start<-as_datetime(ymd_hms(c("2018/04/10 08:05:00"),tz=TimeZone),tz=TimeZone)
  End<-as_datetime(ymd_hms(c("2018/04/10 14:20:00"),tz=TimeZone),tz=TimeZone) 

PlotFigure(DataFig[DataFig$Time>Start&DataFig$Time<End,])+
geom_point(data=Calibration.df,aes(x=Time, y=Concentration_ppm), color="red",size=4)

PlotFigure(DataFig[DataFig$Time>Start&DataFig$Time<End&DataFig$Gas==c("CH4"),])
  

#Tout sauf NH3 Calib 2

Time<-rep(as_datetime(ymd_hms(c("2018/04/20 12:06:00"),tz=TimeZone),tz=TimeZone),6)+(0:5)*300
Duration<-300 #Seconds
N2O<-c(0,0.082, 1.604, 2.406, 3.220, 4.01)
NH3<-c(rep(0,6))
CH4<-c(0,10,20,30,40.025,50)
CO2<-c(0,400, 800, 1200, 1601, 2000)

Calibration.df<-data.frame(Time,Duration,N2O,NH3,CH4,CO2)
Calibration.df<-melt( Calibration.df,id=c("Time","Duration"),variable.name=c("Gas"),value.name=c("Concentration_ppm"))

Start<-as_datetime(ymd_hms(c("2018/04/2012:00:00"),tz=TimeZone),tz=TimeZone)
End<-as_datetime(ymd_hms(c("2018/04/20 12:40:00"),tz=TimeZone),tz=TimeZone) 

PlotFigure(DataFig[DataFig$Time>Start&DataFig$Time<End,])+
  geom_point(data=Calibration.df,aes(x=Time, y=Concentration_ppm), color="red",size=4)+
  geom_point(data=Calibration.df[Calibration.df$Gas==c("CO2"),],aes(x=Time, y=Concentration_ppm/2012*2500), color="blue",size=4)

PlotFigure(DataFig[DataFig$Time>Start&DataFig$Time<End&DataFig$Gas==c("CH4"),])

#NH3
DataFig<-FormatFig(Datacompil)
Time<-as_datetime(ymd_hms(c("2018/04/13 08:07:00"),tz=TimeZone),tz=TimeZone)
Duration<-300 #Seconds
N2O<-c(NA)
NH3<-c(200)
CH4<-c(NA)
CO2<-c(NA)

Calibration.df<-data.frame(Time,Duration,N2O,NH3,CH4,CO2)
Calibration.df<-melt( Calibration.df,id=c("Time","Duration"),variable.name=c("Gas"),value.name=c("Concentration_ppm"))

Start<-as_datetime(ymd_hms(c("2018/04/13 08:05:00"),tz=TimeZone),tz=TimeZone)
End<-as_datetime(ymd_hms(c("2018/04/13 08:10:00"),tz=TimeZone),tz=TimeZone) 

ggplot(DataFig[DataFig$Time>Start&DataFig$Time<End&DataFig$Gas==c("NH3"),],aes(y=Concentration_ppm,x=Time))+
  geom_point()+
  ggtitle(c("NH3"))+
  geom_point(data=Calibration.df,aes(x=Time, y=Concentration_ppm), color="red",size=4)




Calibration.df<-data.frame(Time,Duration,N2O,NH3,CH4,CO2)
Calibration.df<-melt( Calibration.df,id=c("Time","Duration"),variable.name=c("Gas"),value.name=c("Concentration_ppm"))

Start<-as_datetime(ymd_hms(c("2018/03/30 10:30:00"),tz=TimeZone),tz=TimeZone)
End<-as_datetime(ymd_hms(c("2018/03/30 15:00:00"),tz=TimeZone),tz=TimeZone) 

ggplot(DataFig[DataFig$Time>Start&DataFig$Time<End,],aes(y=Concentration_ppm,x=Time))+
  geom_point()+
  ggtitle(c(""))+
  facet_grid(Gas~.,scales="free_y")


Start<-as_datetime(ymd_hms(c("2018/04/01 10:30:00"),tz=TimeZone),tz=TimeZone)
End<-as_datetime(ymd_hms(c("2018/04/30 15:00:00"),tz=TimeZone),tz=TimeZone) 

ggplot(DataFig[DataFig$Time>Start&DataFig$Time<End,],aes(y=Concentration_ppm,x=Time))+
  geom_point()+
  ggtitle(c(""))+
  facet_grid(Gas~.,scales="free_y")



 
#Formating and Saving of Data
print("Formating and Saving of Data")    

if(class(AllreadyCompiled)!=c("try-error")){AllreadyCompiled<-c(t(as.vector(AllreadyCompiled)),FileListToCompil)}else{AllreadyCompiled<-FileListToCompil}
    
DataCompiled<-Datacompil
CompiledFilesList<-AllreadyCompiled
Compiled<-Compiles
    
write.table(DataCompiled,file=NameFileFct(PathRawData,ExistingCompiledDataName),dec=c(","),sep=c(";"),row.names = FALSE,col.names = TRUE)
write.table(as.data.frame(CompiledFilesList),file=NameFileFct(PathRawData,CompiledFilesListName),sep=c(";"),dec=c(","), row.names = F)
write.table(as.data.frame(LogProb),file=NameFileFct(PathRawData,LogProbFileName),sep=c(";"),dec=c(","), row.names = F)
write.table(Compiled,file=NameFileFct(PathRawData,CompiledName),dec=c(","),sep=c(";"),row.names = FALSE,col.names = TRUE)
    

