# Script compilation of data from INNOVa, save selected Data and source files and plot it with compilation of errors in log file

  library(rstudioapi)
  rm(list=ls())
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  
  
  PathCommonPlotParameters<-c("./CommonPlotParameters")
  PathRawData<-c("./../GHGData/RawData/InnovaAnalyser") 
  PathPlot<-c("./../GHGData/RawData/InnovaAnalyser") 
  
# File name definition
  CommonLibrName<-c("CommonLibraries.R")
  CommonPlotParametersName<-c("CommonPlotParam01.R")
  
  CompiledFilesListName<-c("CompiledFilesList.csv")
  ExistingCompiledDataName<-c("InnovaAnalyserCompiled.csv")
  LogProbFileName<-c("LogProb.csv")
  CompiledName<-c("InnovaAnalyserCompiledProb.csv")
  CompiledPlot<-c("InnovaAnalyserCompiled.png")
  
  
#Parameters
  TimeZone<-c("Europe/brussels")
  Gas <-c("N2O","C2H6O","CO2","CH4","NH3","Water") # Gas list definition 
  
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
        facet_grid(Gas~Site,scale="free")+
        CommonTheme
      }
  # for data transformation  
    FormatFig<-function(PourTrans){melt(PourTrans,id=c("Site","Time","Unit","Source"),variable.name=c("Gas"),value.name=c("Concentration_ppm"))}

  # names definitions function
    NameFileFct<-function(FilePath,FileName){paste(FilePath,FileName,sep=c("/"))} #Create file names
    NameFigFct<-function(FigPath,FigName){paste(FigPath,FigName,sep=c("/"))} #create fig Names

  # Data opening
   OpenCSVFct<-function(TabToOpen){read.table(file=TabToOpen,sep=c(";"),dec=c(","), header=T,stringsAsFactors = F)}# Open CSV type files

# raw data formating and Compilation

  print("raw data formating and Compilation")
  
  RawDataList_Innova<-list.files(PathRawData) # creation of a list of present data in raw data file
  RawDataList_Innova<-RawDataList_Innova[!(RawDataList_Innova%in%c(CompiledFilesListName,ExistingCompiledDataName,LogProbFileName,CompiledName,CompiledPlot))]#remove compiled files
  RawDataList_Innova<-RawDataList_Innova[grepl(c(".txt"),RawDataList_Innova)]# Selection of only . txt files
  
  AllreadyCompiled<-try( OpenCSVFct(NameFileFct(PathRawData,CompiledFilesListName)), silent = TRUE) # Check Werther allready compiled files 
  Compiles<-data.frame(Consulte=RawDataList_Innova,Compile=c(""),stringsAsFactors = F)


  if(class(AllreadyCompiled)!=c("try-error")){# Create a list of new file to compile according to already compiled files and list of file to compile
    FileListToCompil<-RawDataList_Innova[!RawDataList_Innova %in% t(as.vector(AllreadyCompiled))]
    Datacompil<-OpenCSVFct(NameFileFct(PathRawData,ExistingCompiledDataName))
    Datacompil$Time<-as_datetime(ymd_hms(Datacompil$Time,tz=TimeZone),tz=TimeZone)
  }else{
    FileListToCompil<-RawDataList_Innova
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
      Data01<-read.table(file=NameFileFct(PathRawData,SourceFile),sep=c("\t"),header=T,dec=c("."),stringsAsFactors =FALSE)
      
      if(class(Data01[,2])==c("character")){
        Data01<-read.table(file=NameFileFct(PathRawData,SourceFile),sep=c("\t"),header=T,dec=c(","),stringsAsFactors =FALSE)
        Data01$Time<-as_datetime(dmy_hm(Data01$Time,tz=TimeZone),tz=TimeZone)
      }else{
        Data01$Time<-as_datetime(dmy_hms(Data01$Time,tz=TimeZone),tz=TimeZone)
      }

      Data01$Site<-c("Barn","Storage")[c(grepl(c("_E.txt"),SourceFile),grepl(c("_S.txt"),SourceFile))] #défini le site sur base du nom du ficheir source
      Compiles$Compile[Compiles$Consulte==SourceFile]<-c("x")
      # Check that dataframe are not null
      if(dim(Data01)[1]==0|dim(Data01)[2]==0){ #
        LogProb$Line[i]<-dim(Data01)[1]
        LogProb$Column[i]<-dim(Data01)[2]
      }
      
        
      #Write unit in data frame. check for ppm   
      if (sum(grepl(pattern ="ppm",colnames(Data01)))!=6){
        LogProb$Unit[i]<-"Toutes les unités ne sont pas en ppm"
        Data01$Unit<-c("")
      }else{Data01$Unit<-c("ppm")}
        
        #Vérifie la présence des Gas dans je jeu de données
      for (j in 1:length(Gas)){
        GasSel<-Gas[j]
        CheckGas<-grepl(GasSel,colnames(Data01))
        if(sum(CheckGas)==1){colnames(Data01)[CheckGas]<-GasSel
          }else{
            LogProb$Gas[i]<-c(paste(GasSel,"=",sum(CheckGas),sep=c("")))
          }
      }  
        
      Data01$Source<-SourceFile
      Data01<-Data01[,c("Time","Site","Unit","Source",Gas)]

      # si premier fichier Initial
      if (i==1){
          Data<-Data01
      }
      if (i!=1){
        Data<- rbind(Data,Data01)
      }
        
    }
      
  }
    
  if(class(AllreadyCompiled)!=c("try-error")){Datacompil<- rbind(Datacompil,Data)}else{Datacompil<- Data}
  print(summary(Datacompil))
#Plot all data   
 print("Plot all data ")  
 if(LogProb[1,1]!=""){ 
   DataFig<-FormatFig(Datacompil)
      PlotFigure(DataFig)
      ggsave(NameFigFct(PathPlot,CompiledPlot))
 }
 
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
    

