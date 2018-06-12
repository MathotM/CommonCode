
# # function for pretreatment of data from Acid Trap system it merge the data according to validated NH3 concentration measument correct the concnetration for pressure and temperature measurment after modelisation, if required, of missins DP and temperature values
# 
NH3TrapPretreatment<-function(
                  PathNH3TrapDPTSelected=PathNH3TrapDPTSelected,
                  PathConcentrationSelected=PathConcentrationSelected,
                  PathWeatherSelected=PathWeatherSelected,
                  NH3TrapDPTSelectedName=NH3TrapDPTSelectedName,
                  NH3TrapConcentrationSelectedName=NH3TrapConcentrationSelectedName,
                  WeatherSelectedName=WeatherSelectedName,
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
  ImportCSV<-function(SourceFileName,Head=T){
                                            NewObject<-read.table(file=SourceFileName,header=Head,dec=",",sep=";",stringsAsFactors = F)  
                                            return(NewObject)
                                            }
  # date and time formating
  DateTime<-function(data,year,month,day,hour,minute){
                                                      Y<-data[,year]
                                                      M<-data[,month]
                                                      D<-data[,day]
                                                      H<-data[,hour]
                                                      Mi<-data[,minute]
                                                      DateTimeValue<-as_datetime(ymd_hm(paste(Y,"-",M,"-",D," ",H,":",Mi),tz=TimeZone),tz=TimeZone)
                                                      return(DateTimeValue)
                                                    }


  OpenData<-function(Path=Path,Filename=Filename){read.csv(file=paste(Path,Filename,sep=c("/")),header=T,sep=c(";"),dec=c(","),stringsAsFactors = F)}
  DTFct<-function(TimeVariable)  {as_datetime(ymd_hms(TimeVariable,tz=TimeZone),tz=TimeZone)}      
  

#Path definition
  # PathNH3TrapDPTSelected<-c("./../DAQBBMLIM/SelectedData/NH3Trap/Data") # 
  # PathConcentrationSelected<-c("./../DAQBBMLIM/SelectedData/NH3Trap/Data") # chemin d'accés au ficher source des Flux d'Air
  # PathWeatherSelected<-c("./../DAQBBMLIM/SelectedData/Weather/Data")
  # PathPretreatedFiles<-c("./Data")
  # PathPlot<-c("./Plot")
  # NH3TrapDPTSelectedName<-c("NH3TrapDPTSelected.csv")
  # NH3TrapConcentrationSelectedName<<-c("NH3TrapConcentrationSelected.csv") 
  # 
  # NH3TrapEmissionPretreatedName<-c("NH3TrapEmissionPretreated.csv") 
  # 
  # NH3TrapEmissionPretreatedFig<-c("NH3TrapEmissionPretreatedFig.png")
  # 
  # WeatherSelectedName<-c("WeatherSelected.csv")
  # 
  # # Defintion of files names
  # FunctionCompil<-c("NH3TrapPretreatment.R")
  # CommonLibr<-c("CommonLibraries.R")
# 
# #Parameters
#   TimeZone<-c("Europe/brussels")

 

# raw data formating and Compilation
 
  print("Opening of data sets and formating")
  NH3FlowConcentrationSelected.df<-OpenData(PathConcentrationSelected,NH3TrapConcentrationSelectedName)
  NH3TrapDPTSelected.df<- OpenData(PathNH3TrapDPTSelected,NH3TrapDPTSelectedName)
  WeatherSelected.df<-OpenData(PathWeatherSelected,WeatherSelectedName)
  
  summary(NH3FlowConcentrationSelected.df)
  summary(NH3TrapDPTSelected.df)
  summary(WeatherSelected.df)

#Removal of NA values in concentration df and fiomating Data
  NH3FlowConcentrationSelected.df<-NH3FlowConcentrationSelected.df[!is.na(NH3FlowConcentrationSelected.df$AirFlowUncor),]
  NH3FlowConcentrationSelected.df<-NH3FlowConcentrationSelected.df[!is.na(NH3FlowConcentrationSelected.df$Ngper100g),]
  NH3FlowConcentrationSelected.df$Num_Sample<-as.factor(NH3FlowConcentrationSelected.df$Num_Sample)
  NH3TrapDPTSelected.df$Time<-DTFct(NH3TrapDPTSelected.df$Time)

#Creation of a df containing for each sample a time serie at minute resolution step

  AnalysList<-levels(NH3FlowConcentrationSelected.df$Num_Sample)
  NbAnalys<-length(AnalysList)
  
  print("creation of data frame at minute level for each sample")
  
  for (a in 1:NbAnalys){
    AnalyseName<-AnalysList[a]
    Common<-NH3FlowConcentrationSelected.df[NH3FlowConcentrationSelected.df$Num_Sample==AnalyseName,]
    
    Time<-seq(DTFct(Common$StartMes),DTFct(Common$EndMes),by="mins")
    NbValues<-length(Time)
    Temporary<-data.frame(Common,freq=NbValues)
    Temporary<-expandRows(Temporary, "freq")
    Temporary$Time<-Time
    
    if (a==1){
      NH3TrapLong.df<-Temporary
    }else{NH3TrapLong.df<-rbind(NH3TrapLong.df,Temporary)}
  print(paste(a, " of ", NbAnalys," analysis", sep=c("")))
  }
  summary(NH3TrapLong.df)



#################################Merging of data and modeilsation onf missinf values of dp and temperature
  print("Merging of analysis and pression and temperature Data")
  
  NH3TrapDPTSelected.df$TimeAll<-NH3TrapDPTSelected.df$Time
  NH3TrapDPTSelected.df$Time<-as.character(trunc(NH3TrapDPTSelected.df$TimeAll,units=c("mins")))
  
  MinutPDPTNH3Data<-aggregate(data=NH3TrapDPTSelected.df,cbind(Value,TimeAll)~.,FUN=c("mean"))
  MinutPDPTNH3Data$Time<-DTFct(MinutPDPTNH3Data$Time)
  MinutPDPTNH3Data<-MinutPDPTNH3Data[order(MinutPDPTNH3Data$Time),]
  MinutPDPTNH3Data$TimeAll<-as_datetime(MinutPDPTNH3Data$TimeAll,tz=TimeZone)
  
  MinutPDPTNH3Data$MergeFact<-paste(MinutPDPTNH3Data$Time,MinutPDPTNH3Data$Site,MinutPDPTNH3Data$Position)
  NH3TrapLong.df$MergeFact<-paste(NH3TrapLong.df$Time,NH3TrapLong.df$Site,NH3TrapLong.df$Position)
  MergePDPTNH3Data<-dcast(data=MinutPDPTNH3Data[,],MergeFact~Parameter,value.var=c("Value"),mean)
  
  for (a in 1:NbAnalys){#by step otherwise too heavy
   if(a==1){NH3TrapLongAll.df<-merge(NH3TrapLong.df[NH3TrapLong.df$Num_Sample==AnalysList[a],],MergePDPTNH3Data,all.x=T)
   }else{NH3TrapLongAll.df<-rbind(NH3TrapLongAll.df,merge(NH3TrapLong.df[NH3TrapLong.df$Num_Sample==AnalysList[a],],MergePDPTNH3Data,all.x=T))}
    print(paste(a, " of ", NbAnalys," analysis, merging data", sep=c("")))
  }

#################################Modelisation of missing DP and temperature value based on relation with weather data and previous real measurment

  print("Modelisation of missing DP and temperature value based on relation with weather data and previous real measurment")
  
  NH3TrapLongAll.df$DPModel<-c(FALSE)
  NH3TrapLongAll.df$TempModel<-c(FALSE)
  NH3TrapLongAll.df$TimeHour<-as.character(trunc(NH3TrapLongAll.df$Time,units=c("hour")))
  WeatherSelected.df$TimeHour<-as.character(DTFct(WeatherSelected.df$Time))
  NH3TrapLongAll.df<-merge(NH3TrapLongAll.df,WeatherSelected.df[,!colnames(WeatherSelected.df)==c("Time")],by=c("TimeHour"),all.x=T)
  
  for (a in 1:NbAnalys){#correction des temperatures et pressions manquantes sur base soit du jeu de données correspondant à l'échantill si existant sour le jeu de données général correpondant au poste et au site 
      Site<-NH3TrapLongAll.df$Site[NH3TrapLongAll.df$Num_Sample==AnalysList[a]][1]
      Position<-NH3TrapLongAll.df$Position[NH3TrapLongAll.df$Num_Sample==AnalysList[a]][1]
      
      meanDP<-mean(NH3TrapLongAll.df$DP[NH3TrapLongAll.df$Num_Sample==AnalysList[a]&!is.na(NH3TrapLongAll.df$DP)],na.rm = TRUE)
      if(!is.finite(meanDP)){meanDP<-mean(NH3TrapLongAll.df$DP[NH3TrapLongAll.df$Site==Site&NH3TrapLongAll.df$Position==Position&!NH3TrapLongAll.df$DPModel],na.rm = TRUE)
        NH3TrapLongAll.df$DPModel[NH3TrapLongAll.df$Num_Sample==AnalysList[a]&is.na(NH3TrapLongAll.df$DP)]<-c(TRUE)
        NH3TrapLongAll.df$DP[NH3TrapLongAll.df$Num_Sample==AnalysList[a]&is.na(NH3TrapLongAll.df$DP)]<-meanDP
      }
      
      TempsReg<-try(summary(lm(data=NH3TrapLongAll.df[NH3TrapLongAll.df$Num_Sample==AnalysList[a]&!is.na(NH3TrapLongAll.df$Temp)&NH3TrapLongAll.df$Site==Site&NH3TrapLongAll.df$Position==Position,],formula=Temp~tsa)), silent = TRUE)
      if(class(TempsReg)==c("try-error")){TempsReg<-summary(lm(data=NH3TrapLongAll.df[!is.na(NH3TrapLongAll.df$Temp)&NH3TrapLongAll.df$Site==Site&NH3TrapLongAll.df$Position==Position&!NH3TrapLongAll.df$TempModel,],formula=Temp~tsa))}
      
        
      NH3TrapLongAll.df$TempModel[NH3TrapLongAll.df$Num_Sample==AnalysList[a]&is.na(NH3TrapLongAll.df$Temp)]<-c(TRUE)
      NH3TrapLongAll.df$Temp[NH3TrapLongAll.df$Num_Sample==AnalysList[a]&is.na(NH3TrapLongAll.df$Temp)]<-TempsReg$coefficients [1]+TempsReg$coefficients [2]*NH3TrapLongAll.df$tsa[NH3TrapLongAll.df$Num_Sample==AnalysList[a]&is.na(NH3TrapLongAll.df$Temp)]
      print(paste(a, " of ", NbAnalys," analysis for modelisation of missing values", sep=c("")))
      
  }
  summary(NH3TrapLongAll.df)

#################################calculation of concentration in air with correction of pressure and temperature
  print("calculation of concentration in air with correction of pressure and temperature")
  
  NH3Trap.df<-aggregate(data=NH3TrapLongAll.df[,!colnames(NH3TrapLongAll.df)%in%c("TimeHour","MergeFact","Time","timestamp")],cbind(DP,Temp,tsa,plu,hra)~.,FUN=mean)
  
  NH3Trap.df$MoleNTrapped<-NH3Trap.df$H2SO4_Amount*NH3Trap.df$Ngper100g/100/14.0067 #Calcule Du nombre de moles d'azote présentes dans le flacon en supposant le densité du liquide =1kg/l /100 car dans 100 g d'eau et / 14 pour avoir en mole
  NH3Trap.df$VolumeCorrFactor<-(95400+NH3Trap.df$DP)/(273.15+NH3Trap.df$Temp)*(273.15+20)/101325# Calcul du volume (m³ à 20°C et 1 atm) filtré en corrigeant pour la température et le pression L apression atmophérique à 500m d'atitude est supposée être de 95.4 kpa
  NH3Trap.df$AirAmountcor<-NH3Trap.df$AirAmountUncor*NH3Trap.df$VolumeCorrFactor # in liters
  NH3Trap.df$AirFlowcor<-NH3Trap.df$AirFlowUncor*NH3Trap.df$VolumeCorrFactor
  NH3Trap.df$AirMolecor<-NH3Trap.df$AirAmountcor/(24.04) #24.4 est la densité de l'air à 20°C 1 atm
  NH3Trap.df$ppmNTrapped<-NH3Trap.df$MoleNTrapped/NH3Trap.df$AirMolecor*10^6 # Concentration en ppm mole/mole 
  
  NH3Trap.df$DurationMes<-as.numeric(NH3Trap.df$DurationMes) 
  View(NH3Trap.df[NH3Trap.df$AirFlowUncor<0.3,])
  NH3Trap.df$MeanTimeMes<-DTFct(NH3Trap.df$MeanTimeMes)
  NH3TrapEmissionPretreatedData.df<-NH3Trap.df

return(NH3TrapEmissionPretreatedData.df)
}