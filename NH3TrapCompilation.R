# # function for compilation of data from Acid Trap system
#Save compiled files for Air_Flow, diffential pressure and temperature at acid trap system and NH3 concentration
#condition to all type of file to be compiled or not

rm(list=ls())

#Libraries
library(ggplot2)
library(lubridate)
library(reshape2)
library(splitstackshape)
library(rstudioapi)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

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



#Conditions
Debug<-FALSE # avoid data saving
DPComp<-TRUE #condition to copmilation of DP data. If FALSE, No compilation
AirFlowComp<-TRUE #condition to compilation of DP data. If FALSE, No compilation
ConcentrationComp<-TRUE #condition to compilation of DP data. If FALSE, No compilation

#Path for files definition
PathRawDataDPT<-"./../GHGData/RawData/NH3Trap/DiffPessTemp"
PathRawDataAirFlow<-"./../GHGData/RawData/NH3Trap/AirFlow"
PathRawDataConcentration<-"./../GHGData/RawData/NH3Trap/NH3Concentration"

#Name of commpiled files
NH3TrapDPTCompiledName<-c("NH3TrapDPTCompiled.csv")
NH3TrapDPTCompileFig<-c("NH3TrapDPTCompiled.png")
NH3TrapAirFlowCompiledName<-c("NH3TrapAirFlowCompiled.csv")
NH3TrapAirFlowCompileFig<-c("NH3TrapAirFlowCompiled.png")
NH3TrapConcentrationCompiledName<-c("NH3TrapConcentrationCompiled.csv")  
NH3TrapConcentrationCompileFig<-c("NH3TrapConcentrationCompiled.png")

#Parametrisation of sile opening loops with path and number of items 
RawDataList_DP_T<-list.files(PathRawDataDPT) #Differential pressure and temperature
RawDataList_DP_T<-RawDataList_DP_T[!(RawDataList_DP_T%in%c(NH3TrapDPTCompiledName,NH3TrapDPTCompileFig))] # Avoid selection of previously compiled files
NBRawDataFiles_DP_T<-length(RawDataList_DP_T)

RawDataList_Air_Flow<-list.files(PathRawDataAirFlow) # AirFlow
RawDataList_Air_Flow<-RawDataList_Air_Flow[!(RawDataList_Air_Flow%in%c(NH3TrapAirFlowCompiledName,NH3TrapAirFlowCompileFig))] # Avoid selection of previously compiled files
NBRawDataFiles_Air_Flow<-length(RawDataList_Air_Flow)

RawDataList_Titration_NH3<-list.files(PathRawDataConcentration) #NH3 concentrations
RawDataList_Titration_NH3<-RawDataList_Titration_NH3[!(RawDataList_Titration_NH3%in%c(NH3TrapConcentrationCompiledName,NH3TrapConcentrationCompileFig))] # Avoid selection of previously compiled files
NBRawDataFiles_Titration_NH3<-length(RawDataList_Titration_NH3)

#parameters
TimeZone<- "Europe/brussels"

#################################
#################################
#################################Compilation DP and T files + transformation of raw data to DP and T, 2 formats F1 et F2
if(DPComp){ 
  print(c("Compilation of DP and T acid Trap source Data"))
  ValidFileName<-T
  F1<-1 #to count
  F2<-1 #to count
  
  for (i in 1:NBRawDataFiles_DP_T){ #Il y a deux types de fichiers ceux commencant par S (F1) et par l'année (F2) qui sont compilé dans deux jeu de données différents pour être fusionnées après transformation plus loin
    InValidFileName<-T
    
    if(!is.na(as.numeric(substr(RawDataList_DP_T[i],1,12)))&sum(substr(RawDataList_DP_T[i],14,14)%in%c("E","S"))==1){ # Les noms des fichers source sont une date de 12 caractére et finissent pas S ou E
      InValidFileName<-F
      ReadFile<-try(ImportCSV(paste(PathRawDataDPT,RawDataList_DP_T[i],sep=c("/")),Head=F))
      if(class(ReadFile)!="try-error"){
        if(dim(ReadFile)[1]>0){
          ReadFile[ReadFile[,]==c("Inf")]<-NA
          #redimensionnement des fichiers non conformes au format pour les premiers fichiers non conformes
          if(dim(ReadFile)[2]==32){ReadFile<-data.frame(ReadFile[,1:30],V31="",V32="",V33="",V34="",V35="",V36="",V37=ReadFile[,31],V38=ReadFile[,32],stringsAsFactors = F)} 
          if(dim(ReadFile)[2]==36){ReadFile<-data.frame(ReadFile[,1:36],V37="",V38="")} 
          
          ReadFile$DateRec<-strsplit(RawDataList_DP_T[i],"_")[[1]][1]
          
          if(gsub(".txt","",strsplit(RawDataList_DP_T[i],"_")[[1]][2])=="E"){ReadFile$Site<-c("Barn")}
          if(gsub(".txt","",strsplit(RawDataList_DP_T[i],"_")[[1]][2])=="S"){ReadFile$Site<-c("Storage")}
          if(F1==1){CompiledRawF1<-ReadFile}
          if(F1>1){CompiledRawF1<-rbind(CompiledRawF1,ReadFile)}
          F1<-F1+1
        }
      }
    }
    
    if(substr(RawDataList_DP_T[i], 1, 1)%in%c("S","E")){#Les fichiers source comencent par S ouE
      
      InValidFileName<-F
      ReadFile<-try(read.table(file=paste(PathRawDataDPT,RawDataList_DP_T[i],sep=c("/")),sep=",",header=T,stringsAsFactors = F))
      if(class(ReadFile)!="try-error"){
        if(dim(ReadFile)[1]>0){
          ReadFile$DateRec<-substr(gsub("_","",RawDataList_DP_T[i]),2,13)
          ReadFile$Site<-c("Storage","Barn")[substr(RawDataList_DP_T[i], 1, 1)==c("S","E")]
          ReadFile$Time<-as_datetime(ymd_hms(ReadFile$Datetime,tz=TimeZone),tz=TimeZone)
         
          if(F2==1){CompiledRawF2<-ReadFile}
          if(F2>1){CompiledRawF2<-rbind(CompiledRawF2,ReadFile)}
          F2<-F2+1
        }
      }
    } 
  
    print(paste("DP_file:",i,"of",NBRawDataFiles_DP_T,sep=" "))
    if(InValidFileName){print(paste(RawDataList_DP_T[i], "Non reconnu ni compilé dans le jeu de donnée  pression temperature NH3, verfifer format"))}
  }
  print(c("formating data and transformation to °C and Pa units"))
  # Data formating of data from type 1 source data
    CompileRaWFromated<-CompiledRawF1
    colnames(CompileRaWFromated)[seq(2,30,2)]<-CompileRaWFromated[1,][seq(1,29,2)] # passe le noms des variables en nom de colonnes
    CompileRaWFromated$Time<-as_datetime(mdy_hms(substr(CompileRaWFromated$V37,3,nchar(CompileRaWFromated$V37)-2),tz=TimeZone),tz=TimeZone)
    colnames(CompileRaWFromated)[c(32,34,36,37,38)]<-c("Heure","Horaire","Jour","Time_R","Time_Rel_R")
    CompileRaWFromated<-CompileRaWFromated[,!grepl("V",as.character(colnames(CompileRaWFromated)))]
    colnames(CompileRaWFromated)<-gsub("=","",as.character(colnames(CompileRaWFromated)))
    colnames(CompileRaWFromated)<-gsub(" ","",as.character(colnames(CompileRaWFromated)))
    CompileRaWFromated<-melt(CompileRaWFromated,id=c("DateRec","Time","Time_R","Time_Rel_R","Horaire","Jour","Heure","Minute","Secondes","Nombre_Mesure","Site"),variable.name=c("Parametre"),value.name=c("Mesure"))
    CompileRaWFromated<-cbind(CompileRaWFromated,colsplit(string=CompileRaWFromated$Parametre, pattern="_", names=c("Type_Parametre", "Poste")))
    CompileRaWFromated$Time<-as_datetime(ymd_hms(CompileRaWFromated$Time,tz=TimeZone),tz=TimeZone)
    Deb_Y_arduino<-as.numeric(substr(CompileRaWFromated$DateRec,1,4))
    Deb_M_arduino<-as.numeric(substr(CompileRaWFromated$DateRec,5,6))
    Deb_D_arduino<-as.numeric(substr(CompileRaWFromated$DateRec,7,8))
    Deb_h_arduino<-as.numeric(substr(CompileRaWFromated$DateRec,9,10))
    Deb_m_arduino<-as.numeric(substr(CompileRaWFromated$DateRec,11,12))
    CompileRaWFromated$Time_Start_Arduino<-as_datetime(ymd_hm(paste(paste(Deb_Y_arduino,Deb_M_arduino,Deb_D_arduino,sep="/"),paste(Deb_h_arduino,Deb_m_arduino,sep=c(":")),sep=c(" ")),tz=TimeZone),tz=TimeZone)
    CompileRaWFromated$Horaire<-as.numeric(CompileRaWFromated$Horaire)
    CompileRaWFromated$Time_Arduino<-CompileRaWFromated$Time_Start_Arduino+CompileRaWFromated$Horaire
    CompileRaWFromated$Type_Parametre<-as.factor(CompileRaWFromated$Type_Parametre)
    CompileRaWFromated$Valeur<-c(NA)
    CompileRaWFromated$Unite_Valeur<-c("")
    
    #Temprature calculation to °C unit
    SelT<-CompileRaWFromated$Type_Parametre==c("Temp")
    Temporaire<-as.numeric(CompileRaWFromated$Mesure[SelT])
    CompileRaWFromated$Valeur[SelT]<-(1/(1.129336*(10^-3)+2.341350*(10^-4)*log(Temporaire)+8.758753*(10^-8)*(log(Temporaire)^3))-273.15) # en °C
    CompileRaWFromated$Unite_Valeur[SelT]<-c("Deg_Celsius")
    
    #DP to pa unit calculation
    SelDP<-CompileRaWFromated$Type_Parametre==c("DP")
    Temporaire<-as.numeric(CompileRaWFromated$Mesure[SelDP])
    CompileRaWFromated$Valeur[SelDP]<--(Temporaire-34.816706)/0.847368/100*9806.6 # en pa
    CompileRaWFromated$Unite_Valeur[SelDP]<-c("Pascal")
    
    ASauver<-CompileRaWFromated[,c("DateRec","Time_Arduino","Nombre_Mesure","Site","Type_Parametre","Poste","Mesure","Valeur","Unite_Valeur")]
    ASauver$Mesure[ASauver$Type_Parametre==c("P")]<-NA#Retire les valeurs non mesurées et abérante
    ASauver<-ASauver[!is.na(ASauver$Mesure)&!is.na(ASauver$Time_Arduino),]
    ASauverF1<-unique(ASauver)
    colnames(ASauverF1)<-c("DateRec","Time","MeasurmentNumber","Site","Parameter","Position","Measure","Value","Unit_Value")
  
  # Data formating of data from type 2 source data
    summary(CompiledRawF2)
    CompiledRawF2<-unique(CompiledRawF2)
    colnames(CompiledRawF2)<-gsub("_","",colnames(CompiledRawF2))
    ASauverF2<-melt(CompiledRawF2,id.vars=c("Datetime","DateRec","Time","Site"),value.name = "Value",variable.name = "Measure",factorsAsStrings = TRUE)
    ASauverF2$AggregMinute<-as.character(trunc(ASauverF2$Time,units=c("mins")))
    ASauverF2<-aggregate(cbind(Value,Time)~DateRec+Site+Measure+AggregMinute,data=ASauverF2[,],FUN="mean")
    ASauverF2$Time<-as_datetime(ASauverF2$Time,tz=TimeZone)
    ASauverF2<-ASauverF2[order(ASauverF2$Time),]
    ASauverF2$Position<-as.character(ASauverF2$Measure)
    ASauverF2$Parameter<-as.character(ASauverF2$Measure)
    ASauverF2$Position<-substr(ASauverF2$Parameter,nchar(ASauverF2$Parameter),nchar(ASauverF2$Parameter))
    ASauverF2$Parameter<-substr(ASauverF2$Parameter,1,nchar(ASauverF2$Parameter)-1)
    ASauverF2$Parameter[ASauverF2$Parameter==c("T")]<-c("Temp")
    ASauverF2$Parameter<-as.factor(ASauverF2$Parameter)
    ASauverF2$Unit_Value<-c("")
    ASauverF2$Unit_Value[ASauverF2$Parameter==c("DP")|ASauverF2$Parameter==c("P")]<-c("Pascal")
    ASauverF2$Unit_Value[ASauverF2$Parameter==c("Temp")]<-c("Deg_Celsius")
  
  #Binding of data of various initial format
  print(c("Binding of all data"))
    
    AllPDPTNH3Data<-rbind(ASauverF1[,c("DateRec","Time","Site","Parameter","Position","Value","Unit_Value")],ASauverF2[,c("DateRec","Time","Site","Parameter","Position","Value","Unit_Value")])
    AllPDPTNH3Data$Position[AllPDPTNH3Data$Position==c("T")]<-c("Ext")
  
  #Removal of outliers due to default in captors
    print(c("Removal of captor failed data"))  
    summary(AllPDPTNH3Data)
    AllPDPTNH3Data<-AllPDPTNH3Data[!(AllPDPTNH3Data$Parameter==c("Temp")&(AllPDPTNH3Data$Value<c(-50)|AllPDPTNH3Data$Value>100)),] # Temperature comprises entre -100 et 50 °C
    AllPDPTNH3Data<-AllPDPTNH3Data[!is.na(AllPDPTNH3Data$Value),] # Pas de NA
    AllPDPTNH3Data<-AllPDPTNH3Data[!AllPDPTNH3Data$Parameter==c("P"),] #Pas de valerus de pression

  print(c("Plot of all Data"))  
  ggplot(AllPDPTNH3Data,aes(x=Time,y=Value,color=Position)) +
  geom_point()+
  facet_grid(Parameter~.,scales = "free")
  ggsave(file=paste(PathRawDataDPT,NH3TrapDPTCompileFig,sep=c("/")))

  NH3TrapDPTCompiled<-AllPDPTNH3Data

  #Saving of compiled data
  print(c("Saving of DP compiled data")) 
  if(!Debug){write.table(NH3TrapDPTCompiled,file=paste(PathRawDataDPT,NH3TrapDPTCompiledName,sep="/" ),row.names = FALSE,dec=c(","),sep=c(";"))}
}

#################################
#################################
#################################Compilation Air Flow 
if(AirFlowComp){
  print(c("Compilation of Air flow acid Trap source Data"))
  for (j in 1:NBRawDataFiles_Air_Flow){
    ReadFileF<-ImportCSV(paste(PathRawDataAirFlow,RawDataList_Air_Flow[j],sep=c("/")))
    if(j==1){AirFlow<-ReadFileF}else{AirFlow<-rbind(AirFlow,ReadFileF)}
    print(paste("Air Flow:",j,"of",NBRawDataFiles_Air_Flow,sep=" "))
  }
  
  # Data formating of data from type 2 source data
  
  AirFlow$StartMes<-DateTime(AirFlow,"Year_Start","Month_Start","Day_Start","Hour_Start","Min_Start")# formating starting hour
  AirFlow$EndMes<-DateTime(AirFlow,"Year_End","Month_End","Day_End","Hour_End","Min_End")# formating ending hour
  AirFlow$MeanTimeMes<-AirFlow$StartMes+(AirFlow$EndMes-AirFlow$StartMes)/2
  AirFlow$DurationMes<-AirFlow$EndMes-AirFlow$StartMes # measurment duration in hour
  AirFlow<-unique(AirFlow)
  AirFlow$Site[AirFlow$Site==c("Stockage")]<-c("Storage")
  AirFlow$Site[AirFlow$Site==c("Etable")]<-c("Barn")
 
  print(c("Plot of all Data"))  
  ggplot(AirFlow,aes(x=StartMes,y=Vol_Start,color=as.factor(System))) +
  geom_point()+
  facet_grid(~Site)
  ggsave(file=paste(PathRawDataAirFlow,NH3TrapAirFlowCompileFig,sep=c("/")))
  
  #Saving of compiled data
  NH3TrapAirFlowCompiled<-AirFlow
  if(!Debug){write.table(NH3TrapAirFlowCompiled,file=paste(PathRawDataAirFlow,NH3TrapAirFlowCompiledName,sep="/" ),row.names = FALSE,dec=c(","),sep=c(";"))}
}

#################################
#################################
#################################Compilation NH3 Concentration

if(ConcentrationComp){
  print(c("Compilation of concentration of acid Trap source Data"))
  for (k in 1:NBRawDataFiles_Titration_NH3){
    ReadFileT<-ImportCSV(paste(PathRawDataConcentration,RawDataList_Titration_NH3[k],sep=c("/")))
    if(k==1){NH3Tit<-ReadFileT}else{NH3Tit<-rbind(NH3Tit,ReadFileT)}
  print(paste("Titration number:",k,"of",NBRawDataFiles_Titration_NH3,sep=" "))
  }

  print(c("Plot of all Data"))  
  ggplot(NH3Tit,aes(x=Num_Sample,y=Ngper100g)) +
    geom_point()+
  ggsave(file=paste(PathRawDataConcentration,NH3TrapConcentrationCompileFig,sep=c("/")))
  
  #Saving of compiled data
  NH3TrapConcentrationCompiled<-NH3Tit
  if(!Debug){write.table(NH3TrapConcentrationCompiled,file=paste(PathRawDataConcentration,NH3TrapConcentrationCompiledName,sep="/" ),row.names = FALSE,dec=c(","),sep=c(";"))}
}
