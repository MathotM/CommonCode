ITMCalibJournalPretreatDataFct<-function(
                                PathSelectedITMCalibData=PathSelectedITMCalibData,
                                PathRawDataVentilation=PathRawDataVentilation,
                                ITMCalibSelectedJournalName=ITMCalibSelectedJournalName,
                                VentilationCompiledFileName=VentilationCompiledFileName,
                                TimeZone=TimeZone,
                                FigType=FigType
                              ){
  
  library(lubridate)
  library(reshape2)
  library(ggplot2)
  print("Opening ItM calibration Selected Data ")
  
  ITMCaliSelectedJournal.df<-try(read.csv(file=paste(PathSelectedITMCalibData,ITMCalibSelectedJournalName,sep=c("/")),sep=c(";"),dec=c(","),header=TRUE,stringsAsFactors = FALSE),silent = TRUE)
  Ventilation.df<-read.csv(file=paste(PathRawDataVentilation,VentilationCompiledFileName,sep=c("/")),sep=c(";"),dec=c(","),header=TRUE,stringsAsFactors = FALSE)

  
  print("Time setting and dat formating")
  #Time Setting
  if(class(ITMCaliSelectedJournal.df)!=c("try-error")){ITMCaliSelectedJournal.df$Time<-as_datetime(ymd_hms(ITMCaliSelectedJournal.df$Time,tz=TimeZone),tz=TimeZone)}
  if(class(Ventilation.df)!=c("try-error")){Ventilation.df$Time<-as_datetime(ymd_hms(Ventilation.df$Time,tz=TimeZone),tz=TimeZone)}
  
  
  #formating 
  if(class(ITMCaliSelectedJournal.df)!=c("try-error")){
    ITMCaliSelectedJournal.df<-dcast(ITMCaliSelectedJournal.df, Site+Position+Comment+Synchronisation~Stage,value.var=c("Time"),mean)
    ITMCaliSelectedJournal.df$Start_Time<-as_datetime(ITMCaliSelectedJournal.df$Start,tz=TimeZone)
    ITMCaliSelectedJournal.df$End_Time<-as_datetime(ITMCaliSelectedJournal.df$End,tz=TimeZone)
    colnames(ITMCaliSelectedJournal.df)[colnames(ITMCaliSelectedJournal.df)==c("Position")]<-"PositionCalibITM"
    ITMCaliSelectedJournal.df$PositionCalibITM<-as.character(ITMCaliSelectedJournal.df$PositionCalibITM)
  }

  #Selection ventilation data for ITM Calibration
  print("Selection ventilation data for ITM Calibration")
  
  NbCalibration<-dim(ITMCaliSelectedJournal.df)[1]
  GoodDataCounter<-0
  for (i in 1:NbCalibration){
   # Data Selection
    Selection<-Ventilation.df$Site==ITMCaliSelectedJournal.df$Site[i]&Ventilation.df$Time>=ITMCaliSelectedJournal.df$Start_Time[i]&Ventilation.df$Time<=ITMCaliSelectedJournal.df$End_Time[i]
    if(sum(Selection)>0){
      GoodDataCounter<-GoodDataCounter+1
      if(GoodDataCounter==1){
        VentilationITMCalib.df<-cbind(Ventilation.df[Selection,],ITMCaliSelectedJournal.df[i,c("Synchronisation","PositionCalibITM")],CalibItem=i)
      }
      if(GoodDataCounter>=1){
        VentilationITMCalib.df<-rbind(VentilationITMCalib.df,cbind(Ventilation.df[Selection,],ITMCaliSelectedJournal.df[i,c("Synchronisation","PositionCalibITM")],CalibItem=i))
      }
      
    }
  print(paste("treatement of ITM Calibration : ",i, "of ",NbCalibration, "calibration measure"))
  }

  
  print("compiled data and formating for plot")
  VentilationITMCalib.df<-VentilationITMCalib.df[VentilationITMCalib.df$Position==VentilationITMCalib.df$PositionCalibITM|VentilationITMCalib.df$Position=="5",]
  VentilationITMCalib.df<-VentilationITMCalib.df[VentilationITMCalib.df$Parameter==c("Flow_in_PC"),]
    
  VentilationITMCalib.df<-dcast(VentilationITMCalib.df,Time+Site+Parameter+Synchronisation+PositionCalibITM+CalibItem~Position,value.var=c("Value"),mean)
  VentilationITMCalib.df$PCTestedITMValue<-apply(VentilationITMCalib.df[,as.character(1:4)],1,FUN=function(x){mean(x,na.rm=TRUE)})
  VentilationITMCalib.df$PCTestedITMLenght<-apply(VentilationITMCalib.df[,as.character(1:4)],1,FUN=function(x){sum(!is.na(x))})
  VentilationITMCalib.df$PCcontrolITMValue<-VentilationITMCalib.df[,"5"]
  VentilationITMCalib.df<-VentilationITMCalib.df[VentilationITMCalib.df$PCcontrolITMValue!=0,]
  summary(VentilationITMCalib.df)
  
  ggplot(VentilationITMCalib.df,aes(x=PCcontrolITMValue,y=PCTestedITMValue,color=PositionCalibITM))+
    geom_point()+
    facet_grid(CalibItem~Site)
  
  
  CalibrationItem<-levels(as.factor(VentilationITMCalib.df$CalibItem))
  NbCalibration2<-length(CalibrationItem)  

  
  print("plot of calibration and lm calculations")
  for (i in 1:NbCalibration2){
    Calib<-CalibrationItem[i]
    ggplot(VentilationITMCalib.df[VentilationITMCalib.df$CalibItem==Calib,],aes(x=Time,y=PCTestedITMValue))+
      geom_point(color=c("blue"))+
      geom_point(aes(x=Time,y=PCcontrolITMValue),color="red")+
      facet_grid(.~Site)+
      ggtitle(VentilationITMCalib.df$Synchronisation[VentilationITMCalib.df$CalibItem==Calib][1])
    ggsave(paste(PathPretreatedPlot,"/",Calib,"_",gsub("/","",gsub(":","",VentilationITMCalib.df$Synchronisation[VentilationITMCalib.df$CalibItem==Calib][1])),".png",sep=c("")))
  
    ggplot(VentilationITMCalib.df[VentilationITMCalib.df$CalibItem==Calib,],aes(x=PCcontrolITMValue,y=PCTestedITMValue))+
      geom_point(color=c("blue"))+
      geom_abline(slope=1,intercept=0)+
      facet_grid(.~Site)+
      ggtitle(VentilationITMCalib.df$Synchronisation[VentilationITMCalib.df$CalibItem==Calib][1])
    ggsave(paste(PathPretreatedPlot,"/LM",Calib,"_",gsub("/","",gsub(":","",VentilationITMCalib.df$Synchronisation[VentilationITMCalib.df$CalibItem==Calib][1])),".png",sep=c("")))
    
    
    
    
  print(paste("Plot ITM Calibration : ",i, "of ",NbCalibration2, "calibration measure"))
  
  
  
  
    
  }
  
summary(VentilationITMCalib.df)

  # plot Full Range calibration
  CalibItemFullRangeItem<-c(2,13,25,36)
  CalibItemFullRange<-VentilationITMCalib.df[VentilationITMCalib.df$CalibItem%in%CalibItemFullRangeItem,]
  
  ggplot(CalibItemFullRange,aes(x=PCcontrolITMValue,y=PCTestedITMValue,colour=PositionCalibITM))+
    geom_point()+
    facet_grid(.~Site)+
    geom_abline(slope=1,intercept=0)+
    stat_smooth(method=lm)+
    theme_bw()+
    labs(x="Réference (% max)",y="Mesuré (% max)",colour="Etable")
  ggsave(paste(PathPretreatedPlot,"/","FullRange",".","png",sep=c("")))
  
    
  ggplot(CalibItemFullRange,aes(x=PCcontrolITMValue,y=(PCTestedITMValue-PCcontrolITMValue)/PCcontrolITMValue,colour=PositionCalibITM))+
    geom_point()+
    facet_grid(.~Site)+
    geom_abline(slope=0,intercept=0)+
    stat_smooth(method=lm)+
    theme_bw()+
    labs(y="difference relative par rapport à la référence ((mesuré-référence)/référence)",x="Réference (% max)",colour="Etable")
    ggsave(paste(PathPretreatedPlot,"/","FullRangeRatio",".","png",sep=c("")))
 
  ggplot(CalibItemFullRange,aes(x=PCcontrolITMValue,y=(PCTestedITMValue-PCcontrolITMValue),color=PositionCalibITM))+
    geom_point()+
    facet_grid(CalibItem~Site)+
    geom_abline(slope=0,intercept=0)+
    stat_smooth(method=lm)+
    theme_bw()+
    labs(y="difference par rapport à la référence (mesuré-référence)",x="Réference (% max)",colour="Etable")
  
    ggsave(paste(PathPretreatedPlot,"/","FullRangeDifference",".","png",sep=c("")))
    
     
  
  # Data formating for linear regressions ajustement
  VentilationITMCalib.df$Synchronisation
  VentilationITMCalib.df$State<-substr(VentilationITMCalib.df$Synchronisation, nchar(VentilationITMCalib.df$Synchronisation), nchar(VentilationITMCalib.df$Synchronisation))
  VentilationITMCalib.df$Sync_Time<-as_datetime(dmy_hms(substr(VentilationITMCalib.df$Synchronisation, 0, nchar(VentilationITMCalib.df$Synchronisation)-2),tz=TimeZone),tz=TimeZone)

  VentilationITMCalib.df$Synchronisation
  VentilationITMCalib.df$State<-substr(VentilationITMCalib.df$Synchronisation, nchar(VentilationITMCalib.df$Synchronisation), nchar(VentilationITMCalib.df$Synchronisation))
  VentilationITMCalib.df$Sync_Time<-as_datetime(dmy_hms(substr(VentilationITMCalib.df$Synchronisation, 0, nchar(VentilationITMCalib.df$Synchronisation)-2),tz=TimeZone),tz=TimeZone)
  VentilationITMCalib.df<-VentilationITMCalib.df[order(VentilationITMCalib.df$Sync_Time),]
  #Ratio -1
  VentilationITMCalib.df$PCTestePCControlRatio<-VentilationITMCalib.df$PCTestedITMValue/VentilationITMCalib.df$PCcontrolITMValue-1
  
  ITMCaliSelectedJournal.df$State<-substr(ITMCaliSelectedJournal.df$Synchronisation, nchar(ITMCaliSelectedJournal.df$Synchronisation), nchar(ITMCaliSelectedJournal.df$Synchronisation))
  ITMCaliSelectedJournal.df$Sync_Time<-as_datetime(dmy_hms(substr(ITMCaliSelectedJournal.df$Synchronisation, 0, nchar(ITMCaliSelectedJournal.df$Synchronisation)-2),tz=TimeZone),tz=TimeZone)
  
  ITMCaliSelectedJournal.df<-ITMCaliSelectedJournal.df[order(ITMCaliSelectedJournal.df$Site,ITMCaliSelectedJournal.df$Position,ITMCaliSelectedJournal.df$Sync_Time),]
  
  SiteLevel<-levels(as.factor(ITMCaliSelectedJournal.df$Site))
  Nbsite<-length(SiteLevel)
  PositionLevel<-levels(as.factor(ITMCaliSelectedJournal.df$Position))
  NBPosition<-length(PositionLevel)
  count<-0
  for (i in 1:Nbsite){
    SiteSel<-SiteLevel[i]
    
    for (j in 1:NBPosition){
      PositionSel<-PositionLevel[j]
      Selection<-ITMCaliSelectedJournal.df$Site==SiteSel&ITMCaliSelectedJournal.df$Position==PositionSel
      if(sum(Selection)>2){
        ITMCalibSel<-ITMCaliSelectedJournal.df[Selection,]
        ITMCalibSel<-ITMCalibSel[order(ITMCalibSel$Sync_Time),]
        if(((dim(ITMCalibSel)[1])%%2)==1){ITMCalibSel<-ITMCalibSel[1:(dim(ITMCalibSel)[1]-1),]}
        if(mean(ITMCalibSel$State==c("S","E"))==1){#check that State is a sequence of "S,E" Vector 
          NBITMCalibCycle<-(dim(ITMCalibSel)[1])/2
          for (k in 1:NBITMCalibCycle){
            count<-count+1
            SelectionLine<-c(k*2+c(-1,0))
            
            ITMCalibCyclData<-ITMCalibSel[SelectionLine,]
            StartState<-ITMCalibCyclData$State=="S"
            EndState<-ITMCalibCyclData$State=="E"
           
            StartCalib<-ITMCalibCyclData$Start_Time[StartState]
            EndCalib<-ITMCalibCyclData$End_Time[EndState]
            StartCorrection<-ITMCalibCyclData$Sync_Time[StartState]
            EndCorrection<-ITMCalibCyclData$Sync_Time[EndState]
            
            SelectionVentilation<-VentilationITMCalib.df$Time>=StartCalib&VentilationITMCalib.df$Time<=EndCalib&VentilationITMCalib.df$Position==PositionSel&VentilationITMCalib.df$Site==SiteSel
            VentilationITMCalibItem.df<-VentilationITMCalib.df[SelectionVentilation,]
            VentilationITMCalibItem.df$Corr_Time<-StartCorrection
            VentilationITMCalibItem.df$Corr_Time[VentilationITMCalibItem.df$Time>ITMCalibCyclData$Start_Time[EndState]]<-EndCorrection
            VentilationITMCalibItem.df$RelTimeDay<-difftime(VentilationITMCalibItem.df$Corr_Time,StartCorrection,units=c("days"))
            
            
            LMCalibITM<-lm(formula=PCTestePCControlRatio~RelTimeDay,data=VentilationITMCalibItem.df)
            SumCalibITM<-summary(LMCalibITM)
            
            ggplot(VentilationITMCalibItem.df,aes(x=as.numeric(RelTimeDay),y=PCTestePCControlRatio))+
            geom_point(color=c("red"))+
            facet_grid(.~Site)+
            geom_abline(slope=1,intercept=0)+
            geom_smooth(method=lm)+
            ggtitle(paste("Calibrations : ", StartCalib," and ", EndCalib,"\n","Corr. Period from", StartCorrection, "to", EndCorrection, sep=c("")))
            ggsave(paste(PathPretreatedPlot,"/","ITMCalibration","_",SiteSel,"_",PositionSel,"_",k,".","png",sep=c("")))
            
            StartCalibItem<-ITMCalibCyclData$CalibItem[StartState]
            EndCalibItem<-ITMCalibCyclData$CalibItem[EndState]
            
            if(count==1){ITMLinearCorrection<-data.frame(Site=SiteSel,Position=PositionSel,ITMCalibCycl=k,StartCalib=StartCalib,EndCalib=EndCalib,StartCorrection=StartCorrection,EndCorrection=EndCorrection,Intercept=SumCalibITM$coefficients[1,1],Slope=SumCalibITM$coefficients[2,1],rsq=round(SumCalibITM$adj.r.squared,3),p_slope=SumCalibITM$coefficients[2,4],p_intercept=SumCalibITM$coefficients[1,4],n_S=NA,n_E=NA)
            }else{ITMLinearCorrection<-rbind(ITMLinearCorrection,data.frame(Site=SiteSel,Position=PositionSel,ITMCalibCycl=k,StartCalib=StartCalib,EndCalib=EndCalib,StartCorrection=StartCorrection,EndCorrection=EndCorrection,Intercept=SumCalibITM$coefficients[1,1],Slope=SumCalibITM$coefficients[2,1],rsq=round(SumCalibITM$adj.r.squared,3),p_slope=SumCalibITM$coefficients[2,4],p_intercept=SumCalibITM$coefficients[1,4],n_S=NA,n_E=NA))}
          }  
        }else{print(paste("No calibration for",SiteSel,"_",PositionSel,"_",k,sep=c("")))}
      }
    }
  }
  
  
  ggplot(ITMLinearCorrection,aes(x=StartCorrection,y=Slope,color=Position))+
    geom_point()+
    facet_grid(.~Site)+
    ggtitle("Slope")
  ggsave(paste(PathPretreatedPlot,"/","ITMCalibration Slope",".png",sep=c("")))
  
  ggplot(ITMLinearCorrection,aes(x=StartCorrection,y=Intercept,color=Position))+
    geom_point()+
    facet_grid(.~Site)+
    ggtitle("Slope")
  ggsave(paste(PathPretreatedPlot,"/","ITMCalibration Intercept",".png",sep=c("")))
  
  ITMCalibVentilationPretreatData.df<-VentilationITMCalib.df
  ITMCalibJournalPretreatData.df<-ITMCaliSelectedJournal.df
  ITMLinearCorrection.df<-ITMLinearCorrection

  ITMCalibPretreated.list<-list(ITMCalibVentilationPretreatData.df,ITMCalibJournalPretreatData.df,ITMLinearCorrection.df)
  
  
  
  return(ITMCalibPretreated.list)
}

