InnovaAnalyserPretreatment<-function( 
                            PathFig=PathFig,
                            PathJournal=PathJournal,
                            PathConcentrationsInnnovaSelected=PathConcentrationsInnnovaSelected,
                            PathPretreatData=PathPretreatData,
                            PathPretreatPlot=PathPretreatPlot,
                            PathFunction=PathFunction,
                            ConcentrationsInnnovaSelectedName=ConcentrationsInnnovaSelectedName,
                            JournalName=JournalName,
                            PretreatInnovaDataName=PretreatInnovaDataName,
                            FunctionPreTreat=FunctionPreTreat,
                            ExclCriteria=ExclCriteria,
                            TimeZone=TimeZone){ # Critere de temps (minute d'exclusion des données. Premier agumen= minutes depuis le debut du poste, seconde agrument minuteavant la fin du poste. Par defaut 2 et 1 )
  
  #source(paste(PositionFichFunction,"./Param_Graphique/FctTemps.R",sep=c("/"))) 
  #Commontheme<-FctTemps()
  library(lubridate)
  library(reshape2)
  library(ggplot2)
  
  ##### Common graphic function
  CommonPlotParam01V<-theme_bw() +
    theme(text = element_text(size=15),
          axis.line.x = element_line(color="black", size = 1),
          axis.line.y = element_line(color="black", size = 1),
          axis.text.x = element_text(angle=45, hjust=1),
          # legend.title=element_blank(),
          # legend.position=c(0.85,0.89),
          # legend.text=element_text(size=15),
          # legend.background = element_rect(fill="transparent"),
          # legend.key = element_rect(colour = "transparent", fill = "transparent"),
          # panel.border = element_blank(),
          panel.background = element_blank(),
          panel.grid.major = element_line(colour = "gray", size=0.4),
          panel.grid.minor = element_line(colour = "gray", size=0.2))
  
  
  #####Local functions
  PathNameFct<-function(FilePath,FileName){paste(FilePath,FileName,sep=c("/"))} #filepath creation function
  CSVOpeningFct<-function(TabToOpen){read.table(file=TabToOpen,sep=c(";"),dec=c(","), header=T,stringsAsFactors = F)}# ouvre des tableau de données a entête, séprarateur de décimale (,), séparateur (;), avec entête et sans convertion des charcateres en facteurs)
  
  TranformFigFct<-function(ForTransformation,Selection){melt(ForTransformation[Selection,],id=c("Site","Time","Unit","Source","Position","Analyser","GHGMeasurment"),variable.name=c("Gas"),value.name=c("Concentration_ppm"))}
  


########################################Parametrisation of Data exlusion within a sampling sequence in minutes
  # ExclCriteria=c(2,1)
  Border<-data.frame(Low=ExclCriteria[1],High=ExclCriteria[2])# #Retire les valeurs des low premères and high dernières minutes d'une séquence

# ####################################### Path
# 
#   PathFig<-("./Plot")
#   PathJournal<-c("./../GHGData/Information/Chambers")#c("./../../../GHGData/Information/Chambers")
#   PathConcentrationsInnnovaSelected<-c("./../DAQBBMLIM/SelectedData/InnovaAnalyser/Data") #file:///Y:/GES/GlobalDAQ/DAQBBMLIM/SelectedData/InnovaAnalyser/Data/InnovaAnalyserSelected.csv
#   PathPretreatData<-c("./Data")
#   PathPretreatPlot<-c("./Plot")
#   ConcentrationsInnnovaSelectedName<-c("InnovaAnalyserSelected.csv")
#   JournalName<-c("Journal_Mes_Chambers.csv")  
#   PretreatInnovaDataName<-c("InnovaPretreat.csv")
#   TimeZone<-c("Europe/brussels")
  
#######################################Opening of innova concentrations and journal
  print("Opening of innova concentrations and journal and formating of Time Data")
  
  GasConcentrationInnova<-CSVOpeningFct(PathNameFct(PathConcentrationsInnnovaSelected,ConcentrationsInnnovaSelectedName))
  GasConcentrationInnova$Time<-ymd_hms(GasConcentrationInnova$Time,tz=TimeZone)
  
  JournalGHG<- CSVOpeningFct(PathNameFct(PathJournal,JournalName))
  JournalGHG$Time<-as_datetime(ymd_hms(paste(JournalGHG$Year,"/",JournalGHG$Mont,"/",JournalGHG$Day," ",JournalGHG$Hour,":",JournalGHG$Minute,":",JournalGHG$Second,sep=""),tz=TimeZone),tz=TimeZone)
  
#######################################formating of journal to select only gas emission validated Data
  JournalGHG<- JournalGHG[!is.na(JournalGHG$Time)&(grepl("Gas_Emission",JournalGHG$Activity)|grepl("Injection",JournalGHG$Activity)), colnames(JournalGHG)%in% c("Time","Site","Position","Activity","Stage","Sampling","Synchronisation","MeasurmentNum", "SamplingDelay", "Comment","ToNotUse")]#selection des données pertinentes sur base sur des informations concernant les mesures d'émissions et avec données temps complètes
  
  #JournalGHG<- JournalGHG[!is.na(JournalGHG$Time)&grepl("Gas_Emission",JournalGHG$Activity), colnames(JournalGHG)%in% c("Time","Site","Position","Activity","Stage","Sampling","Synchronisation","MeasurmentNum", "SamplingDelay", "Comment","ToNotUse")]#selection des données pertinentes sur base sur des informations concernant les mesures d'émissions et avec données temps complètes
  JournalGHGValid<- JournalGHG[!JournalGHG$ToNotUse%in%c("X","x"),] #selection des données validées
  
  LogSelection<-data.frame(NumberEmissionsTot=dim(JournalGHG)[1],NumberEmissionvalid=dim(JournalGHGValid)[1]) 
  print(LogSelection)
  JournalGHGValid$MeasurmentNum<-as.factor(JournalGHGValid$MeasurmentNum)
  
  GHGMeasurment<-levels(JournalGHGValid$MeasurmentNum)
  NbGHGMeasurment<-length(GHGMeasurment)
  

  
#######################################Formating of GHG emissions Data journal for futur merging/compilation of data

  print("Formating of GHG emissions Data journal for futur merging/compilation of data")
  GHGJounalCompiled<-data.frame(GHGMeasurment=NA,Start=now(tzone =TimeZone), End=now(tzone =TimeZone), Sampling="None",Synchronisation="None",SamplingDelay=0,ToNotUse="",Activity=c(""),stringsAsFactors=F)

   for (k in 1:NbGHGMeasurment){
      GHGMesurementSel<-GHGMeasurment[k]
      SelMesGES<-JournalGHGValid$MeasurmentNum==GHGMesurementSel #vecteur de selection des mesures
      GHGJounalCompiled<-rbind(GHGJounalCompiled,GHGJounalCompiled[k,])#ajoute un eligne au tableau de reception des résultats
      
      GHGJounalCompiled$GHGMeasurment[k]<-GHGMesurementSel
      GHGJounalCompiled$Start[k]<-JournalGHGValid$Time[SelMesGES&JournalGHGValid$Stage==("Start")]
      GHGJounalCompiled$End[k]<-JournalGHGValid$Time[SelMesGES&JournalGHGValid$Stage==("End")]
      GHGJounalCompiled$Sampling[k]<-JournalGHGValid$Sampling[SelMesGES&JournalGHGValid$Stage==("Start")]
      GHGJounalCompiled$Synchronisation[k]<-c("Nok","ok")[sum(JournalGHGValid$Synchronisation[SelMesGES]==c("ok"))]
      GHGJounalCompiled$SamplingDelay[k]<-JournalGHGValid$SamplingDelay[SelMesGES&JournalGHGValid$Stage==("Start")]
      GHGJounalCompiled$ToNotUse[k]<-JournalGHGValid$ToNotUse[SelMesGES&JournalGHGValid$Stage==("Start")]
      GHGJounalCompiled$Activity[k]<-JournalGHGValid$Activity[SelMesGES&JournalGHGValid$Stage==("Start")]
      
      if (k==NbGHGMeasurment){GHGJounalCompiled<-GHGJounalCompiled[1:k,]} #formate GHGJounalCompiled
    }

#######################################Formating of GHG concentration Data for futur merging/compilation of data
  print("Formating of GHG emissions concetration for futur merging/compilation of data")
  
  #Formatage du fichier de mesure de concentration
  GasConcentrationInnova$Analyser<-c("Innova")
  GHGCompiledConc<-GasConcentrationInnova
  GHGCompiledConc<-unique(GHGCompiledConc)  
  GHGCompiledConc$GHGMeasurment<-c("")
  GHGCompiledConc$Position<-c("")
  GHGCompiledConc$ToNotUse<-c("")
  GHGCompiledConc$BorderValues<-c(NA)
  GHGCompiledConc$BorderSelection<-c(FALSE)
  GHGCompiledConc$Activity<-c("")
#######################################Merging of GHG concentration Data and Data journal and position assignement

  for (k in 1:NbGHGMeasurment){
    JournalSel<-GHGJounalCompiled$GHGMeasurment==GHGMeasurment[k]
    SamplingSeq<-unlist(strsplit(GHGJounalCompiled$Sampling[JournalSel],"_"))
    NbSamplingPoint<-length(SamplingSeq)
    GHGJounalCompiledTemp<-GHGJounalCompiled[JournalSel,]
    try(as.numeric(GHGJounalCompiledTemp$SamplingDelay[is.na(GHGJounalCompiledTemp$SamplingDelay)]<-0))
   
    GHGCompiledConcSel<-GHGCompiledConc$Time>=GHGJounalCompiledTemp$Start&GHGCompiledConc$Time<GHGJounalCompiledTemp$End
    
    GHGCompiledConc$GHGMeasurment[GHGCompiledConcSel]<- GHGJounalCompiledTemp$GHGMeasurment
    GHGCompiledConc$Position[GHGCompiledConcSel]<-SamplingSeq[as.integer(((60+minute(GHGCompiledConc$Time[GHGCompiledConcSel])+second(GHGCompiledConc$Time[GHGCompiledConcSel])/60-(GHGJounalCompiledTemp$SamplingDelay+minute(GHGJounalCompiledTemp$Start)))%% 60)/(60/NbSamplingPoint)+1)]
    GHGCompiledConc$BorderValues[GHGCompiledConcSel]<-(60+minute(GHGCompiledConc$Time[GHGCompiledConcSel])+second(GHGCompiledConc$Time[GHGCompiledConcSel])/60-(GHGJounalCompiledTemp$SamplingDelay+minute(GHGJounalCompiledTemp$Start)))%%60%%(60/NbSamplingPoint)
    GHGCompiledConc$ToNotUse[GHGCompiledConcSel]<-GHGJounalCompiledTemp$ToNotUse
    GHGCompiledConc$BorderSelection[GHGCompiledConcSel]<-GHGCompiledConc$BorderValues[GHGCompiledConcSel]>Border$Low&GHGCompiledConc$BorderValues[GHGCompiledConcSel]<(60/NbSamplingPoint-Border$High)
    GHGCompiledConc$Activity[GHGCompiledConcSel]<-GHGJounalCompiledTemp$Activity
  print(paste(round(k/NbGHGMeasurment*100,0),"%"))
  }
  summary(GHGCompiledConc)
  GHGCompiledConc<-GHGCompiledConc[GHGCompiledConc$BorderSelection&!GHGCompiledConc$ToNotUse%in%c("X","x")&GHGCompiledConc$Position!=c(""),]
 
#######################################Data Plot
  print("Data plot")  
  GHGCompiledFig<-TranformFigFct(GHGCompiledConc[,colnames(GHGCompiledConc)%in%c("Time","Site","Unit","Source","N2O","C2H6O","CO2","CH4","NH3","Water","Position","Analyser","GHGMeasurment")],T)
  
  
  for (k in 1:NbGHGMeasurment){
    print(paste(k,"mesure___________________"))
    if(dim(GHGCompiledFig[GHGCompiledFig$GHGMeasurment==GHGMeasurment[k],])[1]>0){
       ggplot(GHGCompiledFig[GHGCompiledFig$GHGMeasurment==GHGMeasurment[k],],aes(x=Time,y=Concentration_ppm,color=Position,shape=as.factor(Analyser)))+
        geom_point()+
        facet_grid(Gas~Site,scale="free")+
        ggtitle(as.character(GHGCompiledFig$Source[GHGCompiledFig$GHGMeasurment==GHGMeasurment[k]][1]))+
        CommonPlotParam01V
      ggsave(paste(PathPretreatPlot,c("/"),GHGMeasurment[k],".png",sep=c("")))
    print(paste("plot", k ,"of" , NbGHGMeasurment,"figures",sep=c(" ")))
    }
  }
InnovaPretreat.df<-GHGCompiledConc
return(InnovaPretreat.df)
}