# # function for compilation of data from Feed and litter saving of compiled data and figure in main folder
# no empty file to be imported
# Feed amount, Feed analysis, Chamber visiting time
#Libraries
  library(rstudioapi)
  library(ggplot2)
  library(lubridate)
  library(reshape2)
  library(glue)

#removal of object in memory
  rm(list=ls())

#definition of working folder
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Path to files

PathRawDataFeedLitterAmount<-c("./../HerdData/FeedLitter/Amount")
PathRawDataFeedLitterQuality<-c("./../HerdData/FeedLitter/Quality")
PathRawDataGHGChamberVisit<-c("./../HerdData/FeedLitter/GHGChambertVisit")
PathtRawDataHerdID<-c("./../HerdData/Infos")
PathRawDataFeedLitterCompiled<-c("./../HerdData/FeedLitter")
PathRawDataFeedLitterFig<-c("./../HerdData/FeedLitter")


library (plyr)
# Parameter

TimeZone<-c("Europe/brussels")
ToCompile<-c("FeedLitterAmount","FeedQuality","GHGChambertVisit")[c(TRUE,TRUE,TRUE)]# modify the logical vector in function of files  to compile

FeedParameter<-c("DM","MPT","CT","CEL","NDF","ADF","ADL","SSt","DMO","MOD","PBD","ENA","EB","EM","VEM","VEVI","DVE","OEB","UFL","UFV","PDIN","PDIE","Fat","Starch")

#Compiled file definition
  FeedLitterAmountCompiledData<-c("FeedLitterAmountCompiled.csv") #Data
  FeedLitterAmountCompiledFig<-c("FeedLitterAmountCompiled.png") #figure
   
  FeedLitterQualityCompiledData<-c("FeedLitterQualityCompiled.csv") #Data
  FeedLitterQualityCompiledFig<-c("FeedLitterQualityCompiled.png") #figure
  
  GHGChamberVisitCompiledData<-c("GHGChamberVisitCompiledCompiled.csv") #Data
  GHGChamberVisitCompiledFig<-c("GHGChamberVisitCompiledCompiled.png") #figure
DataHerdIDName<-c("BBMLIM.csv")

# Synthesis of PathFig and Data file dataframe

Path<-c(PathRawDataFeedLitterAmount,PathRawDataFeedLitterQuality,PathRawDataGHGChamberVisit)
Data<-c(FeedLitterAmountCompiledData,FeedLitterQualityCompiledData,GHGChamberVisitCompiledData)
Fig<-c(FeedLitterAmountCompiledFig,FeedLitterQualityCompiledFig,GHGChamberVisitCompiledFig)

FeedDataSource.df<-data.frame(cbind(ToCompile,Path,Fig,Data))

#Functions

Compilation.fct<-function(DF){
  PathFiles<-as.character(DF$Path)
  ListOfFiles<-dir(PathFiles)
  ListData<-lapply(ListOfFiles,FUN=function(x){read.table(paste(PathFiles,x,sep=c("/")),header=T,sep=c(";"),stringsAsFactors = FALSE,dec=c(","))})
  Data<-ldply(ListData,data.frame)
  return(Data)
}

Compilationset.fct<-function(DF){
  ToReturn<-as.list(apply(DF,1,FUN=function(x){Compilation.fct(as.data.frame(t(x)))}))
  names(ToReturn)<-DF$ToCompile
  ToReturn$FeedQuality$Date_Start<-as_datetime(dmy(ToReturn$FeedQuality$Date_Start,tz=TimeZone),tz=TimeZone)
  ToReturn$FeedQuality$Date_End<-as_datetime(dmy(ToReturn$FeedQuality$Date_End,tz=TimeZone),tz=TimeZone)
  ToReturn$FeedLitterAmount$Date<-as_datetime(dmy(ToReturn$FeedLitterAmount$Date,tz=TimeZone),tz=TimeZone)
  ToReturn$FeedLitterAmount$Fresh_Weight<-as.numeric(gsub(",",".",ToReturn$FeedLitterAmount$Fresh_Weight))
  ToReturn$FeedLitterAmount$Fresh_Weight[ToReturn$FeedLitterAmount$Feed_Item==c("Refus")]<--ToReturn$FeedLitterAmount$Fresh_Weight[ToReturn$FeedLitterAmount$Feed_Item==c("Refus")]
  ToReturn$FeedLitterAmount<-unique(ToReturn$FeedLitterAmount)
  ToReturn$FeedLitterAmount$Feed_Item<-as.factor(ToReturn$FeedLitterAmount$Feed_Item)
  return(ToReturn)
}
DateFR.fct<-function(x){return(as_datetime(dmy(x,tz=TimeZone),tz=TimeZone))}
DateEN.fct<-function(x){return(as_datetime(ymd(x,tz=TimeZone),tz=TimeZone))}

SampleAssignement.fct<-function(DF){#return the number of sample and the name of the sample for a given Feed item weighted
    DF$FeedQuality$ID_analysis<-paste(c("Sample"),1:dim(DF$FeedQuality)[1],sep=c(""))
    ID_Animal_Analysis<-strsplit(x=DF$FeedQuality$Cattle_ID,split=c("_"),fixed=TRUE)
      CorrID<-apply(as.data.frame(DF$FeedLitterAmount),1,FUN=function(z){apply(apply(as.data.frame(strsplit(as.character(as.data.frame(t(z))$Cattle_ID),split=c("_"),fixed=TRUE)),1,function(y){grepl(y,DF$FeedQuality$Cattle_ID)}),1,FUN=function(a){sum(a,na.rm=TRUE)})>0})
      CorrType<-apply(as.data.frame(DF$FeedLitterAmount$Type),1,function(y){y==DF$FeedQuality$Type})
      CorrFeed<-apply(as.data.frame(DF$FeedLitterAmount$Feed_Item),1,function(y){y==DF$FeedQuality$Feed_Item})
      CorrDate<-apply(as.data.frame(DF$FeedLitterAmount$Date),1,function(y){y>=DF$FeedQuality$Date_Start&y<=DF$FeedQuality$Date_End})
      Selection.df<-as.data.frame(t(CorrID&CorrType&CorrFeed&CorrDate))
      colnames(Selection.df)<-DF$FeedQuality$ID_analysis
      Selection.df[is.na(Selection.df)]<-FALSE
    NameSample<-as.data.frame(apply(Selection.df,1,FUN=function(x){x[x]<-rownames(as.data.frame(x))[x]
      x[x==FALSE]<-c("")
      return(x)}))
    NameSampleAgg<-data.frame(apply(NameSample,2,FUN=function(x){paste(x[x!=""],collapse=",")}))
    NBSample<-as.data.frame(apply(Selection.df,1,FUN=function(x){sum(x,na.rm=TRUE)}))

    DF$FeedLitterAmount<-cbind(DF$FeedLitterAmount,NameSampleAgg,NBSample)
    colnames(DF$FeedLitterAmount)[(dim(DF$FeedLitterAmount)[2]-1):dim(DF$FeedLitterAmount)[2]]<-c("Sample","NBSample")
    
    Feed_Quality_Mean<-as.data.frame(t(apply(DF$FeedLitterAmount[,],1,function(x){
                                    apply(DF$FeedQuality[DF$FeedQuality$ID_analysis%in%as.data.frame(strsplit(as.character(as.data.frame(t(x))$Sample),",",fixed=TRUE),stringsAsFactors=FALSE)[,1],],2,function(y){
                                      if(length(y)[1]==0){return(NA)}
                                      if(length(y)[1]==1){return(as.numeric(y))}
                                      if(length(y)[1]>=1){return(mean(as.numeric(y),na.rm=TRUE))}
                                      #mean(y,na.rm=TRUE)
                                    })
                                })),stringsAsFactors=FALSE)
    
    Feed_Quality_SD<-as.data.frame(t(apply(DF$FeedLitterAmount[,],1,function(x){
                                  apply(DF$FeedQuality[DF$FeedQuality$ID_analysis%in%as.data.frame(strsplit(as.character(as.data.frame(t(x))$Sample),",",fixed=TRUE),stringsAsFactors=FALSE)[,1],],2,function(y){
                                    if(length(y)[1]<=1){return(NA)}
                                    if(length(y)[1]>1){return(sd(as.numeric(y),na.rm=TRUE))}
                                    #mean(y,na.rm=TRUE)
                                  })
                                })),stringsAsFactors=FALSE)
    
    Feed_Quality_NB<-as.data.frame(t(apply(DF$FeedLitterAmount[,],1,function(x){
                                  apply(DF$FeedQuality[DF$FeedQuality$ID_analysis%in%as.data.frame(strsplit(as.character(as.data.frame(t(x))$Sample),",",fixed=TRUE),stringsAsFactors=FALSE)[,1],],2,function(y){
                                    if(is.na(length(y)[1])){return(NA)}
                                    if(length(y)[1]==0){return(0)}
                                    if(length(y)[1]>=0){return(length(y))}
                                    #mean(y,na.rm=TRUE)
                                  })
                                })),stringsAsFactors=FALSE)
    
  colnames(Feed_Quality_Mean)<-paste(colnames(Feed_Quality_Mean),c("mean"),sep=c("_"))
  colnames(Feed_Quality_SD)<-paste(colnames(Feed_Quality_SD),c("sd"),sep=c("_"))
  colnames(Feed_Quality_NB)<-paste(colnames(Feed_Quality_NB),c("NB"),sep=c("_"))
  Ingestion<-cbind(DF$FeedLitterAmount,Feed_Quality_Mean,Feed_Quality_SD,Feed_Quality_NB)
  DF[["Ingestion"]]<-Ingestion
  
  return(DF)
}

GeneralPlot.fct<-function(DF,Parameter,Start,End,TimeParameter,ColorFactor){
  Start<-DateFR.fct(Start)
  End<-DateFR.fct(End)
  Selection<-DF[DF[,TimeParameter]>=Start&DF[,TimeParameter]<=End,c(TimeParameter,Parameter,ColorFactor)]
  colnames(Selection)<-c("Time","Parameter","Item")
  
  ggplot(Selection,aes(x=Time,y=Parameter,color=as.factor(Item)))+
    geom_point()+
    theme_bw()+
    theme(axis.text.x  = element_text(angle=90))+
    labs(y=Parameter)
}

FeedFreq.fct<-function(DF,Parameter,Start,End,TimeParameter,ColorFactor){
  Start<-DateFR.fct(Start)
  End<-DateFR.fct(End)
  Selection<-DF[DF[,TimeParameter]>=Start&DF[,TimeParameter]<=End,c(TimeParameter,Parameter,ColorFactor)]
  colnames(Selection)<-c("Time","Parameter","Item")
  
  ggplot(Selection,aes(x=Time,y=Parameter,color=as.factor(Item)))+
    geom_point()+
    theme_bw()
}

FeedInventory.df<-function(DF,FeedItem,TimeParameter,Start,End){  # Synthesis of feed samples inventory over between two dates
  Start<-DateFR.fct(Start)
  End<-DateFR.fct(End)
  Selection<-DF[,TimeParameter]>=Start&DF[,TimeParameter]<=End
  return(summary(as.factor(DF[Selection,FeedItem])))
  }

#Ajustement of DM in Feed Quality

# Compilation of data according to functions predefined

  #Compilation of Data sets
    CompiledRawData<-Compilationset.fct(FeedDataSource.df) #Compilation of data
    ##names(CompiledRawData)
    
    ##summary(CompiledRawData[[1]])
    
  # Grouping of ingestion according to Treatment 
    CompiledRawData$FeedLitterAmount$Group<-c("Individual")
    CompiledRawData$FeedLitterAmount$Group[grepl("_",CompiledRawData$FeedLitterAmount$Cattle_ID)]<-c("Group")
    CompiledRawData$FeedLitterAmount$DataType<-c("Raw")# Raw data not aggregated according to group #Calculated
    RecordedIndividualID<-levels(as.factor(CompiledRawData$FeedLitterAmount$Cattle_ID[CompiledRawData$FeedLitterAmount$Group==c("Individual")]))
    DataHerdID<-read.table(file=paste(PathtRawDataHerdID,DataHerdIDName,sep=c("/")),header=TRUE,dec=c(","),sep=c(";"))
    DataHerdID$Boucle<-as.character(DataHerdID$Boucle)
    DataHerdID$Boucle[DataHerdID$Boucle==c("234")]<-c("0234")
    GroupingID.fct<-function(x){
        x<-x[order(x)]
        y<-collapse(x,sep=c("_"))
        return(y)
        }
    
    Group<-aggregate(.~Race+Etat+Repetition,data=DataHerdID[,c("Boucle","Race","Etat","Repetition")],FUN=GroupingID.fct)
    
    DataHerdID$GroupID<-unlist(apply(DataHerdID,1,FUN=function(x){return(Group$Boucle[grepl(as.character(as.data.frame(t(x))$Boucle),Group$Boucle)])}))
    
    DataHerdID$Boucle[DataHerdID$Boucle==c("234")]<-c("0234")
    
    CompiledRawData$FeedLitterAmount$Cattle_ID[CompiledRawData$FeedLitterAmount$Cattle_ID==c("234")]<-c("0234")
    
    CompiledRawData$FeedLitterAmount<-merge(CompiledRawData$FeedLitterAmount,DataHerdID[,c("Boucle","GroupID")],by.x =c("Cattle_ID"),by.y=c("Boucle"),all.x=TRUE)
    CompiledRawData$FeedLitterAmount$GroupID[CompiledRawData$FeedLitterAmount$Group==c("Group")]<-CompiledRawData$FeedLitterAmount$Cattle_ID[CompiledRawData$FeedLitterAmount$Group==c("Group")]
    CompiledRawData$FeedLitterAmount$GroupID[CompiledRawData$FeedLitterAmount$Group==c("Group")]<-CompiledRawData$FeedLitterAmount$Cattle_ID[CompiledRawData$FeedLitterAmount$Group==c("Group")]
    CompiledRawData$FeedLitterAmount<-merge(CompiledRawData$FeedLitterAmount,DataHerdID[,colnames(DataHerdID)!=c("Boucle")],by =c("GroupID"),all.x=TRUE)
    #View(CompiledRawData$FeedLitterAmount[CompiledRawData$FeedLitterAmount$GroupID==c(""),])
    
    
#Visualisation of fresh matter intake for verification of correct allocation of intake to Cattle lot and individue
    
    DF<-CompiledRawData$FeedLitterAmount

    StartDate<-c("01/10/2016)")
    EndDate<-c("01/08/2018")
    SelTime<-as_datetime(dmy(c(StartDate,EndDate)))
    DF$RepFig<-as.numeric(as.factor(DF$GroupID))
        ##View(DF[is.na(as.numeric(as.factor(DF$GroupID))),])
        ##levels(as.factor(DF$GroupID))
        
    for (i in 1:max(DF$RepFig)){
      
      ggplot(DF[DF$RepFig==i&DF$Type!="Litter"&DF$Date>=SelTime[1]&DF$Date<=SelTime[2],])+#
        theme_bw()+
        theme(axis.text.x  = element_text(angle=90))+
        geom_col(aes(x=Date,y=Fresh_Weight,fill=Feed_Item),na.rm=TRUE)+
        facet_grid(.~Race+Etat+Repetition)+
        labs(title=levels(as.factor(DF$GroupID))[i])
      
      ggsave(file=(paste(PathRawDataFeedLitterFig,"/","Ing_Fresh",i,".png",sep=c(""))))
      print(i)
    } 
 
#Ajustment of DM in feed Quality
    CompiledRawData$FeedQuality$DMet_pcFM<-(CompiledRawData$FeedQuality$Dry_WeightWithTare-CompiledRawData$FeedQuality$Tare)/(CompiledRawData$FeedQuality$Wet_Weight_WithTare-CompiledRawData$FeedQuality$Tare)*100
    CompiledRawData$FeedQuality$DMSr_pcFM<-CompiledRawData$FeedQuality$DMet_pcFM*CompiledRawData$FeedQuality$DM_pcDMet/100
    CompiledRawData$FeedQuality$DMSr_pcFM[is.na(CompiledRawData$FeedQuality$DMSr_pcFM)]<-CompiledRawData$FeedQuality$DMet_pcFM[is.na(CompiledRawData$FeedQuality$DMSr_pcFM)]*mean(CompiledRawData$FeedQuality$DM_pcDMet[!is.na(CompiledRawData$FeedQuality$DMSr_pcFM)])/100#SI pas de Facteuir de correction MS etuve attriube la vvaleur moyenne des échantillons avec valeurs

  #merging of data set to ingestion calculation according to date of samples and treatments  
    CompiledRawData<-SampleAssignement.fct(CompiledRawData) #Assignement of Sample analysis to freshweight

# Calculation of amount of ingested by Feed Item
  CompiledRawData$Ingestion$RawDMIkgperItem<-CompiledRawData$Ingestion$DMSr_pcFM_mean*CompiledRawData$Ingestion$Fresh_Weight/100
  CompiledRawData$Ingestion$RawMPTkgperItem<-CompiledRawData$Ingestion$RawDMIkgperItem*CompiledRawData$Ingestion$MPT_pcDM_mean/100

# Visualisations of data
  GeneralPlot.fct(CompiledRawData$FeedQuality,"MPT_pcDM","1/09/2016","01/08/2018","Date_Start","Feed_Item")
  GeneralPlot.fct(CompiledRawData$FeedQuality,"DMet_pcFM","1/09/2016","01/08/2017","Date_Start","Feed_Item")
  GeneralPlot.fct(CompiledRawData$FeedQuality,"DMet_pcFM","1/09/2017","01/08/2018","Date_Start","Feed_Item")
  GeneralPlot.fct(CompiledRawData$FeedLitterAmount,"Fresh_Weight","1/09/2016","01/08/2018","Date","Feed_Item")
  GeneralPlot.fct(CompiledRawData$Ingestion,"DMSr_pcFM_mean","1/09/2016","01/08/2018","Date","Feed_Item")
  GeneralPlot.fct(CompiledRawData$Ingestion,"RawDMIkgperItem","1/09/2016","01/08/2018","Date","Feed_Item")

  
  DF<-CompiledRawData$Ingestion
  colnames(DF)
  Start<-DateFR.fct("1/11/2016")#"1/11/2016"
  End<-DateFR.fct("1/08/2018")#("1/08/2017")#
  TypeInput<-c("Feed")
   ggplot(DF[DF$Date>=Start&DF$Date<=End&DF$Type==TypeInput,])+
     geom_col(aes(x=Date,y=RawDMIkgperItem,fill=Feed_Item))+
     facet_grid(GroupID~.,scales = "free_x")+
     theme_bw()+
     ylab(c("Dry matter (kg) per cattle Group"))

  
  
  
# Vérification de présence de NA, de la validité des MS et de la présence d'analyses des analyses 
  
  View(CompiledRawData$FeedLitterAmount[is.na(CompiledRawData$FeedLitterAmount$Fresh_Weight),])
  View(CompiledRawData$FeedQuality[CompiledRawData$FeedQuality$DMet_pcFM>93|is.na(CompiledRawData$FeedQuality$DMet_pcFM),])
  View(CompiledRawData$FeedQuality[CompiledRawData$FeedQuality$DMet_pcFM<20|is.na(CompiledRawData$FeedQuality$DMet_pcFM),])
  
  FeedInventory.df(CompiledRawData$FeedQuality,"Feed_Item","Date_Start","01/05/2017","01/08/2018")

for (i in 1:length(CompiledRawData)){
write.table(CompiledRawData[[i]],file=paste(PathRawDataFeedLitterCompiled,"/",names(CompiledRawData)[i],"_Compiled.csv",sep=c("")),sep=c(";"),row.names = FALSE)
}




# CompiledRawData$FeedLitterAmount<-CompiledRawData$FeedLitterAmount[!(CompiledRawData$FeedLitterAmount$Date>=DateFR.fct("28/11/2016")&CompiledRawData$FeedLitterAmount$Date<=DateFR.fct("4/12/2016")),]#Retire la période ou les bouef et taureaux ont été alimentés ensemble
# CompiledRawData$FeedLitterAmount$Feed_Item<-factor(CompiledRawData$FeedLitterAmount$Feed_Item,levels(CompiledRawData$FeedLitterAmount$Feed_Item)[rev(c(4,5,6,1:3,8:10,7))])
# CompiledRawData$FeedLitterAmount$Phase<-""
# CompiledRawData$FeedLitterAmount$Phase[CompiledRawData$FeedLitterAmount$Date>=DateFR.fct("31/10/2016")&CompiledRawData$FeedLitterAmount$Date<=DateFR.fct("09/01/2017")]<-c("Growing")
# CompiledRawData$FeedLitterAmount$Phase[CompiledRawData$FeedLitterAmount$Date>=DateFR.fct("17/01/2017")&CompiledRawData$FeedLitterAmount$Date<=DateFR.fct("27/03/2017")]<-c("Fattening")
# 
# 
# CompiledRawData$FeedLitterAmount$Phase<-as.factor(CompiledRawData$FeedLitterAmount$Phase)
# CompiledRawData$FeedLitterAmount$Phase<-factor(CompiledRawData$FeedLitterAmount$Phase,levels(CompiledRawData$FeedLitterAmount$Phase)[c(3,2,1)])
# 
# 
# 