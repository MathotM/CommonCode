# # function for selection in ventilation compiled file and plot of data seleced
NH3Selection<-function(
                          PathFunction=PathFunction,
                          PathRawDataDPT=PathRawDataDPT,
                          PathRawDataAirFlow=PathRawDataAirFlow,
                          PathRawDataNH3Concentration=PathRawDataNH3Concentration,
                          NH3TrapDPTCompiledName=NH3TrapDPTCompiledName,
                          NH3TrapAirFlowCompiledName=NH3TrapAirFlowCompiledName,
                          NH3TrapConcentrationCompiledName=NH3TrapConcentrationCompiledName,
                          PathCompiledFiles=PathCompiledFiles,
                          StartData=StartData,
                          EndData=EndData,
                          TimeZone=TimeZone
    ){
    
    DTFct<-function(DTData){as_datetime(ymd_hms(DTData,tz=TimeZone),tz=TimeZone)}
    library(lubridate)
    
    print(c("opening of compiled raw DT, concentration and flow data files"))
    NH3TrapDPT.df<-read.csv(file=paste(PathRawDataDPT,NH3TrapDPTCompiledName,sep=c("/")),sep=c(";"),dec=c(","),header=TRUE,stringsAsFactors = FALSE)
    NH3TrapAirFlow.df<-read.csv(file=paste(PathRawDataAirFlow,NH3TrapAirFlowCompiledName,sep=c("/")),sep=c(";"),dec=c(","),header=TRUE,stringsAsFactors = FALSE)
    NH3TrapConcentration.df<-read.csv(file=paste(PathRawDataNH3Concentration,NH3TrapConcentrationCompiledName,sep=c("/")),sep=c(";"),dec=c(","),header=TRUE,stringsAsFactors = FALSE)
    
    if(class(StartData)==c("character")){StartData<-DTFct(StartData)}
    if(class(EndData)==c("character")){EndData<-DTFct(EndData)}
    
    print(c("formating merging and selection of concentration and flow Data"))
    NH3FlowConcentration<-merge(NH3TrapAirFlow.df,NH3TrapConcentration.df,by=c("Num_Sample"),all.x=T) #Merge Air flow and NH3 titration
    NH3FlowConcentration$StartMes<-DTFct(NH3FlowConcentration$StartMes)#formating time
    NH3FlowConcentration$EndMes<-DTFct(NH3FlowConcentration$EndMes)#formating time
    NH3FlowConcentration$MeanTimeMes<-DTFct(NH3FlowConcentration$MeanTimeMes)#formating time
    NH3FlowConcentration<-NH3FlowConcentration[NH3FlowConcentration$StartMes>=StartData&NH3FlowConcentration$EndMes<=EndData, ]
    NH3FlowConcentration<-NH3FlowConcentration[!NH3FlowConcentration$Validation%in%c("x","X"),]
    NH3FlowConcentration$AirAmountUncor<-(NH3FlowConcentration$Vol_End-NH3FlowConcentration$Vol_Start)*1000 #Amount of air filtrates uncorected for temperature and pressure in liter
    NH3FlowConcentration$AirFlowUncor<-NH3FlowConcentration$AirAmountUncor/as.numeric(as.numeric(NH3FlowConcentration$DurationMes)*60)# air flow uncorrected for temperature and pressure in minute
    NH3FlowConcentration<-NH3FlowConcentration[,c("Num_Sample","Site","Position", "System", "Unit","Nom_Sample","Ngper100g","H2SO4_Amount","StartMes","EndMes","MeanTimeMes","DurationMes","AirAmountUncor","AirFlowUncor")]
    NH3FlowConcentrationSelected.df<-NH3FlowConcentration
    
    print(c("formating and selection of DPT Data"))
    NH3TrapDPT.df$Time<-DTFct(NH3TrapDPT.df$Time)#formating time
    NH3TrapDPT.df<-NH3TrapDPT.df[NH3TrapDPT.df$Time>=StartData&NH3TrapDPT.df$Time<=EndData, ]
    NH3TrapDPTSelected.df<-NH3TrapDPT.df

    NH3Trap.list<-list(NH3FlowConcentrationSelected.df,NH3TrapDPTSelected.df)
    return(NH3Trap.list)
  }