library(xts)
library(datetime)
library(ggplot2)

bxl_tz = "Europe/Brussels"
Sys.setenv(TZ=bxl_tz)

getInnovaData <- function(Site = "Barn", Position = 1){
  
  ghgdata.df <- ghgdata.df[ghgdata.df$Site==Site & ghgdata.df$Position==Position ,]
  ghgdata.df$Time <- ghgdata.df$Time - 9*60*60 # making the day start at 10
  
  # transform as xts with only the methane values
  ghgdata.xts <- na.omit(xts(ghgdata.df$CH4_mgperhour, order.by=as.Date(ghgdata.df$Time)))
  
  dataperday.xts <- apply.daily(ghgdata.xts, length)
  valueperday.xts <- apply.daily(ghgdata.xts, mean)
  both.xts <- merge(n=dataperday.xts, ch4=24*valueperday.xts)
  
  return(both.xts)
}


# example on how to use :

ghgdata.df <- read.table("../DAQBBMLIM/TreatmentData/Data/Emission_Innova.CSV", 
                         sep = ";", 
                         header = TRUE, 
                         dec = ",",
                         stringsAsFactors = FALSE)
ghgdata.df$Time <- as.POSIXct(ghgdata.df$Time, format="%Y-%m-%d %H:%M:%S", tz = bxl_tz)


# return the data of barn position 1:
alldata.xts <- getInnovaData(Site = "Barn", Position = 1)

# select days with at least 20 entries
data.xts <- alldata.xts["2017-10-01/"]

#transform to data frame
data.df <- data.frame(date=index(data.xts), coredata(data.xts))


ggplot(data.df, aes(x=date, y=n)) + geom_point()

View(data.xts)

ggplot(alldata.xts)

