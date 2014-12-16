
if (!require("devtools"))
  install.packages("devtools")
devtools::install_github("rWMS", "davesteps")

library(rWMS)

#connect to server
TDS <- WMS("http://148.252.96.22:8080/ncWMS-1.1/wms?")

#data sources on server:
TDS@uniqueParents

#examine layer names (WMS variables are referred to as layers)
head(layerNames(TDS))

#choose the one we're interested in
lyr <- "5/u"

#examine the temporal coverage of the layer
dateRange(TDS,lyr)

#created a formatted date string for the period we are intersed in
from <- "2008-01-01"
to <- "2008-12-31"
ds <- formattedDateString(TDS,lyr,from,to,n=120)

#query lat lon
lat <- 55.5
lon <- 3.2
df <- TDSquery(TDS,lat,lon,lyr,ds)

str(df)

plot(df$date,df$values,type='l')



##############################################


library(rWMS)

src<-read.csv('C:/Users/ds10/Dropbox/shiny_csv_reader/wms_src.csv')

sst1 <- WMS('http://data.ncof.co.uk/thredds/wms/METOFFICE-GLO-SST-L4-NRT-OBS-SST-V2?')
sst2 <- WMS("http://data.ncof.co.uk/thredds/wms/METOFFICE-GLO-SST-L4-RAN-OBS-ANOM?")

chl1 <- WMS("http://myocean.artov.isac.cnr.it/thredds/wms/dataset-oc-glo-chl-multi-l3-gsm_4km_daily-rt-v01?" )
chl2 <- WMS("http://myocean.artov.isac.cnr.it/thredds/wms/dataset-oc-glo-chl-multi_cci-l3-oc4_4km_daily-rep-v02?")

lat = 25.8
lon = -67.55

sst.df<- TDSqueryAll(sst,'analysed_sst',lat,lon,'month')
sst2.df <- TDSqueryAll(sst2,'analysed_sst',lat,lon,'month')

sst.df$src='a'
sst2.df$src='b'

df <- rbind(sst.df,sst2.df)
ggplot(df,aes(x=date,y=values))+geom_line()

layerNames(chl2)
dateRange(chl1,'CHL')
dateRange(chl2,'CHL4')
chl1.df<- TDSqueryAll(chl1,'CHL',25.8,-87.55,'month')
chl2.df <- TDSqueryAll(chl2,'CHL4',25.8,-87.55,'month')

plot(chl1.df$date,chl1.df$values)
chl1.df$src='a'
chl2.df$src='b'

df <- rbind(chl1.df,chl2.df)

ggplot(df,aes(x=date,y=values,col=src))+geom_point()+stat_smooth(method='lm')






















