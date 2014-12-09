# rWMS

[Web Map Services](http://www.opengeospatial.org/standards/wms) provides a standard interface to map images.

This R package reads the capabilites XML document associated with a WMS source and creates a 'WMS' R class.

I created the package to allow an interface to WMS sources in [shiny](http://shiny.rstudio.com/) apps using [leaflet-shiny](https://github.com/davesteps/leaflet-shiny).

[Example app](https://davesteps.shinyapps.io/wms_example/)


----
###Usage:

Extract some sea surface temperature from this [THREDDS WMS source](http://www.myocean.eu/web/69-myocean-interactive-catalogue.php/?option=com_csw&view=details&product_id=SST_GLO_SST_L4_NRT_OBSERVATIONS_010_001) for a specific date range. 
  
```r
if (!require("devtools"))
  install.packages("devtools")
devtools::install_github("rWMS", "davesteps")

src <- WMS('http://data.ncof.co.uk/thredds/wms/METOFFICE-GLO-SST-L4-NRT-OBS-SST-V2?')

#use layerNames to examine the available variables on the source 
layerNames(src)

frm <- "2010-01-01"
to <- "2010-03-01"

df <- TDSquery(src,lat = 53,lon = -5.5,from = frm,to = to,lyr = "analysed_sst")

plot(df$date,df$values,type='l')


```



