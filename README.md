# rWMS

[Web Map Services](http://www.opengeospatial.org/standards/wms) provides a standard interface to map images.

This R package reads the capabilites XML document associated with a WMS source and creates a 'WMS' R class.

I created the package to allow an interface to WMS sources in [shiny](http://shiny.rstudio.com/) apps using [leaflet-shiny](https://github.com/davesteps/leaflet-shiny).

[Example app](https://davesteps.shinyapps.io/wms_example/)


----
###Usage:

  
```r
if (!require("devtools"))
  install.packages("devtools")
devtools::install_github("rWMS", "davesteps")

require(rWMS)

src <- WMS('http://data.ncof.co.uk/thredds/wms/METOFFICE-GLO-SST-L4-NRT-OBS-SST-V2?')

layerNames(src)
```



