

setClass("WMS",
         representation(
           url='character',
           name='character',
           title='character',
           abstract="character",
           keywords="vector",
           contact = 'character',
           org = 'character',
           email = 'character',
           fees = 'character',
           access = 'character',
           layerLimit = "integer",
           maxWidth = "integer",
           maxHeight = "integer",
           mapFormat = 'vector',
           featFormat = 'vector',
           expFormat = 'vector',
           layers='list',
           layerParentIndex='character',#unlist(pi),
           queryLayerIndex='logical',#qi,
           uniqueParents='character'#parent_list
         )
)



setClass("WMSdim",
         representation(
           name='character',#"time",
           units='character',#"ISO8601",
           multipleValues='character',#"true",
           current='character',#"true",
           default='character'#"2008-12-31T00:00:00.000Z"
         )
)

WMSdim <- function(x){
  
  new('WMSdim',
      name=x["name"],
      units=x["units"],
      multipleValues=x['multipleValues'],#"true",
      current=x['current'],#"true",
      default=x['default']#"2008-12-31T00:00:00.000Z"
  )
  
}


setClass("WMSmetadata",
         slots=c(
           'units',#='character',
           'bbox',#,='numeric',
           'scaleRange',#='character',
           'numColorBands',#='character',
           'supportedStyles',#='character',
           'zaxis',#='character',
           'datesWithData',#='character',
           'nearestTimeIso',#='character',
           'timeAxisUnits',#='character',
           'moreInfo',#='character',
           'copyright',#='character',
           'palettes',#='character',
           'defaultPalette',#='character',
           'logScaling'#='character'
         )
)



get_layerMetadata<- function(url,layer){
  #layer <- l.att[[3]]@name
  #require(stringr)
  
  metaURL <- paste(url,"item=layerDetails&request=GetMetadata&layerName=",layer,sep='')
  
  md <- getURL(metaURL)
  
  md<- gsub('\"|\n|\\[|\\]','',md)
  
  md
  
  iv <- c(
    str_locate(md,'units:'),
    str_locate(md,',bbox:'),
    str_locate(md,',scaleRange:'),
    str_locate(md,',numColorBands:'),
    str_locate(md,',supportedStyles:'),
    str_locate(md,',zaxis:'),
    str_locate(md,',datesWithData:'),
    str_locate(md,',nearestTimeIso:'),
    str_locate(md,',timeAxisUnits:'),
    str_locate(md,',moreInfo:'),
    str_locate(md,',copyright:'),
    str_locate(md,',palettes:'),
    str_locate(md,',defaultPalette:'),
    str_locate(md,',logScaling:'))
  md
  iv
  iv <- c(iv[-1],nchar(md))
  
  iv <- t(matrix(iv,nrow=2))
  
  l <- apply(iv,1,function(x) str_sub(md,x[1]+1,x[2]-1))
  
  
  t <- gsub("\\{|\\}",'',l[6])
  tv <- c(
    str_locate(t,'units:'),
    str_locate(t,',positive:'),
    str_locate(t,',values:'))
  tv
  tv <- c(tv[-1],nchar(md))
  
  tv <- t(matrix(tv,nrow=2))
  
  t <- apply(tv,1,function(x) str_sub(t,x[1]+1,x[2]-1))
  
  new("WMSmetadata",
      units=l[1],#'character',
      bbox= l[2],#as.numeric(strsplit(l[2],',')[[1]]),
      scaleRange=l[3],#as.numeric(strsplit(l[3],',')[[1]]),
      numColorBands=l[4],#as.numeric(l[4]),
      supportedStyles=l[5],
      zaxis=t,
      datesWithData='-',
      nearestTimeIso=l[8],
      timeAxisUnits=l[9],
      moreInfo=l[10],
      copyright=l[11],
      palettes=strsplit(l[12],',')[[1]],
      defaultPalette=l[13],#
      logScaling=l[14]
  )
  
}


layerinfo <- function(xml,ns,str){
  as.character(xmlApply(getNodeSet(xml,path = str,namespaces = ns),
                             xmlValue))}

setClass("WMSlayer",
         representation(
           title='character',
           name='character',
           abstract="character",
           parent='character',
           queryable='logical',
           bbox='list',
           dims='list',
           dimValues='list',
#            dateList='character',
#            startDate='character',
#            endDate='character',
           #timestamps='character',
           styles='list',
           metadata='WMSmetadata'
         )
)

setClass("WMSstyle",
         slots=c(
           'title',#='character',
           'name',#='character',
           'abstract',#="character",
           'leg.w',#='character',
           'leg.h',#='character',
           'leg.format',#='character',
           'leg.URL'#,#='character'
         )
)

WMSstyle <- function(s,ns){
  
  dim <- unlist(xmlApply(getNodeSet(s,path = 'ns:LegendURL',namespaces = ns),
                              xmlAttrs))
  url <- unlist(xmlApply(getNodeSet(s,path = 'ns:LegendURL/ns:OnlineResource',
                                    namespaces = ns),xmlAttrs))['href']
  
  
  new("WMSstyle",
      title=layerinfo(s,ns,'ns:Title')[[1]],
      name=layerinfo(s,ns,'ns:Name')[[1]],
      abstract=layerinfo(s,ns,'ns:Abstract')[[1]],
      leg.w=dim[1],
      leg.h= dim[2],
      leg.format=layerinfo(s,ns,'ns:LegendURL/ns:Format'),
      leg.URL=url#'list'
  )
  
  
}

WMS <- function(url){
  #require(XML)
  #require(RCurl)
  #require(stringr)
  
  #url <- "http://localhost:8080/geoserver/ows?"#"
  #url <- 'http://148.252.96.22:8080/ncWMS-1.1/wms?'
  #url <- "http://myocean.artov.isac.cnr.it/thredds/wms/dataset-oc-atl-opt-modis_a-l3-kd490_1km_daily-rt-v01?"
  #url <- "http://data.ncof.co.uk/thredds/wms/METOFFICE-GLO-SST-L4-NRT-OBS-SST-V2?"
  
  #url <- 'http://data.ncof.co.uk/thredds/wms/METOFFICE-GLO-SST-L4-NRT-OBS-SST-V2?'#service=WMS&request=GetCapabilities'#"http://localhost:8080/geoserver/ows?"#"http://mis.myocean.org.ua:8080/thredds/wms/dataset-bs-mfc-instan-phys-for-v3?"
  #url <- "http://148.252.96.22:8081/geoserver/ows?"#service=wms&version=1.1.1&request=GetCapabilities

  capURL <- paste(url,"service=WMS&request=GetCapabilities",sep='')
  xp <- xmlParse(getURL(capURL))
  ns <- c(ns="http://www.opengis.net/wms")
  
  
  l.att <- xpathSApply(doc = xp, "//*/ns:Layer",
                       namespaces =ns,fun = function(x){
                         
                         dims <- sapply(xmlApply(getNodeSet(x,path = 'ns:Dimension',namespaces = ns),xmlAttrs),WMSdim)
                         names(dims) <- sapply(dims,FUN = function(x)x@name)
                         dimVals <- xmlApply(getNodeSet(x,path = 'ns:Dimension',namespaces = ns),
                                             FUN = function(x){
                                               str_trim(gsub("\n",'',xmlValue(x)))
                                             })
                         names(dimVals) <- names(dims)
                         
                         t <- dimVals['time'][[1]]
                         
#                          #class(t)
#                          s <- ''
#                          t1<- ''
#                          t2 <- ''
#                          t
#                          if(!is.null(t)){
#                           s <- strsplit(t,',')[[1]]
#                           ri <- grep('/',s)
#                           s <- strsplit(s,'/')
#                          
#                           s[ri] <- sapply(s[ri],FUN = function(x) {
#                             sp <- strsplit(x,'T')
#                             sq <- seq(from = as.Date(sp[[1]][1]),to = as.Date(sp[[2]][1]), by="day")
#                              paste(sq,sp[[1]][2],sep='T')})
#                          
#                           s <- unlist(s)
# #                         s <- gsub('P1D','',dimVals['time'])
# #                          s1 <- gsub('T.*','',strsplit(s,'/')[[1]])
#                           t1 <- s[1]
#                           t2 <- s[length(s)]
#                          }
#                          #ts <- unique(paste('T',gsub('.*T','',strsplit(s,'/')[[1]]),sep=''))

  
                         styles <- xpathSApply(doc = x, "ns:Style",namespaces =ns,fun = WMSstyle,ns)
                         names(styles) <- sapply(styles,FUN = function(x)x@name)
                         
                         name <- layerinfo(x,ns,'ns:Name') 
                         metadata <- get_layerMetadata(url,name)
                         
                         new('WMSlayer',
                             title=layerinfo(x,ns,'ns:Title'),
                             name=name,
                             abstract = layerinfo(x,ns,'ns:Abstract'),
                             parent=layerinfo(x,ns,'../ns:Title'),
                             queryable="queryable"%in%names(xmlAttrs(x)),
                             bbox=xmlApply(getNodeSet(x,path = 'ns:BoundingBox',namespaces = ns),xmlAttrs),
                             dims=dims,
                             dimValues=dimVals,
                             #dateList=s,
                             #startDate=t1,
                             #endDate=t2,
                             #timestamps=ts,
                             styles=styles,
                             metadata=metadata
                             
                         )
                       })


  #l.att[[3]]@dimValues

  names(l.att) <- sapply(l.att,function(x) x@title)
  
  length(l.att)
  
  qi <- sapply(l.att,function(x) x@queryable)
  
  length(qi)
  
  pi <- unlist(c('TOP_LEVEL',sapply(l.att,function(x) x@parent)))
  
  #head(pi)
  #unique parents of querable layers
  parent_list <- unlist(unique(pi[unlist(qi)]))
  
  #l.att[[100]]@styles
  
  new("WMS",
      url=url,
      name=xpathSApply(doc = xp, "//ns:Service/ns:Name",namespaces=ns,xmlValue),
      title=xpathSApply(doc = xp, "//ns:Service/ns:Title",namespaces =ns,xmlValue),
      abstract=xpathSApply(doc = xp, "//ns:Service/ns:Abstract",namespaces =ns,xmlValue),
      keywords=xpathSApply(doc = xp, "//ns:Service/ns:KeywordList/ns:Keyword",namespaces =ns,xmlValue),
      contact=xpathSApply(doc = xp, "//ns:ContactPerson",namespaces =ns,xmlValue),
      org=xpathSApply(doc = xp, "//ns:ContactOrganization",namespaces =ns,xmlValue),
      email=xpathSApply(doc = xp, "//ns:ContactElectronicMailAddress",namespaces =ns,xmlValue),
      fees=xpathSApply(doc = xp, "//ns:Fees",namespaces =ns,xmlValue),
      access=xpathSApply(doc = xp, "//ns:AccessConstraints",namespaces =ns,xmlValue),
      layerLimit=as.integer(xpathSApply(doc = xp, "//ns:LayerLimit",namespaces =ns,xmlValue)),
      maxWidth=as.integer(xpathSApply(doc = xp, "//ns:MaxWidth",namespaces =ns,xmlValue)),
      maxHeight=as.integer(xpathSApply(doc = xp, "//ns:MaxHeight",namespaces =ns,xmlValue)),
      mapFormat=xpathSApply(doc = xp, "//*/ns:GetMap/ns:Format",namespaces =ns,xmlValue),
      featFormat=xpathSApply(doc = xp, "//*/ns:GetFeatureInfo/ns:Format",namespaces =ns,xmlValue),
      expFormat=xpathSApply(doc = xp, "//*/ns:Exception/ns:Format",namespaces =ns,xmlValue),
      layers = l.att,
      layerParentIndex=pi,
      queryLayerIndex=qi,
      uniqueParents=parent_list
  )
}

layerTitles <- function(wms,parent=NULL){
  #wms <- gs
  #parent <- 'MODIS ifremer'
  #given a wms 
  #returns layer titles
  if(!is.null(parent)){
    i <- layerParents(wms)==parent
    unlist(sapply(wms@layers[i],function(x) x@title))      
  } else {
    unlist(sapply(wms@layers,function(x) x@title))  
  }
}

layerNames <- function(wms,parent=NULL){
  #wms <- gs
  #parent <- 'MODIS ifremer'
  #given a wms 
  #returns layer titles
  if(!is.null(parent)){
    i <- layerParents(wms)==parent
    unlist(sapply(wms@layers[i],function(x) x@name))      
  } else {
    unlist(sapply(wms@layers,function(x) x@name))  
  }
}

layerParents <- function(wms){
  #wms <- gs
  #parent <- 'MODIS ifremer'
  #given a wms 
  #returns layer titles
  unlist(sapply(wms@layers,function(x) x@parent))  
}

getFeatureInfo <- function(url,lon,lat,layers,
                           date=NULL,
                           format,
                           count=NULL,
                           elevation=NULL){
  
  url <- paste(url,
               'bbox=',lon,',',lat,
               ',',lon+0.000001,',',lat+0.000001,
               "&request=GetFeatureInfo&",
               "layers=",layers,'&',
               "query_layers=",layers,'&',
               "width=2&",
               "version=1.1.1&",
               "srs=EPSG:4326&",
               ifelse(is.null(count),'',paste('feature_count=',count,'&',sep='')),
               ifelse(is.null(date),'',paste('time=',date,'&',sep='')),
               ifelse(is.null(elevation),'',paste('elevation=',elevation,'&',sep='')),
               'info_format=',format,'&',
               'height=2&X=1&Y=1',sep='')
  
  getURL(url)
  
}

getXMLVals <- function(xml){
  
  xp <- xmlParse(xml)
  df <- xmlToDataFrame(getNodeSet(xp, "//value"),colClasses = 'numeric')
  df$text
  
}

layerDates <- function(l){
  t <- l@dimValues$time
  
  s <- strsplit(t,',')[[1]]
  ri <- grep('/',s)
  s <- strsplit(s,'/')
  
  tv <- sapply(s,function(x){strsplit(x[1],'T')[[1]][2]})
  
  s <- sapply(s,FUN = function(x) {
    #x <- s[[1]]
    sp <- strsplit(x,'T')
    if(length(sp)>1){
      as.character(seq(from = as.Date(sp[[1]][1]),to = as.Date(sp[[2]][1]), by="day"))
    } else {
      sp[[1]][1]
    }
  })
  
  if(class(s)!='list'){
     s <- list(s)}
  
  si <- sapply(1:length(s),function(x)rep(x,length(s[[x]])))
  
  si <- unlist(si)
  s <- unlist(s)
  
  names(si) <- s
  
  return(list(dates=si,times=tv))
}


dateList <- function(start,end,by){
  
  seq(from = as.Date(start),to = as.Date(end), by=by)
  
}



formattedDateString <- function(TDS,lyr,from=NULL,to=NULL,dates=NULL,n=60){
  
  if(is.null(dates)){
    dates <- dateList(from,to)}
  
  TDSlyr <- TDS@layers[TDS@queryLayerIndex][layerNames(TDS)==lyr][[1]]
  TDSdate <- layerDates(TDSlyr)
  ts <- TDSdate$times[TDSdate$dates[as.character(dates)]]
  
  dl <- paste(dates,ts,sep='T')
  
  if(length(dl)>n){
    dl <- split(dl, ceiling(seq_along(dl)/n))
    
    dl <- sapply(dl,function(x) paste(x,collapse = ','))
    
  } else {
    
    dl <- paste(dl,collapse = ',')
    
  }
    
  dl
  
}

dateRange <- function(TDS,lyr){
  
  TDSlyr <- TDS@layers[TDS@queryLayerIndex][layerNames(TDS)==lyr][[1]]
  TDSdate <- names(layerDates(TDSlyr)$dates)
  c(TDSdate[1],tail(TDSdate,1))
}


TDSquery <- function(TDS,lat,lon,lyr,datestring,elevation=NULL){
  # given THREDD WMS object lat/lon/elevation
  # datestring is an output from formatteddateSttring
  # returns values
#   from <- "2010-01-01"
#   to <- "2010-03-01"
#   lat <- 53.998
#   lon <- -5.461
#   lyr <- "analysed_sst"
#   #####################################
  
  if(length(datestring)>1){
    fi <- sapply(datestring,getFeatureInfo,url=TDS@url,lon=lon,lat=lat,layers=lyr,elevation = elevation,format="text/xml")
    vals <- unlist(sapply(fi,getXMLVals))
  } else {
    fi <- getFeatureInfo(url=TDS@url,lon=lon,lat=lat,date = datestring,layers=lyr,elevation = elevation,format="text/xml")
    vals <- getXMLVals(fi)
  }

  data.frame(date=as.Date(unlist(strsplit(datestring,','))),values=vals)
 
}


hasElevation <- function(l){'elevation'%in%names(l@dims)}
hasTime<-function(x){'time'%in%names(x@dims)}


vertProfile <- function(TDS,lon,lat,layer,date){
    #given: lat/lon/3dlayer/date
    #returns all values in vertical profile 
#   require(XML)
#   require(RCurl)
#   load("C:/Users/ds10/Dropbox/shiny_csv_reader/TDS.rdata")
#   lat <- 55.75185#55.3
#   lon <- 2.678604#4.64
#   layer <- "5/salt"#"5/h"#"5/temp"#"5/elev""5/hcc" "5/bathymetry"
#   date <- '2008-12-01'
#   layerNames(TDS)
  #vertProfile(TDS,,,,"2008-12-08T00:00:00.000Z")
  ###############################
  
  format <- 'text/xml'
  
  l <- TDS@layers[TDS@queryLayerIndex][layerNames(TDS)==layer][[1]]
  if(!hasElevation(l))
    return(NULL)
  
  vl <- strsplit(l@dimValues$elevation,',')[[1]]
  
#   if(hasTime(l)){
#     TDSdate <- layerDates(l)
#     t <- TDSdate$times[TDSdate$dates[date]]
#     t <- paste(date,t,sep='T')
#   } else {t <- NULL}
  
  fi <- sapply(vl,getFeatureInfo,url=TDS@url,lon=lon,lat=lat,layers=layer,date=date,format=format,count=NULL)
  vals <- unlist(sapply(fi,getXMLVals))
  vals
}

TDSqueryAll <- function(src,lyr,lat,lon,by,n=60){
#     src=sst
#     lyr='analysed_sst'
#     lat=25.8
#     lon=-87.55
#     n=60
#     by='month'
  ######################
  dr <- dateRange(src,lyr)
  dl <- dateList(start = dr[1],end =dr[2],by = by)
  datestring <- formattedDateString(TDS = src,lyr = lyr,dates = dl,n = n)
  TDSquery(src,lat,lon,lyr,datestring = datestring)}


#vertProfile(TDS,4.64,56.3,'5/temp','2008-06-01')



