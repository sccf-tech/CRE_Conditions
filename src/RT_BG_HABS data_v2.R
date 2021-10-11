
library(sp)
library(tmap)
library(httr)
library(jsonlite)
library(raster)

wgs84=CRS("+init=epsg:4326")
utm17=CRS("+init=epsg:26917")


HABext=extent(-82.5,
              -80.0,
              24.4,
              27.4)
HABext.poly=as(HABext,"SpatialPolygons")
proj4string(HABext.poly)=wgs84
tm_shape(HABext.poly)+tm_polygons(alpha=0.5)
HABext2=extent(HABext)

CRE.ext=extent(-82.3,
              -81.8,
              26.3,
              26.7)
CRE.ext=as(CRE.ext,"SpatialPolygons")
proj4string(CRE.ext)=wgs84
CRE.ext=SpatialPolygonsDataFrame(CRE.ext,data.frame(ID="CRE"))
tm_shape(CRE.ext)+tm_polygons(alpha=0.5)

# Red Tide ----------------------------------------------------------------
## south of Tampa Bay 
# Based on https://github.com/tbep-tech/piney-point/blob/141824adc082a57d79ba58ff38ac31d41dea4e44/R/dat_proc.R#L1262
path <- 'https://gis.ncdc.noaa.gov/arcgis/rest/services/ms/HABSOS_CellCounts/MapServer/0/query?'

request <- GET(
  url = path,
  query= list(       
    where= paste("LATITUDE <",round(HABext2[4],1),"AND LATITUDE >",round(HABext2[3],1),"AND LONGITUDE >",round(HABext2[1],1),"AND LONGITUDE <",round(HABext2[2],1)),
    outFields = 'DESCRIPTION,SAMPLE_DATE,LATITUDE,LONGITUDE,SALINITY,SALINITY_UNIT,WATER_TEMP,WATER_TEMP_UNIT,GENUS,SPECIES,CATEGORY,CELLCOUNT,CELLCOUNT_UNIT',
    f = 'pjson'
  )
)

response <- content(request, as = "text", encoding = "UTF-8")
results <- jsonlite::fromJSON(response,flatten=T)
results

kbrdat=results$features
vars=c("attributes.DESCRIPTION", "attributes.SAMPLE_DATE", "attributes.LATITUDE", 
       "attributes.LONGITUDE", "attributes.SALINITY", "attributes.SALINITY_UNIT", 
       "attributes.WATER_TEMP", "attributes.WATER_TEMP_UNIT", "attributes.GENUS", 
       "attributes.SPECIES", "attributes.CATEGORY", "attributes.CELLCOUNT", 
       "attributes.CELLCOUNT_UNIT", "geometry.x", "geometry.y")
vars=strsplit(vars,"\\.")
vars=sapply(vars,"[",2)

colnames(kbrdat)<-tolower(vars)

kbrdat$date=as.POSIXct(as.numeric(gsub('000$', '',format(kbrdat$sample_date,scientific=F))), 
                         origin = c('1970-01-01'), tz = 'UTC')
kbrdat$datetime=date.fun(kbrdat$date,form="%F %R")
kbrdat$date=date.fun(kbrdat$date)
kbrdat$year=as.numeric(format(kbrdat$date,"%Y"))
range(kbrdat$year,na.rm=T)
kbrdat=subset(kbrdat,is.na(year)==T|year!=153)

## 
TODAY=date.fun("2021-10-11")
sdate=date.fun(TODAY-lubridate::duration(30,"days"))

cat.abund.xwalk=data.frame(category=c("not observed","very low","low","medium","high"),
                           abund=c("not present/background (0-1,000)", "very low (>1,000-10,000)",
                                   "low (>10,000-100,000)", "medium (>100,000-1,000,000)", 
                                   "high (>1,000,000)"))

kbrdat=merge(kbrdat,cat.abund.xwalk,"category")
kbrdat$Abundance=factor(kbrdat$abund,levels=cat.abund.xwalk$abund)
kbrdat$category=factor(kbrdat$category,levels=cat.abund.xwalk$category)

kbrdat_30d=subset(kbrdat,date<TODAY&date>sdate)
kbrdat_30d.shp=SpatialPointsDataFrame(coords=kbrdat_30d[,c("longitude","latitude")],
                                          data=kbrdat_30d,
                                          proj4string = wgs84)

tm_shape(kbrdat_30d.shp)+tm_dots(col="Abundance",
                                 palette =c("white","grey","dodgerblue1","orange","indianred1"),
                                 size=0.03,
                                 id="date",
                                 popup.vars=c("date","cellcount","category","genus","species"),
                                 group="NOAA/FWC Redtide Data")+
  tm_shape(CRE.ext)+tm_borders(group="Greater Caloosahatchee Estuary")+
  tm_layout("Red Tide")


# CRE.inter=rgeos::gIntersects(CRE.ext,kbrdat_30d.shp,byid=T)
# plyr::ddply(kbrdat_30d.shp[as.vector(CRE.inter),]@data,"Abundance",summarise,N.val=N.obs(category))  
rslt =plyr::ddply(kbrdat_30d.shp[CRE.ext,]@data,"category",summarise,N.val=N.obs(category))  

rslt=merge(cat.abund.xwalk,rslt,"category",all.x=T)

rslt=rslt[match(cat.abund.xwalk$abund,rslt$abund),]
library(flextable)
library(magrittr)

rslt[,c("abund","N.val")]%>%
  flextable()%>%
  colformat_num(j=2,big.mark = "",digits=0,na_str = "---")%>%
  set_header_labels("abund"="Cell Abundance (Cells L\u207B\u00B9)",
                    "N.val"="Number\nof\nSamples")%>%
  align(j=2,align="center",part="all")%>%
  width(width=c(2.5,0.5))%>%
  font(fontname="Times New Roman",part="all")%>%
  padding(padding=2,part="all")


# Blue-Green Algae --------------------------------------------------------
path <- 'https://services1.arcgis.com/nRHtyn3uE1kyzoYc/arcgis/rest/services/VIEW_FL_Algal_Bloom_Site_Visits_1/FeatureServer/0/query?'

request <- GET(
  url = path,
  query= list(       
    where = "County='Lee' OR County='Collier' OR County='Hendry' OR County='Glades'",
    outFields = '*',
    f = 'pjson'
  )
)
response <- content(request, as = "text", encoding = "UTF-8")
results <- jsonlite::fromJSON(response,flatten=T)
nrow(results$features)

bgdat=results$features

request <- GET(
  url = path,
  query= list(       
    where = "County='Okeechobee' OR County='PalmBeach' OR County='Broward' OR County='MiamiDade'",
    outFields = '*',
    f = 'pjson'
  )
)
response <- content(request, as = "text", encoding = "UTF-8")
results <- jsonlite::fromJSON(response,flatten=T)
nrow(results$features)

bgdat=rbind(bgdat, results$features)

vars=c("attributes.objectid", "attributes.globalid", "attributes.SiteVisitDate", 
       "attributes.Location", "attributes.County", "attributes.Visitor", 
       "attributes.AlgaeObserved", "attributes.SampleTaken", "attributes.DepthDesc", 
       "attributes.SampleDepth", "attributes.AnalyzedBy", "attributes.Otherlab", 
       "attributes.Comments", "attributes.Latitude", "attributes.Longitude", 
       "attributes.AlgalID", "attributes.Microcystin", "attributes.OtherToxin", 
       "attributes.EditDate", "attributes.PicURL", "attributes.ToxinPresent", 
       "attributes.CyanobacteriaDominant", "geometry.x", "geometry.y"
)
vars=strsplit(vars,"\\.")
vars=sapply(vars,"[",2)
colnames(bgdat)<-tolower(vars)

bgdat$sitevisitdate=as.numeric(gsub('000$', '',format(bgdat$sitevisitdate,scientific=F)))
bgdat$datetime=as.POSIXct(bgdat$sitevisitdate,origin = c('1970-01-01'), tz = 'UTC')
bgdat$datetime=date.fun(bgdat$datetime,form="%F %R")
bgdat$date=date.fun(bgdat$datetime)

bgdat_30d=subset(bgdat,date<TODAY&date>sdate)
bgdat_30d.shp=SpatialPointsDataFrame(coords=bgdat[,c("longitude","latitude")],
                                              data=bgdat,
                                              proj4string = wgs84)

tm_shape(bgdat_30d.shp)+tm_dots(col="toxinpresent",
                                palette =c("green","red"),
                                colorNA="grey",
                                size=0.03,
                                id="date",
                                popup.vars=c("date","visitor","depthdesc",
                                             "microcystin",
                                             "cyanobacteriadominant","toxinpresent",
                                             "algalid","othertoxin"),
                                group="FDEP Blue-green Algae Data",
                                title="Cyanobacteria dominate")+
  tm_layout("Blue-Green Algae")


sum(bgdat_30d$cyanobacteriadominant=="Yes")
sum(bgdat_30d$toxinpresent=="Yes")
range(as.numeric(bgdat_30d$microcystin),na.rm = T)
