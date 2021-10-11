


#GIS Libraries
library(sp)
library(rgdal)
library(PROJ)
library(rgeos)
library(tmap)
library(raster)

wgs84=CRS("+init=epsg:4326")
utm17=CRS("+init=epsg:26917")

fwc.pro=CRS("+init=epsg:3857")

FWC.ext=raster::extent(-9755473.9711,
                       -8803995.378754802, 
                       2801646.760499999,
                       3632857.472099997)
FWC.ext.poly=as(FWC.ext,"SpatialPolygons")
proj4string(FWC.ext.poly)=fwc.pro

# tmap_mode("view")
tm_shape(FWC.ext.poly)+tm_polygons(alpha=0.5)

FWC.ext.poly2=SpatialPolygonsDataFrame(FWC.ext.poly,data.frame(id=1))
FWC.ext.poly2=spTransform(FWC.ext.poly2,wgs84)
extent(FWC.ext.poly2)
tm_shape(FWC.ext.poly2)+tm_polygons(alpha=0.5)
FWC.ext2=extent(FWC.ext.poly2)

TBEP.ext=extent(-83.4,
                -82.08,
                27.0,
                28.2)
TBEP.ext.poly=as(TBEP.ext,"SpatialPolygons")
proj4string(TBEP.ext.poly)=wgs84
tm_shape(TBEP.ext.poly)+tm_polygons(alpha=0.5)


HABext=extent(-82.5,
              -80.0,
              24.4,
              27.4)
HABext.poly=as(HABext,"SpatialPolygons")
proj4string(HABext.poly)=wgs84
tm_shape(HABext.poly)+tm_polygons(alpha=0.5)
HABext2=extent(HABext)

## 

# based on metadata full extent
# https://atoll.floridamarine.org/arcgis/rest/services/Projects_FWC/HAB_Current/MapServer/
path="https://atoll.floridamarine.org/arcgis/rest/services/Projects_FWC/HAB_Current/MapServer/0/query?f=json&returnGeometry=true&spatialRel=esriSpatialRelIntersects&geometry=%7B%22xmin%22%3A-9755473.9711%2C%22ymin%22%3A2801646.760499999%2C%22xmax%22%3A9192007.6904%2C%22ymax%22%3A3632857.472099997%2C%22spatialReference%22%3A%7B%22wkid%22%3A102100%7D%7D&geometryType=esriGeometryEnvelope&inSR=102100&outFields=*&returnCentroid=false&returnExceededLimitFeatures=false&outSR=102100&quantizationParameters=%7B%22mode%22%3A%22view%22%2C%22originPosition%22%3A%22upperLeft%22%2C%22tolerance%22%3A1222.992452562501%2C%22extent%22%3A%7B%22xmin%22%3A-10018754.171394993%2C%22ymin%22%3A1878516.4071389847%2C%22xmax%22%3A-9392582.035682991%2C%22ymax%22%3A2504688.542850985%2C%22spatialReference%22%3A%7B%22wkid%22%3A102100%7D%7D%7D"


fwc_dat=fromJSON(file=path)
fwc_dat=fwc_dat$features
dat=data.frame()
for(i in 1:length(fwc_dat)){
  tmp=data.frame(fwc_dat[[i]]$attributes)
  dat=rbind(dat,tmp)
}
#dat
# unique(dat$Abundance)
abund=c("not present/background (0-1,000)", "very low (>1,000-10,000)", 
        "low (>10,000-100,000)", "medium (>100,000-1,000,000)", "high (>1,000,000)"
)
dat$Abundance=factor(dat$Abundance,levels=abund)

dat$date=date.fun(dat$SampleDate_t,form="%b  %d %Y",tz="EST")
range(dat$date)

dat.shp=SpatialPointsDataFrame(coords=dat[,c("LONGITUDE","LATITUDE")],
                               data=dat,
                               proj4string = wgs84)

library(httr)

path <- 'https://gis.ncdc.noaa.gov/arcgis/rest/services/ms/HABSOS_CellCounts/MapServer/0/query?'

# paste("LATITUDE <",TBEP.ext[4],"AND LATITUDE >",TBEP.ext[3],"AND LONGITUDE >",TBEP.ext[1],"AND LONGITUDE <",TBEP.ext[2])

paste("LATITUDE <",round(FWC.ext2[4],1),"AND LATITUDE >",round(FWC.ext2[3],1),"AND LONGITUDE >",round(FWC.ext2[1],1),"AND LONGITUDE <",round(FWC.ext2[2],1))
paste("LATITUDE <",round(TBEP.ext[4],1),"AND LATITUDE >",round(TBEP.ext[3],1),"AND LONGITUDE >",round(TBEP.ext[1],1),"AND LONGITUDE <",round(TBEP.ext[2],1))
paste("LATITUDE <",round(HABext2[4],1),"AND LATITUDE >",round(HABext2[3],1),"AND LONGITUDE >",round(HABext2[1],1),"AND LONGITUDE <",round(HABext2[2],1))

request <- GET(
  url = path,
  query= list(       
    # where = "STATE_ID='FL'",
    # where = "LATITUDE < 28.2 AND LATITUDE > 27 AND LONGITUDE > -83.4 AND LONGITUDE < -82.08",
    where= paste("LATITUDE <",round(HABext2[4],1),"AND LATITUDE >",round(HABext2[3],1),"AND LONGITUDE >",round(HABext2[1],1),"AND LONGITUDE <",round(HABext2[2],1)),
    outFields = 'DESCRIPTION,SAMPLE_DATE,LATITUDE,LONGITUDE,SALINITY,SALINITY_UNIT,WATER_TEMP,WATER_TEMP_UNIT,GENUS,SPECIES,CATEGORY,CELLCOUNT,CELLCOUNT_UNIT',
    f = 'pjson'
  )
)

response <- content(request, as = "text", encoding = "UTF-8")
results <- jsonlite::fromJSON(response,flatten=T)
results

results2=results$features

names(results2)
vars=c("attributes.DESCRIPTION", "attributes.SAMPLE_DATE", "attributes.LATITUDE", 
       "attributes.LONGITUDE", "attributes.SALINITY", "attributes.SALINITY_UNIT", 
       "attributes.WATER_TEMP", "attributes.WATER_TEMP_UNIT", "attributes.GENUS", 
       "attributes.SPECIES", "attributes.CATEGORY", "attributes.CELLCOUNT", 
       "attributes.CELLCOUNT_UNIT", "geometry.x", "geometry.y")
vars=strsplit(vars,"\\.")
vars=sapply(vars,"[",2)

colnames(results2)<-tolower(vars)

results2$date=as.POSIXct(as.numeric(gsub('000$', '',format(results2$sample_date,scientific=F))), 
                         origin = c('1970-01-01'), tz = 'UTC')
results2$date2=date.fun(results2$date,form="%F %R")
# results2$date2= format(results2$date,usetz=T,tz="EST")# date.fun(results2$date,form="%F %R")
tail(results2[,16:17])
range(results2$date2)
results2$year=as.numeric(format(results2$date2,"%Y"))
range(results2[results2$year!=153,]$year)

## 
TODAY=date.fun("2021-10-11")
sdate=date.fun(TODAY-lubridate::duration(30,"days"))

results2=subset(results2,date2<TODAY&date2>sdate)

habsos.shp=dat.shp=SpatialPointsDataFrame(coords=results2[,c("longitude","latitude")],
                                          data=results2,
                                          proj4string = wgs84)

tm_shape(dat.shp)+tm_dots("red")+
  tm_shape(habsos.shp)+tm_dots("blue")

library(tidyverse)

kbrdat <- results$features %>% 
  rename_all(function(x) gsub('^attributes\\.', '', x)) %>% 
  rename_all(tolower) %>% 
  mutate(
    date = format(sample_date, scientific = F),
    date = as.numeric(gsub('000$', '', date)), 
    date = as.POSIXct(date, origin = c('1970-01-01'), tz = 'UTC'), 
    date = as.Date(date)
  ) %>% 
  select(
    date, station = description, sal_ppt = salinity, temp_c = water_temp, kb_100kcelll = cellcount, longitude, latitude
  ) %>% 
  mutate(
    kb_100kcelll = kb_100kcelll / 1e5
  ) %>% 
  gather('var', 'val', -date, -station, -longitude, -latitude) %>% 
  separate(var, c('var', 'uni'), sep = '_') %>% 
  filter(!is.na(val))
tail(kbrdat)



## Blue green algae

# https://services1.arcgis.com/nRHtyn3uE1kyzoYc/arcgis/rest/services/VIEW_FL_Algal_Bloom_Site_Visits_1/FeatureServer
# https://community.esri.com/t5/gis-blog/accessing-arcgis-rest-services-using-r/ba-p/898451
bg.url=parse_url("https://services1.arcgis.com/nRHtyn3uE1kyzoYc/arcgis/rest/services")
bg.url$path<-paste(bg.url$path,"VIEW_FL_Algal_Bloom_Site_Visits_1/FeatureServer/0/query")
bg.url$query<-list(where="County = 'Lee'",
                   outfields="*",
                   returnGeometry="true")


#/VIEW_FL_Algal_Bloom_Site_Visits_1/FeatureServer"

bg.request <- build_url(bg.url)

bg.dat=sf::st_read(bg.request)


path <- 'https://services1.arcgis.com/nRHtyn3uE1kyzoYc/arcgis/rest/services/VIEW_FL_Algal_Bloom_Site_Visits_1/FeatureServer/0/query?'

# paste("LATITUDE <",TBEP.ext[4],"AND LATITUDE >",TBEP.ext[3],"AND LONGITUDE >",TBEP.ext[1],"AND LONGITUDE <",TBEP.ext[2])

cnty=c("Lee","Collier","Hendry","Glades","Okeechobee","Palm Beach","Broward")
DEP.bg.dat=data.frame()
# for(i in 1:length(cnty)){
request <- GET(
  url = path,
  query= list(       
    # where = "1=1",
    # where = paste("LATITUDE <",FWC.ext2[4],"AND LATITUDE >",TBEP.ext[3],"AND LONGITUDE >",FWC.ext2[1],"AND LONGITUDE <",FWC.ext2[2]),
    # where = paste0("County=","'",cnty[i],"'"),
    where = "County='Lee' OR County='Collier' OR County='Hendry' OR County='Glades' OR County='Palm Beach' OR County='Broward' OR County='MiamiDade'",
    outFields = '*',
    f = 'pjson'
  )
)
response <- content(request, as = "text", encoding = "UTF-8")
results <- jsonlite::fromJSON(response,flatten=T)
nrow(results$features)
# DEP.bg.dat=rbind(results$features,DEP.bg.dat)
# print(i)
# }

test=results$features
unique(test$attributes.County)
names(test)
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

colnames(test)<-tolower(vars)

test$sitevisitdate2=as.numeric(gsub('000$', '',format(test$sitevisitdate,scientific=F)))
test$date=as.POSIXct(test$sitevisitdate2,origin = c('1970-01-01'), tz = 'UTC')
test$date2=date.fun(test$date,form="%F %R")
head(test[,c("sitevisitdate","sitevisitdate2","date","date2")])

sum(is.na(test$date2))

test[14,c("sitevisitdate","sitevisitdate2","date","date2")]

## 
TODAY=date.fun("2021-10-11")
sdate=date.fun(TODAY-lubridate::duration(14,"days"))
sdate2=date.fun(TODAY-lubridate::duration(30,"days"))
range(test$date2,na.rm=T)

test2=subset(test,date.fun(date2)%in%seq(sdate,TODAY,"1 day"))
sum(test2$cyanobacteriadominant=="Yes")
sum(test2$toxinpresent=="Yes")
range(as.numeric(test2$microcystin),na.rm = T)

test3=subset(test,date.fun(date2)%in%seq(sdate2,TODAY,"1 day"))


dep.bg.shp=dat.shp=SpatialPointsDataFrame(coords=test[,c("longitude","latitude")],
                                          data=test,
                                          proj4string = wgs84)
tm_shape(dep.bg.shp)+tm_dots()
