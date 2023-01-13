
## 
# Title:      red tide data for CRE area
# Objective:  Download remote sensing products from NOAA
# Created by: Paul Julian; pjulian@sccf.org
# Created on: 01/05/2023
## 
## 

## BAD ## https://www.tidyverse.org/articles/2017/12/workflow-vs-script/
#Clears Everything...start fresh.
rm(list=ls(all=T));cat("\014");dev.off()

## Libraries
library(AnalystHelper)
library(reshape)
library(plyr)

library(jsonlite)
library(sp)
library(rgdal)
library(rgeos)
# library(tmap)
library(httr)
library(raster)

##
wgs84=CRS("+init=epsg:4326")
utm17=CRS("+init=epsg:26917")
# tmap_mode("view")

## Paths
wd="C:/Julian_LaCie/_Github/CRE_Conditions"

paths=paste0(wd,c("/Plots/","/Export/","/Data/","/src","/GIS"))
# Folder.Maker(paths);#One and done. Creates folders in working directory.
plot.path=paths[1]
export.path=paths[2]
data.path=paths[3]
GIS.path=paths[5]


#### GIS data
shore=spTransform(readOGR("C:/Julian_LaCie/_GISData/FWC","FWC_Shoreline"),wgs84)

TODAY=as.POSIXct(strptime(Sys.time(),"%F"),tz="EST") # -duration(1,"days")
# Red tide ----------------------------------------------------------------

HABext=extent(-82.5,
              -80.0,
              24.4,
              27.4)
# HABext=extent(-83.1,
#               -80.0,
#               24.4,
#               28.1)
HABext.poly=as(HABext,"SpatialPolygons")
proj4string(HABext.poly)=wgs84
HABext.poly=SpatialPolygonsDataFrame(HABext.poly,data.frame(ID="HAB"))
# tm_shape(HABext.poly)+tm_polygons(alpha=0.5)
HABext2=extent(HABext)

CRE.ext=extent(-82.3,
               -81.8,
               26.3,
               26.7)
CRE.ext=as(CRE.ext,"SpatialPolygons")
proj4string(CRE.ext)=wgs84
CRE.ext=SpatialPolygonsDataFrame(CRE.ext,data.frame(ID="CRE"))
# tm_shape(CRE.ext)+tm_polygons(alpha=0.5)

TBEP.ext=extent(-83.4,
                -82.08,
                27.0,
                28.2)
TBEP.ext.poly=as(TBEP.ext,"SpatialPolygons")
proj4string(TBEP.ext.poly)=wgs84

kbr.AOI=extent(bind(HABext.poly,TBEP.ext.poly))
kbr.AOI=as(kbr.AOI,"SpatialPolygons")
proj4string(kbr.AOI)=wgs84

HABext2=extent(CRE.ext)
# Red Tide ----------------------------------------------------------------
## south of Tampa Bay 
## Based on https://github.com/tbep-tech/piney-point/blob/141824adc082a57d79ba58ff38ac31d41dea4e44/R/dat_proc.R#L1262
path <- 'https://gis.ncdc.noaa.gov/arcgis/rest/services/ms/HABSOS_CellCounts/MapServer/0/query?'

request <- GET(
  url = path,
  query= list(
    where= paste("LATITUDE <",round(HABext2[4],1),"AND LATITUDE >",round(HABext2[3],1),"AND LONGITUDE >",round(HABext2[1],1),"AND LONGITUDE <",round(HABext2[2],1)),
    outFields = 'DESCRIPTION,SAMPLE_DATE,SAMPLE_DEPTH,LATITUDE,LONGITUDE,SALINITY,SALINITY_UNIT,WATER_TEMP,WATER_TEMP_UNIT,GENUS,SPECIES,CATEGORY,CELLCOUNT,CELLCOUNT_UNIT',
    f = 'pjson'
  )
)


response <- content(request, as = "text", encoding = "UTF-8")
results <- jsonlite::fromJSON(response,flatten=T)
kbrdat=results$features
 
  vars=c("attributes.DESCRIPTION","attributes.SAMPLE_DATE",
         "attributes.SAMPLE_DEPTH","attributes.LATITUDE",
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
  # range(kbrdat$year,na.rm=T)
  kbrdat=subset(kbrdat,is.na(year)==T|year!=153)
  # min(kbrdat$date)
  # max(kbrdat$date)
  ##
  cat.abund.xwalk=data.frame(category=c("not observed","very low","low","medium","high"),
                             abund=c("not present/background (0-1,000)", "very low (>1,000-10,000)",
                                     "low (>10,000-100,000)", "medium (>100,000-1,000,000)",
                                     "high (>1,000,000)"))
  
  kbrdat=merge(kbrdat,cat.abund.xwalk,"category")
  kbrdat$Abundance=factor(kbrdat$abund,levels=cat.abund.xwalk$abund)
  kbrdat$category=factor(kbrdat$category,levels=cat.abund.xwalk$category)
  
  kbrdat.shp=SpatialPointsDataFrame(coords=kbrdat[,c("longitude","latitude")],
                                    data=kbrdat,
                                    proj4string = wgs84)


  plot(shore)
  plot(kbrdat.shp,add=T)
  
kbrdat$week.num=as.numeric(format(kbrdat$date,'%U'))
kbrdat$CY=as.numeric(format(kbrdat$date,'%Y'))

RT.weekmean=ddply(kbrdat,c("CY",'week.num'),summarise,mean.count=mean(cellcount))  
RT.weekmean$weeknum.date=with(RT.weekmean,date.fun(as.Date(paste(week.num,CY,'Sun'),"%U %Y %a")))
RT.weekmean$log10.mean=with(RT.weekmean,ifelse(mean.count==0,0,log10(mean.count)))

abund.cols=c("grey","dodgerblue1","orange","indianred1")
abund.vals=c(1000,10000,100000,1000000)
ylim.val=c(0,7);by.y=1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
# png(filename=paste0(plot.path,"HAB_CRE_week.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,4,0.5,0.25),oma=c(3,2,0.75,0.5));
layout(matrix(1:4,2,2),widths=c(1,0.5))

xlim.val=date.fun(c("2019-01-01","2020-01-01"));xmaj=seq(xlim.val[1],xlim.val[2],by="3 months");xmin=seq(xlim.val[1],xlim.val[2],by="1 months")
plot(log10.mean~weeknum.date,RT.weekmean,xlim=xlim.val,ylim=ylim.val,type="n",axes=F,ann=F)
abline(h=ymaj,v=xmaj,lty=3,col="grey")
with(RT.weekmean,pt_line(weeknum.date,log10.mean,2,"salmon",1,21,"salmon",pt.lwd = 0.01,cex=1.25))
abline(h=log10(abund.vals),col=abund.cols,lwd=1.5,lty=2)
axis_fun(1,xmaj,xmin,format(xmaj,"%b-%Y"),line=-0.5)
axis_fun(2,ymaj,ymaj,format(c(0,10^(ymaj[2:8])),scientific = F));box(lwd=1)
mtext(side=2,line=4,expression(paste(italic("K. brevis")," (cells L"^"-1",")")))

xlim.val=date.fun(c("2022-01-01","2023-01-01"));xmaj=seq(xlim.val[1],xlim.val[2],by="3 months");xmin=seq(xlim.val[1],xlim.val[2],by="1 months")
plot(log10.mean~weeknum.date,RT.weekmean,xlim=xlim.val,ylim=ylim.val,type="n",axes=F,ann=F)
abline(h=ymaj,v=xmaj,lty=3,col="grey")
with(RT.weekmean,pt_line(weeknum.date,log10.mean,2,"salmon",1,21,"salmon",pt.lwd = 0.01,cex=1.25))
abline(h=log10(abund.vals),col=abund.cols,lwd=1.5,lty=2)
axis_fun(1,xmaj,xmin,format(xmaj,"%b-%Y"),line=-0.5)
axis_fun(2,ymaj,ymaj,format(c(0,10^(ymaj[2:8])),scientific = F));box(lwd=1)
mtext(side=2,line=4,expression(paste(italic("K. brevis")," (cells L"^"-1",")")))
mtext(side=1,line=1.75,"Date (Month-Year)")

par(mar=c(1,0.5,0.5,0.1))
bbox.lims=bbox(kbr.AOI)
plot(shore,col="cornsilk",border="grey",bg="lightblue",ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],lwd=0.01)
plot(CRE.ext,add=T,border="salmon",lwd=2)
box(lwd=1)
mapmisc::scaleBar(wgs84,"bottomleft",bty="n",cex=1,seg.len=4,outer=F)

plot(0:1,0:1,type = 'n', axes = F,xlab=NA, ylab=NA)
legend("center",legend=c("Spatially Averaged Weekly Mean",
                         paste(subset(cat.abund.xwalk,category!="not observed")$category,"cell abundance")),
       pt.bg = c("salmon",rep(NA,4)),pch=c(21,rep(NA,4)),pt.cex=c(1.5),
       col=c("black",abund.cols),lty=c(NA,2,2,2,2),lwd=c(0.01,rep(2,4)),
       ncol=1,cex=0.8,bty="n",y.intersp=1,x.intersp=0.5,xpd=NA,xjust=0.5)
dev.off()