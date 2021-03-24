## 
## Caloosahatchee Conditions Data Viz
##
##
##
## Code was compiled by Paul Julian
## contact info: pjulian@sccf.org

## BAD 
## https://www.tidyverse.org/articles/2017/12/workflow-vs-script/
## Clears Everything...start fresh.
rm(list=ls(all=T));cat("\014");dev.off()

## Libraries
#devtools::install_github("SwampThingPaul/AnalystHelper")
library(AnalystHelper);
library(plyr)
library(reshape)
library(zoo)
# GIS libraries 
library(rgdal)
library(rgeos)
library(raster)
library(gstat)

#thin plate spline https://rspatial.org/raster/analysis/4-interpolation.html
library(fields)
library(raster)

## Paths
wd="C:/Julian_LaCie/_Github/CRE_Conditions"

paths=paste0(wd,c("/Plots/","/Export/","/Data/","/src"))
# Folder.Maker(paths);#One and done. Creates folders in working directory.
plot.path=paths[1]
export.path=paths[2]
data.path=paths[3]

GIS.path.gen=paste0(dirname(dirname(wd)),"/_GISData")

# Helper variables
nad83.pro=CRS(SRS_string ="EPSG:4269")
utm17=CRS(SRS_string ="EPSG:26917")

dates=date.fun(c("2000-05-01","2020-04-30"))
# GIS Data ----------------------------------------------------------------
shoreline=spTransform(readOGR(paste0(GIS.path.gen,"/FWC"),"FWC_Shoreline"),utm17)
shoreline=gSimplify(shoreline,100)

roads.all=spTransform(readOGR(paste0(GIS.path.gen,"/FDOT"),"FDOT_Roads"),utm17)
lakes=spTransform(readOGR(paste0(GIS.path.gen,"/NHD"),"NHD100_Waterbody"),utm17)
wetland=subset(lakes,FTYPE%in%c("466"))


wmd.mon=spTransform(readOGR(paste0(GIS.path.gen,"/SFWMD_Monitoring_20200221"),"Environmental_Monitoring_Stations"),wkt(utm17))

wq.mon=subset(wmd.mon,ACTIVITY_S=="Surface Water Grab")
sites=c(paste0("CES0",2:9),"CES11","ROOK471")
plot(subset(wq.mon,SITE%in%sites))

CRE.ext=raster::extent(gBuffer(subset(wq.mon,STATION%in%c("S79","ROOK471","ROOK477")),width=1000))
CREr=raster(xmn=CRE.ext[1],xmx=CRE.ext[2],ymn=CRE.ext[3],ymx=CRE.ext[4],crs=utm17)
CREr=setValues(CREr,0)
CREr=raster::mask(CREr,subset(wq.mon,STATION=="S79"))
CRErD=distance(CREr)

plot(CRErD)

est_nnc_seg=spTransform(readOGR(paste0(GIS.path.gen,"/FDEP"),"Estuary_NNC"),wkt(utm17))
library(tmap)
tmap_mode("view")
tm_shape(est_nnc_seg)+tm_polygons()

segs=c("Upper Caloosahatchee River Estuary","Middle Caloosahatchee River Estuary","Lower Caloosahatchee River Estuary","San Carlos Bay")
cre.nnc.segs=subset(est_nnc_seg,SEGMENT_NA%in%segs)
plot(cre.nnc.segs)
cre.nnc.segs.dis=gUnaryUnion(cre.nnc.segs)
cre.nnc.segs.dis=SpatialPolygonsDataFrame(cre.nnc.segs.dis,data.frame(ID=1))
plot(gSimplify(cre.nnc.segs.dis,100))
plot(gBuffer(cre.nnc.segs.dis,width=400))

cre.nnc.segs.dis=gBuffer(cre.nnc.segs.dis,width=800)

# Discharge ---------------------------------------------------------------
S79.Q=DBHYDRO_daily(dates[1]-lubridate::ddays(14),dates[2],"00865")

plot(Data.Value~Date,S79.Q)
range(S79.Q$Data.Value,na.rm=T)
subset(S79.Q,is.na(Data.Value)==T)

S79.Q$Date.EST=date.fun(S79.Q$Date)
S79.Q$Q14d=with(S79.Q,c(rep(NA,13),rollapply(Data.Value,width=14,FUN=function(x)mean(x,na.rm=T))))

with(S79.Q,lines(Date,Q14d,col="red"))

# WQ Data -----------------------------------------------------------------

# params=data.frame(Test.Number=c(21,20,18,80,61,179,25,23),param=c("TKN","NH4","NOx","TN","Chla","Chla","TP","SRP"))
params=data.frame(Test.Number=c(98),param=c("sal"))
dat=data.frame()
for(i in 1:length(sites)){
  tmp=DBHYDRO_WQ(dates[1],dates[2],sites[i],params$Test.Number)
  dat=rbind(tmp,dat)
  print(i)
}
dat=merge(dat,params,"Test.Number")

da.dat=ddply(dat,c("Station.ID","Date.EST"),summarise,mean.Sal=mean(HalfMDL,na.rm=T))

# wq.q.dat=merge(dat,S79.Q,"Date.EST",all.x=T)
wq.q.dat=merge(da.dat,S79.Q,"Date.EST",all.x=T)

# plot(HalfMDL~Data.Value,wq.q.dat)
plot(mean.Sal~Q14d,wq.q.dat)


# GAM ---------------------------------------------------------------------
library(mgcv)
library(gratia)

sites.shp=cbind(data.frame(Station.ID=subset(wq.mon,STATION%in%sites)@data$STATION),
                coordinates(subset(wq.mon,STATION%in%sites)))
colnames(sites.shp)=c("Station.ID","UTMX","UTMY")
# head(wq.q.dat[,c("Station.ID",'Date.EST',"param","HalfMDL","Data.Value")])
# subset(dat,Station.ID=="ROOK471"&Date.EST==date.fun("2000-05-03"))
# range(dat$Depth,na.rm=T)
# nrow(subset(dat,Depth<0.5|is.na(Depth)==T))

wq.q.dat=merge(sites.shp,wq.q.dat,"Station.ID",all.x=T)

coord.K=9
sal.m<-bam(mean.Sal~
            s(Q14d,k=5)+
            s(UTMX,UTMY,bs="ds",k=coord.K,m=c(1,0.5)),
          data=wq.q.dat)
summary(sal.m)
nvar=2;layout(matrix(1:nvar,1,nvar))
plot(sal.m,residuals=T,pch=21)

nvar=4;layout(matrix(1:nvar,1,nvar))
gam.check(sal.m,pch=21)
shapiro.test(residuals(sal.m))
draw(sal.m)

###
library(DHARMa)

testDispersion(sal.m)
sim.out=simulateResiduals(sal.m,plot=T)
residuals(sim.out)
residuals(sim.out,quantileFunction = qnorm, outlierValues = c(-7,7))

testResiduals(sim.out)

###

cre.ex=raster::extent(gBuffer(subset(wq.mon,STATION%in%c("S79","ROOK471","ROOK477")),width=1000))
pdata<-expand.grid(Q14d=seq(200,3000,100),
              UTMX=seq(cre.ex[1],cre.ex[2],length.out=100),
              UTMY=seq(cre.ex[3],cre.ex[4],length.out=100)
            )
# plot(UTMY~UTMX,pdata)

fit <- predict(sal.m, pdata)
pred <- cbind(pdata, Fitted = fit)

library(ggplot2)
library(viridis)
ggplot(pred, aes(x = UTMX, y = UTMY)) +
  geom_raster(aes(fill = Fitted)) + facet_wrap(~ Q14d, ncol = 12) +
  scale_fill_viridis(name = "Sal", option = 'plasma', na.value = 'transparent') +
  coord_quickmap() +
  theme(legend.position = 'right')


q.val=seq(200,3000,100)
for(i in 1:length(q.val)){
  tmp=subset(pred,Q14d==q.val[i])[,c("Fitted","UTMX","UTMY")]
  coordinates(tmp)<-~UTMX + UTMY
  gridded(tmp)<-TRUE
  tmp=as(tmp,"RasterLayer")
  proj4string(tmp)<-utm17
  tmp.m=raster::mask(tmp,cre.nnc.segs.dis)
  assign(paste0("Q14d.",q.val[i]),tmp.m)
  print(i)
}

GAM.sal.stack=raster::stack(Q14d.200,
                            Q14d.300,
                            Q14d.400,
                            Q14d.500,
                            Q14d.600,
                            Q14d.700,
                            Q14d.800,
                            Q14d.900,
                            Q14d.1000,
                            Q14d.1100,
                            Q14d.1200,
                            Q14d.1300,
                            Q14d.1400,
                            Q14d.1500,
                            Q14d.1600,
                            Q14d.1700,
                            Q14d.1800,
                            Q14d.1900,
                            Q14d.2000,
                            Q14d.2100,
                            Q14d.2200,
                            Q14d.2300,
                            Q14d.2400,
                            Q14d.2500,
                            Q14d.2600,
                            Q14d.2700,
                            Q14d.2800,
                            Q14d.2900,
                            Q14d.3000)
# tmap_mode("plot")
bbox.lims=bbox(cre.nnc.segs.dis)

bbox.poly=as(raster::extent(cre.nnc.segs.dis),"SpatialPolygons")#makes the polygon
proj4string(bbox.poly)=utm17#projects the polygon
plot(crop(roads.all,gBuffer(bbox.poly,width=200)))

GAM.sal=tm_shape(GAM.sal.stack,bbox=bbox.lims)+
  tm_raster(title="",palette="Blues",
            breaks=c(-3,5,10,15,20,30,40),
            labels=c("0","< 5","5 - 10","10 - 15", "15 - 20","20 - 30","30 - 40"))+
  tm_facets(free.scales=FALSE,nrow=1,ncol=1)+
  tm_layout(panel.labels=paste("14-day Avg",q.val,"cfs"),fontfamily = "serif")+
  tm_legend(title="Salinity\n(PSU)",legend.outside=T, legend.text.size=0.75,legend.title.size=1)+
  tm_shape(shoreline)+tm_polygons(col="cornsilk")+
  tm_shape(crop(roads.all,gBuffer(bbox.poly,width=200)))+tm_lines("grey")+
  tm_shape(subset(wmd.mon,STATION=="S79_TOT"))+tm_symbols(col="indianred1",size=0.75)+tm_text("SITE",shadow=T,ymod=-0.75,size=0.75)
# GAM.sal

tm_shape(shoreline,bbox=bbox.lims)+tm_polygons(col="cornsilk")+
  tm_shape(crop(roads.all,gBuffer(bbox.poly,width=200)))+tm_lines("grey")+
  tm_shape(subset(wmd.mon,STATION=="S79_TOT"))+tm_symbols(col="indianred1",size=0.5)+tm_text("SITE",shadow=T,ymod=-0.75,size=0.75)

tmap_animation(GAM.sal,filename="./Plots/Sal_GAM.gif",delay=90,width=600,height=300,loop=TRUE)
