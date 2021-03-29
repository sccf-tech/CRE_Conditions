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
library(ggplot2)
library(viridis)

library(mgcv)
library(gratia)
library(DHARMa)

# GIS libraries 
library(rgdal)
library(rgeos)
library(raster)
library(tmap)


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

##GGPLOT theme defaults
theme_set(theme_minimal(base_size = 16))

dates=date.fun(c("2000-05-01","2020-04-30"))
# GIS Data ----------------------------------------------------------------
shoreline=spTransform(readOGR(paste0(GIS.path.gen,"/FWC"),"FWC_Shoreline"),utm17)
shoreline=gSimplify(shoreline,100)

roads.all=spTransform(readOGR(paste0(GIS.path.gen,"/FDOT"),"FDOT_Roads"),utm17)
# lakes=spTransform(readOGR(paste0(GIS.path.gen,"/NHD"),"NHD100_Waterbody"),utm17)
# wetland=subset(lakes,FTYPE%in%c("466"))

wmd.mon=spTransform(readOGR(paste0(GIS.path.gen,"/SFWMD_Monitoring_20200221"),"Environmental_Monitoring_Stations"),wkt(utm17))

wq.mon.logged=subset(wmd.mon,ACTIVITY_S=="Logged")
sites.logged=c("S79_T", "VALI75", "FORTMYERSM", "CCORAL", "MARKH", "MARKERH", "SANIB1", "SANIB2")
plot(subset(wq.mon.logged,STATION%in%sites.logged))

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
# library(tmap)
# tmap_mode("view")
# tm_shape(est_nnc_seg)+tm_polygons()

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

ddply(dat,c("Station.ID","Date.EST"),summarise,N.val=N.obs(HalfMDL),min.depth=min(Depth,na.rm=T))

da.dat=ddply(subset(dat,Depth<=0.5),c("Station.ID","Date.EST"),summarise,mean.Sal=mean(HalfMDL,na.rm=T))
nrow(da.dat)

# wq.q.dat=merge(dat,S79.Q,"Date.EST",all.x=T)
wq.q.dat=merge(da.dat,S79.Q,"Date.EST",all.x=T)

# plot(HalfMDL~Data.Value,wq.q.dat)
plot(mean.Sal~Q14d,wq.q.dat)


# GAM ---------------------------------------------------------------------
sites.shp=cbind(data.frame(Station.ID=subset(wq.mon,STATION%in%sites)@data$STATION),
                coordinates(subset(wq.mon,STATION%in%sites)))
colnames(sites.shp)=c("Station.ID","UTMX","UTMY")
# head(wq.q.dat[,c("Station.ID",'Date.EST',"param","HalfMDL","Data.Value")])
# subset(dat,Station.ID=="ROOK471"&Date.EST==date.fun("2000-05-03"))
# range(dat$Depth,na.rm=T)
# nrow(subset(dat,Depth<0.5|is.na(Depth)==T))

wq.q.dat=merge(sites.shp,wq.q.dat,"Station.ID",all.x=T)

coord.K=10
sal.k=30
sal.m<-bam(mean.Sal~
            s(Q14d,k=sal.k)+
            s(UTMX,UTMY,bs="ds",k=coord.K,m=c(1,0.5))+
            ti(UTMX,UTMY,Q14d,d=c(2,1),bs=c("ds","tp"),k=c(coord.K,sal.k)),
          data=wq.q.dat)
summary(sal.m)
nvar=2;layout(matrix(1:nvar,1,nvar))
plot(sal.m,residuals=T,pch=21)

nvar=4;layout(matrix(1:nvar,1,nvar))
gam.check(sal.m,pch=21)
shapiro.test(residuals(sal.m))
draw(sal.m)

###
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

tmap_mode("plot")
bbox.lims=bbox(gBuffer(cre.nnc.segs.dis,width=2000))

bbox.poly=as(raster::extent(gBuffer(cre.nnc.segs.dis,width=2000)),"SpatialPolygons")#makes the polygon
proj4string(bbox.poly)=utm17#projects the polygon
# plot(crop(roads.all,gBuffer(bbox.poly,width=200)))

GAM.sal=tm_shape(GAM.sal.stack,bbox=bbox.lims)+
  tm_raster(title="",palette="Blues",
            breaks=c(-3,5,10,15,20,30,40),
            labels=c("0","< 5","5 - 10","10 - 15", "15 - 20","20 - 30","30 - 40"))+
  tm_facets(free.scales=FALSE,nrow=1,ncol=1)+
  tm_layout(panel.labels=paste("14-day Avg",q.val,"cfs"),fontfamily = "serif",bg.color="lightblue1")+
  tm_legend(title="Salinity\n(PSU)",legend.outside=T, legend.text.size=0.75,legend.title.size=1)+
  tm_shape(shoreline)+tm_polygons(col="cornsilk")+
  tm_shape(crop(roads.all,gBuffer(bbox.poly,width=200)))+tm_lines("grey")+
  tm_shape(subset(wmd.mon,STATION=="S79_TOT"))+tm_symbols(col="indianred1",size=0.75)+tm_text("SITE",shadow=T,ymod=-0.75,size=0.75)
# GAM.sal

tm_shape(shoreline,bbox=bbox.lims)+tm_polygons(col="cornsilk")+
 tm_shape(crop(roads.all,gBuffer(bbox.poly,width=200)))+tm_lines("grey")+
 tm_shape(subset(wmd.mon,STATION=="S79_TOT"))+tm_symbols(col="indianred1",size=0.5)+tm_text("SITE",shadow=T,ymod=-0.75,size=0.75)+tm_layout(bg.color="lightblue1")

# tmap_animation(GAM.sal,filename="./Plots/Sal_GAM.gif",delay=90,width=600,height=300,loop=TRUE)



# Daily Data --------------------------------------------------------------
dates=date.fun(c("2009-05-01","2020-04-30"))

dat.dbkeys.sur=data.frame(depth="surface",SITE=c(rep("S79_T",2),rep("VALI75",2),rep("FORTMYERSM",4),rep("CCORAL",2),rep("MARKH",2),rep("MARKERH",2),rep("SANIB1",2)),param=c(rep(c("WT","SPC"),8)),DBKEY=c("15286","15287","UL031","UL027","PE681","PE685","88285","88289","UO833","PS985","88199","88203","15269","15271","WN366","WN368"))
dat.dbkeys.bot=data.frame(depth="bottom", SITE=c(rep("S79_T",2),rep("VALI75",2),rep("FORTMYERSM",4),rep("CCORAL",2),rep("MARKH",2),rep("MARKERH",2),rep("SANIB2",2)),param=c(rep(c("WT","SPC"),8)),DBKEY=c("15285","15289","UL029","UL025","PE684","PE688","88286","88290","UO831","PS984","88197","88201","15268","15270","WN374","WN376"))

# dat.dbkeys.sur=subset(dat.dbkeys.sur,!(DBKEY%in%c("PE681","PE685"))); #POR May 00 - Oct 11
# dat.dbkeys.bot=subset(dat.dbkeys.bot,!(DBKEY%in%c("PE684","PE688"))); #POR May 00 - Oct 11

dat.surf=data.frame()
for(i in 1:nrow(dat.dbkeys.sur)){
  tmp=DBHYDRO_daily(dates[1],dates[2],dat.dbkeys.sur$DBKEY[i])
  tmp$DBKEY=dat.dbkeys.sur$DBKEY[i]
  dat.surf=rbind(dat.surf,tmp)
  print(i)
}
dat.surf=merge(dat.surf,dat.dbkeys.sur,"DBKEY")
dat.surf$Date.EST=date.fun(dat.surf$Date)

dat.bot=data.frame()
for(i in 1:nrow(dat.dbkeys.bot)){
  tmp=DBHYDRO_daily(dates[1],dates[2],dat.dbkeys.bot$DBKEY[i])
  tmp$DBKEY=dat.dbkeys.bot$DBKEY[i]
  dat.bot=rbind(dat.bot,tmp)
  print(i)
}
dat.bot=merge(dat.bot,dat.dbkeys.bot,"DBKEY")
dat.bot$Date.EST=date.fun(dat.bot$Date)

## Surf
dat.surf.xtab=cast(dat.surf,SITE+Date.EST~param,value="Data.Value",fun.aggregate = function(x)mean(x,na.rm=T))
dat.surf.xtab$Sal=with(dat.surf.xtab,SalinityCalc(SPC,WT))
dat.surf.xtab$SITE=factor(dat.surf.xtab$SITE,levels=sites.logged)

ggplot(dat.surf.xtab, aes(x = Date.EST, y = Sal)) + geom_line() + facet_wrap(~ SITE)

## Bottom
dat.bot.xtab=cast(dat.bot,SITE+Date.EST~param,value="Data.Value",fun.aggregate = function(x)mean(x,na.rm=T))
dat.bot.xtab$Sal=with(dat.bot.xtab,SalinityCalc(SPC,WT))
dat.bot.xtab$SITE=factor(dat.bot.xtab$SITE,levels=sites.logged)

ggplot(dat.bot.xtab, aes(x = Date.EST, y = Sal)) + geom_line() + facet_wrap(~ SITE)

# Discharge ---------------------------------------------------------------
S79.Q=DBHYDRO_daily(dates[1]-lubridate::ddays(14),dates[2],"00865")

plot(Data.Value~Date,S79.Q)

S79.Q$Date.EST=date.fun(S79.Q$Date)
S79.Q$Q14d=with(S79.Q,c(rep(NA,13),rollapply(Data.Value,width=14,FUN=function(x)mean(x,na.rm=T))))

with(S79.Q,lines(Date,Q14d,col="red"))

# GAM ---------------------------------------------------------------------
dat.surf.xtab=subset(dat.surf.xtab,SITE%in%c("S79_T", "VALI75", "FORTMYERSM", "CCORAL", "MARKH","SANIB1"))
dat.bot.xtab=subset(dat.surf.xtab,SITE%in%c("S79_T", "VALI75", "FORTMYERSM", "CCORAL", "MARKH","SANIB2"))


# Surface -----------------------------------------------------------------
wq.q.dat=merge(dat.surf.xtab,S79.Q[,c("Date.EST","Q14d")],"Date.EST",all.x=T)
wq.q.dat$Station.ID=wq.q.dat$SITE

sites.shp=cbind(data.frame(Station.ID=subset(wq.mon.logged,STATION%in%sites.logged)@data$STATION),
                coordinates(subset(wq.mon.logged,STATION%in%sites.logged)))
colnames(sites.shp)=c("Station.ID","UTMX","UTMY")

wq.q.dat=merge(sites.shp,wq.q.dat,"Station.ID",all.x=T)

coord.K=6
sal.k=50
sal.m<-bam(Sal~
             s(Q14d,k=sal.k)+
             s(UTMX,UTMY,bs="ds",k=coord.K,m=c(1,0.5)),
           data=wq.q.dat)
summary(sal.m)
nvar=2;layout(matrix(1:nvar,1,nvar))
plot(sal.m,residuals=T,pch=21)

nvar=4;layout(matrix(1:nvar,1,nvar))
gam.check(sal.m,pch=21)

draw(sal.m)


###
dev.off()
testDispersion(sal.m)
sim.out=simulateResiduals(sal.m,plot=T)
# residuals(sim.out)
# residuals(sim.out,quantileFunction = qnorm, outlierValues = c(-7,7))
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

tmap_mode("plot")
bbox.lims=bbox(gBuffer(cre.nnc.segs.dis,width=2000))

bbox.poly=as(raster::extent(gBuffer(cre.nnc.segs.dis,width=2000)),"SpatialPolygons")#makes the polygon
proj4string(bbox.poly)=utm17#projects the polygon
# plot(crop(roads.all,gBuffer(bbox.poly,width=200)))

GAM.sal=tm_shape(GAM.sal.stack[1],bbox=bbox.lims)+
  tm_raster(title="",palette="Blues",
            breaks=c(-3,5,10,15,20,25,30,35,40),
            labels=c("0","< 5","5 - 10","10 - 15", "15 - 20","20 - 25","25 - 30","30 - 35","35 - 40"))+
  tm_facets(free.scales=FALSE,nrow=1,ncol=1)+
  tm_layout(panel.labels=paste("14-day Avg",q.val,"cfs"),fontfamily = "serif",bg.color="lightblue1")+
  tm_legend(title="Surface Salinity\n(PSU)",legend.outside=T, legend.text.size=0.75,legend.title.size=1)+
  tm_shape(shoreline)+tm_polygons(col="cornsilk")+
  tm_shape(crop(roads.all,gBuffer(bbox.poly,width=200)))+tm_lines("grey")+
  tm_shape(subset(wmd.mon,STATION=="S79_TOT"))+tm_symbols(col="indianred1",size=0.75)+tm_text("SITE",shadow=T,ymod=-0.75,size=0.75)
# GAM.sal

# tmap_animation(GAM.sal,filename="./Plots/Surf_Sal_da_GAM.gif",delay=90,width=600,height=300,loop=TRUE)


# Bottom ------------------------------------------------------------------
wq.q.dat=merge(dat.bot.xtab,S79.Q[,c("Date.EST","Q14d")],"Date.EST",all.x=T)
wq.q.dat$Station.ID=wq.q.dat$SITE

sites.shp=cbind(data.frame(Station.ID=subset(wq.mon.logged,STATION%in%sites.logged)@data$STATION),
                coordinates(subset(wq.mon.logged,STATION%in%sites.logged)))
colnames(sites.shp)=c("Station.ID","UTMX","UTMY")

wq.q.dat=merge(sites.shp,wq.q.dat,"Station.ID",all.y=T)

coord.K=6
sal.k=50
sal.m<-bam(Sal~
             s(Q14d,k=sal.k)+
             s(UTMX,UTMY,bs="ds",k=coord.K,m=c(1,0.5)),
           data=wq.q.dat)
summary(sal.m)
nvar=2;layout(matrix(1:nvar,1,nvar))
plot(sal.m,residuals=T,pch=21)

nvar=4;layout(matrix(1:nvar,1,nvar))
gam.check(sal.m,pch=21)
draw(sal.m)

cre.ex=raster::extent(gBuffer(subset(wq.mon,STATION%in%c("S79","ROOK471","ROOK477")),width=1000))
pdata<-expand.grid(Q14d=seq(200,3000,100),
                   UTMX=seq(cre.ex[1],cre.ex[2],length.out=100),
                   UTMY=seq(cre.ex[3],cre.ex[4],length.out=100)
)
# plot(UTMY~UTMX,pdata)

fit <- predict(sal.m, pdata)
pred <- cbind(pdata, Fitted = fit)

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

tmap_mode("plot")
bbox.lims=bbox(gBuffer(cre.nnc.segs.dis,width=2000))

bbox.poly=as(raster::extent(gBuffer(cre.nnc.segs.dis,width=2000)),"SpatialPolygons")#makes the polygon
proj4string(bbox.poly)=utm17#projects the polygon
# plot(crop(roads.all,gBuffer(bbox.poly,width=200)))

GAM.sal=tm_shape(GAM.sal.stack,bbox=bbox.lims)+
  tm_raster(title="",palette="Blues",
            breaks=c(-5,5,10,15,20,25,30,35,40),
            labels=c("0","0 - 5","5 - 10","10 - 15", "15 - 20","20 - 25","25 - 30","30 - 35","35 - 40"))+
  tm_facets(free.scales=FALSE,nrow=1,ncol=1)+
  tm_layout(panel.labels=paste("14-day Avg",q.val,"cfs"),fontfamily = "serif",bg.color="lightblue1")+
  tm_legend(title="Bottom Salinity\n(PSU)",legend.outside=T, legend.text.size=0.75,legend.title.size=1)+
  tm_shape(shoreline)+tm_polygons(col="cornsilk")+
  tm_shape(crop(roads.all,gBuffer(bbox.poly,width=200)))+tm_lines("grey")+
  tm_shape(subset(wmd.mon,STATION=="S79_TOT"))+tm_symbols(col="indianred1",size=0.75)+tm_text("SITE",shadow=T,ymod=-0.75,size=0.75)+
  tm_shape(subset(wq.mon.logged,STATION%in%c("S79_T", "VALI75", "FORTMYERSM", "CCORAL", "MARKH","SANIB2")))+tm_symbols(col="grey",size=0.25,alpha=0.5)+
  tm_credits("SFWMD Data\nDaily Avg Salinity\nMay 2009 - May 2020",fontface="bold",align="right")
# GAM.sal

tmap_animation(GAM.sal,filename="./Plots/Surf_bot_da_GAM.gif",delay=90,width=600,height=300,loop=TRUE)
