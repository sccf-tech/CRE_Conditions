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

# GAM ---------------------------------------------------------------------
dat.surf.xtab=subset(dat.surf.xtab,SITE%in%c("S79_T", "VALI75", "FORTMYERSM", "CCORAL", "MARKH","SANIB1"))
dat.bot.xtab=subset(dat.surf.xtab,SITE%in%c("VALI75", "FORTMYERSM", "CCORAL", "MARKH"))

# Bottom ------------------------------------------------------------------
wq.q.dat=merge(dat.bot.xtab,S79.Q[,c("Date.EST","Data.Value","Q14d")],"Date.EST",all.x=T)
wq.q.dat$Station.ID=wq.q.dat$SITE
wq.q.dat$ff.sal=with(wq.q.dat,1-(Sal/max(Sal,na.rm=T)))

set.seed(1)
tr.index=sample(1:nrow(wq.q.dat),nrow(wq.q.dat)*0.7)

sites.shp=cbind(data.frame(Station.ID=subset(wq.mon.logged,STATION%in%sites.logged)@data$STATION),
                coordinates(subset(wq.mon.logged,STATION%in%sites.logged)))
colnames(sites.shp)=c("Station.ID","UTMX","UTMY")

wq.q.dat=merge(sites.shp,wq.q.dat,"Station.ID",all.y=T)

### 
set.seed(1)
coord.K=4
sal.k.val=seq(40,120,5)
k.check.int=data.frame()
for(i in 1:length(sal.k.val)){
  mod=bam(Sal~
            s(Data.Value,bs="cr",k=sal.k.val[i])+
            s(UTMX,UTMY,bs="ds",k=coord.K,m=c(1,0.5)),
          data=wq.q.dat[tr.index,])
  rslt=as.data.frame(k.check(mod))
  rslt$input.k=sal.k.val[i]
  rslt$params=rownames(rslt)
  k.check.int=rbind(k.check.int,rslt)
  print(i)
}
subset(k.check.int,params=="s(Data.Value)")
k.check.int=rename(k.check.int,c("k-index"="k.index"))
plot(k.index~input.k,subset(k.check.int,params=="s(Data.Value)"))


coord.K=4
sal.k=70
sal.m<-bam(Sal~
             s(Data.Value,bs="cr",k=sal.k)+
             s(UTMX,UTMY,bs="ds",k=coord.K,m=c(1,0.5)),
           data=wq.q.dat[tr.index,])
summary(sal.m)
nvar=2;layout(matrix(1:nvar,1,nvar))
plot(sal.m,residuals=T,pch=21)

nvar=4;layout(matrix(1:nvar,1,nvar))
gam.check(sal.m,pch=21)
draw(sal.m)

dev.off()
mod.pred=predict(sal.m,wq.q.dat[-tr.index,])
actuals_preds <-data.frame(cbind(actuals=wq.q.dat[-tr.index,"Sal"],predicted=mod.pred))
actuals_preds=na.omit(actuals_preds)
plot(actuals~predicted,actuals_preds);abline(0,1)
cor.test(actuals_preds$actuals,actuals_preds$predicted,method="spearman")

#min_max_accuracy
mean(apply(actuals_preds, 1,FUN=function(x) min(x,na.rm=T)) / apply(actuals_preds, 1, FUN=function(x) max(x,na.rm=T))) 
#Mean Absolute Percent Error
mean(abs((actuals_preds$predicted - actuals_preds$actuals))/actuals_preds$actuals,na.rm=T)
#Percent bias
with(actuals_preds,(sum((actuals-predicted),na.rm=T)/sum(actuals,na.rm=T)))

# Nash-Sutcliffe model efficiency coefficient
with(actuals_preds,1-(sum((actuals-predicted)^2,na.rm=T)/sum((actuals-mean(actuals))^2,na.rm=T)))

# Kling-Gupta Efficiency (2009)
r.val=with(na.omit(actuals_preds),cor(predicted,actuals,method="pearson"))
alpha.val=with(actuals_preds,sd(predicted,na.rm=T)/sd(actuals,na.rm=T))
beta.val=with(actuals_preds,mean(predicted,na.rm=T)/mean(actuals,na.rm=T))
KG.val=1-sqrt((r.val-1)^2 + (alpha.val-1)^2 + (beta.val-1)^2)
KG.val

###
dev.off()
testDispersion(sal.m)
sim.out=simulateResiduals(sal.m,plot=T)
# residuals(sim.out)
# residuals(sim.out,quantileFunction = qnorm, outlierValues = c(-7,7))
# testResiduals(sim.out)


cre.ex=raster::extent(gBuffer(subset(wq.mon,STATION%in%c("S79","ROOK471","ROOK477")),width=1000))
pdata<-expand.grid(Data.Value=seq(200,3000,100),
                   UTMX=seq(cre.ex[1],cre.ex[2],length.out=150),
                   UTMY=seq(cre.ex[3],cre.ex[4],length.out=150)
)
# plot(UTMY~UTMX,pdata)

fit <- predict(sal.m, pdata)
pred <- cbind(pdata, Fitted = fit)
pred$Fitted[pred$Fitted<0]<-0

ggplot(pred, aes(x = UTMX, y = UTMY)) +
  geom_raster(aes(fill = Fitted)) + facet_wrap(~ Data.Value, ncol = 12) +
  scale_fill_viridis(name = "Sal", option = 'plasma', na.value = 'transparent') +
  coord_quickmap() +
  theme(legend.position = 'right')

q.val=seq(200,3000,100)
for(i in 1:length(q.val)){
  tmp=subset(pred,Data.Value==q.val[i])[,c("Fitted","UTMX","UTMY")]
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
            labels=c("0","<5","5 - 10","10 - 15", "15 - 20","20 - 25","25 - 30","30 - 35","35 - 40"))+
  tm_facets(free.scales=FALSE,nrow=1,ncol=1)+
  tm_layout(panel.labels=paste("S-79 Discharge:",q.val,"cfs"),fontfamily = "serif",bg.color="lightblue1")+
  tm_legend(title="Bottom Salinity\n(PSU)",legend.outside=T, legend.text.size=0.75,legend.title.size=1)+
  tm_shape(shoreline)+tm_polygons(col="cornsilk")+
  tm_shape(crop(roads.all,gBuffer(bbox.poly,width=200)))+tm_lines("grey")+
  tm_shape(subset(wmd.mon,STATION=="S79_TOT"))+tm_symbols(col="indianred1",size=0.75)+tm_text("SITE",shadow=T,ymod=-0.75,size=0.75)+
  #tm_shape(subset(wq.mon.logged,STATION%in%c("S79_T", "VALI75", "FORTMYERSM", "CCORAL", "MARKH","SANIB2")))+tm_symbols(col="grey",size=0.25,alpha=0.5)+
  tm_credits("SFWMD Data\nDaily Avg Salinity\nMay 2009 - May 2020",fontface="bold",align="right")
# GAM.sal

tmap_animation(GAM.sal,filename="./Plots/Surf_bot_da_GAM.gif",delay=90,width=600,height=300,loop=TRUE)


wq.q.dat2=wq.q.dat[,c("Station.ID","UTMX","UTMY","Date.EST","Sal","Data.Value")]
wq.q.dat2=cbind(wq.q.dat2,data.frame(pred.fit=predict(sal.m,wq.q.dat2)))
wq.q.dat2=wq.q.dat2[order(wq.q.dat2$Station.ID,wq.q.dat2$Date.EST),]

plot(Sal~Date.EST,subset(wq.q.dat2,Station.ID=="CCORAL"))
with(subset(wq.q.dat2,Station.ID=="CCORAL"),lines(Date.EST,pred.fit,col="Red"))

# Bottom FreshFrac --------------------------------------------------------
coord.K=4
ff.k=50
ff.m<-bam(ff.sal~
             s(Data.Value,k=ff.k)+
             s(UTMX,UTMY,bs="ds",k=coord.K,m=c(1,0.5)),
           data=wq.q.dat[tr.index,])
summary(ff.m)
nvar=2;layout(matrix(1:nvar,1,nvar))
plot(ff.m,residuals=T,pch=21)

nvar=4;layout(matrix(1:nvar,1,nvar))
gam.check(ff.m,pch=21)
draw(ff.m)

###
dev.off()
testDispersion(ff.m)
sim.out=simulateResiduals(ff.m,plot=T)
# residuals(sim.out)
# residuals(sim.out,quantileFunction = qnorm, outlierValues = c(-7,7))
# testResiduals(sim.out)


cre.ex=raster::extent(gBuffer(subset(wq.mon,STATION%in%c("S79","ROOK471","ROOK477")),width=1000))
pdata<-expand.grid(Data.Value=seq(200,3000,100),
                   UTMX=seq(cre.ex[1],cre.ex[2],length.out=150),
                   UTMY=seq(cre.ex[3],cre.ex[4],length.out=150)
)
# plot(UTMY~UTMX,pdata)

fit <- predict(ff.m, pdata)
pred <- cbind(pdata, Fitted = fit)

ggplot(pred, aes(x = UTMX, y = UTMY)) +
  geom_raster(aes(fill = Fitted)) + facet_wrap(~ Q14d, ncol = 12) +
  scale_fill_viridis(name = "Sal", option = 'plasma', na.value = 'transparent') +
  coord_quickmap() +
  theme(legend.position = 'right')

q.val=seq(200,3000,100)
for(i in 1:length(q.val)){
  tmp=subset(pred,Data.Value==q.val[i])[,c("Fitted","UTMX","UTMY")]
  coordinates(tmp)<-~UTMX + UTMY
  gridded(tmp)<-TRUE
  tmp=as(tmp,"RasterLayer")
  proj4string(tmp)<-utm17
  tmp.m=raster::mask(tmp,cre.nnc.segs.dis)
  assign(paste0("Q14d.",q.val[i]),tmp.m)
  print(i)
}

GAM.ff.stack=raster::stack(Q14d.200,
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

brks=seq(0,1.3,length.out=6)
GAM.sal=tm_shape(GAM.ff.stack,bbox=bbox.lims)+
  tm_raster(title="",palette="viridis",
            breaks=brks,style = "cont",
            labels=c("0.0 (Marine)",rep(" ",4),"1.0 (Freshwater)"))+
  tm_facets(free.scales=FALSE,nrow=1,ncol=1)+
  tm_layout(panel.labels=paste("14-day Avg",q.val,"cfs"),fontfamily = "serif",bg.color="lightblue1")+
  tm_legend(title="Freshwater Fraction",legend.outside=T, legend.text.size=0.75,legend.title.size=1)+
  tm_shape(shoreline)+tm_polygons(col="cornsilk")+
  tm_shape(crop(roads.all,gBuffer(bbox.poly,width=200)))+tm_lines("grey")+
  tm_shape(subset(wmd.mon,STATION=="S79_TOT"))+tm_symbols(col="indianred1",size=0.75)+tm_text("SITE",shadow=T,ymod=-0.75,size=0.75)+
  #tm_shape(subset(wq.mon.logged,STATION%in%c("S79_T", "VALI75", "FORTMYERSM", "CCORAL", "MARKH","SANIB2")))+tm_symbols(col="grey",size=0.25,alpha=0.5)+
  tm_credits("SFWMD Data\nDaily Avg Salinity\nMay 2009 - May 2020",fontface="bold",align="right")
# GAM.sal

tmap_animation(GAM.sal,filename="./Plots/Surf_bot_da_ff_GAM.gif",delay=90,width=600,height=300,loop=TRUE)

