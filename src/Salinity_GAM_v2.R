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
nad83.pro=CRS("+init=epsg:4269")
utm17=CRS("+init=epsg:26917")#  CRS(SRS_string ="EPSG:26917")

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
plot(gBuffer(cre.nnc.segs.dis,width=200))

cre.nnc.segs.dis=gBuffer(cre.nnc.segs.dis,width=400)
plot(cre.nnc.segs.dis)
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
pdata<-expand.grid(Data.Value=c(seq(200,3000,100),seq(4000,7000,500)),
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

q.val=c(seq(200,3000,100),seq(4000,7000,500))
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
                            Q14d.3000,
                            Q14d.4000,
                            Q14d.4500,
                            Q14d.5000,
                            Q14d.5500,
                            Q14d.6000,
                            Q14d.6500,
                            Q14d.7000)

# library(RColorBrewer)
# png(filename=paste0(plot.path,"sal_grad.png"),width=2,height=1.5,units="in",res=200,type="windows",bg=NA)
par(family="serif",mar=c(0.1,0.1,0.1,0.1),oma=c(0,0,0,0))
bbox.lims=bbox(gBuffer(cre.nnc.segs.dis,width=-1000))
plot(Q14d.200,xlim=bbox.lims[1,],ylim=bbox.lims[2,],axes=F,box=F,legend=F,col=brewer.pal(9,"YlGnBu"))
dev.off()

tmap_mode("plot")
bbox.lims=bbox(gBuffer(cre.nnc.segs.dis,width=2000))

bbox.poly=as(raster::extent(gBuffer(cre.nnc.segs.dis,width=2000)),"SpatialPolygons")#makes the polygon
proj4string(bbox.poly)=utm17#projects the polygon
# plot(crop(roads.all,gBuffer(bbox.poly,width=200)))

tm_shape(Q14d.600,bbox=bbox.lims)+
  tm_raster(title="",palette="Blues",
            breaks=c(-5,5,10,15,20,25,30,35,40),
            labels=c("0","<5","5 - 10","10 - 15", "15 - 20","20 - 25","25 - 30","30 - 35","35 - 40"))

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


#new animation


bbox.lims=bbox(gBuffer(subset(est_nnc_seg,SEGMENT_NA%in%segs),width=2000))
# bbox.lims=bbox(gBuffer(cre.nnc.segs.dis,width=2000))
bbox.poly=as(raster::extent(gBuffer(subset(est_nnc_seg,SEGMENT_NA%in%segs),width=6000)),"SpatialPolygons")#makes the polygon
proj4string(bbox.poly)=utm17#projects the polygon
b=seq(0,30,5)
pal=rev(viridis::cividis(length(b)-1,alpha = 1))
pal=adjustcolor(MetBrewer::met.brewer("Isfahan1",length(b)-1,type="continuous"),0.75)
con.labcex=0.4
con.lwd=0.3

roads.clip=crop(roads.all,gBuffer(bbox.poly,width=5000))

for(i in 1:length(q.val)){
png(filename=paste0(plot.path,"GAM_SalMod/CRE_SalMod_",i,".png"),width=7,height=3,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(0.1,0.5,0.5,0.1),oma=c(0.1,0.1,1,0.1))
layout(matrix(1:3,1,3),widths=c(1,0.25,0.3))

plot(shoreline,ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],lty=0,bg="lightblue")
image(GAM.sal.stack[[i]],add=T,breaks=b,col=pal)
plot(shoreline,lwd=0.05,col="cornsilk",add=T)
plot(roads.clip,col="grey",add=T)
box(lwd=1)
mtext(side=3,paste0("S79 14-d Avg: ",q.val[i]," cfs"),adj=0,font=2)
mapmisc::scaleBar(utm17,"bottomleft",bty="n",cex=1,seg.len=4,outer=F)

plot(0:1,0:1,ann=F,axes=F,type="n")
l.b=length(b)
labs=c(paste0("< ",b[2]),paste(b[2:(l.b-2)],b[3:(l.b-1)],sep=" - "),paste(paste0(">",b[(l.b-1)])))
n.bks=length(b)-1
top.val=0.8
bot.val=0.2
mid.v.val=0.3# bot.val+(top.val-bot.val)/2
x.max=0.3
x.min=0.1
mid.val=x.min+(x.max-x.min)/2
txt.offset.val=-0.01
bx.val= seq(bot.val,top.val,(top.val-bot.val)/n.bks)
rect(x.min,bx.val[1:n.bks],x.max,bx.val[2:(n.bks+1)],col=rev(pal),lty=0)
text(y=bx.val[2:(n.bks+1)]-c(mean(diff(bx.val[2:(n.bks+1)]))/2), x = x.max, labels = rev(labs),cex=0.75,xpd=NA,pos=4,adj=0)
text(x=mid.val,y=top.val,"Bottom\nSalinity",adj=0,cex=1,pos=3,xpd=NA)

plot(0:1,0:1,type="n",axes=F,ann=F)
polygon(x=c(0.2,0.2,0.8,0.8),
        y=c(0.1,0.9,0.9,0.1),col="grey20")
bks=c(0,457,750,2100,2600,30000)
cols=c("red","yellow","green","yellow","red")
findInterval(q.val[i],bks,left.open = FALSE,rightmost.closed = TRUE)
cond.col=cols[findInterval(q.val[i],bks,left.open = FALSE,rightmost.closed = TRUE)]
text(x=0.5,y=0.9,"Estuary\nSalinity Envelope",adj=0,cex=1.25,pos=3,xpd=NA)

points(0.5,0.75,pch=21,bg=ifelse(cond.col=="red",cond.col,"grey"),cex=12)
points(0.5,0.5,pch=21,bg=ifelse(cond.col=="yellow",cond.col,"grey"),cex=12)
points(0.5,0.25,pch=21,bg=ifelse(cond.col=="green",cond.col,"grey"),cex=12)
dev.off()
print(i)
}
# prepare "circle data"
# radius = 0.1
# center_x = 0.5
# center_y = 0.8
# theta = seq(0, 2 * pi, length = 200) # angles for drawing points around the circle
# polygon(x = radius * cos(theta) + center_x,
#         y = radius * sin(theta) + center_y,
#         col=ifelse(cond.col=="red",cond.col,"grey"))
# center_y = 0.5
# theta = seq(0, 2 * pi, length = 200) # angles for drawing points around the circle
# polygon(x = radius * cos(theta) + center_x,
#         y = radius * sin(theta) + center_y,
#         col=ifelse(cond.col=="yellow",cond.col,"grey"))
# center_y = 0.2
# theta = seq(0, 2 * pi, length = 200) # angles for drawing points around the circle
# polygon(x = radius * cos(theta) + center_x,
#         y = radius * sin(theta) + center_y,
#         col=ifelse(cond.col=="green",cond.col,"grey"))
# dev.off()

files <- list.files(path = paste0(plot.path,"GAM_SalMod"))
files=files[match(paste0("CRE_SalMod_",1:length(q.val),".png"),files)]
files=c(files,rev(files))
k <- length(files)
# PPI*inches
# 200 * 7.5
gifski::gifski(file.path(paste0(plot.path,"GAM_SalMod"), files), delay = 50/100, 
               width=1400,height=600, gif_file = paste0(plot.path,"GAM_SalMod/Cre_SalMod.gif"), 
               progress = T, loop = T)



q.val2=q.val[q.val<2800]
for(i in 1:length(q.val2)){
png(filename=paste0(plot.path,"tmp/resiliance_",i,".png"),width=3,height=1,units="in",res=200,type="windows",bg="white")
par(mar=c(0.1,0.1,0.1,0.1),oma=c(0,0,0,0))

x.val=seq(200,2800,10)
mu=1425
sigma=400
y.val=dnorm(x.val,mean=mu,sd=sigma)

plot(x.val,y.val*-1,type="n",xaxs="i",xlim=c(100,2900),ann=F,axes=F)
# polygon(c(0,x.val),
#        c(0,-1*y.val),
#        col = "NA")
cord.x <- c(0,seq(0,457,1),457) 
cord.y <- c(0,dnorm(seq(0,457,1),mu,sigma),0)*-1 
polygon(cord.x,cord.y,col='red',lwd=0.75)
cord.x <- c(457,seq(457,750,1),750) 
cord.y <- c(0,dnorm(seq(457,750,1),mu,sigma),0)*-1 
polygon(cord.x,cord.y,col='yellow',lwd=0.75)
cord.x <- c(750,seq(750,2100,1),2100) 
cord.y <- c(0,dnorm(seq(750,2100,1),mu,sigma),0)*-1
polygon(cord.x,cord.y,col='green',lwd=0.75)
cord.x <- c(2100,seq(2100,2600,1),2600) 
cord.y <- c(0,dnorm(seq(2100,2600,1),mu,sigma),0)*-1
polygon(cord.x,cord.y,col='yellow',lwd=0.75)
cord.x <- c(2600,seq(2600,2800,1),2800) 
cord.y <- c(0,dnorm(seq(2600,2800,1),mu,sigma),0)*-1 
polygon(cord.x,cord.y,col='red',lwd=0.75)

q.val[i]
xpt=if(q.val[i]>2800){2800}else{q.val[i]}

points(xpt,
       dnorm(xpt,mu,sigma)*-1,
       pch=21,bg="grey",cex=1.25,lwd=0.1)
dev.off()
}

files <- list.files(path = paste0(plot.path,"tmp"))
files=files[match(paste0("resiliance_",1:length(q.val2),".png"),files)]
files=c(files,rev(files))
k <- length(files)
# PPI*inches
gifski::gifski(file.path(paste0(plot.path,"tmp"), files), delay = 20/100, 
               width = 600, height = 200, gif_file = paste0(plot.path,"tmp/_resiliance.gif"), 
               progress = T, loop = T)

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

