## 
## 
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
library(reshape2)
library(zoo)

library(lubridate)
# GIS libraries 
# library(rgdal)
# library(rgeos)
# library(raster)
# library(tmap)
# 
## Paths
wd="C:/Julian_LaCie/_Github/CRE_Conditions"

paths=paste0(wd,c("/Plots/","/Export/","/Data/","/src","/GIS"))
# Folder.Maker(paths);#One and done. Creates folders in working directory.
plot.path=paths[1]
export.path=paths[2]
data.path=paths[3]
GIS.path=paths[5]

# GIS.path.gen="C:/Julian_LaCie/_GISData"
# # Helper variables
# nad83.pro=CRS(SRS_string ="EPSG:4269")
# utm17=CRS(SRS_string ="EPSG:26917")
# 
# tmap_mode("view")
# # -------------------------------------------------------------------------
# ogrListLayers(paste(GIS.path.gen,"/SFER_GIS_Geodatabase.gdb",sep=""))
# ogrListLayers(paste(GIS.path.gen,"/AHED_release/AHED_20171102.gdb",sep=""))
# 
# structures=spTransform(readOGR(paste(GIS.path.gen,"/AHED_release/AHED_20171102.gdb",sep=""),"STRUCTURE"),wkt(utm17))
# 
# shoreline=spTransform(readOGR(paste0(GIS.path.gen,"/FWC"),"FWC_Shoreline"),wkt(utm17))
# shoreline=gSimplify(shoreline,100)
# canal=spTransform(readOGR(paste(GIS.path.gen,"/SFER_GIS_Geodatabase.gdb",sep=""),"SFWMD_Canals"),utm17)
# 
# roads.all=spTransform(readOGR(paste0(GIS.path.gen,"/FDOT"),"FDOT_Roads"),utm17)
# 
# wbids=spTransform(readOGR(paste0(GIS.path.gen,"/FDEP"),"WBIDs"),wkt(utm17))
# # tm_shape(wbids)+tm_polygons(alpha=0.5)
# 
# C44_wbid=subset(wbids,WATERBODY_=="C-44")
# C44_est_wbid=subset(wbids,WATERBODY_%in%c("C-44","ST LUCIE CANAL","ST LUCIE RIVER (SOUTH FORK)"))
# 
# wbids2=merge(wbids,
#              data.frame(PLANNING_U=c("West Caloosahatchee", 'East Caloosahatchee',"Caloosahatchee Estuary","Telegraph Swamp","Orange River"),Region=c("C43","C43","Tidal Basin","Tidal Basin","Tidal Basin")),
#              "PLANNING_U")
# plot(subset(wbids2,is.na(Region)==F))
# plot(subset(wbids,GROUP_NAME=="Caloosahatchee"))
# 
# wbids2.dis=gUnaryUnion(wbids2,id=wbids2@data$Region)
# IDlist <- data.frame(ID=sapply(slot(wbids2.dis, "polygons"), function(x) slot(x, "ID")))
# rownames(IDlist)  <- IDlist$ID
# wbids2.dis=SpatialPolygonsDataFrame(wbids2.dis,IDlist)
# 
# bbox.poly=as(raster::extent(gBuffer(wbids2.dis,width=2000)),"SpatialPolygons")#makes the polygon
# proj4string(bbox.poly)=utm17#projects the polygon
# # png(filename=paste0(plot.path,"CRE.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
# par(family="serif",oma=c(0.25,0.25,0.25,0.25),mar=c(0.1,0.1,0.1,0.1),xpd=F)
# bbox.lims=bbox(wbids2.dis)
# 
# plot(shoreline,col="cornsilk",border="grey",bg="lightblue",ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],lwd=0.01)
# plot(canal,add=T,col="lightblue",lwd=1)
# plot(crop(roads.all,gBuffer(bbox.poly,width=200)),col="grey",add=T)
# plot(subset(wbids2.dis,ID=="C43"),col=adjustcolor("grey",0.5),add=T)
# plot(subset(structures,NAME%in%c("S77","S78","S79")),pch=21,bg="dodgerblue1",cex=1.25,add=T)
# raster::text(subset(structures,NAME%in%c("S77","S78","S79")),
#              subset(structures,NAME%in%c("S77","S78","S79"))$NAME,halo=T,pos=3)
# mapmisc::scaleBar(utm17,"bottomright",bty="n",cex=1,seg.len=4)
# box(lwd=1)
# 
# 
# bbox.poly=as(raster::extent(gBuffer(C44_est_wbid,width=2000)),"SpatialPolygons")#makes the polygon
# proj4string(bbox.poly)=utm17#projects the polygon
# # png(filename=paste0(plot.path,"SLE.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
# par(family="serif",oma=c(0.25,0.25,0.25,0.25),mar=c(0.1,0.1,0.1,0.1),xpd=F)
# bbox.lims=bbox(C44_est_wbid)
# 
# plot(shoreline,col="cornsilk",border="grey",bg="lightblue",ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],lwd=0.01)
# plot(canal,add=T,col="lightblue",lwd=1)
# plot(crop(roads.all,gBuffer(bbox.poly,width=200)),col="grey",add=T)
# plot(C44_wbid,col=adjustcolor("grey",0.5),add=T)
# plot(subset(structures,NAME%in%c("S308","S80")),pch=21,bg="dodgerblue1",cex=1.25,add=T)
# raster::text(subset(structures,NAME%in%c("S308","S80")),
#              subset(structures,NAME%in%c("S308","S80"))$NAME,halo=T,pos=3)
# mapmisc::scaleBar(utm17,"bottomright",bty="n",cex=1,seg.len=4)
# box(lwd=1)


# -------------------------------------------------------------------------
dates=date.fun(c("1979-05-01","2021-04-30"))

## Caloosahatchee
CRE.site=data.frame(SITE=c("S79",rep("S77",2)),
                    DBKEY=c("00865","15635","DJ235"),
                    Priority=c("P1","P1","P2"))

CR.Q.dat=data.frame()
for(i in 1:nrow(CRE.site)){
  tmp=DBHYDRO_daily(dates[1],dates[2],CRE.site$DBKEY[i])
  tmp$DBKEY=as.character(CRE.site$DBKEY[i])
  CR.Q.dat=rbind(CR.Q.dat,tmp)
  print(i)
}
CR.Q.dat
CR.Q.dat=merge(CR.Q.dat,CRE.site,"DBKEY")
# CR.Q.dat=dcast(CR.Q.dat,SITE+Date~Priority,value.var="Data.Value",sum)
# CR.Q.dat$fflow.cfs=with(CR.Q.dat,ifelse(is.na(P1)==F|(P2=0&P1!=0),P1,P2))
# plot(fflow.cfs~Date,subset(CR.Q.dat,SITE=="S77"))
# plot(fflow.cfs~Date,subset(CR.Q.dat,SITE=="S79"))

CR.Q.dat.xtab=dcast(CR.Q.dat,Date~SITE,value.var = "Data.Value",mean)
CR.Q.dat.xtab$WY=WY(CR.Q.dat.xtab$Date)

CR.Q.dat.xtab$S77BF=with(CR.Q.dat.xtab,ifelse(S77<0,abs(S77),0))
CR.Q.dat.xtab$S77=with(CR.Q.dat.xtab,ifelse(S77<0,0,S77))
CR.Q.dat.xtab$S79=with(CR.Q.dat.xtab,ifelse(S79<0,0,S79))
CR.Q.dat.xtab$C43=with(CR.Q.dat.xtab,ifelse(S79>S77,S79-S77,0))

CR.Q.dat=melt(CR.Q.dat.xtab,id.vars=c('Date','WY'))
CRE.WY=ddply(CR.Q.dat,c("Date","WY","variable"),summarise,TFlow.acft=sum(cfs.to.acftd(value),na.rm=T))
CRE.WY$variable=factor(CRE.WY$variable,levels=c("S77BF","S77","C43","S79"))
CRE.WY.mean=ddply(CRE.WY,"variable",summarise,mean.val=mean(TFlow.acft,na.rm=T))
CRE.WY.mean=CRE.WY.mean[match(CRE.WY.mean$variable,c("S77BF","S77","C43","S79")),]

cols=wesanderson::wes_palette("Zissou1",4,"continuous")

ylim.val=c(0,15000);by.y=5000;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
par(family="serif",mar=c(2,2,0.5,0.75),oma=c(2,2,1,0.5));

boxplot(TFlow.acft~variable,CRE.WY,outline=F,ylim=ylim.val,col=cols,ann=F,axes=F)
axis_fun(1,1:4,1:4,c("S77\nBack Flow","S77","C43\nBasin","S79"),padj=1,line=-1)
axis_fun(2,ymaj,ymin,ymaj/1000);box(lwd=1)
mtext(side=2,line=2,"Discharge (x1000 Ac-Ft WY\u207B\u00B9)")
mtext(side=1,line=2.75,"Structure/Source")
mtext(side=3,adj=0,"Caloosahatchee River & Estuary")
mtext(side=3,adj=1,"FL WY 1979 - 2021")

ylim.val=c(0,5000);by.y=1000;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
par(family="serif",mar=c(2,2,0.5,0.75),oma=c(2,2,2,0.5));
x=barplot(CRE.WY.mean$mean.val,ylim=ylim.val,ann=F,axes=F,col=cols)
text(x,CRE.WY.mean$mean.val,format(round(CRE.WY.mean$mean.val,0),nsmall=0),pos=3,offset=0.25)
axis_fun(1,x,x,c("S77\nBack Flow","S77","C43\nBasin","S79"),padj=1,line=-1)
axis_fun(2,ymaj,ymin,format(ymaj/1000));box(lwd=1)
mtext(side=2,line=2.75,"Discharge (x1000 Ac-Ft WY\u207B\u00B9)")
mtext(side=1,line=2.75,"Structure/Source")
mtext(side=3,adj=0,"Caloosahatchee River & Estuary")
mtext(side=3,adj=1,"POR Mean\nFL WY 1979 - 2021")

#St Lucie
SL.site=data.frame(SITE=c(rep("S308",2),rep("S80",2)),
                   DBKEY=c("15626","06548","JW224","DJ238"),
                   Priority=rep(c("P1","P2"),2))


SL.Q.dat=data.frame()
for(i in 1:nrow(SL.site)){
tmp=DBHYDRO_daily(dates[1],dates[2],SL.site$DBKEY[i])
tmp$DBKEY=as.character(SL.site$DBKEY[i])
SL.Q.dat=rbind(SL.Q.dat,tmp)
print(i)
}
SL.Q.dat
SL.Q.dat=merge(SL.Q.dat,SL.site,"DBKEY")

SL.Q.dat.xtab=dcast(SL.Q.dat,SITE+Date~Priority,value.var="Data.Value",sum)
SL.Q.dat.xtab$fflow.cfs=with(SL.Q.dat.xtab,ifelse(is.na(P1)==F|(P2=0&P1!=0),P1,P2))
plot(fflow.cfs~Date,subset(SL.Q.dat.xtab,SITE=="S308"))

SL.Q.dat.xtab2=dcast(SL.Q.dat.xtab,Date~SITE,value.var = "fflow.cfs",mean)
SL.Q.dat.xtab2$WY=WY(SL.Q.dat.xtab2$Date)
SL.Q.dat.xtab2$S308BF=with(SL.Q.dat.xtab2,ifelse(S308<0,abs(S308),0))
SL.Q.dat.xtab2$S308=with(SL.Q.dat.xtab2,ifelse(S308<0,0,S308))
SL.Q.dat.xtab2$C44=with(SL.Q.dat.xtab2,ifelse(S80>S308,S80-S308,0))

SL.Q.dat=melt(SL.Q.dat.xtab2,id.vars=c('Date','WY'))
SLE.WY=ddply(SL.Q.dat,c("Date","WY","variable"),summarise,TFlow.acft=sum(cfs.to.acftd(value),na.rm=T))
SLE.WY$variable=factor(SLE.WY$variable,levels=c("S308BF","S308","C44","S80"))
SLE.WY.mean=ddply(SLE.WY,"variable",summarise,mean.val=mean(TFlow.acft,na.rm=T))
SLE.WY.mean=SLE.WY.mean[match(SLE.WY.mean$variable,c("S308BF","S308","C44","S80")),]


par(family="serif",mar=c(2,2,0.5,0.75),oma=c(2,2,1,0.5));

boxplot(TFlow.acft~variable,SLE.WY,outline=F,ylim=ylim.val,col=cols,ann=F,axes=F)
axis_fun(1,1:4,1:4,c("S308\nBack Flow","S308","C44\nBasin","S80"),padj=1,line=-1)
axis_fun(2,ymaj,ymin,format(ymaj/1000));box(lwd=1)
mtext(side=2,line=2.5,"Discharge (x1000 Ac-Ft WY\u207B\u00B9)")
mtext(side=1,line=2.75,"Structure/Source")
mtext(side=3,adj=0,"St Lucie River & Estuary")
mtext(side=3,adj=1,"FL WY 1979 - 2021")


ylim.val=c(0,1100);by.y=250;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
par(family="serif",mar=c(2,2,0.5,0.75),oma=c(2,2,2,0.5));
x=barplot(SLE.WY.mean$mean.val,ylim=ylim.val,ann=F,axes=F,col=cols)
text(x,SLE.WY.mean$mean.val,format(round(SLE.WY.mean$mean.val,0),nsmall=0),pos=3,offset=0.25)
axis_fun(1,x,x,c("S308\nBack Flow","S308","C44\nBasin","S80"),padj=1,line=-1)
axis_fun(2,ymaj,ymin,format(ymaj/1000));box(lwd=1)
mtext(side=2,line=2.75,"Discharge (x1000 Ac-Ft WY\u207B\u00B9)")
mtext(side=1,line=2.75,"Structure/Source")
mtext(side=3,adj=0,"St Lucie River & Estuary")
mtext(side=3,adj=1,"POR Mean\nFL WY 1979 - 2021")


# png(filename=paste0(plot.path,"Estuary_Discharge_WY.png"),width=7,height=5,units="in",res=200,type="windows",bg="white")
layout(matrix(1:2,1,2))
par(family="serif",mar=c(2,2,0.5,0.75),oma=c(4,2,1.5,0.5));

ylim.val=c(0,5000);by.y=1000;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=barplot(CRE.WY.mean$mean.val,ylim=ylim.val,ann=F,axes=F,col=cols)
text(x,CRE.WY.mean$mean.val,format(round(CRE.WY.mean$mean.val/1000,2),nsmall=2),pos=3,offset=0.25)
axis_fun(1,x,x,c("S77\nBack Flow","S77","C43\nBasin\nTo Estuary","S79"),padj=1,line=-1,cex=0.8)
axis_fun(2,ymaj,ymin,format(ymaj/1000));box(lwd=1)
mtext(side=2,line=2.5,"Discharge (x1000 Ac-Ft WY\u207B\u00B9)")
mtext(side=1,line=3.5,"Structure/Source")
mtext(side=3,adj=0,"Caloosahatchee River & Estuary")
# mtext(side=3,adj=1,"POR Mean\nFL WY 1979 - 2021")

ylim.val=c(0,1100);by.y=250;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=barplot(SLE.WY.mean$mean.val,ylim=ylim.val,ann=F,axes=F,col=cols)
text(x,SLE.WY.mean$mean.val,format(round(SLE.WY.mean$mean.val/1000,2),nsmall=2),pos=3,offset=0.25)
axis_fun(1,x,x,c("S308\nBack Flow","S308","C44\nBasin\nTo Estuary","S80"),padj=1,line=-1,cex=0.8)
axis_fun(2,ymaj,ymin,format(ymaj/1000));box(lwd=1)
# mtext(side=2,line=2.75,"Discharge (x1000 Ac-Ft WY\u207B\u00B9)")
mtext(side=1,line=3.5,"Structure/Source")
mtext(side=3,adj=1,"St Lucie River & Estuary")
mtext(side=1,outer=T,line=2.5,adj=1,"POR Mean FL WY 1979 - 2021",cex=0.75,col="grey40",font=3)
dev.off()