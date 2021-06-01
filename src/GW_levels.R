## 
## Lake Okeechobee plot
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

library(dataRetrieval)
## Paths
wd="C:/Julian_LaCie/_Github/CRE_Conditions"

paths=paste0(wd,c("/Plots/","/Export/","/Data/","/src"))
# Folder.Maker(paths);#One and done. Creates folders in working directory.
plot.path=paths[1]
export.path=paths[2]
data.path=paths[3]

# https://pubs.usgs.gov/wdr/2005/wdr-wa-05-1/pdf/wa00103ADR2005_BookK.pdf
# -------------------------------------------------------------------------
dates=date.fun(c("2020-05-01","2021-05-26"))

NorthCapeinfo=readNWISsite("264053081572501")
# -88 is tranducer elevation
# NorthCape=readNWISgwl("264053081572501",
#                  startDate=dates[1],
#                  endDate=dates[2])
NorthCape=readNWISuv("264053081572501",
                   startDate=dates[1],
                   endDate=dates[2],parameterCd=62610)
NorthCape=renameNWISColumns(NorthCape)
NorthCape$DateTime=date.fun(NorthCape$dateTime,form="%F %X")
NorthCape$Date=date.fun(NorthCape$dateTime)
NorthCape$FtBLS=NorthCapeinfo$alt_va-NorthCape$X_62610_Inst


plot(X_62610_Inst~dateTime,NorthCape)
NorthCape.da=ddply(NorthCape,"Date",summarise,mean.val=mean(FtBLS,na.rm=T),min.val=min(FtBLS,na.rm=T),max.val=max(FtBLS,na.rm=T))
plot(mean.val~Date,NorthCape.da,type="l")
with(NorthCape.da,shaded.range(Date,min.val,max.val,"dodgerblue1",lty=0))

ELehighinfo=readNWISsite("263344081361703")
# ELehigh=readNWISgwl("263344081361703",
#                       startDate=dates[1],
#                       endDate=dates[2])
ELehigh=readNWISuv("263344081361703",
                     startDate=dates[1],
                     endDate=dates[2],parameterCd=62610)
ELehigh=renameNWISColumns(ELehigh)
ELehigh$DateTime=date.fun(ELehigh$dateTime,form="%F %X")
ELehigh$Date=date.fun(ELehigh$dateTime)
ELehigh$FtBLS=ELehighinfo$alt_va-ELehigh$X_62610_Inst


ELehigh.da=ddply(ELehigh,"Date",summarise,mean.val=mean(FtBLS,na.rm=T),min.val=min(FtBLS,na.rm=T),max.val=max(FtBLS,na.rm=T))
ELehigh.da=merge(ELehigh.da,data.frame(Date=seq(dates[1],dates[2],"1 days")),"Date",all.y=T)

plot(mean.val~Date,ELehigh.da,type="l")
with(subset(ELehigh.da,Date%in%seq(dates[1],date.fun(min(subset(ELehigh.da,is.na(mean.val)==T)$Date-lubridate::ddays(1))),"1 days")),shaded.range(Date,min.val,max.val,"indianred1",lty=0))
with(subset(ELehigh.da,Date%in%seq(date.fun(max(subset(ELehigh.da,is.na(mean.val)==T)$Date+lubridate::ddays(1))),dates[2],"1 days")),shaded.range(Date,min.val,max.val,"indianred1",lty=0))


plot(X_62610_Inst~dateTime,ELehigh)
plot(X_62610_Inst~dateTime,ELehigh,xlim=xlim.val,ylim=c(-1,1))


# Map ---------------------------------------------------------------------
# GIS libraries 
library(rgdal)
library(rgeos)
library(raster)
library(tmap)
library(ceramic)

wgs84=CRS(SRS_string = "EPSG:4326")
utm17=CRS(SRS_string = "EPSG:26917")
## 
sites=rbind(NorthCapeinfo,ELehighinfo)
sites=sites[,c("site_no","dec_lat_va","dec_long_va")]

sites.shp=SpatialPointsDataFrame(sites[,c("dec_long_va","dec_lat_va")],data=sites,proj4string = wgs84)
sites.shp.wgs=sites.shp
sites.shp=spTransform(sites.shp,wkt(utm17))
plot(sites.shp)
plot(gBuffer(sites.shp,width=8000),add=T)


public.token="pk.eyJ1IjoicGp1bGlhbiIsImEiOiJjanllbmJ0eXkxMzV0M2dzNXh5NGRlYXdqIn0.g4weKGOt1WdNZLg2hxBz1w"
Sys.setenv(MAPBOX_API_KEY=public.token)

roi=extent(spTransform(gBuffer(sites.shp,width=10000),wgs84))
im <- cc_location(roi,zoom=12)
im=projectRaster(im,crs=wkt(utm17))
im=setValues(im,scales::rescale(values(im), c(0,255)))


plotRGB(im)
plot(sites.shp,add=T,pch=21,bg=c("dodgerblue1","indianred1"),cex=2)
text(sites.shp,"site_no",pos=3,halo=T,font=2,cex=0.75)


# -------------------------------------------------------------------------

# tiff(filename=paste0(plot.path,"GndWaterTrend.tiff"),width=6.5,height=5,units="in",res=200,type="windows",compression=c("lzw"),bg="white")
# png(filename=paste0(plot.path,"GndWaterTrend.png"),width=6.5,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(4,2,1,0.25),oma=c(1,3,0.5,1));
layout(matrix(c(1,2,3,3),2,2,byrow=T),heights=c(1,0.5))

xlim.val=date.fun(c("2021-05-22","2021-05-27"));xmaj=seq(xlim.val[1],xlim.val[2],"24 hours");xmin=seq(xlim.val[1],xlim.val[2],"6 hours")
# ylim.val=c(-88,-84);by.y=1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
# plot(X_62610_Inst~dateTime,NorthCape,xlim=xlim.val,ylim=ylim.val,ann=F,axes=F,type="n")
ylim.val=c(98,102);by.y=1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(FtBLS~dateTime,NorthCape,xlim=xlim.val,ylim=rev(ylim.val),ann=F,axes=F,type="n")
abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
# with(NorthCape,lines(DateTime,X_62610_Inst,col="dodgerblue1",lwd=2))
with(NorthCape,lines(DateTime,FtBLS,col="dodgerblue1",lwd=2))
axis_fun(1,xmaj,xmin,format(xmaj,"%m-%d"),line=-0.5)
# axis_fun(1,xmaj,xmin,format(xmaj,"%m-%d %H:%M"))
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=2.5,"Depth to water level\n(Ft below land surface)")
# mtext(side=2,line=2.5,"Stage Elevation (Ft, NGVD29)")
mtext(side=3,adj=0,"SITE: 264053081572501 (Cape Coral)",cex=0.75)

ylim.val=c(30,33);by.y=1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(FtBLS~dateTime,ELehigh,xlim=xlim.val,ylim=rev(ylim.val),ann=F,axes=F,type="n")
abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
with(ELehigh,lines(DateTime,FtBLS,col="Indianred1",lwd=2))
axis_fun(1,xmaj,xmin,format(xmaj,"%m-%d"),line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
# mtext(side=2,line=2.5,"Depth to water level\n(Ft below land surface)")
mtext(side=3,adj=0,"SITE: 263344081361703 (Lehigh Acres)",cex=0.75)

plotRGB(im)
plot(sites.shp,add=T,pch=21,bg=c("dodgerblue1","indianred1"),cex=2)
text(sites.shp,"site_no",pos=3,halo=T,font=2,cex=0.75)
mapmisc::scaleBar(utm17,"bottom",bty="n",cex=0.8,col="white")

mtext(side=3,line=1.5,"Date (Month-Day)")

dev.off()

# tiff(filename=paste0(plot.path,"GndWaterTrend_year.tiff"),width=6.5,height=3.5,units="in",res=200,type="windows",compression=c("lzw"),bg="white")
# png(filename=paste0(plot.path,"GndWaterTrend_year.png"),width=6.5,height=3.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2,2,1,0.25),oma=c(1,3,0.5,1));
layout(matrix(c(1,2),1,2,byrow=T))

xlim.val=date.fun(dates);xmaj=seq(xlim.val[1],xlim.val[2],"6 month");xmin=seq(xlim.val[1],xlim.val[2],"1 month")
ylim.val=c(85,100);by.y=5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(mean.val~Date,NorthCape.da,xlim=xlim.val,ylim=rev(ylim.val),ann=F,axes=F,type="n")
abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
with(NorthCape.da,shaded.range(Date,min.val,max.val,"dodgerblue1",lty=0,col.adj=0.75))
with(NorthCape.da,lines(Date,mean.val))
axis_fun(1,xmaj,xmin,format(xmaj,"%m-%Y"),line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=2.5,"Depth to water level\n(Ft below land surface)")
mtext(side=3,adj=0,"SITE: 264053081572501 (Cape Coral)",cex=0.75)

ylim.val=c(10,40);by.y=10;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(mean.val~Date,ELehigh.da,xlim=xlim.val,ylim=rev(ylim.val),ann=F,axes=F,type="n")
abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
with(subset(ELehigh.da,Date%in%seq(dates[1],date.fun(min(subset(ELehigh.da,is.na(mean.val)==T)$Date-lubridate::ddays(1))),"1 days")),shaded.range(Date,min.val,max.val,"indianred1",lty=0,col.adj=0.75))
with(subset(ELehigh.da,Date%in%seq(date.fun(max(subset(ELehigh.da,is.na(mean.val)==T)$Date+lubridate::ddays(1))),dates[2],"1 days")),shaded.range(Date,min.val,max.val,"indianred1",lty=0,col.adj=0.75))
with(ELehigh.da,lines(Date,mean.val))
axis_fun(1,xmaj,xmin,format(xmaj,"%m-%Y"),line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=3,adj=0,"SITE: 263344081361703 (Lehigh Acres)",cex=0.75)
mtext(side=1,line=-0.5,"Date (Month-Year)",outer=T)
dev.off()