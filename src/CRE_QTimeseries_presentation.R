## 
## S79 Discharge
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

library(lubridate)


#GIS Libraries
library(sp)
library(rgdal)
library(PROJ)
library(rgeos)
library(tmap)
library(raster)


## Paths
wd="C:/Julian_LaCie/_Github/CRE_Conditions"

paths=paste0(wd,c("/Plots/","/Export/","/Data/","/src","/GIS"))
# Folder.Maker(paths);#One and done. Creates folders in working directory.
plot.path=paths[1]
export.path=paths[2]
data.path=paths[3]
GIS.path=paths[5]

gen.GIS="C:/Julian_LaCie/_GISData"
db.path=paste(gen.GIS,"/SFER_GIS_Geodatabase.gdb",sep=""); 

wgs84=CRS("+init=epsg:4326")
utm17=CRS("+init=epsg:26917")

## Functions
consec.startend=function(var){
  runs=rle(var)
  myruns = which(runs$values == TRUE)
  runs.lengths.cumsum = cumsum(runs$lengths)
  ends = runs.lengths.cumsum[myruns]
  newindex = ifelse(myruns>1, myruns-1, 0)
  starts = runs.lengths.cumsum[newindex] + 1
  if (0 %in% newindex) starts = c(1,starts)
  rslt=list(starts=starts,ends=ends)
  return(rslt)
}

# -------------------------------------------------------------------------
dates=date.fun(c("2005-01-01","2021-12-31"))

cal.dbkeys=data.frame(DBKEY=c("DJ237","00865"),priority=c("P1","P2"))
cal.q=DBHYDRO_daily(dates[1],dates[2],cal.dbkeys$DBKEY)
cal.q$Date=date.fun(cal.q$Date)
cal.q=merge(cal.q,cal.dbkeys,"DBKEY")
cal.q=cast(cal.q,Date~priority,value="Data.Value",mean)
cal.q$Data.Value=with(cal.q,ifelse(is.na(P1)==T,P2,P1))
cal.q$Q.30=with(cal.q,c(rep(NA,29),rollapply(Data.Value,width=30,FUN=function(x)mean(x,na.rm=T))))
cal.q$Q.14=with(cal.q,c(rep(NA,13),rollapply(Data.Value,width=14,FUN=function(x)mean(x,na.rm=T))))

q.dat=cal.q
q.dat$Data.Value.fill=with(q.dat,ifelse(is.na(Data.Value),0,Data.Value))
xlim.val=date.fun(c("2005-01-01","2021-01-01"));xmaj=seq(xlim.val[1],xlim.val[2],"3 years");xmin=seq(xlim.val[1],xlim.val[2],"1 years")
ylim.val=c(0,25000);by.y=5000;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
# png(filename=paste0(plot.path,"CRE_S79Q.png"),width=7,height=3.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,3,0.5,0.5),oma=c(2,1,0.5,1),lwd=0.1);

plot(Data.Value~Date,q.dat,xlim=xlim.val,ylim=ylim.val,ann=F,axes=F,type="n",yaxs="i")
abline(h=ymin,v=xmin,lty=2,col=adjustcolor("grey",0.5))
abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
# with(q.dat,shaded.range(Date,rep(0,length(Date)),Data.Value.fill,bg="grey",lty=1))
with(q.dat,lines(Date,Data.Value,col=adjustcolor("dodgerblue1",0.75),lwd=1.25))
axis_fun(1,xmaj,xmin,format(xmaj,"%Y"),line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=1,line=1.5,"Date (Year)")
mtext(side=2,line=3,"Discharge (ft\u207B\u00B3 s\u207B\u00B9)")
mtext(side=3,adj=0,"S-79 Daily Discharges")
dev.off()


xlim.val=date.fun(c("2017-05-01","2019-01-01"));xmaj=seq(xlim.val[1],xlim.val[2],"1 years");xmin=seq(xlim.val[1],xlim.val[2],"1 months")
ylim.val=c(0,30000);by.y=5000;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
# png(filename=paste0(plot.path,"CRE_S79Q.png"),width=7,height=3.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,3,0.5,0.5),oma=c(2,1,0.5,1),lwd=0.1);

plot(Data.Value~Date,q.dat,xlim=xlim.val,ylim=ylim.val,ann=F,axes=F,type="n",yaxs="i")
abline(h=ymin,v=xmin,lty=2,col=adjustcolor("grey",0.5))
abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
# with(q.dat,shaded.range(Date,rep(0,length(Date)),Data.Value.fill,bg="grey",lty=1))
with(q.dat,lines(Date,Data.Value,col=adjustcolor("dodgerblue1",0.75),lwd=1.25))
axis_fun(1,xmaj,xmin,format(xmaj,"%Y"),line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=1,line=1.5,"Date (Year)")
mtext(side=2,line=3,"Discharge (ft\u207B\u00B3 s\u207B\u00B9)")
mtext(side=3,adj=0,"S-79 Daily Discharges")



# Drone
xlim.val=date.fun(c("2017-05-01","2019-02-01"));xmaj=seq(xlim.val[1],xlim.val[2],"1 years");xmin=seq(xlim.val[1],xlim.val[2],"1 months")
ylim.val=c(0,30000);by.y=30000/3;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

date.vals=date.fun(c("2018-06-03","2018-07-17","2018-08-15","2018-09-27","2018-10-31","2018-11-21"))
for(i in 1:length(date.vals)){
png(filename=paste0(plot.path,"drone/CRE_S79Q_",format(date.vals[i],"%Y%m%d"),".png"),width=4,height=2.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2.5,0.5,0.5),oma=c(1,1,0.5,1),lwd=0.1);

plot(Data.Value~Date,q.dat,xlim=xlim.val,ylim=ylim.val,ann=F,axes=F,type="n",yaxs="i")
abline(h=ymin,v=xmin,lty=2,col=adjustcolor("grey",0.5))
abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
abline(h=88000)
with(subset(q.dat,Date<=date.vals[i]),shaded.range(Date,rep(0,length(Date)),Data.Value.fill,bg="grey",lty=1))
with(subset(q.dat,Date<=date.vals[i]),lines(Date,Q.14,col=adjustcolor("red",0.75),lwd=1.25))
with(subset(q.dat,Date==date.vals[i]),points(Date,max(c(Q.14,Data.Value.fill)),pch=21,bg="green"))
with(subset(q.dat,Date==date.vals[i]),text(Date,max(c(Q.14,Data.Value.fill)),format(Date,"%b-%d"),pos=3,cex=0.75))
# with(q.dat,lines(Date,Data.Value,col=adjustcolor("dodgerblue1",0.75),lwd=1.25))

axis_fun(1,xmaj,xmin,format(xmaj,"%b-%Y"),line=-0.75,cex=0.75)
axis_fun(2,ymaj,ymin,ymaj,cex=0.75);box(lwd=1)
mtext(side=1,line=1,"Date",cex=0.75)
mtext(side=2,line=2.5,"Discharge (ft\u207B\u00B3 s\u207B\u00B9)",cex=0.75)
mtext(side=3,adj=0,"S-79 Daily Discharges",cex=0.75)
dev.off()
}



# HABSOS ------------------------------------------------------------------
hab.sos=read.csv(paste0(data.path,"HABSOS/habsos_20210413.csv"))
hab.sos$CELLCOUNT=as.numeric(hab.sos$CELLCOUNT)

spl=strsplit(hab.sos$SAMPLE_DATE,split="-")
date_split=data.frame(SAMPLE_DATE=hab.sos$SAMPLE_DATE,
                      day=sapply(spl,"[",1),month=sapply(spl,"[",2),yr_time=sapply(spl,"[",3))
date_split2=strsplit(date_split$yr_time,split=" ")
date_split=cbind(date_split[,1:3],data.frame(yr=sapply(date_split2,"[",1),time=sapply(date_split2,"[",2),AMPM=sapply(date_split2,"[",3)))
date_split$yr=with(date_split,ifelse(yr%in%seq(53,99,1),paste0("19",yr),paste0("20",yr)))
date_split$SAMPLE_DATE2=with(date_split,paste(paste(day,month,yr,sep="-"),time,AMPM))
date_split$datetime=date.fun(date_split$SAMPLE_DATE2,form="%d-%b-%Y %I.%M.%OS %p")
hab.sos=merge(hab.sos,date_split[,c("SAMPLE_DATE","datetime")],"SAMPLE_DATE")
hab.sos$date=date.fun(hab.sos$datetime)

range(hab.sos$CELLCOUNT)
bks=c(0,1000,10000,100000,1000000,max(hab.sos$CELLCOUNT,na.rm=T)*1.2)

hab.sos$cat=as.factor(findInterval(hab.sos$CELLCOUNT,bks,rightmost.closed = T,left.open = T))
ddply(hab.sos,"cat",summarise,min.val=min(CELLCOUNT,na.rm=T),max.val=format(max(CELLCOUNT,na.rm=T),scientific=F))

cat.abund.xwalk=data.frame(cat=1:5,
                           category=c("not.observed","very.low","low","medium","high"),
                           abund=c("not present/background (0-1,000)", "very low (>1,000-10,000)",
                                   "low (>10,000-100,000)", "medium (>100,000-1,000,000)", 
                                   "high (>1,000,000)"))
hab.sos=merge(hab.sos,cat.abund.xwalk,"cat",all.x=T)
hab.sos$category=as.factor(hab.sos$category)

hab.sos.shp=spTransform(SpatialPointsDataFrame(hab.sos[,c("LONGITUDE","LATITUDE")],
                                   data=hab.sos,proj4string =wgs84),utm17)

subset(hab.sos.shp,date%in%seq(date.fun(date.vals[1]-duration(21,"days")),date.vals[1],"days"))

library(USAboundaries)
states.shp=us_boundaries(resolution ="low")
states.shp=as(states.shp,"Spatial")
states.shp=spTransform(states.shp,utm17)
attributes(states.shp)
SW.US=c("Florida","Georgia","Alabama","Mississippi","Louisiana")#,"South Carolina")
FL.shp=us_boundaries(resolution ="high",states=SW.US)
FL.shp=as(FL.shp,"Spatial")
FL.shp=spTransform(FL.shp,utm17)
attributes(FL.shp)

# library(wikilake)
# library(nhdR)
# ok.meta=wikilake::lake_wiki("Lake Okeechobee")
# ok=nhd_plus_query(ok.meta$Lon,ok.meta$Lat,
#                   dsn = c("NHDWaterbody", "NHDFlowLine"),buffer_dist=units::as_units(5, "km"))
# 
# plot(ok$sp$NHDWaterbody$geometry, col = "blue")
# plot(ok$sp$NHDFlowLine$geometry, col = "blue")

CRE.ext=extent(-82.3,
               -81.8,
               26.3,
               26.7)
CRE.ext=as(CRE.ext,"SpatialPolygons")
proj4string(CRE.ext)=wgs84
CRE.ext=SpatialPolygonsDataFrame(CRE.ext,data.frame(ID="CRE"))
CRE.ext=spTransform(CRE.ext,utm17)

cols=c("grey60","white","yellow","orange","red")
for(i in 1:length(date.vals)){
  png(filename=paste0(plot.path,"drone/KBrevisMap_",format(date.vals[i],"%Y%m%d"),".png"),width=4,height=2.5,units="in",res=200,type="windows",bg="white")
  
par(family="serif",oma=c(0.1,0.1,0.1,0.1),mar=c(0,0,0,0),xpd=F)
bbox.lims=bbox(subset(FL.shp,state_abbr=="FL"))
plot(FL.shp,col="cornsilk",border="grey",bg="lightblue",ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],lwd=0.01)
tmp=subset(hab.sos.shp,date%in%seq(date.fun(date.vals[i]-duration(21,"days")),date.vals[i],"days"))
plot(tmp,add=T,pch=21,bg=adjustcolor(cols[tmp$cat],0.5),lwd=0.1)
plot(CRE.ext,add=T,lty=2,border="grey")
legend("bottomleft",legend=c(cat.abund.xwalk$abund),
       pch=c(21),lty=c(NA),lwd=c(0.1),
       col=c("black"),pt.bg=c(cols),
       pt.cex=1,ncol=1,cex=0.75,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=1,
       title.adj = 0,title=expression(italic(" Karenia brevis")*" (cells/liter)"))
mapmisc::scaleBar(utm17,"bottomright",bty="n",cex=0.75,seg.len=2)
mtext(side=3,adj=1,line=-1,cex=0.75,at=par("usr")[2]-0.01*diff(par("usr")[1:2]),
      paste(format(date.fun(date.vals[i]-duration(21,"days")),"%b %d"),format(date.vals[i],"%b %d,%Y"),sep=" - "))
dev.off()
}

for(i in 1:length(date.vals)){
png(filename=paste0(plot.path,"drone/KBrevisMap_pie_",format(date.vals[i],"%Y%m%d"),".png"),
    width=2,height=1.5,units="in",res=200,type="windows",bg="white")

par(family="serif",oma=c(0.1,0.1,0.1,0.1),mar=c(0,0,0,0),xpd=F,lwd=0.1)
tmp=subset(hab.sos.shp,date%in%seq(date.fun(date.vals[i]-duration(21,"days")),date.vals[i],"days"))
tmp=raster::intersect(tmp,CRE.ext)
test=ddply(tmp@data,"cat",summarise,N.val=sum(CELLCOUNT>0))
labs=c("Background", "Very Low","Low","Medium","High")
pie(test$N.val,labels=labs,col=cols,cex=0.5)
mtext(side=3,adj=1,line=-0.75,cex=0.5,at=par("usr")[2]-0.01*diff(par("usr")[1:2]),"Caloosahatchee Estuary")
dev.off()
}

layout(matrix(1:2,1,2))
i=1
par(family="serif",oma=c(0.1,0.1,0.1,0.1),mar=c(0,0,0,0),xpd=F)
bbox.lims=bbox(CRE.ext)
plot(FL.shp,col="cornsilk",border="grey",bg="lightblue",ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],lwd=0.01)
tmp=subset(hab.sos.shp,date%in%seq(date.fun(date.vals[i]-duration(21,"days")),date.vals[i],"days"))
# tmp=tmp[CRE.ext,]
tmp=raster::intersect(tmp,CRE.ext)
plot(tmp,add=T,pch=21,bg=adjustcolor(cols[tmp$cat],0.5),lwd=0.1)
plot(CRE.ext,add=T,lty=2,border="grey")

test=ddply(tmp@data,"cat",summarise,N.val=sum(CELLCOUNT>0))
labs=c("Background", "Very Low","Low","Medium","High")
pie(test$N.val,labels=labs,col=cols,cex=0.5)
