## 
## Garfield Bight data
##
## Code was compiled by Paul Julian
## contact info: pauljulianphd@gmail.com

## BAD 
## https://www.tidyverse.org/articles/2017/12/workflow-vs-script/
## Clears Everything...start fresh.
rm(list=ls(all=T));cat("\014");dev.off()

## Libraries
#devtools::install_github("SwampThingPaul/AnalystHelper")
library(AnalystHelper);
library(plyr)
library(zoo)
library(reshape2)

## Paths
wd="C:/Julian_LaCie/_GitHub/CRE_Conditions"

paths=paste0(wd,c("/Plots/","/Export/","/data/CyanoHAB_RS","/GIS"))
# Folder.Maker(paths);#One and done. Creates folders in working directory.
plot.path=paths[1]
export.path=paths[2]
data.path=paths[3]
GIS.path=paths[4]
GIS.path.gen="C:/Julian_LaCie/_GISData"




# -------------------------------------------------------------------------
# NOAA data

dat1=read.table("https://www.ndbc.noaa.gov/data/realtime2/GBTF1.ocean",na.strings = "MM")
colnames(dat1)=c("YY","MM","DD","hh","mm","DEPTH","OTMP","COND","SAL","O2per","O2Conc",'CLCON','Turb',"PH","EH")

head(dat1)

dat1$date=with(dat1,date.fun(paste(YY,MM,DD,sep="-")))
dat1$datetime=with(dat1,date.fun(paste(paste(YY,MM,DD,sep="-"),paste(hh,mm,sep=":")),form="%F %R",tz="GMT"))
dat1$datetime.EDT=date.fun(format(dat1$datetime,tz="EST"),form="%F %R")

dat1$Temp.DegF=(dat1$OTMP*9/5)+32
               
plot(Temp.DegF~datetime,dat1)
plot(SAL~datetime,dat1)


dat1.da=ddply(dat1,"date",summarise,mean.temp=mean(Temp.DegF),max.temp=max(Temp.DegF),mean.sal=mean(SAL))
subset(dat1.da,max.temp==max(max.temp))$date
subset(dat1,date==date.fun("2023-07-12"))

subset(dat1,Temp.DegF==max(Temp.DegF))
subset(dat1,Temp.DegF>quantile(Temp.DegF,probs=0.99))
length(unique(subset(dat1,Temp.DegF>95)$date))
length(unique(dat1$date))

# png(filename=paste0(plot.path,"GBT1_recent.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,0.5,0.5),oma=c(2,1,0.5,0.75));
layout(matrix(1:2,2,1))
xlim.val=date.fun(c("2023-07-08","2023-08-22"));by.x=10;xmaj=seq(xlim.val[1],xlim.val[2],"10 days");xmin=seq(xlim.val[1],xlim.val[2],"1 days")

ylim.val=c(80,100);by.y=5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(Temp.DegF~datetime.EDT,dat1,ylim=ylim.val,xlim=xlim.val,type="n",axes=F,ann=F);
abline(h=ymaj,v=xmaj,lty=3,col=adjustcolor("grey",0.5))
lines(Temp.DegF~datetime.EDT,dat1,col="dodgerblue1",lwd=1.25)
lines(mean.temp~date,dat1.da,col="red")
axis_fun(1,xmaj,xmin,format(xmaj,"%b-%d"),line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
# with(subset(dat1,Temp.DegF==max(Temp.DegF)),text(datetime.EDT,Temp.DegF,paste0(round(Temp.DegF),"\u00B0 F")),pos=3,font=2,offset=1)
mtext(side=2,line=2,"Water Temperature (\u00B0 F)")
mtext(side=3,adj=0,"Garfield Bight (GBTF1)")
mtext(side=3,adj=1,"Data Source: NOAA & ENP")
legend("topright",legend=c("Hourly Data","Daily Avg"),
       pch=NA,
       lty=1,
       lwd=2,
       pt.bg=NA,
       col=c("dodgerblue1","red"),
       pt.cex=1,ncol=1,cex=0.8,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)

ylim.val=c(20,35);by.y=5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(SAL~datetime.EDT,dat1,ylim=ylim.val,xlim=xlim.val,type="n",axes=F,ann=F);
abline(h=ymaj,v=xmaj,lty=3,col=adjustcolor("grey",0.5))
lines(SAL~datetime.EDT,dat1,col="dodgerblue1",lwd=1.25)
# lines(mean.sal~date,dat1.da,col="red")
axis_fun(1,xmaj,xmin,format(xmaj,"%b-%d"),line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=2,"Salinity (PSU)")
mtext(side=1,line=1.75,"Date (Month-Day 2023)")
dev.off()


library(dataRetrieval)
readNWISsite("251032080473400")

# readNWISdv("251032080473400","00060",format(xlim.val[1],"%Y-%m-%d"),format(xlim.val[2],"%Y-%m-%d"))
gator.Q=readNWISuv("251032080473400","00060",format(xlim.val[1],"%Y-%m-%d"),format(xlim.val[2],"%Y-%m-%d"))
gator.Q=renameNWISColumns(gator.Q)

gator.Q$dateTime2=date.fun(format(gator.Q$dateTime,tz="EST"),form="%F %R")
gator.Q$date=date.fun(gator.Q$dateTime2)
# head(gator.Q)
plot(Flow_Inst~dateTime2,gator.Q)

gator.Q.da=ddply(gator.Q,"date",summarise,mean.Q=mean(Flow_Inst))
# png(filename=paste0(plot.path,"GatorQ_recent.png"),width=6.5,height=3,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,0.5,0.5),oma=c(2,1,0.5,0.75));
xlim.val=date.fun(c("2023-07-08","2023-08-22"));by.x=10;xmaj=seq(xlim.val[1],xlim.val[2],"10 days");xmin=seq(xlim.val[1],xlim.val[2],"1 days")

ylim.val=c(-100,100);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(Flow_Inst~dateTime2,gator.Q,ylim=ylim.val,xlim=xlim.val,type="n",axes=F,ann=F);
abline(h=ymaj,v=xmaj,lty=3,col=adjustcolor("grey",0.5))
abline(h=0)
lines(Flow_Inst~dateTime2,gator.Q,col="dodgerblue1",lwd=1.25)
lines(mean.Q~date,gator.Q.da,col="red")
axis_fun(1,xmaj,xmin,format(xmaj,"%b-%d"),line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
# with(subset(dat1,Temp.DegF==max(Temp.DegF)),text(datetime.EDT,Temp.DegF,paste0(round(Temp.DegF),"\u00B0 F")),pos=3,font=2,offset=1)
mtext(side=2,line=2,"Discharge (cfs)")
mtext(side=3,adj=0,"Alligator Creek")
mtext(side=3,adj=1,"Data Source: USGS")
mtext(side=1,line=1.75,"Date (Month-Day 2023)")
dev.off()