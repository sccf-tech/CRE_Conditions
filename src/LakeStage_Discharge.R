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
library(lubridate)
library(zoo)
library(ggplot2)
library(viridis)


## Paths
wd="C:/Julian_LaCie/_Github/CRE_Conditions"

paths=paste0(wd,c("/Plots/","/Export/","/Data/","/src"))
# Folder.Maker(paths);#One and done. Creates folders in working directory.
plot.path=paths[1]
export.path=paths[2]
data.path=paths[3]

dates=date.fun(c("1999-05-01","2021-04-30"))
dates=date.fun(c("1999-05-01",as.character(Sys.Date()-ddays(1))))
WYs=WY(dates)[1]:WY(dates)[2]
# Lake Stage --------------------------------------------------------------
lake.stg=DBHYDRO_daily(dates[1],dates[2],"00268")
lake.stg$WY=WY(lake.stg$Date)
lake.stg$DOWY=hydro.day(lake.stg$Date)

# CRE Discharge -----------------------------------------------------------
q.DBKEY=data.frame(SITE=c("S77","S79"),DBKEY=c("DJ235","DJ237"))
q.dat=DBHYDRO_daily(dates[1],dates[2],q.DBKEY$DBKEY)

q.dat$WY=WY(q.dat$Date)
q.dat$DOWY=hydro.day(q.dat$Date)
q.dat$Data.Value[q.dat$Data.Value<0]<-0
q.dat$Data.Value[is.na(q.dat$Data.Value)==T]<-0
range(q.dat$Data.Value)

flow.dat.xtab=cast(merge(q.dat,q.DBKEY,"DBKEY"),Date+DOWY+WY~SITE,value="Data.Value",mean)
flow.dat.xtab$C43=with(flow.dat.xtab,ifelse(S79>S77,0,S77-S79))
flow.dat.xtab$CumFlow.S77=with(flow.dat.xtab,ave(S77,WY,FUN=function(x) cumsum(cfs.to.acftd(x))))
flow.dat.xtab$CumFlow.S79=with(flow.dat.xtab,ave(S79,WY,FUN=function(x) cumsum(cfs.to.acftd(x))))
flow.dat.xtab$S77.Q14=with(flow.dat.xtab,c(rep(NA,13),rollapply(S77,width=14,FUN=function(x)mean(x,na.rm=T))))
flow.dat.xtab$S79.Q14=with(flow.dat.xtab,c(rep(NA,13),rollapply(S79,width=14,FUN=function(x)mean(x,na.rm=T))))


cols=wesanderson::wes_palette("Zissou1",length(WYs),"continuous")

xlim.val=c(1,366);by.x=90;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/3)
xmaj.month=c("May","Jul","Oct","Jan","Apr")

par(family="serif",mar=c(1.5,2,0.5,1),oma=c(2,5,1,1));
layout(matrix(1:6,3,2,byrow=T),width=c(1,0.25))  

ylim.val=c(8,18);by.y=1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(Data.Value~DOWY,lake.stg,xlim=xlim.val,ylim=ylim.val,type="n",ann=F,axes=F,xaxs="i")
abline(h=ymaj,v=xmaj,lty=3,col="grey")
for(i in 1:length(WYs)){
  with(subset(lake.stg,WY==WYs[i]),lines(DOWY,Data.Value,col=adjustcolor(cols[i],0.5),lwd=2))
}
axis_fun(1,xmaj,xmin,NA,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj)
box(lwd=1)
mtext(side=2,line=3.5,"Stage Elevation\n(Ft, NGVD29)")
# mtext(side=1,line=2,"Month")
plot(0:1,0:1,ann=F,axes=F,type="n")
legend(0,0.8,legend=paste0("WY",WYs),
       pch=NA,
       lty=1,
       lwd=2,
       col=cols,
       ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=1)

ylim.val=c(0,30000);by.y=10000;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(S79~DOWY,flow.dat.xtab,xlim=xlim.val,ylim=ylim.val,type="n",ann=F,axes=F,xaxs="i",yaxs="i")
abline(h=ymaj,v=xmaj,lty=3,col="grey")
for(i in 1:length(WYs)){
  with(subset(flow.dat.xtab,WY==WYs[i]),lines(DOWY,S79,col=adjustcolor(cols[i],0.5),lwd=1))
}
abline(h=2600,lty=3)
axis_fun(1,xmaj,xmin,NA,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=3.5,"Daily Discharge (cfs)")
mtext(side=3,"S79",adj=0)

plot(0:1,0:1,ann=F,axes=F,type="n")

ylim.val=c(0,40e5);by.y=10e5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(S79~DOWY,flow.dat.xtab,xlim=xlim.val,ylim=ylim.val,type="n",ann=F,axes=F,xaxs="i",yaxs="i")
abline(h=ymaj,v=xmaj,lty=3,col="grey")
for(i in 1:length(WYs)){
  with(subset(flow.dat.xtab,WY==WYs[i]),lines(DOWY,CumFlow.S79,col=adjustcolor(cols[i],0.5),lwd=1.5))
  with(subset(flow.dat.xtab,WY==WYs[i]),lines(DOWY,CumFlow.S77,col=adjustcolor(cols[i],0.5),lwd=1.5,lty=2))
}
abline(h=2600,lty=3.25)
axis_fun(1,xmaj,xmin,xmaj.month,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj/10e3);box(lwd=1)
mtext(side=2,line=3.5,"Cum Discharge\n(x10\u00B3 Ac-Ft WY\u207B\u00B9)")

legend("topleft",legend=c("S79","S77"),
       pch=NA,
       lty=c(1,2),
       lwd=2,
       col="grey",
       ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=1)


input=data.frame(WY=WYs)

input=input[c(2,4,6),]
cols=wesanderson::wes_palette("Zissou1",length(input),"continuous")

library(plotly)

P<-plot_ly(data=lake.stg)
for(i in 1:length(input)){
  tmp=subset(lake.stg,WY==input[i])
  col.vals=cols[i]
  leg.lab=paste0("WY",input[i])
  P<-add_trace(P,x=~DOWY,y=~Data.Value,
                 data=tmp,
                 color=I(col.vals),
                 type="scatter",mode='lines',name=leg.lab,
                 line=list(width=3))
}
P

P2<-plot_ly(data=flow.dat.xtab)
for(i in 1:length(input)){
  tmp=subset(flow.dat.xtab,WY==input[i])
  col.vals=cols[i]
  leg.lab=paste0("WY",input[i])
  P2<-add_trace(P2,x=~DOWY,y=~S79,
               data=tmp,
               color=I(col.vals),
               type="scatter",mode='lines',name=leg.lab,
               line=list(width=2))
}
P2

P3<-plot_ly(data=flow.dat.xtab)
for(i in 1:length(input)){
  tmp=subset(flow.dat.xtab,WY==input[i])
  col.vals=cols[i]
  leg.lab=paste0("WY",input[i])
  P3<-add_trace(P3,x=~DOWY,y=~S79,
                data=tmp,
                color=I(col.vals),
                type="scatter",mode='lines',name=leg.lab,
                line=list(width=2))
}
P3

P4<-plot_ly(data=flow.dat.xtab)
for(i in 1:length(input)){
  tmp=subset(flow.dat.xtab,WY==input[i])
  col.vals=cols[i]
  leg.lab=paste0("WY",input[i])
  P3<-add_trace(P3,x=~DOWY,y=~S79,
                data=tmp,
                color=I(col.vals),
                type="scatter",mode='lines',name=leg.lab,
                line=list(width=2))
}
P3