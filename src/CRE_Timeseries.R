## 
## SW Florida rain
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

## Paths
wd="C:/Julian_LaCie/_Github/CRE_Conditions"

paths=paste0(wd,c("/Plots/CRE_TS/","/Export/","/Data/","/src","/GIS"))
# Folder.Maker(paths);#One and done. Creates folders in working directory.
plot.path=paths[1]
export.path=paths[2]
data.path=paths[3]
GIS.path=paths[5]

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

# Discharge ---------------------------------------------------------------
Q.dbkeys=data.frame(SITE=c("S79",rep("S78",3),rep("S77",2)),
                    DBKEY=c("00865","00857","WN161","DJ236","15635","DJ235"))
Q.dbkeys=subset(Q.dbkeys,DBKEY=="00865")
q.dat=data.frame()
for(i in 1:nrow(Q.dbkeys)){
  tmp=DBHYDRO_daily(dates[1],dates[2],Q.dbkeys$DBKEY[i])
  tmp$DBKEY=as.character(Q.dbkeys$DBKEY[i])
  q.dat=rbind(q.dat,tmp)
  print(i)
}
q.dat$CY=as.numeric(format(q.dat$Date,"%Y"))
q.dat$WY=WY(q.dat$Date)
q.dat$DOY=as.numeric(format(q.dat$Date,"%j"))
q.dat$cumQ=with(q.dat,ave(Data.Value,CY,FUN=function(x) cumsum(ifelse(is.na(x),0,cfs.to.acftd(x)/1000))))

Q.dbkeys=data.frame(SITE=c("S79",rep("S78",3),rep("S77",2)),
                    DBKEY=c("00865","00857","WN161","DJ236","15635","DJ235"))
Q.dbkeys=subset(Q.dbkeys,DBKEY=="DJ235")
q.dat2=data.frame()
for(i in 1:nrow(Q.dbkeys)){
  tmp=DBHYDRO_daily(dates[1],dates[2],Q.dbkeys$DBKEY[i])
  tmp$DBKEY=as.character(Q.dbkeys$DBKEY[i])
  q.dat2=rbind(q.dat2,tmp)
  print(i)
}
q.dat2$CY=as.numeric(format(q.dat2$Date,"%Y"))
q.dat2$WY=WY(q.dat2$Date)
q.dat2$DOY=as.numeric(format(q.dat2$Date,"%j"))
q.dat2$cumQ=with(q.dat2,ave(Data.Value,CY,FUN=function(x) cumsum(ifelse(is.na(x),0,cfs.to.acftd(x)/1000))))




plot(Data.Value~Date,q.dat)

cols=viridis::magma(length(seq(2005,2020,1)),alpha=0.5)
plot(cumQ~DOY,subset(q.dat,CY==2005),type="n")
for(i in 1:length(seq(2005,2020,1))){
  with(subset(q.dat,CY==seq(2005,2020,1)[i]),lines(DOY,cumQ,col=cols[i],lwd=2))  
}
with(subset(q.dat,CY==2006),lines(DOY,cumQ))
with(subset(q.dat,CY==2007),lines(DOY,cumQ))

q.dat.CY.sum=ddply(q.dat,"CY",summarise,TQ.kacft=sum(cfs.to.acftd(Data.Value)/1000,na.rm=T))

xlim.val=c(2005,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
ylim.val=c(0,3500);by.y=500;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
# png(filename=paste0(plot.path,"CRE_annual_S79Q.png"),width=7,height=3.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,3,0.5,0.5),oma=c(2,1,0.5,1),lwd=0.1);

plot(TQ.kacft~CY,q.dat.CY.sum,xlim=xlim.val,ylim=ylim.val,ann=F,axes=F,type="n",yaxs="i")
abline(h=ymin,v=xmin,lty=2,col=adjustcolor("grey",0.5))
abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
with(q.dat.CY.sum,pt_line(CY,TQ.kacft,1,adjustcolor("dodgerblue1",0.5),2,21,"dodgerblue1",pt.lwd=0.1,cex=1.5))
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=1,line=1.5,"Calendar Year")
mtext(side=2,line=3,"Discharge (x1000 Ac-Ft Yr\u207B\u00B9)")
mtext(side=3,adj=0,"S-79 Annual Discharges")
dev.off()


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


xlim.val=date.fun(c("2019-01-01","2022-01-01"));xmaj=seq(xlim.val[1],xlim.val[2],"1 years");xmin=seq(xlim.val[1],xlim.val[2],"6 months")
ylim.val=c(0,15000);by.y=5000;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
# png(filename=paste0(plot.path,"CRE_S79Q_2019_.png"),width=7,height=3.5,units="in",res=200,type="windows",bg="white")
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


xlim.val=date.fun(c("2019-01-01","2022-01-01"));xmaj=seq(xlim.val[1],xlim.val[2],"1 years");xmin=seq(xlim.val[1],xlim.val[2],"6 months")
ylim.val=c(0,5000);by.y=1000;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
# png(filename=paste0(plot.path,"CRE_S77Q_2019_.png"),width=7,height=3.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,3,0.5,0.5),oma=c(2,1,0.5,1),lwd=0.1);

plot(Data.Value~Date,q.dat2,xlim=xlim.val,ylim=ylim.val,ann=F,axes=F,type="n",yaxs="i")
abline(h=ymin,v=xmin,lty=2,col=adjustcolor("grey",0.5))
abline(h=ymaj,v=xmaj,lty=1,col=adjustcolor("grey",0.5))
# with(q.dat,shaded.range(Date,rep(0,length(Date)),Data.Value.fill,bg="grey",lty=1))
with(q.dat2,lines(Date,Data.Value,col=adjustcolor("dodgerblue1",0.75),lwd=1.25))
axis_fun(1,xmaj,xmin,format(xmaj,"%Y"),line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=1,line=1.5,"Date (Year)")
mtext(side=2,line=3,"Discharge (ft\u207B\u00B3 s\u207B\u00B9)")
mtext(side=3,adj=0,"S-77 Daily Discharges")
dev.off()

# Consecutive  -------------------------------------------------------------
q.dat$GT6500=ifelse(q.dat$Data.Value>6500,1,0)
highQ_consec=data.frame()

tmp=q.dat
tmp$highQ_6500=0

for(i in 2:nrow(tmp)){
  # tmp$consec[i]=with(tmp,ifelse(Q_GT2100[i-1]>0&Q_GT2100[i]>0,1,0))
  tmp$highQ_6500[i]=with(tmp,ifelse(GT6500[i-1]==0&GT6500[i]>0,1,
                               ifelse(GT6500[i-1]>0&GT6500[i]>0,1,0)))
}

highQ=consec.startend(tmp$highQ>0)
tmp$sum.highQ=0
for(i in 1:length(highQ$ends)){
  tmp[highQ$ends[i],]$sum.highQ=with(tmp[c(highQ$starts[i]:highQ$ends[i]),],sum(highQ_6500,na.rm=T))
}

highQ_consec=tmp

plot(Data.Value~Date,q.dat,type="l")
with(highQ_consec,points(Date,ifelse(GT6500==1,6500,NA),pch=21,bg="red"))


ddply(highQ_consec,c("sum.highQ"),summarise,N.val=N.obs(sum.highQ))

rslt.highQ=reshape2::dcast(highQ_consec,sum.highQ~CY,value.var = "sum.highQ",fun.aggregate = function(x)N.obs(x))

rslt.highQ=ddply(highQ_consec,c("CY","sum.highQ"),summarise,count.event=N.obs(sum.highQ))
rslt.highQ$cat=with(rslt.highQ,ifelse(sum.highQ>0&sum.highQ<7,1,
                                      ifelse(sum.highQ>=7&sum.highQ<14,2,
                                             ifelse(sum.highQ>=14&sum.highQ<30,3,
                                                    ifelse(sum.highQ>=30&sum.highQ<60,4,
                                                           ifelse(sum.highQ>=60&sum.highQ<90,5,
                                                                  ifelse(sum.highQ>=90&sum.highQ<180,6,
                                                                         ifelse(sum.highQ>=180,7,NA))))))))
rslt.highQ$CY.f=paste("CY",rslt.highQ$CY,sep=".")
rslt.highQ.sum=reshape2::dcast(subset(rslt.highQ,is.na(cat)==F),cat~CY.f,value.var="count.event",sum,na.rm=T)
rslt.highQ.sum$CY.2007=0
rslt.highQ.sum$CY.2014=0
rslt.highQ.sum=rslt.highQ.sum[,c("cat",paste("CY",seq(2005,2020,1),sep="."))]

cols=wesanderson::wes_palette("Zissou1",5,"continuous")
cols=colorRampPalette(c("red","grey","blue"))(5)
# png(filename=paste0(plot.path,"CRE_extreme.png"),width=7,height=3.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,0.5,0.5),oma=c(2,1,0.5,1),lwd=0.1);
layout(matrix(c(1:2),1,2,byrow=F),widths=c(1,0.25))
xlim.val=c(2005,2020);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
ylim.val=c(0,13);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
x=barplot(as.matrix(rslt.highQ.sum[,2:ncol(rslt.highQ.sum)]),
          col=adjustcolor(cols,0.5),ylim=ylim.val,space=0,
          axes=F,ann=F,names.arg = rep(NA,length(2:ncol(rslt.highQ.sum))))
axis_fun(1,x[seq(1,length(x),by.x)],x,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=2,"Count of Events")
mtext(side=1,line=1.5,"Calendar Year")
mtext(side=3,adj=0,"S-79 Discharges > 6500 cfs")

plot(0:1,0:1,ann=F,axes=F,type="n")
legend(0.5,0.5,legend=c("< 7", "7 - 14","14 - 30","30 - 60","60 - 90"),
       pch=22,lty=0,lwd=0.1,
       pt.bg=adjustcolor(cols,0.5),
       pt.cex=1.5,ncol=1,cex=1,bty="n",y.intersp=1.25,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5,
       title.adj = 0,title="Duration\n(Consecutive Days)")
dev.off()


tmp=apply(rslt.highQ.sum[1:4,2:ncol(rslt.highQ.sum)],2,sum)
barplot(tmp)

# png(filename=paste0(plot.path,"CRE_consec.png"),width=6.5,height=6,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2,0.5,1),oma=c(3,2,2,0.25),lwd=0.5);
layout(matrix(c(1:12),6,2,byrow=F))

ylim.val=c(0,6);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)