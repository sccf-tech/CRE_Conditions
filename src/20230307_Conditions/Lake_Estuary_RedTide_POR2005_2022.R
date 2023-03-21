## Title:      LOK Area conditions and redtide
## Created by: Paul Julian (pjulian@evergladesfoundation.org)
## Created on: 02/13/2023

## BAD ## https://www.tidyverse.org/articles/2017/12/workflow-vs-script/
#Clears Everything...start fresh.
rm(list=ls(all=T));cat("\014");dev.off()

# Libraries
# devtools::install_github("SwampThingPaul/AnalystHelper")
library(AnalystHelper);
library(plyr)
library(reshape2)
library(openxlsx)

library(rgdal)
library(rgeos)
library(raster)

library(jsonlite)
library(httr)

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


# GIS Data ----------------------------------------------------------------
# shore=spTransform(readOGR(paste0(gen.GIS,"/FWC"),"FWC_Shoreline_simp"),wgs84)
shore=spTransform(readOGR(paste0(gen.GIS,"/FWC"),"FWC_Shoreline"),wgs84)

# -------------------------------------------------------------------------
CurWY=WY(Sys.Date())
# dates=date.fun(c("2016-05-01",as.character(Sys.Date())))
dates=date.fun(c("2004-05-01","2022-05-01"))
# Lake O ------------------------------------------------------------------

#LORS
# load("LORS.RData")
LORS=read.csv(paste0(wd,"/report/LORS.csv"))
LORS$Year=2017
LORS$Date2=with(LORS,date.fun(paste(Year,Month,Day,sep="-")))

LORS2=LORS
LORS2$Year=2018
LORS2$Date2=with(LORS2,date.fun(paste(Year,Month,Day,sep="-")))

LORS3=LORS
LORS3$Year=2022
LORS3$Date2=with(LORS3,date.fun(paste(Year,Month,Day,sep="-")))

LORS4=LORS
LORS4$Year=2023
LORS4$Date2=with(LORS4,date.fun(paste(Year,Month,Day,sep="-")))

LORS.gaph2Yrs=rbind(LORS,LORS2,LORS3,LORS4)
rm(LORS,LORS2,LORS3,LORS4)
LORS.gaph2Yrs$Date2=date.fun(LORS.gaph2Yrs$Date2)

#Lake Okeechobee
comp.dbkey=data.frame(DBKEY=c("N3466","06832"),Priority=c("P2","P1"))

stg.da=data.frame()
for(i in 1:nrow(comp.dbkey)){
  tmp=DBHYDRO_daily(dates[1],dates[2],comp.dbkey$DBKEY[i])
  tmp$DBKEY=as.character(comp.dbkey$DBKEY[i])
  stg.da=rbind(stg.da,tmp)
}
stg.da=merge(stg.da,comp.dbkey,"DBKEY")
stg.da$DATE=date.fun(stg.da$Date)

LakeO.xtab=dcast(stg.da,DATE~Priority,value.var="Data.Value",mean)
LakeO.xtab$Mean=with(LakeO.xtab,ifelse(is.na(P1)==T,P2,P1))
LakeO.xtab$WY=WY(LakeO.xtab$DATE)

tail(LakeO.xtab)

LakeO.xtab=merge(x=LakeO.xtab,y=LORS.gaph2Yrs,by.x="DATE",by.y="Date2",all.x=T)

LakeO.xtab$month=as.numeric(format(LakeO.xtab$DATE,"%m"))
LakeO.xtab$day=as.numeric(format(LakeO.xtab$DATE,"%d"))
LakeO.xtab$plot.dat=with(LakeO.xtab,date.fun(ifelse(month>4,paste(CurWY-1,month,day,sep="-"),paste(CurWY,month,day,sep="-"))))
LakeO.xtab$hydro.day=hydro.day(LakeO.xtab$DATE)

subset(LakeO.xtab,hydro.day==hydro.day("2023-03-15"))
## LOR08 Plot -------------------------------------------------------------
HighLakeLab.x=as.POSIXct(paste(CurWY-1,9,1,sep="-"))
WSMLab.x=as.POSIXct(paste(CurWY-1,9,1,sep="-"))
BENLab.x=as.POSIXct(paste(CurWY-1,7,15,sep="-"))
BASELab.x=as.POSIXct(paste(CurWY,4,1,sep="-"))
HIGHLab.x=as.POSIXct(paste(CurWY,3,15,sep="-"))
InterLab.x=as.POSIXct(paste(CurWY,3,1,sep="-"))
LowLab.x=as.POSIXct(paste(CurWY,2,15,sep="-"))

lwd.val=1
xlim.vals=as.POSIXct(strptime(c(as.Date(paste(CurWY-1,05,01,sep="-")),as.Date(paste(CurWY,05,01,sep="-"))),"%Y-%m-%d"),tz="EST")#as.POSIXct(strptime(dates,"%Y-%m-%d"),tz="EST")
xmaj=seq(xlim.vals[1],xlim.vals[2],by="2 months");xmin=seq(xlim.vals[1],xlim.vals[2],by="1 months")
ylim.val=c(9,18);by.y=1
ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
# png(filename=paste0(plot.path,"202303_Conditions/LORS08_20182023.png"),width=6.5,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(0.5,2,1,1),oma=c(0.5,3,0.5,1));
layout(matrix(1:2,2,1,byrow=T),heights=c(1,0.25))
plot(High~Date2,LORS.gaph2Yrs,ylim=ylim.val,xlim=xlim.vals,type="n",lwd=2,ylab=NA,xlab=NA,yaxs="i",xaxs="i",xaxt="n",yaxt="n")
abline(h=seq(9,18,1),lwd=1,col="grey",lty=3)
abline(v=seq(xlim.vals[1],xlim.vals[2],by="1 months"),lwd=1,col="grey",lty=3)
with(LORS.gaph2Yrs,lines(High~Date2,lwd=2,col="black"))
with(LORS.gaph2Yrs,lines(Intermediate~Date2,lwd=2,col="black"))
with(LORS.gaph2Yrs,lines(Low~Date2,lwd=2,col="black"))
with(LORS.gaph2Yrs,lines(BaseFlow~Date2,lwd=2,col="black"))
with(LORS.gaph2Yrs,lines(BeneficialUse~Date2,lwd=2,col="black"))
with(LORS.gaph2Yrs,lines(WSM~Date2,lwd=2,col="grey"))
with(LORS.gaph2Yrs,lines(Inter1ft~Date2,lwd=2,lty=5,col="black"))
with(LORS.gaph2Yrs,lines(LowLow~Date2,lwd=2,lty=5,col="grey"))
with(LORS.gaph2Yrs,lines(LowMid~Date2,lwd=2,lty=5,col="grey"))
text(HighLakeLab.x,17.5,"High Lake Management Band",font=2)
text(WSMLab.x,9.5,"Water Shortage Management Band",font=2)
text(BENLab.x,12,"Beneficial Use",font=2)
text(BASELab.x,13,"Base Flow",font=2)
text(HIGHLab.x,17,"High",font=2)
text(InterLab.x,16.25,"Intermediate ",font=2,cex=0.75)
text(LowLab.x,14.5,"Low",font=2,cex=0.75)
with(subset(LakeO.xtab,WY==2018),lines(plot.dat,Mean,lwd=4,col=adjustcolor("forestgreen",0.5),lty=1))
with(subset(LakeO.xtab,WY==2023),lines(plot.dat,Mean,lwd=4,col=adjustcolor("red",0.5),lty=1))
with(subset(LakeO.xtab,DATE==date.fun("2017-09-10")),points(plot.dat,Mean,pch=21,bg=NA,cex=1.25))
with(subset(LakeO.xtab,DATE==date.fun("2022-09-28")),points(plot.dat,Mean,pch=21,bg=NA,cex=1.25))
with(subset(LakeO.xtab,DATE==date.fun("2017-09-10")),text(plot.dat,Mean,"Irma",pos=2,offset=0.25,font=4))
with(subset(LakeO.xtab,DATE==date.fun("2022-09-28")),text(plot.dat,Mean,"Ian",pos=4,offset=0.25,font=4))

axis_fun(1,xmaj,xmin,format(xmaj,"%b"),line=-0.5)
axis_fun(2,ymaj,ymin,ymaj)
box(lwd=lwd.val)
mtext(side=1,"Month",line=2,cex=1.25)
mtext(side=2,"Stage Elevation (Feet, NGVD29)",line=2.5,cex=1.25)

plot(0:1,0:1,type = 'n', axes = F,xlab=NA, ylab=NA)
legend(0.5,0.75,
       legend=c("WY2018 (5/1/2017 - 4/30/2018)","WY2023 (5/1/2022 - 4/30/2023)","Hurricane Landfall"),
       pch=c(NA,NA,21),pt.bg=NA,
       col=c(adjustcolor(c("forestgreen","red"),0.5),"black"),lty=c(1,1,NA),lwd=c(4,4,1),
       ncol=2,cex=1,bty="n",y.intersp=1,x.intersp=0.5,xpd=NA,xjust=0.5)
dev.off()



# Discharges --------------------------------------------------------------

cal.flow=data.frame(DBKEY=c("DJ235","88280","DJ237","00865"),SITE=c(rep("S77",2),rep("S79",2)),priority=rep(c("P1","P2"),2))
cal.flow.dat=data.frame()
for(i in 1:nrow(cal.flow)){
  tmp=DBHYDRO_daily(dates[1],dates[2],cal.flow$DBKEY[i])
  tmp$DBKEY=as.character(cal.flow$DBKEY[i])
  cal.flow.dat=rbind(tmp,cal.flow.dat)
}
cal.flow.dat$Date=date.fun(cal.flow.dat$Date)
cal.flow.dat$WY=WY(cal.flow.dat$Date)
cal.flow.dat=merge(cal.flow.dat,cal.flow,"DBKEY")
cal.flow.dat.xtab=dcast(cal.flow.dat,SITE+Date+WY~priority,value.var = "Data.Value",mean)
cal.flow.dat.xtab$P1=with(cal.flow.dat.xtab,ifelse(SITE=="S77"&P1>=7587,NA,P1)); #Extreme value reported for 6/7/2021
cal.flow.dat.xtab$Data.Value=with(cal.flow.dat.xtab,ifelse(is.na(P1)==T,P2,P1))

cal.flow.dat.xtab=dcast(cal.flow.dat.xtab,Date+WY~SITE,value.var="Data.Value",mean)
cal.flow.dat.xtab$hydro.day=hydro.day(cal.flow.dat.xtab$Date)
cal.flow.dat.xtab$S77=with(cal.flow.dat.xtab,ifelse(S77<0,0,S77))
cal.flow.dat.xtab$C43=with(cal.flow.dat.xtab,ifelse(S79<S77,0,S79-S77))

cal.flow.dat.xtab$cum.S79=with(cal.flow.dat.xtab,ave(cfs.to.acftd(S79),WY,FUN = function(x)cumsum(ifelse(is.na(x),0,x))))
cal.flow.dat.xtab$cum.S77=with(cal.flow.dat.xtab,ave(cfs.to.acftd(S77),WY,FUN = function(x)cumsum(ifelse(is.na(x),0,x))))
library(RcppRoll)
cal.flow.dat.xtab$Q.14=with(cal.flow.dat.xtab,roll_meanr(S79,n=14))
cal.flow.dat.xtab$SalEnv.cat=with(cal.flow.dat.xtab,ifelse(Q.14<750,"low",
                                                           ifelse(Q.14>=750&Q.14<2100,"optimum",
                                                                  ifelse(Q.14>=2100&Q.14<2600,"stress",
                                                                         ifelse(Q.14>2600,"damaging",NA)))))

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

S79dat2=cal.flow.dat.xtab
S79dat2$StressDam=with(S79dat2,ifelse(Q.14>=2100,1,0))
S79dat2$Date=date.fun(S79dat2$Date)

S79dat2$StressDam.consec=0
for(i in 2:nrow(S79dat2)){
  S79dat2$StressDam.consec[i]=with(S79dat2,ifelse(StressDam[i-1]==0&StressDam[i]>0,1,
                                            ifelse(StressDam[i-1]>0&StressDam[i]>0,1,0)))
}
StressDam.consec.val=consec.startend(S79dat2$StressDam.consec)
S79dat2$sum.StressDam.consec=0
if(length(StressDam.consec.val$ends)!=0){
  for(i in 1:length(StressDam.consec.val$ends)){
    S79dat2[StressDam.consec.val$ends[i],]$sum.StressDam.consec=with(S79dat2[c(StressDam.consec.val$starts[i]:StressDam.consec.val$ends[i]),],sum(StressDam.consec,na.rm=T))
  }
}



pre.irma=consec.startend(subset(S79dat2,WY==2018&Date<=date.fun("2017-09-10"))$StressDam)
pre.irma$ends-pre.irma$starts
consec.startend(subset(S79dat2,WY==2018&Date>date.fun("2017-09-10"))$StressDam)

subset(S79dat2,WY==2023&Date<=date.fun("2022-09-28"))$StressDam
pre.ian=consec.startend(subset(S79dat2,WY==2023&Date<=date.fun("2022-09-28"))$StressDam)
pre.ian$ends-pre.ian$starts
consec.startend(subset(S79dat2,WY==2023&Date>date.fun("2022-09-28"))$StressDam)


subset(S79dat2,WY==2018)$sum.StressDam.consec
subset(S79dat2,WY==2023)$StressDam
subset(S79dat2,WY==2023)$sum.StressDam.consec

plot(cumsum(subset(S79dat2,WY==2023)$sum.StressDam.consec))

# RECOVER like analysis ---------------------------------------------------
library(zoo)
cal.flow.dat.xtab$LOK=apply(cal.flow.dat.xtab[,c("S77","S79")],1,min,na.rm=T)
cal.flow.dat.xtab$S79.14d=with(cal.flow.dat.xtab, c(rep(NA,13),rollapply(S79,width=14,FUN=function(x)mean(x,na.rm=T))))
cal.flow.dat.xtab$LOK.14d=with(cal.flow.dat.xtab, c(rep(NA,13),rollapply(LOK,width=14,FUN=function(x)mean(x,na.rm=T))))

cal.flow.dat.xtab$CRE.low=with(cal.flow.dat.xtab,ifelse(S79.14d<750,1,0)) # RECOVER Low
cal.flow.dat.xtab$CRE.opt=with(cal.flow.dat.xtab,ifelse(S79.14d>=750&S79.14d<2100,1,0)) # RECOVER Optimum
cal.flow.dat.xtab$CRE.high=with(cal.flow.dat.xtab,ifelse(S79.14d>=2100&S79.14d<2600,1,0)) # RECOVER Stress
cal.flow.dat.xtab$CRE.dam=with(cal.flow.dat.xtab,ifelse(S79.14d>=2600,1,0)) # RECOVER Damaging

cal.flow.dat.xtab$CRE.low.count=0
cal.flow.dat.xtab$CRE.opt.count=0
cal.flow.dat.xtab$CRE.high.count=0
cal.flow.dat.xtab$CRE.high.LOK.count=0
cal.flow.dat.xtab$CRE.high.basin.count=0
cal.flow.dat.xtab$CRE.dam.count=0
cal.flow.dat.xtab$CRE.dam.LOK.count=0
cal.flow.dat.xtab$CRE.dam.basin.count=0

CRE.Q.dat.xtab2=data.frame()
WY.vals=c(2018,2023)
for(j in 1:2){
  tmp=subset(cal.flow.dat.xtab,WY==WY.vals[j])
for(i in 14:nrow(tmp)){
  tmp$CRE.low.count[i]=with(tmp,ifelse(CRE.low[i]==1&sum(CRE.low.count[(i-13):(i-1)],na.rm=T)==0,1,0))  

  tmp$CRE.opt.count[i]=with(tmp,ifelse(CRE.opt[i]==1&sum(CRE.opt.count[(i-13):(i-1)],na.rm=T)==0,1,0))

  tmp$CRE.high.count[i]=with(tmp,ifelse(CRE.high[i]==1&sum(CRE.high.count[(i-13):(i-1)],na.rm=T)==0,1,0))
  tmp$CRE.high.LOK.count[i]=with(tmp,
                                 ifelse(CRE.high.count[i]==1,
                                        ifelse((S79.14d[i]-LOK.14d[i])<=2100,1,0),0))
  tmp$CRE.high.basin.count[i]=with(tmp,CRE.high.count[i]-CRE.high.LOK.count[i])
  
  tmp$CRE.dam.count[i]=with(tmp,ifelse(CRE.dam[i]==1&sum(CRE.dam.count[(i-13):(i-1)],na.rm=T)==0,1,0))
  tmp$CRE.dam.LOK.count[i]=with(tmp,
                                ifelse(CRE.dam.count[i]==1,
                                       ifelse((S79.14d[i]-LOK.14d[i])<=2600,1,0),0))
  tmp$CRE.dam.basin.count[i]=with(tmp,CRE.dam.count[i]-CRE.dam.LOK.count[i])
  
}
  CRE.Q.dat.xtab2=rbind(CRE.Q.dat.xtab2,tmp)
  print(j) 

}

vars=paste0("CRE.",c("low.count","opt.count","high.LOK.count","high.basin.count","dam.LOK.count","dam.basin.count"))
CRE.counts=reshape2::melt(CRE.Q.dat.xtab2[,c("WY",vars)],id.vars = "WY")
CRE.SalEnv_count=reshape2::dcast(CRE.counts,WY~variable,value.var = "value",sum)
CRE.SalEnv_count=CRE.SalEnv_count[match(alts.sort,CRE.SalEnv_count$Alt),c("Alt",vars)]
CRE.SalEnv_count


###



ylim.max=signif(max(cal.flow.dat.xtab$cum.S79),0)
ylim.val=c(0,ylim.max);by.y=ylim.max/4;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(0,366);xmaj=seq(xlim.val[1],xlim.val[2],90);xmin=seq(xlim.val[1],xlim.val[2],30)
cols=c("indianred1","orange","dodgerblue1")

# png(filename=paste0(plot.path,"202303_Conditions/CRE_20182023.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
layout(matrix(1:2,1,2,byrow=F),widths = c(1,1))
par(family="serif",mar=c(2,2,1,1),oma=c(1,3,0.5,2));
plot(cum.S79~hydro.day,cal.flow.dat.xtab,type="n",ylim=ylim.val,xlim=xlim.val,xaxs="i",yaxs="i",xaxt="n",yaxt="n",xlab=NA,ylab=NA)
abline(h=ymaj,v=xmaj,lty=3,col="grey")
with(subset(cal.flow.dat.xtab,WY==2018),lines(hydro.day,cum.S79,lwd=2.5,col=adjustcolor("forestgreen",0.5)))
with(subset(cal.flow.dat.xtab,WY==2023),lines(hydro.day,cum.S79,lwd=2.5,col=adjustcolor("red",0.5)))
with(subset(cal.flow.dat.xtab,hydro.day==hydro.day(as.Date("2017-09-11"))&WY==2018),points(hydro.day,cum.S79,pch=21,bg=NA,cex=1.25))
with(subset(cal.flow.dat.xtab,hydro.day==hydro.day(as.Date("2022-09-28"))&WY==2023),points(hydro.day,cum.S79,pch=21,bg=NA,cex=1.25))
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj/1000,cex.axis=1);box(lwd=lwd.val)
mtext("Day of Water Year",side=1,line=1.75,cex=1.25)
mtext("Cumulative Discharge (kAc-Ft D\u207B\u00B9)",side=2,line=3.5,cex=1.25)
mtext(side=3,"S-79",adj=0)

plot(cum.S79~hydro.day,cal.flow.dat.xtab,type="n",ylim=ylim.val,xlim=xlim.val,xaxs="i",yaxs="i",xaxt="n",yaxt="n",xlab=NA,ylab=NA)
abline(h=ymaj,v=xmaj,lty=3,col="grey")
with(subset(cal.flow.dat.xtab,WY==2018),lines(hydro.day,cum.S77,lwd=2.5,col=adjustcolor("forestgreen",0.5)))
with(subset(cal.flow.dat.xtab,WY==2023),lines(hydro.day,cum.S77,lwd=2.5,col=adjustcolor("red",0.5)))
with(subset(cal.flow.dat.xtab,hydro.day==hydro.day(as.Date("2017-09-11"))&WY==2018),points(hydro.day,cum.S77,pch=21,bg=NA,cex=1.25))
with(subset(cal.flow.dat.xtab,hydro.day==hydro.day(as.Date("2022-09-28"))&WY==2023),points(hydro.day,cum.S77,pch=21,bg=NA,cex=1.25))
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,NA);box(lwd=lwd.val)
mtext("Day of Water Year",side=1,line=1.75,cex=1.25)
mtext(side=3,"S-77",adj=0)

#plot(0:1,0:1,type="n",yaxt="n",xaxt="n",bty="n")
legend("topright",
       legend=c(paste0("WY",c(2018,2023)),"Hurricane Landfall"),
       pch=c(NA,NA,21),pt.bg=NA,
       col=c(adjustcolor(c("forestgreen","red"),0.5),"black"),lty=c(1,1,0),lwd=c(2.5,2.5,1),
       pt.cex=1,ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=0.5,xpd=NA,xjust=0.5)
dev.off()


max.q=4e4#max(cal.flow.dat.xtab$S79,na.rm=T)
ylim.max=round(max.q+max.q*0.1,-3)

ylim.val=c(10,ylim.max);by.y=ylim.max/4;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
ylim.val=c(10,ylim.max);ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor")
xlim.val=c(0,366);xmaj=seq(xlim.val[1],xlim.val[2],90);xmin=seq(xlim.val[1],xlim.val[2],30)
xlab=as.Date(xmaj,origin="2017-05-01")
# png(filename=paste0(plot.path,"202303_Conditions/CRE_S79_20182023.png"),width=6.5,height=5,units="in",res=200,type="windows",bg="white")
layout(matrix(1:3,3,1,byrow=F),heights=c(1,1,0.5))
par(family="serif",cex.axis=1.2,mar=c(1,2,1,1),oma=c(0.5,3,1,1));

plot(S79~hydro.day,cal.flow.dat.xtab,type="n",ylim=ylim.val,xlim=xlim.val,xaxs="i",yaxs="i",xaxt="n",yaxt="n",xlab=NA,ylab=NA,log="y")
xx=c(xlim.val[1],xlim.val[1],xlim.val[2],xlim.val[2])
yy3=c(2600,ylim.max,ylim.max,2600);polygon(x=xx,y=yy3,col=adjustcolor("red",0.5))
yy4=c(2100,2600,2600,2100);polygon(x=xx,y=yy4,col=adjustcolor("yellow",0.5))
yy5=c(750,2100,2100,750);polygon(x=xx,y=yy5,col=adjustcolor("green",0.5))
abline(h=ymaj,v=xmaj,lty=3,col="grey")
abline(v=hydro.day(as.Date("2017-09-11")))
with(subset(cal.flow.dat.xtab,WY==2018),pt_line(hydro.day,S79,2,"black",1,21,"grey",cex=1,pt.lwd=0.1))
with(subset(cal.flow.dat.xtab,WY==2018),lines(hydro.day,Q.14,lwd=3,col=adjustcolor("dodgerblue3",0.5)))
axis_fun(1,xmaj,xmin,format(xlab,"%m-%Y"),line=-0.5)
axis_fun(2,ymaj,ymin,ymaj,cex.axis=1)
# axis_fun(4,ymaj,ymin,format(round(cfs.to.acftd(ymaj)/1000,1)),cex.axis=1)
box(lwd=lwd.val)
mtext("Discharge (cfs)",side=2,line=3.5,cex=1.25)
mtext(side=3,adj=0,"S-79 (Caloosahatchee River Estuary)")
mtext(side=3,line=-1.25,adj=0," WY2018")

xlab=as.Date(xmaj,origin="2022-05-01")
plot(S79~hydro.day,cal.flow.dat.xtab,type="n",ylim=ylim.val,xlim=xlim.val,xaxs="i",yaxs="i",xaxt="n",yaxt="n",xlab=NA,ylab=NA,log="y")
xx=c(xlim.val[1],xlim.val[1],xlim.val[2],xlim.val[2])
yy3=c(2600,ylim.max,ylim.max,2600);polygon(x=xx,y=yy3,col=adjustcolor("red",0.5))
yy4=c(2100,2600,2600,2100);polygon(x=xx,y=yy4,col=adjustcolor("yellow",0.5))
yy5=c(750,2100,2100,750);polygon(x=xx,y=yy5,col=adjustcolor("green",0.5))
abline(h=ymaj,v=xmaj,lty=3,col="grey")
abline(v=hydro.day(as.Date("2022-09-28")))
with(subset(cal.flow.dat.xtab,WY==2023),pt_line(hydro.day,S79,2,"black",1,21,"grey",cex=1,pt.lwd=0.1))
with(subset(cal.flow.dat.xtab,WY==2023),lines(hydro.day,Q.14,lwd=3,col=adjustcolor("dodgerblue3",0.5)))
axis_fun(1,xmaj,xmin,format(xlab,"%m-%Y"),line=-0.5)
axis_fun(2,ymaj,ymin,ymaj,cex.axis=1)
# axis_fun(4,ymaj,ymin,format(round(cfs.to.acftd(ymaj)/1000,1)),cex.axis=1)
box(lwd=lwd.val)
mtext("Discharge (cfs)",side=2,line=3.5,cex=1.25)
mtext("Date (MM/YYYY)",side=1,line=2,cex=)
mtext(side=3,line=-1.25,adj=0," WY2023")

# mtext("Discharge (kAc-Ft D\u207B\u00B9)",side=4,line=2.5,cex=1.25)

plot(0:1,0:1,type="n",yaxt="n",xaxt="n",bty="n")
legend(0.25,0.75,
       legend=c( "Damaging (>2600 cfs)","Stress (2100 - 2600 cfs)","Optimum (750 - 2100 cfs)"),pch=22,
       pt.bg=adjustcolor(c("red","yellow","green"),0.5),pt.cex=2,ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=1,xpd=NA,xjust=0.5,title.adj = 0,title='RECOVER PM')

legend(0.75,0.75,
       legend=c("Daily Discharge","14-Day moving average","Hurricane Landfall"),
       pch=c(21,NA,NA),pt.bg=c("grey",NA,NA),pt.cex=c(1.5,NA,NA),lty=c(NA,1,1),lwd=c(0.5,2,1),col=c("black",adjustcolor(c("dodgerblue1"),0.5),"black"),
       ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=1,xpd=NA,xjust=0.5,title.adj = 0,title='')
dev.off()

## Charlotte Harbor -------------------------------------------------------
library(dataRetrieval)
charhar.sites=data.frame(SITE=c("02298202","02296750","02298880","02297100","02299472"),
                         SiteName.abbr=c("Shell","Peace","Myakka","Joshua","BigSlough"),
                         Site.Lab=c("Shell Creek","Peace River","Myakka River","Joshua Creek","Big Slough"))
charhar.sites=charhar.sites[match(c("Myakka","BigSlough","Peace","Joshua","Shell"),charhar.sites$SiteName.abbr),]

NWIS.siteinfo=readNWISsite(charhar.sites$SITE)

q.dat=readNWISdv(charhar.sites$SITE,c("00060"),format(dates[1],"%Y-%m-%d"),format(dates[2],"%Y-%m-%d"))
q.dat=renameNWISColumns(q.dat)
q.dat$Date=date.fun(q.dat$Date)
ddply(q.dat,c("site_no"),summarise,min.date=min(Date,na.rm=T),max.date=max(Date,na.rm=T))

q.dat=merge(q.dat,charhar.sites,by.x=c("site_no"),by.y=c("SITE"))
q.dat$Flow.acft=cfs.to.acftd(q.dat$Flow)

# tail(subset(q.dat,site_no=="02296750"))
fill=expand.grid(Date=seq(min(q.dat$Date),max(q.dat$Date),"1 days"),
                 SITE=charhar.sites$SITE)
fill=merge(fill,charhar.sites,"SITE")
q.dat=merge(q.dat,fill,by.x=c("site_no","Date","SiteName.abbr","Site.Lab"),by.y=c("SITE","Date","SiteName.abbr","Site.Lab"),all.y=T)

q.dat.xtab=dcast(q.dat,Date~SiteName.abbr,value.var = "Flow.acft",mean)
q.dat.xtab$TFlow=rowSums(q.dat.xtab[,c("BigSlough",'Joshua',"Myakka","Peace","Shell")],na.rm=T)
q.dat.xtab$WY=WY(q.dat.xtab$Date)
q.dat.xtab$DOY=hydro.day(q.dat.xtab$Date)
q.dat.xtab$cum.CharHar.Q=with(q.dat.xtab,ave(TFlow,WY,FUN = function(x)cumsum(ifelse(is.na(x),0,x))))

plot(cum.CharHar.Q~DOY,subset(q.dat.xtab,WY==2018))
lines(cum.CharHar.Q~DOY,subset(q.dat.xtab,WY==2023))



ylim.max=2.5e6# signif(max(q.dat.xtab$cum.CharHar.Q,na.rm=T),0)
ylim.val=c(0,ylim.max);by.y=ylim.max/4;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(0,366);xmaj=seq(xlim.val[1],xlim.val[2],90);xmin=seq(xlim.val[1],xlim.val[2],30)
cols=c("indianred1","orange","dodgerblue1")

# png(filename=paste0(plot.path,"202303_Conditions/CharHar_20182023.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(2,2,1,1),oma=c(1,3,1,1));
plot(cum.CharHar.Q~DOY,q.dat.xtab,type="n",ylim=ylim.val,xlim=xlim.val,xaxs="i",yaxs="i",xaxt="n",yaxt="n",xlab=NA,ylab=NA)
abline(h=ymaj,v=xmaj,lty=3,col="grey")
with(subset(q.dat.xtab,WY==2018),lines(DOY,cum.CharHar.Q,lwd=2.5,col=adjustcolor("forestgreen",0.5)))
with(subset(q.dat.xtab,DOY==hydro.day(as.Date("2017-09-11"))&WY==2018),points(DOY,cum.CharHar.Q,pch=21,bg=NA))
with(subset(q.dat.xtab,WY==2023),lines(DOY,cum.CharHar.Q,lwd=2.5,col=adjustcolor("red",0.5)))
with(subset(q.dat.xtab,DOY==hydro.day(as.Date("2022-09-28"))&WY==2023),points(DOY,cum.CharHar.Q,pch=21,bg=NA))
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj/1000,cex.axis=1);box(lwd=lwd.val)
mtext("Day of Water Year",side=1,line=1.75,cex=1.25)
mtext("Cumulative Discharge (kAc-Ft D\u207B\u00B9)",side=2,line=3.5,cex=1.25)
mtext(side=3,line=0.95,"Charlotte Harbor",adj=0)
mtext(side=3,adj=0,"(Big Slough, Joshua Creek, Myakka River, Peace River, Shell Creek)",col="grey40",cex=0.85,font=3)

#plot(0:1,0:1,type="n",yaxt="n",xaxt="n",bty="n")
legend("topleft",
       legend=c(paste0("WY",c(2018,2023)),"Hurricane Landfall"),
       pch=c(NA,NA,21),pt.bg=NA,
       col=c(adjustcolor(c("forestgreen","red"),0.5),"black"),lty=c(1,1,0),lwd=c(2.5,2.5,1),
       pt.cex=1,ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=0.5,xpd=NA,xjust=0.5)
dev.off()



## Discharges South -------------------------------------------------------


EAA.flow=data.frame(SITE=c("S354",'S351',"S352","S271","S271"),
                    DBKEY=c("91513","91508","91510","65409","15640"),
                    priority=c("P1","P1","P1","P1","P2"))
eaa.flow.dat=data.frame()
for(i in 1:nrow(EAA.flow)){
  tmp=DBHYDRO_daily(dates[1],dates[2],EAA.flow$DBKEY[i])
  tmp$DBKEY=as.character(EAA.flow$DBKEY[i])
  eaa.flow.dat=rbind(tmp,eaa.flow.dat)
}
eaa.flow.dat$Date=date.fun(eaa.flow.dat$Date)
eaa.flow.dat$WY=WY(eaa.flow.dat$Date)
eaa.flow.dat=merge(eaa.flow.dat,EAA.flow,"DBKEY")
eaa.flow.dat.xtab=dcast(eaa.flow.dat,SITE+Date+WY~priority,value.var = "Data.Value",mean)
eaa.flow.dat.xtab$Data.Value=with(eaa.flow.dat.xtab,ifelse(is.na(P1)==T,P2,P1))
eaa.flow.dat.xtab$Data.Value=with(eaa.flow.dat.xtab,ifelse(Data.Value<0,0,Data.Value))

eaa.flow.dat.xtab2=dcast(eaa.flow.dat.xtab,Date+WY~SITE,value.var = "Data.Value",mean)
eaa.flow.dat.xtab2$TFlow=rowSums(eaa.flow.dat.xtab2[,c("S354","S351","S352","S271")],na.rm=T)


eaa.flow.dat.xtab2$cum.Q=with(eaa.flow.dat.xtab2,ave(cfs.to.acftd(TFlow),WY,FUN = function(x)cumsum(ifelse(is.na(x),0,x))))

eaa.flow.dat.xtab2$hydro.day=hydro.day(eaa.flow.dat.xtab2$Date)



ylim.max=3e6# signif(max(q.dat.xtab$cum.CharHar.Q,na.rm=T),0)
ylim.val=c(0,ylim.max);by.y=ylim.max/4;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(0,366);xmaj=seq(xlim.val[1],xlim.val[2],90);xmin=seq(xlim.val[1],xlim.val[2],30)
cols=c("indianred1","orange","dodgerblue1")

# png(filename=paste0(plot.path,"202303_Conditions/CH_CRE_20182023_v2.png"),width=6.5,height=3,units="in",res=200,type="windows",bg="white")
layout(matrix(1:4,1,4,byrow=F))
par(family="serif",mar=c(2,0.5,1,1),oma=c(1,4.5,0.5,0.5));
plot(cum.CharHar.Q~DOY,q.dat.xtab,type="n",ylim=ylim.val,xlim=xlim.val,xaxs="i",yaxs="i",xaxt="n",yaxt="n",xlab=NA,ylab=NA)
abline(h=ymaj,v=xmaj,lty=3,col="grey")
with(subset(q.dat.xtab,WY==2018),lines(DOY,cum.CharHar.Q,lwd=2.5,col=adjustcolor("forestgreen",0.5)))
with(subset(q.dat.xtab,DOY==hydro.day(as.Date("2017-09-11"))&WY==2018),points(DOY,cum.CharHar.Q,pch=21,bg=NA))
with(subset(q.dat.xtab,WY==2023),lines(DOY,cum.CharHar.Q,lwd=2.5,col=adjustcolor("red",0.5)))
with(subset(q.dat.xtab,DOY==hydro.day(as.Date("2022-09-28"))&WY==2023),points(DOY,cum.CharHar.Q,pch=21,bg=NA))
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj/1000,cex.axis=1);box(lwd=lwd.val)
# mtext("Day of Water Year",side=1,line=1.75,cex=1)
mtext("Cumulative Discharge (kAc-Ft D\u207B\u00B9)",side=2,line=3,cex=1)
mtext(side=3,"Charlotte Harbor",adj=0)
legend("topleft",
       legend=c(paste0("WY",c(2018,2023)),"Hurricane Landfall"),
       pch=c(NA,NA,21),pt.bg=NA,
       col=c(adjustcolor(c("forestgreen","red"),0.5),"black"),lty=c(1,1,0),lwd=c(2.5,2.5,1),
       pt.cex=1,ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=0.5,xpd=NA,xjust=0.5)

plot(cum.S79~hydro.day,cal.flow.dat.xtab,type="n",ylim=ylim.val,xlim=xlim.val,xaxs="i",yaxs="i",xaxt="n",yaxt="n",xlab=NA,ylab=NA)
abline(h=ymaj,v=xmaj,lty=3,col="grey")
with(subset(cal.flow.dat.xtab,WY==2018),lines(hydro.day,cum.S79,lwd=2.5,col=adjustcolor("forestgreen",0.5)))
with(subset(cal.flow.dat.xtab,WY==2023),lines(hydro.day,cum.S79,lwd=2.5,col=adjustcolor("red",0.5)))
with(subset(cal.flow.dat.xtab,hydro.day==hydro.day(as.Date("2017-09-11"))&WY==2018),points(hydro.day,cum.S79,pch=21,bg=NA,cex=1.25))
with(subset(cal.flow.dat.xtab,hydro.day==hydro.day(as.Date("2022-09-28"))&WY==2023),points(hydro.day,cum.S79,pch=21,bg=NA,cex=1.25))
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,NA,cex.axis=1);box(lwd=lwd.val)
# mtext("Day of Water Year",side=1,line=1.75,cex=1)
mtext(side=3,"S-79",adj=0)

plot(cum.S77~hydro.day,cal.flow.dat.xtab,type="n",ylim=ylim.val,xlim=xlim.val,xaxs="i",yaxs="i",xaxt="n",yaxt="n",xlab=NA,ylab=NA)
abline(h=ymaj,v=xmaj,lty=3,col="grey")
with(subset(cal.flow.dat.xtab,WY==2018),lines(hydro.day,cum.S77,lwd=2.5,col=adjustcolor("forestgreen",0.5)))
with(subset(cal.flow.dat.xtab,WY==2023),lines(hydro.day,cum.S77,lwd=2.5,col=adjustcolor("red",0.5)))
with(subset(cal.flow.dat.xtab,hydro.day==hydro.day(as.Date("2017-09-11"))&WY==2018),points(hydro.day,cum.S77,pch=21,bg=NA,cex=1.25))
with(subset(cal.flow.dat.xtab,hydro.day==hydro.day(as.Date("2022-09-28"))&WY==2023),points(hydro.day,cum.S77,pch=21,bg=NA,cex=1.25))
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,NA);box(lwd=lwd.val)
# mtext("Day of Water Year",side=1,line=1.75,cex=1)
mtext(side=3,"S-77",adj=0)

plot(cum.Q~hydro.day,eaa.flow.dat.xtab2,type="n",ylim=ylim.val,xlim=xlim.val,xaxs="i",yaxs="i",xaxt="n",yaxt="n",xlab=NA,ylab=NA)
abline(h=ymaj,v=xmaj,lty=3,col="grey")
with(subset(eaa.flow.dat.xtab2,WY==2018),lines(hydro.day,cum.Q,lwd=2.5,col=adjustcolor("forestgreen",0.5)))
with(subset(eaa.flow.dat.xtab2,WY==2023),lines(hydro.day,cum.Q,lwd=2.5,col=adjustcolor("red",0.5)))
with(subset(eaa.flow.dat.xtab2,hydro.day==hydro.day(as.Date("2017-09-11"))&WY==2018),points(hydro.day,cum.Q,pch=21,bg=NA,cex=1.25))
with(subset(eaa.flow.dat.xtab2,hydro.day==hydro.day(as.Date("2022-09-28"))&WY==2023),points(hydro.day,cum.Q,pch=21,bg=NA,cex=1.25))
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,NA);box(lwd=lwd.val)
# mtext("Day of Water Year",side=1,line=1.75,cex=1)
mtext("Day of Water Year",side=1,outer=T,cex=1)
mtext(side=3,"South",adj=0)

dev.off()

# Red Tide ----------------------------------------------------------------
cat.abund.xwalk=data.frame(category=c("not observed","very low","low","medium","high"),
                           abund=c("not present/background (0-1,000)", "very low (>1,000-10,000)",
                                   "low (>10,000-100,000)", "medium (>100,000-1,000,000)",
                                   "high (>1,000,000)"))

HABext=extent(-82.75,
              -81.5,
              26.2,
              26.8)

HABext.poly=as(HABext,"SpatialPolygons")
proj4string(HABext.poly)=wgs84
HABext.poly=SpatialPolygonsDataFrame(HABext.poly,data.frame(ID="HAB"))
# tm_shape(HABext.poly)+tm_polygons(alpha=0.5)
HABext2=extent(HABext)

plot(shore)
plot(HABext.poly,add=T)

# png(filename=paste0(plot.path,"202303_Conditions/spatialAOI.png"),width=6,height=4.5,units="in",res=200,type="windows",bg="white")
par(mar=c(0,0,0,0),oma=c(0.5,0.5,0.5,0.5))
bbox.lims=bbox(spTransform(gBuffer(spTransform(HABext.poly,utm17),width=2000),wgs84))
plot(shore,col="cornsilk",border="grey",bg="lightblue",ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],lwd=0.01)
plot(HABext.poly,add=T,lty=2)
dev.off()

kbr.AOI=HABext.poly


path="https://atoll.floridamarine.org/arcgis/rest/services/FWC_GIS/OpenData_HAB/MapServer/9/query"
request <- GET(
  url = path,
  query= list(       
    where = "County='Lee'",
    outFields = '*',
    f = 'pjson'
  )
)
response <- content(request, as = "text", encoding = "UTF-8")

results <- jsonlite::fromJSON(response,flatten=T)
# results
kbrdat=results$features
kbrdat



path <- 'https://gis.ngdc.noaa.gov/arcgis/rest/services/EnvironmentalMonitoring/HABSOSViewBase/MapServer/0/query?'

kbrdat.all=data.frame()
months.vals=c(seq(as.Date("2017-05-01"),as.Date("2018-05-01"),"3 months"))
months.vals=c(seq(as.Date("2004-05-01"),as.Date("2021-05-01"),"3 months"))

for(i in 2:length(months.vals)){
request <- GET(
  url = path,
  query= list(
    where= paste("LATITUDE <",round(HABext2[4],1),
                 "AND LATITUDE >",round(HABext2[3],1),
                 "AND LONGITUDE >",round(HABext2[1],1),
                 "AND LONGITUDE <",round(HABext2[2],1),
                 "AND SAMPLE_DATE >= date",paste0("'",paste0(months.vals[i-1], " 00:00:00"),"'"),
                 "AND SAMPLE_DATE < date",paste0("'",paste0(months.vals[i], " 00:00:00"),"'")),
    outFields = 'DESCRIPTION,SAMPLE_DATE,SAMPLE_DEPTH,LATITUDE,LONGITUDE,SALINITY,SALINITY_UNIT,WATER_TEMP,WATER_TEMP_UNIT,GENUS,SPECIES,CATEGORY,CELLCOUNT,CELLCOUNT_UNIT',
    f = 'pjson'
  )
)



response <- content(request, as = "text", encoding = "UTF-8")

results <- jsonlite::fromJSON(response,flatten=T)
# results
kbrdat=results$features
if(length(kbrdat)==0){next}else{
vars=c("attributes.DESCRIPTION","attributes.SAMPLE_DATE",
       "attributes.SAMPLE_DEPTH","attributes.LATITUDE",
       "attributes.LONGITUDE", "attributes.SALINITY", "attributes.SALINITY_UNIT",
       "attributes.WATER_TEMP", "attributes.WATER_TEMP_UNIT", "attributes.GENUS",
       "attributes.SPECIES", "attributes.CATEGORY", "attributes.CELLCOUNT",
       "attributes.CELLCOUNT_UNIT")
vars=strsplit(vars,"\\.")
vars=sapply(vars,"[",2)

colnames(kbrdat)<-tolower(vars)
kbrdat$date=as.POSIXct(as.numeric(gsub('000$', '',format(kbrdat$sample_date,scientific=F))),
                       origin = c('1970-01-01'), tz = 'UTC')
kbrdat$datetime=date.fun(kbrdat$date,form="%F %R")
kbrdat$date=date.fun(kbrdat$date)
kbrdat$year=as.numeric(format(kbrdat$date,"%Y"))
kbrdat$WY=WY(kbrdat$date)
# range(kbrdat$year,na.rm=T)
kbrdat=subset(kbrdat,is.na(year)==T|year!=153)
# min(kbrdat$date)
# max(kbrdat$date)
##

kbrdat.all=rbind(kbrdat.all,kbrdat)
print(paste(i," :(",i/length(months.vals),"%)","nrow:",nrow(kbrdat)))
}
}

path <- 'https://atoll.floridamarine.org/arcgis/rest/services/FWC_GIS/OpenData_HAB/MapServer/18/query?'
kbrdat.all2=data.frame()
months.vals=c(seq(as.Date("2021-05-01"),as.Date("2022-05-01"),"3 months"))
for(i in 2:length(months.vals)){
  request <- GET(
    url = path,
    query= list(
      where= paste("LATITUDE <",round(HABext2[4],1),
                   "AND LATITUDE >",round(HABext2[3],1),
                   "AND LONGITUDE >",round(HABext2[1],1),
                   "AND LONGITUDE <",round(HABext2[2],1),
                   "AND SAMPLE_DATE >= date",paste0("'",paste0(months.vals[i-1], " 00:00:00"),"'"),
                   "AND SAMPLE_DATE < date",paste0("'",paste0(months.vals[i], " 00:00:00"),"'")),
      outFields = '*',
      f = 'pjson'
    )
  )
  
  
  
  response <- content(request, as = "text", encoding = "UTF-8")
  
  results <- jsonlite::fromJSON(response,flatten=T)
  
  # results
  kbrdat=results$features
  if(length(kbrdat)==0){next}else{
  # vars=c("attributes.DESCRIPTION","attributes.SAMPLE_DATE",
  #        "attributes.SAMPLE_DEPTH","attributes.LATITUDE",
  #        "attributes.LONGITUDE", "attributes.SALINITY", "attributes.SALINITY_UNIT",
  #        "attributes.WATER_TEMP", "attributes.WATER_TEMP_UNIT", "attributes.GENUS",
  #        "attributes.SPECIES", "attributes.CATEGORY", "attributes.CELLCOUNT",
  #        "attributes.CELLCOUNT_UNIT")
  vars=names(kbrdat)
  vars=strsplit(vars,"\\.")
  vars=sapply(vars,"[",2)
  
  colnames(kbrdat)<-tolower(vars)
  kbrdat$date=as.POSIXct(as.numeric(gsub('000$', '',format(kbrdat$sample_date,scientific=F))),
                         origin = c('1970-01-01'), tz = 'UTC')
  kbrdat$datetime=date.fun(kbrdat$date,form="%F %R")
  kbrdat$date=date.fun(kbrdat$date)
  kbrdat$year=as.numeric(format(kbrdat$date,"%Y"))
  kbrdat$WY=WY(kbrdat$date)
  # range(kbrdat$year,na.rm=T)
  kbrdat=subset(kbrdat,is.na(year)==T|year!=153)
  # min(kbrdat$date)
  # max(kbrdat$date)
  ##
  
  kbrdat.all2=rbind(kbrdat.all2,kbrdat)
  print(paste(i," :(",i/length(months.vals),"%)","nrow:",nrow(kbrdat)))
  }
}

head(kbrdat.all,4L)

head(kbrdat.all2,4L)
unique(kbrdat.all2$name)
kbrdat.all2$cellcount=kbrdat.all2$count_

vars=c("date","latitude","longitude","cellcount")


kbrdat=rbind(kbrdat.all[,vars],kbrdat.all2[,vars])
kbrdat$hydro.day=hydro.day(kbrdat$date)
# kbrdat=merge(kbrdat,cat.abund.xwalk,"category")
# kbrdat$Abundance=factor(kbrdat$abund,levels=cat.abund.xwalk$abund)
# kbrdat$category=factor(kbrdat$category,levels=cat.abund.xwalk$category)

kbrdat.shp=SpatialPointsDataFrame(coords=kbrdat[,c("longitude","latitude")],
                                  data=kbrdat,
                                  proj4string = wgs84)
plot(kbrdat.shp,add=T)
kbrdat$cellcount.log=with(kbrdat,ifelse(cellcount==0,0,log10(cellcount)))
kbrdat$WY=WY(kbrdat$date)

da.mean.val=ddply(kbrdat,c("date","WY"),summarise,mean.log.val=mean(cellcount.log,na.rm=T),mean.val=mean(cellcount,na.rm=T))
da.mean.val$cellcount.backtrans=with(da.mean.val,ifelse(mean.val==0,0,10^mean.log.val))
da.mean.val$hydro.day=hydro.day(da.mean.val$date)

plot(cellcount.backtrans~date,da.mean.val)
plot(cellcount~date,kbrdat)


plot(cellcount.backtrans~hydro.day,subset(da.mean.val,WY==2018),log="y")
plot(mean.log.val~hydro.day,subset(da.mean.val,WY==2018),type="b")
plot(mean.log.val~hydro.day,subset(da.mean.val,WY==2023),type="b")
plot(mean.val~hydro.day,subset(da.mean.val,WY==2018))
plot(mean.val~hydro.day,subset(da.mean.val,WY==2023))

plot(cellcount~hydro.day,subset(kbrdat,WY==2023))


abund.val2=c("not present/background\n(0-1,000)", "very low\n(>1,000-10,000)",
        "low\n(>10,000-100,000)", "medium\n(>100,000-1,000,000)",
        "high\n(>1,000,000)")
abund.cols=c("white","grey","dodgerblue1","orange","indianred1")
ylim.val=c(0,6);by.y=1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(1,365);by.x=90;by.x2=30;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x2)
xlim.lab=seq(as.Date("2018-05-01"),as.Date("2019-05-01"),"3 months")
# png(filename=paste0(plot.path,"202303_Conditions/HAB_TS_20182023.png"),width=6,height=4.5,units="in",res=200,type="windows",bg="white")
layout(matrix(1:3,3,1,byrow=F),heights = c(1,1,0.4))
par(family="serif",mar=c(1,4,0.25,0.25),oma=c(2,2,0.75,0.5));

plot(mean.log.val~hydro.day,da.mean.val,ylim=ylim.val,xlim=xlim.val,type="n",axes=F,ann=F)
abline(h=ymaj,v=xmaj,lty=3,col="grey")
col.vals=findInterval(subset(da.mean.val,WY==2018)$mean.log.val,c(0,3,4,5,6,7))
# col.vals[col.vals==0]=1
with(subset(da.mean.val,WY==2018),segments(hydro.day,0,hydro.day,mean.log.val,col=adjustcolor("black",0.5)))
with(subset(da.mean.val,WY==2018),points(hydro.day,mean.log.val,pch=21,cex=1,lwd=0.1,bg=abund.cols[col.vals]))# adjustcolor("black",0.5)))
axis_fun(1,xmaj,xmin,NA,line=-0.5)
axis_fun(2,ymaj,ymaj,format(c(0,10^(ymaj[2:7])),scientific = F,big.mark=","));box(lwd=1)
mtext(side=3,line=-1.25,adj=0," WY2018",col="red",font=2)
# abline(h=c(3,4,5,6),col=abund.cols,lwd=1.25,lty=2)

plot(mean.log.val~hydro.day,da.mean.val,ylim=ylim.val,xlim=xlim.val,type="n",axes=F,ann=F)
abline(h=ymaj,v=xmaj,lty=3,col="grey")
col.vals=findInterval(subset(da.mean.val,WY==2023)$mean.log.val,c(0,3,4,5,6,7))
# col.vals[col.vals==0]=1
with(subset(da.mean.val,WY==2023),segments(hydro.day,0,hydro.day,mean.log.val,col=adjustcolor("black",0.5)))
with(subset(da.mean.val,WY==2023),points(hydro.day,mean.log.val,pch=21,cex=1,lwd=0.1,bg=abund.cols[col.vals]))
axis_fun(1,xmaj,xmin,format(xlim.lab,"%b"),line=-0.5)
axis_fun(2,ymaj,ymaj,format(c(0,10^(ymaj[2:7])),scientific = F,big.mark=","));box(lwd=1)
mtext(side=3,line=-1.25,adj=0," WY2023",col="red",font=2)
mtext(side=2,line=0.25,expression(paste(italic("K. brevis")," (cells L"^"-1",")")),outer=T)
mtext(side=1,line=1.75,"Month")
#abline(h=c(3,4,5,6),col=abund.cols,lwd=1.25,lty=2)

plot(0:1,0:1,ann=F,axes=F,type="n")
legend(0.5,1,legend=c(cat.abund.xwalk$abund),
       pch=c(rep(21,length(cat.abund.xwalk$abund)),22),lty=c(NA),
       lwd=c(rep(0.01,length(cat.abund.xwalk$abund)),2),
       col=c(rep("black",length(cat.abund.xwalk$abund)),"red"),
       pt.bg=c(abund.cols,NA),
       pt.cex=2,ncol=1,cex=0.8,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=1.25,
       title.adj = 0,title="Red Tide Abundance")

dev.off()


kbrdat.shp$WY=WY(kbrdat.shp$date)
abund.cols2=adjustcolor(abund.cols,0.75)
# png(filename=paste0(plot.path,"202303_Conditions/HAB_TS_20182023_v2.png"),width=7.25,height=4,units="in",res=200,type="windows",bg="white")
layout(matrix(c(1:4,5,5),2,3,byrow=F),widths = c(1,1,0.5))
par(family="serif",mar=c(0.1,0.1,0.1,0.1),oma=c(0.5,0.5,1,0.5))
bbox.lims=bbox(spTransform(gBuffer(spTransform(HABext.poly,utm17),width=2000),wgs84))

plot(shore,col="cornsilk",border="grey",bg="lightblue",ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],lwd=0.01)
tmp=subset(kbrdat.shp,WY==2018)
col.vals=findInterval(log10(tmp$cellcount),c(0,3,4,5,6,7))
# col.vals[col.vals==0]=1
plot(tmp,add=T,pch=21,bg=abund.cols2[col.vals],col=abund.cols[col.vals],lwd=0.01,cex=1.25)
plot(subset(tmp,log10(cellcount)>6),add=T,pch=21,bg=abund.cols2[5],col=abund.cols[5],lwd=0.01,cex=1.25)
plot(HABext.poly,add=T,lty=2)
mtext(side=3,line=-1.3,adj=0," WY2018",col="red",font=2,cex=0.75)
box(lwd=1)

plot(shore,col="cornsilk",border="grey",bg="lightblue",ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],lwd=0.01)
tmp=subset(kbrdat.shp,WY==2023)
col.vals=findInterval(log10(tmp$cellcount),c(3,4,5,6,7))
col.vals[col.vals==0]=1
plot(tmp,add=T,pch=21,bg=abund.cols2[col.vals],col=abund.cols[col.vals],lwd=0.01,cex=1.25)
plot(subset(tmp,log10(cellcount)>6),add=T,pch=21,bg=abund.cols2[5],col=abund.cols[5],lwd=0.01,cex=1.25)
plot(HABext.poly,add=T,lty=2)
mtext(side=3,line=-1.3,adj=0," WY2023",col="red",font=2,cex=0.75)
mapmisc::scaleBar(wgs84,"bottomleft",bty="n",cex=1,seg.len=4,outer=F)
box(lwd=1)


par(mar=c(2.5,6,0.25,0.25));
ylim.val=c(0,6);by.y=1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(1,365);by.x=180;by.x2=30;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x2)
xlim.lab=seq(as.Date("2018-05-01"),as.Date("2019-05-01"),"6 months")

plot(mean.log.val~hydro.day,da.mean.val,ylim=ylim.val,xlim=xlim.val,type="n",axes=F,ann=F)
abline(h=ymaj,v=xmaj,lty=3,col="grey")
col.vals=findInterval(subset(da.mean.val,WY==2018)$mean.log.val,c(0,3,4,5,6,7))
# col.vals[col.vals==0]=1
with(subset(da.mean.val,WY==2018),segments(hydro.day,0,hydro.day,mean.log.val,col=adjustcolor("black",0.5)))
with(subset(da.mean.val,WY==2018),points(hydro.day,mean.log.val,pch=21,cex=1,lwd=0.01,bg=abund.cols[col.vals]))# adjustcolor("black",0.5)))
axis_fun(1,xmaj,xmin,format(xlim.lab,"%b"),line=-0.5)
axis_fun(2,ymaj,ymaj,format(c(0,10^(ymaj[2:7])),scientific = F,big.mark=","));box(lwd=1)
mtext(side=3,line=-1.25,adj=0," WY2018",col="red",font=2,cex=0.75)
# abline(h=c(3,4,5,6),col=abund.cols,lwd=1.25,lty=2)
mtext(side=3,"Spatially Averaged Red Tide Abundance")

plot(mean.log.val~hydro.day,da.mean.val,ylim=ylim.val,xlim=xlim.val,type="n",axes=F,ann=F)
abline(h=ymaj,v=xmaj,lty=3,col="grey")
col.vals=findInterval(subset(da.mean.val,WY==2023)$mean.log.val,c(0,3,4,5,6,7))
# col.vals[col.vals==0]=1
with(subset(da.mean.val,WY==2023),segments(hydro.day,0,hydro.day,mean.log.val,col=adjustcolor("black",0.5)))
with(subset(da.mean.val,WY==2023),points(hydro.day,mean.log.val,pch=21,cex=1,lwd=0.01,bg=abund.cols[col.vals]))
axis_fun(1,xmaj,xmin,format(xlim.lab,"%b"),line=-0.5)
axis_fun(2,ymaj,ymaj,format(c(0,10^(ymaj[2:7])),scientific = F,big.mark=","));box(lwd=1)
mtext(side=3,line=-1.25,adj=0," WY2023",col="red",font=2,cex=0.75)
mtext(side=2,line=-23.5,expression(paste(italic("K. brevis")," (cells L"^"-1",")")),outer=T)
mtext(side=1,line=1.75,"Month")

abund.val3=c("not present/background", "very low",
            "low", "medium","high")
par(mar=c(2.5,1,0.25,0.25));
plot(0:1,0:1,ann=F,axes=F,type="n")
legend("center",legend=c(abund.val3),
       pch=c(rep(21,length(cat.abund.xwalk$abund))),lty=c(NA),
       lwd=c(rep(0.01,length(cat.abund.xwalk$abund))),
       col=c("black"),
       pt.bg=c(abund.cols),
       pt.cex=2,ncol=1,cex=0.8,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=1.25,
       title.adj = 0,title="Red Tide Abundance")

dev.off()



kbrdat.shp$WY=WY(kbrdat.shp$date)
abund.cols2=adjustcolor(abund.cols,0.75)
# png(filename=paste0(plot.path,"202303_Conditions/HAB_TS_20182023_v3.png"),width=7.25,height=4,units="in",res=200,type="windows",bg="white")
layout(matrix(c(1:4,5,5),2,3,byrow=F),widths = c(1,1,0.5))
par(family="serif",mar=c(0.1,0.1,0.1,0.1),oma=c(0.5,0.5,1,0.5))
bbox.lims=bbox(spTransform(gBuffer(spTransform(HABext.poly,utm17),width=2000),wgs84))

plot(shore,col="cornsilk",border="grey",bg="lightblue",ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],lwd=0.01)
tmp=subset(kbrdat.shp,WY==2018)
col.vals=findInterval(log10(tmp$cellcount),c(0,3,4,5,6,7))
# col.vals[col.vals==0]=1
plot(tmp,add=T,pch=21,bg=abund.cols2[col.vals],col=abund.cols[col.vals],lwd=0.01,cex=1.25)
plot(subset(tmp,log10(cellcount)>6),add=T,pch=21,bg=abund.cols2[5],col=abund.cols[5],lwd=0.01,cex=1.25)
plot(HABext.poly,add=T,lty=2)
mtext(side=3,line=-1.3,adj=0," WY2018",col="red",font=2,cex=0.75)
box(lwd=1)

plot(shore,col="cornsilk",border="grey",bg="lightblue",ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],lwd=0.01)
tmp=subset(kbrdat.shp,WY==2023)
col.vals=findInterval(log10(tmp$cellcount),c(3,4,5,6,7))
col.vals[col.vals==0]=1
plot(tmp,add=T,pch=21,bg=abund.cols2[col.vals],col=abund.cols[col.vals],lwd=0.01,cex=1.25)
plot(subset(tmp,log10(cellcount)>6),add=T,pch=21,bg=abund.cols2[5],col=abund.cols[5],lwd=0.01,cex=1.25)
plot(HABext.poly,add=T,lty=2)
mtext(side=3,line=-1.3,adj=0," WY2023",col="red",font=2,cex=0.75)
mapmisc::scaleBar(wgs84,"bottomleft",bty="n",cex=1,seg.len=4,outer=F)
box(lwd=1)


par(mar=c(2.5,6.5,0.25,0.25));
ylim.val=c(0,7);by.y=1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(1,365);by.x=180;by.x2=30;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x2)
xlim.lab=seq(as.Date("2018-05-01"),as.Date("2019-05-01"),"6 months")

plot(cellcount~hydro.day,subset(kbrdat,WY==2018),ylim=ylim.val,xlim=xlim.val,type="n",axes=F,ann=F)
abline(h=ymaj,v=xmaj,lty=3,col="grey")
col.vals=findInterval(subset(kbrdat,WY==2018)$cellcount.log,c(0,3,4,5,6,7))
with(subset(kbrdat,WY==2018),points(hydro.day,cellcount.log,pch=21,cex=1,lwd=0.01,bg=abund.cols2[col.vals]))# adjustcolor("black",0.5)))
axis_fun(1,xmaj,xmin,format(xlim.lab,"%b"),line=-0.5)
axis_fun(2,ymaj,ymaj,format(c(0,10^(ymaj[2:length(ymaj)])),scientific = F,big.mark=","));box(lwd=1)
mtext(side=3,line=-1.25,adj=0," WY2018",col="red",font=2,cex=0.75)
abline(v=hydro.day(as.Date("2017-09-10")))

plot(cellcount~hydro.day,subset(kbrdat,WY==2023),ylim=ylim.val,xlim=xlim.val,type="n",axes=F,ann=F)
abline(h=ymaj,v=xmaj,lty=3,col="grey")
col.vals=findInterval(subset(kbrdat,WY==2023)$cellcount.log,c(0,3,4,5,6,7))
with(subset(kbrdat,WY==2023),points(hydro.day,cellcount.log,pch=21,cex=1,lwd=0.01,bg=abund.cols2[col.vals]))# adjustcolor("black",0.5)))
axis_fun(1,xmaj,xmin,format(xlim.lab,"%b"),line=-0.5)
axis_fun(2,ymaj,ymaj,format(c(0,10^(ymaj[2:length(ymaj)])),scientific = F,big.mark=","));box(lwd=1)
mtext(side=3,line=-1.25,adj=0," WY2023",col="red",font=2,cex=0.75)
abline(v=hydro.day(as.Date("2022-09-28")))


mtext(side=2,line=-23.5,expression(paste(italic("K. brevis")," (cells L"^"-1",")")),outer=T)
mtext(side=1,line=1.75,"Month")

abund.val3=c("not present/background", "very low",
             "low", "medium","high")
par(mar=c(2.5,1,0.25,0.25));
plot(0:1,0:1,ann=F,axes=F,type="n")
legend("center",legend=c(abund.val3,"Hurricane Landfall"),
       pch=c(rep(21,length(cat.abund.xwalk$abund)),NA),lty=c(rep(NA,length(cat.abund.xwalk$abund)),1),
       lwd=c(rep(0.01,length(cat.abund.xwalk$abund)),1),
       col=c("black"),
       pt.bg=c(abund.cols,NA),
       pt.cex=2,ncol=1,cex=0.8,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=1.25,
       title.adj = 0,title="Red Tide Abundance")

dev.off()

kbrdat$category=findInterval(kbrdat$cellcount.log,c(0,3,4,5,6,7))
kbrdat$category=cat.abund.xwalk$category[kbrdat$category]
rslt1 =ddply(subset(kbrdat,WY==2018),"category",summarise,N.val=N.obs(category))
rslt1$category=factor(rslt1$category,levels=cat.abund.xwalk$category)
rslt1=rslt1[order(rslt1$category),]

rslt2 =ddply(subset(kbrdat,WY==2023),"category",summarise,N.val=N.obs(category))
rslt2$category=factor(rslt2$category,levels=cat.abund.xwalk$category)
rslt2=rslt2[order(rslt2$category),]

library(flextable)
library(magrittr)

  cap.val="Count of samples within the Area of Interest during WY2018\n(May 1, 2017 - April 30, 2018)."
  rslt1[,c("category","N.val")]%>%
    flextable()%>%
    colformat_num(j=2,big.mark = "",digits=0,na_str = "---")%>%
    set_header_labels("category"="Relative Abundance",
                      "N.val"="Number\nof\nSamples")%>%
    align(j=2,align="center",part="all")%>%
    width(width=c(1.5,1.5))%>%
    padding(padding=2,part="all")%>%
    fontsize(size=10,part="body")%>%
    fontsize(size=12,part="header")%>%
    add_header_lines(values=cap.val)%>%
    align(align="center",part="header")%>%fontsize(size=12,part="header")%>%
    font(fontname = "Times New Roman",part="all")#%>%print("docx")
  
  cap.val="Count of samples within the Area of Interest during WY2023\n(May 1, 2022 - April 30, 2023)\ncirca March 7, 2023."
  rslt2[,c("category","N.val")]%>%
    flextable()%>%
    colformat_num(j=2,big.mark = "",digits=0,na_str = "---")%>%
    set_header_labels("category"="Relative Abundance",
                      "N.val"="Number\nof\nSamples")%>%
    align(j=2,align="center",part="all")%>%
    width(width=c(1.5,1.5))%>%
    padding(padding=2,part="all")%>%
    fontsize(size=10,part="body")%>%
    fontsize(size=12,part="header")%>%
    add_header_lines(values=cap.val)%>%
    align(align="center",part="header")%>%fontsize(size=12,part="header")%>%
    font(fontname = "Times New Roman",part="all")#%>%print("docx")
  

# png(filename=paste0(plot.path,"202303_Conditions/HAB_Q_2018.png"),width=6.5,height=5,units="in",res=200,type="windows",bg="white")
layout(matrix(1:2,2,1,byrow=F),widths = c(1,1))
par(family="serif",mar=c(2,4,1,1),oma=c(1,3,0.5,2));
  
ylim.max=signif(max(cal.flow.dat.xtab$cum.S79),0)
ylim.val=c(0,ylim.max);by.y=ylim.max/4;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(1,365);by.x=180;by.x2=30;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x2)
xlim.lab=seq(as.Date("2017-05-01"),as.Date("2018-05-01"),"6 months")

plot(cum.S79~hydro.day,cal.flow.dat.xtab,type="n",ylim=ylim.val,xlim=xlim.val,xaxs="i",yaxs="i",xaxt="n",yaxt="n",xlab=NA,ylab=NA)
abline(h=ymaj,v=xmaj,lty=3,col="grey")
with(subset(cal.flow.dat.xtab,WY==2018),lines(hydro.day,cum.S77,lwd=2.5,col=adjustcolor("forestgreen",0.5)))
axis_fun(1,xmaj,xmin,format(xlim.lab,"%b %Y"),line=-0.5)
axis_fun(2,ymaj,ymin,ymaj/1000,cex.axis=1);box(lwd=lwd.val)
# mtext("Day of Water Year",side=1,line=1.75,cex=1.25)
mtext("Cumulative Discharge\n(kAc-Ft D\u207B\u00B9)",side=2,line=3.5,cex=1)
mtext(side=3,"S-77",adj=0)

ylim.val=c(0,7);by.y=1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(cellcount~hydro.day,subset(kbrdat,WY==2018),ylim=ylim.val,xlim=xlim.val,type="n",axes=F,ann=F)
abline(h=ymaj,v=xmaj,lty=3,col="grey")
col.vals=findInterval(subset(kbrdat,WY==2018)$cellcount.log,c(0,3,4,5,6,7))
with(subset(kbrdat,WY==2018),points(hydro.day,cellcount.log,pch=21,cex=1,lwd=0.01,bg=abund.cols2[col.vals]))# adjustcolor("black",0.5)))
axis_fun(1,xmaj,xmin,format(xlim.lab,"%b %Y"),line=-0.5)
axis_fun(2,ymaj,ymaj,format(c(0,10^(ymaj[2:length(ymaj)])),scientific = F,big.mark=","));box(lwd=1)
mtext(side=3,line=-1.25,adj=0," WY2018",col="red",font=2,cex=0.75)
mtext(side=2,line=5,expression(paste(italic("K. brevis")," (cells L"^"-1",")")),outer=F)
mtext(side=1,line=1.75,"Month")
dev.off()

# png(filename=paste0(plot.path,"202303_Conditions/HAB_Q_2023.png"),width=6.5,height=5,units="in",res=200,type="windows",bg="white")
layout(matrix(1:2,2,1,byrow=F),widths = c(1,1))
par(family="serif",mar=c(2,4,1,1),oma=c(1,3,0.5,2));

ylim.max=signif(max(cal.flow.dat.xtab$cum.S79),0)
ylim.val=c(0,ylim.max);by.y=ylim.max/4;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(1,365);by.x=180;by.x2=30;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x2)
xlim.lab=seq(as.Date("2022-05-01"),as.Date("2023-05-01"),"6 months")

plot(cum.S79~hydro.day,cal.flow.dat.xtab,type="n",ylim=ylim.val,xlim=xlim.val,xaxs="i",yaxs="i",xaxt="n",yaxt="n",xlab=NA,ylab=NA)
abline(h=ymaj,v=xmaj,lty=3,col="grey")
with(subset(cal.flow.dat.xtab,WY==2023),lines(hydro.day,cum.S77,lwd=2.5,col=adjustcolor("forestgreen",0.5)))
axis_fun(1,xmaj,xmin,format(xlim.lab,"%b %Y"),line=-0.5)
axis_fun(2,ymaj,ymin,ymaj/1000,cex.axis=1);box(lwd=lwd.val)
# mtext("Day of Water Year",side=1,line=1.75,cex=1.25)
mtext("Cumulative Discharge\n(kAc-Ft D\u207B\u00B9)",side=2,line=3.5,cex=1)
mtext(side=3,"S-77",adj=0)

ylim.val=c(0,7);by.y=1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(cellcount~hydro.day,subset(kbrdat,WY==2023),ylim=ylim.val,xlim=xlim.val,type="n",axes=F,ann=F)
abline(h=ymaj,v=xmaj,lty=3,col="grey")
col.vals=findInterval(subset(kbrdat,WY==2023)$cellcount.log,c(0,3,4,5,6,7))
with(subset(kbrdat,WY==2023),points(hydro.day,cellcount.log,pch=21,cex=1,lwd=0.01,bg=abund.cols2[col.vals]))# adjustcolor("black",0.5)))
axis_fun(1,xmaj,xmin,format(xlim.lab,"%b %Y"),line=-0.5)
axis_fun(2,ymaj,ymaj,format(c(0,10^(ymaj[2:length(ymaj)])),scientific = F,big.mark=","));box(lwd=1)
mtext(side=3,line=-1.25,adj=0," WY2023",col="red",font=2,cex=0.75)
mtext(side=2,line=5,expression(paste(italic("K. brevis")," (cells L"^"-1",")")),outer=F)
mtext(side=1,line=1.75,"Month")
dev.off()




# cross correlation -------------------------------------------------------

da.mean.val
da.mean.val$month=as.numeric(format(da.mean.val$date,'%m'))
da.mean.val$CY=as.numeric(format(da.mean.val$date,'%Y'))


tmpQ=cal.flow.dat.xtab[,c("Date","S77","S79")]
tmpQ$month=as.numeric(format(tmpQ$Date,'%m'))
tmpQ$CY=as.numeric(format(tmpQ$Date,'%Y'))

attributes(tmpQ$Date)
attributes(da.mean.val$date)


tmpQ.kbr=merge(tmpQ,da.mean.val,by.x="Date",by.y="date",all.x=T)

with(na.omit(tmpQ.kbr),ccf(cellcount.backtrans,S77))
with(na.omit(tmpQ.kbr),ccf(cellcount.backtrans,S79))

with(na.omit(tmpQ.kbr),ccf(mean.val,S77))
tmp=with(na.omit(tmpQ.kbr),ccf(mean.log.val,S77))
tmp=with(na.omit(tmpQ.kbr),ccf(mean.log.val,S79))
tmp
tmp2=na.omit(tmpQ.kbr)

h=-8;#-23
  lagged=lag(zoo::as.zoo(tmp2$S79),h,na.pad=T)
  tmp.dat=zoo::as.zoo(tmp2$mean.log.val)
  with(data.frame(lag=lagged,dat=tmp.dat),cor.test(lag,dat,method="pearson"))
  
ccf.pearsons


plot(lagged~tmp.dat)
###
tmpQ.month=ddply(tmpQ,c("month",'CY'),summarise,mean.S79=mean(S79,na.rm=T),mean.S77=mean(S77,na.rm=T))
da.mean.val.month=ddply(da.mean.val,c("month",'CY'),summarise,mean.val=mean(mean.log.val,na.rm=T))


tmpQ.kbr2=merge(tmpQ.month,da.mean.val.month,c("month",'CY'),all.x=T)
tmpQ.kbr2=tmpQ.kbr2[order(tmpQ.kbr2$CY,tmpQ.kbr2$month),]
# tmpQ.kbr2=subset(tmpQ.kbr2,CY>2013)
range(tmpQ.kbr2$CY)

ccfS79=with(na.omit(tmpQ.kbr2),ccf(mean.S79,mean.val))
ccfS77=with(na.omit(tmpQ.kbr2),ccf(mean.S77,mean.val))

h=-5;#-23
lagged=lag(zoo::as.zoo(tmpQ.kbr2$mean.S79),h,na.pad=T)
tmp.dat=zoo::as.zoo(tmpQ.kbr2$mean.val)
with(data.frame(lag=lagged,dat=tmp.dat),cor.test(lag,dat,method="pearson"))
