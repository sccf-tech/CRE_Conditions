

## Title:      WCA3A Data vis
## Created by: Paul Julian (pjulian@evergladesfoundation.org)
## Created on: 2023-07-07

## BAD ## https://www.tidyverse.org/articles/2017/12/workflow-vs-script/
#Clears Everything...start fresh.
rm(list=ls(all=T));cat("\014");dev.off()

# Libraries
# devtools::install_github("SwampThingPaul/AnalystHelper")
library(AnalystHelper);
library(plyr)
library(reshape2)

#Paths
wd="C:/Julian_LaCie/_GitHub/CRE_Conditions"
paths=paste0(wd,c("/Plots/","/Export/","/Data/","/GIS"))
# Folder.Maker(paths);#One and done. Creates folders in working directory.
plot.path=paths[1]
export.path=paths[2]
data.path=paths[3]
GIS.path=paths[4]


# -------------------------------------------------------------------------


dates=date.fun(c("1962-01-01","2023-10-20"))
dat=DBHYDRO_daily(dates[1],dates[2],"15943")
dat$Date.EST=date.fun(dat$Date)

dat$DOY=as.numeric(format(dat$Date.EST,"%j"))
dat$CY=as.numeric(format(dat$Date.EST,"%Y"))
dat$diff.val=c(NA,diff(dat$Data.Value))

dat$Data.Value=with(dat,ifelse(diff.val<(-2),NA,Data.Value))
dat$Data.Value=with(dat,ifelse(diff.val>0.5,NA,Data.Value))

plot(Data.Value~Date.EST,dat)
plot(diff.val~Date.EST,dat)

EHWL=data.frame(Date.EST=date.fun(c("2023-01-01","2023-05-01","2023-10-31","2023-12-31")),
                val=c(12,11,12,12))
EHWL$DOY=as.numeric(format(EHWL$Date.EST,"%j"))

yrs=seq(1962,2023,1)
# cols=viridis::turbo(length(yrs),alpha=0.5)
cols=wesanderson::wes_palette("Zissou1",length(yrs),"continuous")
ylim.val=c(4.5,13);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val.date=date.fun(c("2023-01-01","2023-12-31"));xmaj.date=seq(xlim.val.date[1],xlim.val.date[2],"3 months");xmin.date=seq(xlim.val.date[1],xlim.val.date[2],"1 months")
xlim.val=as.numeric(format(xlim.val.date,"%j"));xmaj=as.numeric(format(xmaj.date,"%j"));xmin=as.numeric(format(xmin.date,"%j"))

for(i in 1:length(yrs)){
  yrs2=yrs[1]:(yrs[i]-1)
# png(filename=paste0(plot.path,"WCA3A/WCA3A_stage_",yrs[i],".png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1.5,1.5,1,0.5),oma=c(1,2.5,0.25,0.25));
layout(matrix(1:2,1,2),widths=c(1,0.4))

plot(Data.Value~DOY,dat,type="n",ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,xaxs="i")
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
for(j in 1:length(yrs2)){
# lines(Data.Value~DOY,subset(dat,CY==yrs2[j]),col=cols[j],lwd=2)
  lines(Data.Value~DOY,subset(dat,CY==yrs2[j]),col=adjustcolor("grey",0.25),lwd=1.5)
}
lines(Data.Value~DOY,subset(dat,CY==yrs[i]),col=cols[i],lwd=2)
axis_fun(1,xmaj,xmin,format(xmaj.date,"%b"),line=-0.5);axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=2.5,"Stage Elevation (Ft, NGVD29)")
mtext(side=1,line=1.25,"Day of Year")
mtext(side=3,adj=0,"WCA-3A (3-63, 3-64, 3-65 Average)")
mtext(side=3,adj=0,line=-1,paste0(" Year: ", yrs[i]))
abline(h=7.5,lty=2,col="red",lwd=0.75)
# lines(val~DOY,EHWL,col="red")
# if(yrs[i]==2023){
#   points(Data.Value~DOY,subset(dat,Date.EST==date.fun("2023-10-01")),pch=21,bg=NA)
#   with(subset(dat,Date.EST==date.fun("2023-10-01")),text(DOY,Data.Value,"Oct",pos=3,offset=0.2))
# }

par(mar=c(2,1,1,0.1))
plot(0:1,0:1,ann=F,axes=F,type="n")
leg.fun(yrs,cols,leg.type="continuous",leg.title="Year",
        x.max=0.4)
legend(0.5,0.1,legend=c("WS Floor"),
       pch=c(NA),pt.bg=c(NA),
       lty=c(2),lwd=c(1),col=c("red"),
       pt.cex=1.25,cex=0.75,ncol=1,bty="n",y.intersp=1.25,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
dev.off()
print(i)
}

# inches = pixels / PPI
# width 6.5*200
# height 4*200

# files <- c(paste0("WCA3A_stage_",yrs,".png"),paste0("WCA3A_stage_",yrs[62],".png"))
# gifski::gifski(file.path(paste0(plot.path,"WCA3A"), files), delay = 60/100, 
#                width=1300,height=800, gif_file = paste0(plot.path,"WCA3A/WCA3A_stage.gif"), 
#                progress = T, loop = T)
# beepr::beep(4)




# USACE Webscraping Data --------------------------------------------------
dates=seq(date.fun("2023-01-01"),date.fun("2023-10-20"),"1 days")

## Data scrap
## Maps and Archived data
map.q=data.frame()
for(i in 1:(length(dates)-1)){
  map.url=paste0("https://w3.saj.usace.army.mil/h2o/reports/StatusDaily/archive/",format(dates[i],"%m%d"),"/StatusDaily.htm")
  # download.file(map.url, destfile = "map_dat.html", quiet=TRUE)
  mapdata=readLines(map.url)
  val=grep("CA1IN",mapdata)
  WCA1=strsplit(strsplit(mapdata[val],"\\s+")[[1]][13],"</div>")[[1]][1]
  
  val=grep("CA2IN",mapdata)
  WCA2=strsplit(strsplit(mapdata[val],"\\s+")[[1]][13],"</div>")[[1]][1]
  
  val=grep("CA3IN",mapdata)
  WCA3=strsplit(strsplit(mapdata[val],"\\s+")[[1]][13],"</div>")[[1]][1]

  val=grep("S10",mapdata)
  S10s=as.numeric(strsplit(strsplit(mapdata[val],"\\s+")[[1]][8],"</div>")[[1]][1])
  
  
  val=grep("S11",mapdata)
  S11s=as.numeric(strsplit(strsplit(mapdata[val],"\\s+")[[1]][8],"</div>")[[1]][1])
    
  val=grep("S12",mapdata)
  S12s=as.numeric(strsplit(strsplit(mapdata[val],"\\s+")[[1]][8],"</div>")[[1]][1])
  
  val=grep("S333",mapdata)
  S333=as.numeric(strsplit(mapdata[val],"<br>|\\s+")[[1]][12])
  S333N=as.numeric(strsplit(strsplit(mapdata[val],"<br>|\\s+")[[1]][14],"</div>")[[1]][1])
  
  val=grep("S356",mapdata)
  S356=as.numeric(strsplit(strsplit(mapdata[val],"\\s+")[[1]][8],"</div>")[[1]][1])
  
  ENP=S12s+(S333+S333N)-S356
  
  
  # date.val=dates[i]-ddays(1)
  date.val=dates[i]
  rslt=data.frame(Date=date.val,
                  WCA1=as.numeric(WCA1),
                  WCA2=as.numeric(WCA2),
                  WCA3=as.numeric(WCA3),
                  S10s=as.numeric(S10s),
                  S11s=as.numeric(S11s),
                  S12s=as.numeric(S12s),
                  S333=as.numeric(S333),
                  S333N=as.numeric(S333N),
                  S356=as.numeric(S356),
                  ENP=as.numeric(ENP))
  
  map.q=rbind(map.q,rslt)
  print(i)
}

subset(map.q,EAA_WCA3<0)
map.q$WCA2_in=with(map.q,ifelse(WCA2<0,0,WCA2))
# map.q$S10s_in=with(map.q,ifelse(S10s<0,0,S10s))

map.q$EAA_WCA1=apply(map.q[,c("S10s","WCA1")],1,min,na.rm=T)# with(map.q,pmax(0,S10s-WCA1))
map.q$EAA_WCA2=apply(map.q[,c("S11s","WCA2_in")],1,min,na.rm=T)# with(map.q,pmax(0,S10s-WCA1))# with(map.q,pmax(0,S11s-WCA2)+EAA_WCA1)

map.q$EAA_WCA3=with(map.q,WCA3+EAA_WCA2)
map.q$EAA_WCA3.kacft=cfs.to.acftd(map.q$EAA_WCA3)/1000
map.q$EAA_WCA3.kacft.MA=with(map.q,c(rep(NA,6),zoo::rollapply(EAA_WCA3.kacft,7,mean,na.rm=T)))

map.q$WCA3_S11s=with(map.q,WCA3+S11s)
map.q$WCA3_S11s.kacft=cfs.to.acftd(map.q$WCA3_S11s)/1000

WCA3A.area=493456.13;# acres

map.q$per_EAA=with(map.q,zoo::na.approx(EAA_WCA3.kacft/WCA3_S11s.kacft)*100)

plot((cfs.to.acftd(map.q$EAA_WCA3))/(WCA3A.area))
# png(filename=paste0(plot.path,"WCA3A/EAA_QWCA3.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1.5,1.5,1,0.5),oma=c(2,2.5,0.25,0.25));

ylim.val=c(0,13);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=date.fun(c("2023-01-01","2023-12-31"));xmaj=seq(xlim.val.date[1],xlim.val.date[2],"3 months");xmin=seq(xlim.val.date[1],xlim.val.date[2],"1 months")

plot(EAA_WCA3.kacft~Date,map.q,type="n",ylim=ylim.val,xlim=xlim.val,ann=F,axes=F)
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
with(map.q,pt_line(Date,EAA_WCA3.kacft,2,"grey50",1,21,"dodgerblue1",cex=1.25,pt.lwd=0.01))
lines(EAA_WCA3.kacft.MA~Date,map.q,col=adjustcolor("red",0.5),lwd=2)
lines(WCA3_S11s.kacft~Date,map.q,col=adjustcolor("darkolivegreen",0.5),lwd=2)
legend("topleft",legend=c("EAA to WCA3","Total WCA3A inflow","7-day moving average"),
       pch=c(21,NA,NA),pt.bg=c("dodgerblue1",NA,NA),
       lty=c(NA,1,1),lwd=c(0.01,2,2),col=c("black",adjustcolor("darkolivegreen",0.5),adjustcolor("red",0.5)),
       pt.cex=1.25,cex=0.75,ncol=1,bty="n",y.intersp=1.25,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
axis_fun(1,xmaj,xmin,format(xmaj.date,"%b-%Y"),line=-0.5);axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=2.5,"Discharge (kAc-Ft d\u207B\u00B9)")
mtext(side=1,line=1.5,"Date")
mtext(side=1,line=2.25,adj=1,"EAA to WCA3 + min(S10s,EAA to WCA1) + min(S11s,EAA to WCA2)",cex=0.75,col="grey50",font=3)
dev.off()


# png(filename=paste0(plot.path,"WCA3A/EAA_QWCA3_percent.png"),width=6.5,height=3,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1.5,1.5,1,0.5),oma=c(2,2.5,0.25,0.25));

ylim.val=c(0,100);by.y=20;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=date.fun(c("2023-01-01","2023-12-31"));xmaj=seq(xlim.val.date[1],xlim.val.date[2],"3 months");xmin=seq(xlim.val.date[1],xlim.val.date[2],"1 months")

plot(per_EAA~Date,map.q,type="n",ylim=ylim.val,xlim=xlim.val,ann=F,axes=F)
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
with(map.q,shaded.range(Date,rep(-10,length(Date)),per_EAA,"grey",lty=0))
lines(per_EAA~Date,map.q,col="dodgerblue1",lwd=2)
axis_fun(1,xmaj,xmin,format(xmaj.date,"%b-%Y"),line=-0.5);axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=2.5,"Percent Discharge from EAA")
mtext(side=1,line=1.5,"Date")

dev.off()
