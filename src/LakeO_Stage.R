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

## Paths
wd="C:/Julian_LaCie/_Github/CRE_Conditions"

paths=paste0(wd,c("/Plots/","/Export/","/Data/","/src"))
# Folder.Maker(paths);#One and done. Creates folders in working directory.
plot.path=paths[1]
export.path=paths[2]
data.path=paths[3]

#Current WY
CurWY=WY(Sys.time())
CurWY
# -------------------------------------------------------------------------
TODAY=date.fun(Sys.time())
YEST=TODAY-duration(1,"days")
dates=c(date.fun(paste(CurWY-5,"05-01",sep="-")),date.fun(as.Date(Sys.time())+duration(1,"days")))

#LORS
load("C:/Julian_LaCie/_GitHub/CRE_Conditions/Data/LORS.Rdata")
LORS$Year=CurWY-1
LORS$Date2=with(LORS,date.fun(paste(Year,Month,Day,sep="-")))

LORS2=LORS
LORS2$Year=CurWY
LORS2$Date2=with(LORS2,date.fun(paste(Year,Month,Day,sep="-")))

LORS.gaph2Yrs=rbind(LORS,LORS2)
rm(LORS,LORS2)
LORS.gaph2Yrs$Date2=date.fun(LORS.gaph2Yrs$Date2)

#Lake Okeechobee
comp.dbkey=data.frame(DBKEY=c("N3466","06832"),Priority=c("P1","P2"))

stg.da=data.frame()
for(i in 1:nrow(comp.dbkey)){
  tmp=DBHYDRO_daily(dates[1],dates[2],comp.dbkey$DBKEY[i])
  tmp$DBKEY=as.character(comp.dbkey$DBKEY[i])
  stg.da=rbind(stg.da,tmp)
}
stg.da=merge(stg.da,comp.dbkey,"DBKEY")
stg.da$DATE=date.fun(stg.da$Date)

LakeO.xtab=cast(stg.da,DATE~Priority,value="Data.Value",mean)
LakeO.xtab$Mean=with(LakeO.xtab,ifelse(is.na(P1)==T,P2,P1))

tail(LakeO.xtab)


if(max(LakeO.xtab$DATE)!=YEST|sum(is.na(subset(LakeO.xtab,DATE%in%seq(YEST-ddays(1),YEST,"1 days"))$Mean))==1){
  da.dbks=data.frame(SITE=c("L001","L005","L006","LZ40","S133TW","S352HW","S4TW"),DBKEY=c("16022","12509","12519","16265","15826","FF579","15732"),type="Daily")
  date1=if(max(LakeO.xtab$DATE)!=YEST){max(LakeO.xtab$DATE)}else{YEST}
  tmp=DBHYDRO_daily(date1,YEST,da.dbks$DBKEY)
  tmp$DATE=date.fun(tmp$Date)
  tmp2=DBHYDRO_breakpoint(date1,YEST,"AI522")
  tmp2$DATE=date.fun(tmp2$DATE)
  tmp2.mean=ddply(tmp2,"DATE",summarise,Data.Value=round(mean(Data.Value,na.rm=T),2))
  tmp2.mean$Station="S308HW"
  
  tmp.all=rbind(tmp[,c("DATE","Station","Data.Value")],tmp2.mean[,c("DATE","Station","Data.Value")])
  calc.LakeO.stg=ddply(tmp.all,"DATE",summarise,Mean=round(mean(Data.Value,na.rm=T),2))
  calc.LakeO.stg[,c("P1","P2")]=NA
  LakeO.xtab=rbind(LakeO.xtab,calc.LakeO.stg[,names(LakeO.xtab)])
  LakeO.xtab=LakeO.xtab[duplicated(LakeO.xtab$DATE,fromLast = T)==F,]
}

LakeO.xtab=merge(x=LakeO.xtab,y=LORS.gaph2Yrs,by.x="DATE",by.y="Date2",all.x=T)

LakeO.xtab$Status=with(LakeO.xtab,ifelse(Mean>High,"within the High Lake",ifelse(Mean<High&Mean>Intermediate,"within the Operational High Sub-",ifelse(Mean<Intermediate&Mean>Low,"within the Operational Intermediate Sub-",ifelse(Mean<Low&Mean>BaseFlow,"within the Operational Low Sub-",ifelse(Mean<BaseFlow&Mean>BeneficialUse,"within the Operational Base Flow Sub-",ifelse(Mean<BeneficialUse&Mean>WSM,"within the Beneficial Use",ifelse(Mean<WSM,"within the Water Shortage Management",NA))))))))

LakeO.xtab$StatusLow=with(LakeO.xtab,ifelse(Mean<Low,"Below","Above"))
LakeO.xtab$DiffLow=with(LakeO.xtab,Mean-Low)

LakeO.xtab$recess_7day=with(LakeO.xtab,c(rep(NA,7),diff(Mean,lag=7)))
LakeO.xtab$recess_30day=with(LakeO.xtab,c(rep(NA,30),diff(Mean,lag=30)))

LakeO.xtab$WY=WY(LakeO.xtab$DATE)

LakeO.xtab$month=as.numeric(format(LakeO.xtab$DATE,"%m"))
LakeO.xtab$day=as.numeric(format(LakeO.xtab$DATE,"%d"))
LakeO.xtab$plot.dat=with(LakeO.xtab,date.fun(ifelse(month>4,paste(CurWY-1,month,day,sep="-"),paste(CurWY,month,day,sep="-"))))
LakeO.xtab$hydro.day=hydro.day(LakeO.xtab$DATE)



####
HighLakeLab.x=as.POSIXct(paste(CurWY-1,9,1,sep="-"))
WSMLab.x=as.POSIXct(paste(CurWY-1,9,1,sep="-"))
BENLab.x=as.POSIXct(paste(CurWY-1,7,1,sep="-"))
BASELab.x=as.POSIXct(paste(CurWY,4,1,sep="-"))
HIGHLab.x=as.POSIXct(paste(CurWY,3,15,sep="-"))
InterLab.x=as.POSIXct(paste(CurWY,3,1,sep="-"))
LowLab.x=as.POSIXct(paste(CurWY,2,15,sep="-"))

lwd.val=1
xlim.vals=as.POSIXct(strptime(c(as.Date(paste(CurWY-1,05,01,sep="-")),as.Date(paste(CurWY,05,01,sep="-"))),"%Y-%m-%d"),tz="EST")#as.POSIXct(strptime(dates,"%Y-%m-%d"),tz="EST")
xmaj=seq(xlim.vals[1],xlim.vals[2],by="2 months");xmin=seq(xlim.vals[1],xlim.vals[2],by="1 months")
ylim.val=c(9,18);by.y=1
ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

# cols=wesanderson::wes_palette("Zissou1",4,"continuous")
cols=c("red","blue","forestgreen","thistle1")
# png(filename=paste0(plot.path,"okeechobee_Stage.png"),width=7,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(0.5,2,0.5,1),oma=c(0.5,3,0.5,1));
layout(matrix(1:2,2,1,byrow=T),heights=c(1,0.25))
plot(High~Date2,LORS.gaph2Yrs,ylim=ylim.val,xlim=xlim.vals,type="n",lwd=2,ylab=NA,xlab=NA,yaxs="i",xaxs="i",xaxt="n",yaxt="n")

xx=with(LORS.gaph2Yrs,c(Date2,rev(Date2)))
with(LORS.gaph2Yrs,polygon(xx,c(High,rep(18,length(High))),col=rgb(255/255,76/255,76/255)))
with(LORS.gaph2Yrs,polygon(xx,c(High,rev(Low)),col=rgb(255/255,173/255,51/255)))
with(LORS.gaph2Yrs,polygon(xx,c(Low,rev(BaseFlow)),col="grey80"))
with(LORS.gaph2Yrs,polygon(xx,c(BaseFlow,rev(BeneficialUse)),col=rgb(127/255,255/255,127/255)))
with(LORS.gaph2Yrs,polygon(xx,c(BeneficialUse,rev(WSM)),col=rgb(153/255,229/255,255/255)))
with(LORS.gaph2Yrs,polygon(xx,c(WSM,rep(0,length(WSM))),col=rgb(255/255,255/255,204/255),border=0))
with(LORS.gaph2Yrs,lines(High~Date2,lwd=2,col="black"))
with(LORS.gaph2Yrs,lines(Intermediate~Date2,lwd=2,col="black"))
with(LORS.gaph2Yrs,lines(Low~Date2,lwd=2,col="black"))
with(LORS.gaph2Yrs,lines(BaseFlow~Date2,lwd=2,col="black"))
with(LORS.gaph2Yrs,lines(BeneficialUse~Date2,lwd=2,col="black"))
with(LORS.gaph2Yrs,lines(Inter1ft~Date2,lwd=2,lty=5,col="black"))
with(LORS.gaph2Yrs,lines(LowLow~Date2,lwd=2,lty=5,col="grey"))
with(LORS.gaph2Yrs,lines(LowMid~Date2,lwd=2,lty=5,col="grey"))
with(LORS.gaph2Yrs,lines(WSM~Date2,lwd=2,lty=1,col="grey"))
abline(h=seq(9,18,1),v=seq(xlim.vals[1],xlim.vals[2],by="1 months"),lwd=1,col="black",lty=3)
text(HighLakeLab.x,17.5,"High Lake Management Band",font=2)
text(WSMLab.x,9.5,"Water Shortage Management Band",font=2)
text(BENLab.x,12.25,"Beneficial Use",font=2)
text(BASELab.x,13,"Base Flow",font=2)
text(HIGHLab.x,17,"High",font=2)
text(InterLab.x,16.25,"Intermediate ",font=2,cex=0.75)
text(LowLab.x,14.5,"Low",font=2)
# ops.band.date=date.fun(paste(CurWY-1,"06-01",sep="-"))
# ops.band=subset(LORS.gaph2Yrs,Date2==ops.band.date)
# with(ops.band,arrows(ops.band.date,WSM,ops.band.date,High,code=3,length=0.1,lwd=2))
# with(ops.band,text(ops.band.date,14.5,"Operational Band",font=2))
# low.band.date=date.fun(LowLab.x)
# low.band=subset(LORS.gaph2Yrs,Date2==low.band.date)
# with(low.band,arrows(low.band.date,BaseFlow,low.band.date,Low,code=3,length=0.1,lwd=2))     
with(subset(LakeO.xtab,WY==CurWY-3),lines(plot.dat,Mean,lwd=4,col=adjustcolor(cols[4],1),lty=1))
with(subset(LakeO.xtab,WY==CurWY-2),lines(plot.dat,Mean,lwd=4,col=adjustcolor(cols[3],1),lty=1))
with(subset(LakeO.xtab,WY==CurWY-1),lines(plot.dat,Mean,lwd=4,col=adjustcolor(cols[2],1),lty=1))
with(LakeO.xtab,lines(DATE,Mean,lwd=4,col=cols[1],lty=1))
axis_fun(1,xmaj,xmin,format(xmaj,"%b"),line=-0.5)
axis_fun(2,ymaj,ymin,ymaj)
box(lwd=lwd.val)

mtext(side=1,"Month",line=2,cex=1.25)
mtext(side=2,"Stage Elevation (Feet, NGVD29)",line=2.5,cex=1.25)

plot(0:1,0:1,type = 'n', axes = F,xlab=NA, ylab=NA)
legend(0.5,0.5,
       legend=c(paste0("WY",CurWY:(CurWY-3))),
       col=c(cols),lty=c(1),lwd=c(4),ncol=4,cex=0.8,bty="n",y.intersp=1,x.intersp=0.5,xpd=NA,xjust=0.5)
dev.off()


# Calendar Year Version ---------------------------------------------------
Cur.CY=as.numeric(format(Sys.Date(),'%Y'))
#LORS
load("C:/Julian_LaCie/_GitHub/CRE_Conditions/Data/LORS.Rdata")
LORS$Year=Cur.CY
LORS$Date2=with(LORS,date.fun(paste(Year,Month,Day,sep="-")))

LORS2=LORS
LORS2$Year=Cur.CY+1
LORS2$Date2=with(LORS2,date.fun(paste(Year,Month,Day,sep="-")))

LORS.gaph2Yrs=rbind(LORS,LORS2)
rm(LORS,LORS2)
LORS.gaph2Yrs$Date2=date.fun(LORS.gaph2Yrs$Date2)


HighLakeLab.x=as.POSIXct(paste(Cur.CY,9,1,sep="-"))
WSMLab.x=as.POSIXct(paste(Cur.CY,9,1,sep="-"))
BENLab.x=as.POSIXct(paste(Cur.CY,7,1,sep="-"))
BASELab.x=as.POSIXct(paste(Cur.CY,4,1,sep="-"))
HIGHLab.x=as.POSIXct(paste(Cur.CY,3,15,sep="-"))
InterLab.x=as.POSIXct(paste(Cur.CY,3,1,sep="-"))
LowLab.x=as.POSIXct(paste(CurWY,2,15,sep="-"))



LakeO.xtab$CY=as.numeric(format(LakeO.xtab$DATE,"%Y"))
LakeO.xtab$plot.dat.CY=with(LakeO.xtab,date.fun(paste(Cur.CY,month,day,sep="-")))

xlim.vals=as.POSIXct(strptime(c(as.Date(paste(Cur.CY,01,01,sep="-")),as.Date(paste(Cur.CY+1,01,01,sep="-"))),"%Y-%m-%d"),tz="EST")#as.POSIXct(strptime(dates,"%Y-%m-%d"),tz="EST")
xmaj=seq(xlim.vals[1],xlim.vals[2],by="2 months");xmin=seq(xlim.vals[1],xlim.vals[2],by="1 months")
ylim.val=c(9,18);by.y=1
ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
cols=c("red","blue","forestgreen","thistle1")
# png(filename=paste0(plot.path,"okeechobee_Stage_CY.png"),width=7,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(0.5,2,0.5,1),oma=c(0.5,3,0.5,1));
layout(matrix(1:2,2,1,byrow=T),heights=c(1,0.25))
plot(High~Date2,LORS.gaph2Yrs,ylim=ylim.val,xlim=xlim.vals,type="n",lwd=2,ylab=NA,xlab=NA,yaxs="i",xaxs="i",xaxt="n",yaxt="n")

xx=with(LORS.gaph2Yrs,c(Date2,rev(Date2)))
with(LORS.gaph2Yrs,polygon(xx,c(High,rep(18,length(High))),col=rgb(255/255,76/255,76/255)))
with(LORS.gaph2Yrs,polygon(xx,c(High,rev(Low)),col=rgb(255/255,173/255,51/255)))
with(LORS.gaph2Yrs,polygon(xx,c(Low,rev(BaseFlow)),col="grey80"))
with(LORS.gaph2Yrs,polygon(xx,c(BaseFlow,rev(BeneficialUse)),col=rgb(127/255,255/255,127/255)))
with(LORS.gaph2Yrs,polygon(xx,c(BeneficialUse,rev(WSM)),col=rgb(153/255,229/255,255/255)))
with(LORS.gaph2Yrs,polygon(xx,c(WSM,rep(0,length(WSM))),col=rgb(255/255,255/255,204/255),border=0))
with(LORS.gaph2Yrs,lines(High~Date2,lwd=2,col="black"))
with(LORS.gaph2Yrs,lines(Intermediate~Date2,lwd=2,col="black"))
with(LORS.gaph2Yrs,lines(Low~Date2,lwd=2,col="black"))
with(LORS.gaph2Yrs,lines(BaseFlow~Date2,lwd=2,col="black"))
with(LORS.gaph2Yrs,lines(BeneficialUse~Date2,lwd=2,col="black"))
with(LORS.gaph2Yrs,lines(Inter1ft~Date2,lwd=2,lty=5,col="black"))
with(LORS.gaph2Yrs,lines(LowLow~Date2,lwd=2,lty=5,col="grey"))
with(LORS.gaph2Yrs,lines(LowMid~Date2,lwd=2,lty=5,col="grey"))
with(LORS.gaph2Yrs,lines(WSM~Date2,lwd=2,lty=1,col="grey"))
abline(h=seq(9,18,1),v=seq(xlim.vals[1],xlim.vals[2],by="1 months"),lwd=1,col="black",lty=3)
text(HighLakeLab.x,17.5,"High Lake Management Band",font=2)
text(WSMLab.x,9.5,"Water Shortage Management Band",font=2)
text(BENLab.x,12.25,"Beneficial Use",font=2)
text(BASELab.x,13,"Base Flow",font=2)
text(HIGHLab.x,17,"High",font=2)
text(InterLab.x,16.25,"Intermediate ",font=2,cex=0.75)
text(LowLab.x,14.5,"Low",font=2)

with(subset(LakeO.xtab,CY==Cur.CY-3),lines(plot.dat.CY,Mean,lwd=4,col=adjustcolor(cols[4],1),lty=1))
with(subset(LakeO.xtab,CY==Cur.CY-2),lines(plot.dat.CY,Mean,lwd=4,col=adjustcolor(cols[3],1),lty=1))
with(subset(LakeO.xtab,CY==Cur.CY-1),lines(plot.dat.CY,Mean,lwd=4,col=adjustcolor(cols[2],1),lty=1))
with(subset(LakeO.xtab,CY==Cur.CY-0),lines(DATE,Mean,lwd=4,col=cols[1],lty=1))
axis_fun(1,xmaj,xmin,format(xmaj,"%b"),line=-0.5)
axis_fun(2,ymaj,ymin,ymaj)
box(lwd=lwd.val)

mtext(side=1,"Month",line=2,cex=1.25)
mtext(side=2,"Stage Elevation (Feet, NGVD29)",line=2.5,cex=1.25)

plot(0:1,0:1,type = 'n', axes = F,xlab=NA, ylab=NA)
legend(0.5,0.5,
       legend=c(paste0("CY",Cur.CY:(Cur.CY-3))),
       col=c(cols),lty=c(1),lwd=c(4),ncol=4,cex=0.8,bty="n",y.intersp=1,x.intersp=0.5,xpd=NA,xjust=0.5)
dev.off()