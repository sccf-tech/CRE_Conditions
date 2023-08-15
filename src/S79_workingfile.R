
library(AnalystHelper)
library(reshape)
library(RcppRoll)

plot.path="C:/Julian_LaCie/_GitHub/CRE_Conditions/Plots"

dates=date.fun(c("2023-05-01","2023-07-12"))

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
cal.flow.dat.xtab=cast(cal.flow.dat,SITE+Date+WY~priority,value="Data.Value",mean)
cal.flow.dat.xtab$P1=with(cal.flow.dat.xtab,ifelse(SITE=="S77"&P1>=7587,NA,P1)); #Extreme value reported for 6/7/2021
cal.flow.dat.xtab$Data.Value=with(cal.flow.dat.xtab,ifelse(is.na(P1)==T,P2,P1))

cal.flow.dat.xtab=cast(cal.flow.dat.xtab,Date+WY~SITE,value="Data.Value",mean)
cal.flow.dat.xtab$hydro.day=hydro.day(cal.flow.dat.xtab$Date)
cal.flow.dat.xtab$S77=with(cal.flow.dat.xtab,ifelse(S77<0,0,S77))
cal.flow.dat.xtab$C43=with(cal.flow.dat.xtab,ifelse(S79<S77,0,S79-S77))

cal.flow.dat.xtab$cum.S79=with(cal.flow.dat.xtab,ave(cfs.to.acftd(S79),WY,FUN = function(x)cumsum(ifelse(is.na(x),0,x))))
cal.flow.dat.xtab$cum.S77=with(cal.flow.dat.xtab,ave(cfs.to.acftd(S77),WY,FUN = function(x)cumsum(ifelse(is.na(x),0,x))))
cal.flow.dat.xtab$Q.14=with(cal.flow.dat.xtab,roll_meanr(S79,n=14))
cal.flow.dat.xtab$Q.30=with(cal.flow.dat.xtab,roll_meanr(S79,n=30))
cal.flow.dat.xtab$SalEnv.cat=with(cal.flow.dat.xtab,ifelse(Q.14<750,"low",
                                                           ifelse(Q.14>=750&Q.14<2100,"optimum",
                                                                  ifelse(Q.14>=2100&Q.14<2600,"stress",
                                                                         ifelse(Q.14>2600,"damaging",NA)))))

cal.flow.dat.xtab$LOK=apply(cal.flow.dat.xtab[,c("S77","S79")],1,min,na.rm=T)

cal.flow.dat.xtab$S79.gald=cal.flow.dat.xtab$S79*646317
cal.flow.dat.xtab$LOK.gald=cal.flow.dat.xtab$LOK*646317

sum(cal.flow.dat.xtab$S79.gald,na.rm=T)/1e9
sum(cal.flow.dat.xtab$LOK.gald,na.rm=T)/1e9

library(lubridate)
TODAY=date.fun(as.character(Sys.time()))
max.q=max(subset(cal.flow.dat.xtab,Date%in%seq(TODAY-ddays(30),TODAY+ddays(3),"1 days"))$S79,na.rm=T)
ylim.max=round(max.q+max.q*0.25,-3)

ylim.val=c(0,ylim.max);by.y=ylim.max/4;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=date.fun(c(TODAY-duration(1,"months"),TODAY+ddays(3)));xmaj=seq(xlim.val[1],xlim.val[2],by="15 days");xmin=seq(xlim.val[1],xlim.val[2],by="1 days")
# png(filename=paste0(plot.path,"/202307_CREQ.png"),width=5.4,height=5.4,units="in",res=200,type="windows",bg="white")
layout(matrix(1:2,2,1,byrow=F),widths=c(2,1),heights=c(1,0.5))
par(family="serif",cex.axis=1.2,mar=c(1,2,0.5,1),oma=c(0.5,3,1,0.5));
plot(S79~Date,cal.flow.dat.xtab,type="n",ylim=ylim.val,xlim=xlim.val,xaxs="i",yaxs="i",xaxt="n",yaxt="n",xlab=NA,ylab=NA)
xx=c(xlim.val[1],xlim.val[1],xlim.val[2],xlim.val[2])
yy3=c(2600,ylim.max,ylim.max,2600);polygon(x=xx,y=yy3,col=adjustcolor("pink",0.5),border=F)
yy4=c(2100,2600,2600,2100);polygon(x=xx,y=yy4,col=adjustcolor("lightyellow",0.5),border=F)
yy5=c(750,2100,2100,750);polygon(x=xx,y=yy5,col=adjustcolor("lightgreen",0.5),border=F)
abline(h=ymaj,v=xmaj,lty=3,col="grey")
# abline(h=457,col="red")
lines(S79~Date,cal.flow.dat.xtab,lwd=4,col="dodgerblue1")
lines(S77~Date,cal.flow.dat.xtab,lwd=4,col="green1")
axis_fun(1,xmaj,xmin,format(xmaj,"%b %d %Y"),line=-0.5)
axis_fun(2,ymaj,ymin,ymaj,cex.axis=1)
# axis_fun(4,ymaj,ymin,format(round(cfs.to.acftd(ymaj)/1000,1)),cex.axis=1)

box(lwd=1)
mtext("Date",side=1,line=2,cex=1.25)
mtext("Discharge (cfs)",side=2,line=3.5,cex=1.25)

plot(0:1,0:1,type="n",yaxt="n",xaxt="n",bty="n")
legend(0.25,0.75,
       legend=c( "Damaging (>2600 cfs)","Stress (2100 - 2600 cfs)","Optimum (750 - 2100 cfs)"),pch=22,
       pt.bg=adjustcolor(c("pink","lightyellow","lightgreen"),0.5),pt.cex=2,ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=1,xpd=NA,xjust=0.5,
       title.adj = 0,title='RECOVER PM')

legend(0.75,0.75,
       legend=c("S-79 (to Estuary)","S-77 (from Lake)"),
       pch=c(NA),pt.bg=NA,pt.cex=NA,lty=1,lwd=2,col=c("dodgerblue1","green1"),
       ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=1,xpd=NA,xjust=0.5,title.adj = 0,title='')
dev.off()

