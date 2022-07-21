

library(AnalystHelper)


dbkeys=data.frame(DBKEY=c("13077","13076"),param=c('WINDD','WINDS'))
wx.dat=DBHYDRO_daily(as.Date("2022-04-01"),as.Date("2022-07-18"),dbkeys$DBKEY)

wx.dat=merge(wx.dat,dbkeys,'DBKEY')
wx.dat$Date=date.fun(wx.dat$Date)

wx.dat.xtab=reshape2::dcast(wx.dat,Date~param,value.var="Data.Value",mean)
wx.dat.xtab$WINDS.ms=wx.dat.xtab$wx.dat.xtab*0.44704


plot(WINDD~Date,wx.dat.xtab)
plot(WINDS~Date,wx.dat.xtab)

plot(WINDS~Date,wx.dat.xtab,type="n",ylab="Wind Speed (m/s)",ylim=c(-20,30))
stickplot.dat.arrows(Date,WINDS,WINDD,wx.dat.xtab,col="green",lty=1,lwd=1)



ylim.val=c(-10,10);by.y=5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(WINDS~Date,wx.dat.xtab,axes=F,ann=F,type="n",ylim=ylim.val,xlim=xlim.val)
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.75)
stickplot.dat.arrows(Date,WINDS.ms,WINDD,wx.dat.xtab,col="dodgerblue1",lty=1,lwd=1.5)
mapmisc::scaleBar(utm17,"bottomright",bty="n",cex=1,seg.len=4,outer=F)



## 
dbkeys=data.frame(DBKEY=c("IY033","KV252"),param=c('WINVD','WINVS'))
wx.dat=DBHYDRO_breakpoint(as.Date("2022-04-01"),as.Date("2022-07-18"),dbkeys$DBKEY)
wx.dat=merge(wx.dat,dbkeys,'DBKEY')

wx.dat.xtab=cast(wx.dat,DATETIME~param,value="Data.Value",mean)

wx.dat.xtab$WINVS=wx.dat.xtab$WINVS*0.44704
wx.dat.xtab$u_vec=with(wx.dat.xtab,-WINVS*sin(2*pi*(WINVD/360)))
wx.dat.xtab$v_vec=with(wx.dat.xtab,-WINVS*cos(2*pi*(WINVD/360)))
wx.dat.xtab$Date=date.fun(wx.dat.xtab$DATETIME)

wx.dat.xtab.da=ddply(wx.dat.xtab,"Date",summarise,
                     mean.u.scalar=mean(WINVS,na.rm=T),
                     mean.u=mean(u_vec,na.rm=T),
                     mean.v=mean(v_vec,na.rm=T))
wx.dat.xtab.da$wd.average=with(wx.dat.xtab.da,(atan2(mean.u,mean.v)*360/2/pi)+180)
wx.dat.xtab.da$ws.vec.avg=with(wx.dat.xtab.da,((mean.u^2+mean.v^2)^0.5))

plot(ws.vec.avg~mean.u.scalar,wx.dat.xtab.da,xlab="mean scalar wind speed (m/s)",
     ylab="mean vector wind speed (m/s)",ylim=c(0,10),xlim=c(0,10),pch=21,bg="dodgerblue1",las=1);
abline(0,1)

plot(ws.vec.avg~Date,wx.dat.xtab.da,type="n",ylab="Wind Speed (m/s)",ylim=c(-10,10))
stickplot.dat.arrows(Date,ws.vec.avg,wd.average,wx.dat.xtab.da,col="dodgerblue1",lty=1,lwd=1)
mapmisc::scaleBar(utm17,"bottomright",bty="n",cex=1,seg.len=4,outer=F)


wx.dat.xtab$WINDS.ms=wx.dat.xtab$wx.dat.xtab*0.44704
