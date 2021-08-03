## 
## Lake Okeechobee
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
library(reshape2)
library(zoo)

library(lubridate)

library(ggplot2)
library(gganimate)

## Paths
wd="C:/Julian_LaCie/_Github/Everglades_WaterLevels"

paths=paste0(wd,c("/Plots/","/Export/","/Data/","/src"))
# Folder.Maker(paths);#One and done. Creates folders in working directory.
plot.path=paths[1]
export.path=paths[2]
data.path=paths[3]

# Lake Okeechobee EKG -----------------------------------------------------
dates=date.fun(c("2015-05-01","2021-05-01"))
comp.dbkey=data.frame(DBKEY=c("N3466","06832"),Priority=c("P1","P2"))

stg.da=data.frame()
for(i in 1:nrow(comp.dbkey)){
  tmp=DBHYDRO_daily(dates[1],dates[2],comp.dbkey$DBKEY[i])
  tmp$DBKEY=as.character(comp.dbkey$DBKEY[i])
  stg.da=rbind(stg.da,tmp)
}
stg.da=merge(stg.da,comp.dbkey,"DBKEY")
stg.da$DATE=date.fun(stg.da$Date)

LakeO.xtab=dcast(stg.da,DATE~Priority,value.var="Data.Value",mean,na.rm=T)
LakeO.xtab$Mean=with(LakeO.xtab,ifelse(is.na(P1)==T,P2,P1))
LakeO.xtab$Mean.m=ft.to.m(LakeO.xtab$Mean)

subset(LakeO.xtab,DATE%in%seq(date.fun("2018-09-03"),date.fun("2018-09-06"),"1 days"))

stg.animate=ggplot(LakeO.xtab,aes(x=DATE,y=Mean.m))+
  geom_line(color="dodgerblue1",size=1.25,alpha=0.5)+
  geom_point(color="dodgerblue1",size=3)+
  geom_point(data=subset(LakeO.xtab,DATE==date.fun("2017-10-13")),aes(x=DATE,y=Mean.m),size=3)+
  geom_text(data=subset(LakeO.xtab,DATE==date.fun("2017-10-13")),aes(x=DATE,y=Mean.m,label="H. Irma"),hjust=0,nudge_y = 0.075,size=3)+
  # scale_y_continuous(breaks = seq(10.5, 17.5,1))+
  scale_y_continuous(breaks = seq(3,5.25,0.5))+
  scale_x_datetime(date_labels="%m-%Y",
                   breaks=seq(date.fun("2015-05-01"),date.fun("2021-05-01"),"1 years"),
                   limits=date.fun(c("2015-05-01","2022-02-01")))+
  theme_bw() +
  theme(
    #legend.position = 'none',
    text=element_text(family="serif"),
    plot.title=element_text(size=12),
    plot.subtitle = element_text(color = "grey50",size=8),
    plot.caption = element_text(hjust = 0)
  )+
  labs(title = "Lake Okeechobee Stage Elevation",
       # subtitle='Date: {frame_along}',
       x="Date",
       y="Stage Elevation (m, NGVD29)")+
  #geom_label(aes(x = DATE, y=Mean, label=Mean), nudge_x = 10, size=4, hjust=0)+
  geom_segment(aes(x=date.fun("2021-07-01"),xend=DATE,yend=Mean.m), size=0.5, linetype = 2, colour = 'gray50')+
  geom_text(aes(x=date.fun("2022-01-01"),label=sprintf("%0.2f",Mean.m)),family="serif")+
  transition_reveal(DATE)
  # shadow_wake(wake_length = 0.1, alpha = FALSE)
  
animate(stg.animate,
        fps=4,height=3,width=6,units="in",res=150)
anim_save(paste0(plot.path,"LOK_stg_animation.gif"))


# Water Control Plans-south -----------------------------------------------
mng.y=1

cex.txt=0.75
# tiff(filename=paste0(plot.path,"WCA3ENP_operations_Plot.tiff"),width=6.5,height=2.5,units="in",res=200,type="windows",compression=c("lzw"),bg="white")
par(family="serif",mar=c(0.5,5,1,0.75),oma=c(3,3,0.25,0.25),mgp=c(3,1,0));

xlim.val=c(1994,2025.25);by.x=4;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
ylim.val=c(0.5,1.5);by.y=1;ymaj=seq(ylim.val[1],ylim.val[2],by.y)
plot(xmin,rep(4,length(xmin)),type="n",ylim=ylim.val,xlim=xlim.val,yaxt="n",xaxt="n",ylab=NA,xlab=NA)
abline(v=xmaj,h=1:6,lty=3,col="grey")

polygon(c(1984,1984,1999,1999),c(mng.y-0.25,mng.y+0.25,mng.y+0.25,mng.y-0.25),col="grey90");text(1996,mng.y,"Exper.\nWater\nDeliveries",cex=cex.txt)
polygon(c(1999,1999,2001,2001),c(mng.y-0.25,mng.y+0.25,mng.y+0.25,mng.y-0.25),col="grey90");text(2000,mng.y,"ISOP",cex=cex.txt)
polygon(c(2001,2001,2012,2012),c(mng.y-0.25,mng.y+0.25,mng.y+0.25,mng.y-0.25),col="grey90");text(2006.5,mng.y,"IOP",cex=cex.txt)
polygon(c(2012,2012,2015,2015),c(mng.y-0.25,mng.y+0.25,mng.y+0.25,mng.y-0.25),col="grey90");text(2013.5,mng.y,"ERTP",cex=cex.txt)
polygon(c(2015,2015,2016,2016),c(mng.y-0.25,mng.y+0.25,mng.y+0.25,mng.y-0.25),col="grey90");lines(c(2015.5,2015.5),c(mng.y,mng.y+0.3),lwd=2);text(2015.5,mng.y+0.4,"MWD Inc. Test",cex=cex.txt)
polygon(c(2017,2017,2019,2019),c(mng.y-0.25,mng.y+0.25,mng.y+0.25,mng.y-0.25),col="grey90",density=25,border='black');
polygon(c(2016,2016,2017,2017),c(mng.y-0.25,mng.y+0.25,mng.y+0.25,mng.y-0.25),col="grey90");lines(c(2016.5,2016.5),c(mng.y,mng.y-0.3),lwd=2);text(2016.5,mng.y-0.4,"Emg. Dev.",cex=cex.txt)
polygon(c(2019,2019,2030,2030),c(mng.y-0.25,mng.y+0.25,mng.y+0.25,mng.y-0.25),col="black",density=25);lines(c(2023,2023),c(mng.y,mng.y+0.3),lwd=2);text(2023,mng.y+0.4,"COP",cex=cex.txt)
box(lwd=1)
y.txt=c("Water Managemet \nOperations")
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,1,1,y.txt,1)
mtext(side=1,line=2,"Calendar Year")
dev.off()
library(vistime)
WCPs.south=data.frame(Plan=c("Exper. Water Deliveries","ISOP","IOP",'ERTP',"MWD Inc. Test","Emg. Dev.","MWD Inc. Test","COP"),
                      label=NA,
                      start=c("1984-01-01","1999-01-01","2001-01-01","2012-01-01","2015-01-01","2016-01-01","2017-01-01","2019-01-01"),
                      end=c("1999-01-01","2001-01-01","2012-01-01","2015-01-01","2016-01-01","2017-01-01","2019-01-01","2030-01-01"),
                      color="grey");# not exact start and end dates...approximate calendar year
gg_vistime(WCPs.south,col.group="Plan",col.event = "label")      

# WCAs Rainfall -----------------------------------------------------------
dates=date.fun(c("1971-05-01","2021-04-30"))
RF.dbkeys=data.frame(SITE=c("S5A_R","LXWS",rep("S39_R",3),rep("S6_R",2),rep("S7_R",2),rep("S8_R",2),
                            "ROTENWX",rep("3A-NE_R",2),rep("3A-NW_R",2),rep("3A-S_R",2),
                            "NP-FMB",rep("NP-206",2),"NP-P37"),
                     region=c(rep("WCA1",5),rep("WCA2",4),rep("WCA3",9),rep("ENP",4)),
                     DBKEY=c("15202","DU551",
                             "05791","06035","16677",
                             "15203","K8685",
                             "15204","K8688",
                             "15205","K8693",
                             "GE354",
                             "05864","LX283",
                             "05863","LA365",
                             "05865","HC941",
                             "H2005",
                             "00743","06041",
                             "H2001"))

ever.rf.da=data.frame()
pb <- txtProgressBar(min = 0, max = nrow(RF.dbkeys), style = 3)
for(i in 1:nrow(RF.dbkeys)){
  tmp=DBHYDRO_daily(dates[1],dates[2],RF.dbkeys$DBKEY[i])
  tmp$DBKEY=as.character(RF.dbkeys$DBKEY[i])
  ever.rf.da=rbind(ever.rf.da,tmp)
  setTxtProgressBar(pb, i)
}
ever.rf.da=merge(ever.rf.da,RF.dbkeys,"DBKEY")
ever.rf.da$DATE=date.fun(ever.rf.da$Date)

ever.rf.da2=ddply(ever.rf.da,c("Date","SITE","region"),summarise,mean.val=mean(Data.Value,na.rm=T))
ever.rf.da2$WY=WY(ever.rf.da2$Date)

ever.rf.da.mean=ddply(ever.rf.da2,c("Date","WY",'region'),summarise,mean.val=mean(mean.val,na.rm=T),N.val=N.obs(SITE))
ever.rf.da.mean$season=FL.Hydroseason(ever.rf.da.mean$Date)
plot(N.val~Date,subset(ever.rf.da.mean,region=="WCA3"))

ever.rf.region.WY=ddply(ever.rf.da.mean,c("region","WY"),summarise,TRF.cm=sum(in.to.cm(mean.val),na.rm=T))

plot(TRF.in~WY,subset(ever.rf.region.WY,region=="WCA1"),type="l")
with(subset(ever.rf.region.WY,region=="WCA2"),lines(TRF.in~WY,col="red"))
with(subset(ever.rf.region.WY,region=="WCA3"),lines(TRF.in~WY,col="blue"))
with(subset(ever.rf.region.WY,region=="ENP"),lines(TRF.in~WY,col="green"))

plot(CV.val~WY,ddply(ever.rf.region.WY,c("WY"),summarise,CV.val=cv.per(TRF.in)),type="b")

WY.mean=ddply(ever.rf.region.WY,c("WY"),summarise,mean.val=mean(TRF.cm,na.rm=T),SE.val=SE(TRF.cm),N.val=N.obs(TRF.cm))

region.val=c("WCA1","WCA2","WCA3","ENP")
cols=wesanderson::wes_palette("Zissou1",4,"continuous")
# tiff(filename=paste0(plot.path,"EVPA_rainfall.tiff"),width=6.5,height=2.5,units="in",res=200,type="windows",compression=c("lzw"),bg="white")
par(family="serif",mar=c(2,1,1,0.75),oma=c(3,3,0.25,0.25),mgp=c(3,1,0));

xlim.val=c(1970,2021);by.x=10;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
ylim.val=c(50,250);by.y=100;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/4)
plot(mean.val~WY,WY.mean,type="n",ylim=ylim.val,xlim=xlim.val,yaxt="n",xaxt="n",ylab=NA,xlab=NA)
abline(v=xmaj,h=ymaj,lty=1,col=adjustcolor("grey",0.5))
for(i in 1:4){
  with(subset(ever.rf.region.WY,region==region.val[i]),lines(TRF.cm~WY,col=adjustcolor(cols[i],0.3),lwd=2))
}
with(WY.mean,pt_line(WY,mean.val,1,"black",1.5,21,"black",cex=1.5))
# with(WY.mean,lines(WY,mean.val+SE.val,lty=2))
# with(WY.mean,lines(WY,mean.val-SE.val,lty=2))
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj)
dev.off()


# k=loess(mean.val~WY,WY.mean)
# x.val=seq(1972,2021,length.out=200)
# k.pred=predict(k,data.frame(WY=x.val))
# lines(x.val,k.pred)
# lines(c(1972,1994),rep(mean(subset(WY.mean,WY%in%seq(1972,1994,1))$mean.val),2),col="red",lwd=1)
# lines(c(1995,2021),rep(mean(subset(WY.mean,WY%in%seq(1995,2021,1))$mean.val),2),col="blue",lwd=1)

ever.rf.region.wet=ddply(subset(ever.rf.da.mean,season=="A_Wet"),c("region","WY"),summarise,TRF.cm=sum(in.to.cm(mean.val),na.rm=T))
WY.mean2=ddply(ever.rf.region.wet,c("WY"),summarise,mean.val=mean(TRF.cm,na.rm=T),SE.val=SE(TRF.cm),N.val=N.obs(TRF.cm))

cols=wesanderson::wes_palette("Zissou1",4,"continuous")
# tiff(filename=paste0(plot.path,"EVPA_rainfall_wetseason.tiff"),width=6.5,height=2.5,units="in",res=200,type="windows",compression=c("lzw"),bg="white")
par(family="serif",mar=c(2,1,1,0.75),oma=c(1,3,0.25,0.25),mgp=c(3,1,0));
layout(matrix(1:2,1,2),widths=c(1,0.25))

xlim.val=c(1970,2021);by.x=10;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
ylim.val=c(50,170);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/4)
plot(mean.val~WY,WY.mean,type="n",ylim=ylim.val,xlim=xlim.val,yaxt="n",xaxt="n",ylab=NA,xlab=NA)
abline(v=xmaj,h=ymaj,lty=1,col=adjustcolor("grey",0.5))
for(i in 1:4){
  with(subset(ever.rf.region.wet,region==region.val[i]),lines(TRF.cm~WY,col=adjustcolor(cols[i],0.3),lwd=2))
}
with(WY.mean2,pt_line(WY,mean.val,1,"black",1.5,21,"black",cex=1))
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj)
lines(c(1972,1990),rep(mean(subset(WY.mean2,WY%in%seq(1972,1990,1))$mean.val),2),col=adjustcolor("blue",1),lwd=2)
lines(c(1991,2021),rep(mean(subset(WY.mean2,WY%in%seq(1991,2021,1))$mean.val),2),col=adjustcolor("red",1),lwd=2)
mtext(side=3,adj=0,"Wet Season")
mtext(side=1,line=1.5,"Water Year")
mtext(side=2,line=2.5,"Total Rainfall (cm)")

plot(0:1,0:1,ann=F,axes=F,type="n")
legend(0.5,0.5,legend=c("WCA1","WCA2","WCA3","ENP","Regional Mean","Long Term mean\n(1972- 1990)","Long Term mean\n(1991- 2021)"),
       pch=c(rep(NA,4),19,NA,NA),lty=c(1),lwd=c(2),
       pt.bg=NA,col=c(adjustcolor(cols,0.3),"black",adjustcolor(c("blue","red"),1)),
       pt.cex=1,ncol=1,cex=0.6,bty="n",y.intersp=1.5,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
dev.off()


cpt.fit=cpt.mean(WY.mean2$mean.val)
plot(cpt.fit)
summary(cpt.fit)
###

# https://lindeloev.github.io/mcp/articles/packages.html
library(changepoint)
cpt.fit=cpt.mean(WY.mean$mean.val)
plot(cpt.fit)

library(segmented)
mod=lm(mean.val~WY,WY.mean)
gvlma::gvlma(mod)
seg.mod=segmented(mod,seg.Z=~WY,psi=1992)
summary(seg.mod)

plot(mean.val~WY,WY.mean)
plot(seg.mod,add=T)

plot(mean.val~WY,WY.mean,type="b")
lines(c(1972,1994),rep(mean(subset(WY.mean,WY%in%seq(1972,1994,1))$mean.val),2),col="red")
lines(c(1995,2021),rep(mean(subset(WY.mean,WY%in%seq(1995,2021,1))$mean.val),2),col="blue")


## Monthly 
rf.dat.da=ddply(ever.rf.da,"Date",summarise,mean.val=mean(Data.Value,na.rm=T),N.val=N.obs(Data.Value))
rf.dat.da$CY=as.numeric(format(rf.dat.da$Date,"%Y"))
rf.dat.da$month=as.numeric(format(rf.dat.da$Date,"%m"))

rf.dat.mon=ddply(rf.dat.da,c("CY","month"),summarise,TRF.in=sum(mean.val,na.rm=T))
rf.dat.mon=merge(rf.dat.mon,expand.grid(CY=1969:2021,month=1:12),c("CY","month"),all.y=T)
rf.dat.mon$RF.cat=as.factor(findInterval(rf.dat.mon$TRF.in,c(0,1,2.5,5,7.5,10,12.5,100)))
rf.dat.mon=merge(rf.dat.mon,
                 data.frame(RF.cat=c(NA,1:7),
                            RF.cat.txt=c("<NA>","<1","1 - 2.5","2.5 - 5","5 - 7.5","7.5 - 10","10 - 12.5",">12.5"),
                            txt.cols=c(NA,rep("black",3),rep("white",4))),
                 "RF.cat")

library(ggplot2)
cols.vir=rev(viridis::inferno(7))
cols=c("1"=cols.vir[1],"2"=cols.vir[2],"3"=cols.vir[3],"4"=cols.vir[4],"5"=cols.vir[5],"6"=cols.vir[6],"7"=cols.vir[7])
ggplot(rf.dat.mon, aes(x = month, y = CY, fill = RF.cat)) +
  geom_tile(aes(group = RF.cat), colour = 'black')+
  geom_text(aes(label=format(round(TRF.in,1),nsmall=1),fontface = "bold"),size=2,colour=rf.dat.mon$txt.cols,family="serif",)+
  scale_y_reverse(expand = c(0, 0), breaks = rf.dat.mon$CY) +
  # scale_x_discrete(expand = c(0, 0), position = 'top') +
  scale_x_continuous(expand = c(0, 0), breaks = 1:12)+
  scale_fill_manual(values = cols,
                    name="Rainfall\nCategories\n(Inches)",
                    breaks=1:7,
                    labels=c("<1.0","1.0 - 2.5","2.5 - 5.0","5.0 - 7.5","7.5 - 10.0","10.0 - 12.5",">12.5")) +
  theme_bw() +
  theme(
    #legend.position = 'none',
    text=element_text(family="serif"),
    plot.title=element_text(size=12),
    plot.subtitle = element_text(color = "grey50",size=8),
    plot.caption = element_text(hjust = 0)
  )+
  labs(title = "Everglades (WCAs & ENP)",
       subtitle = "Total Monthly Rainfall",
       # caption = paste0("Produced: ",format(Sys.Date(),"%d %b %Y")),
       x="Month",
       y="Year")
