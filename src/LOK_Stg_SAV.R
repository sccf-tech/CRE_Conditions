

## BAD 
## https://www.tidyverse.org/articles/2017/12/workflow-vs-script/
## Clears Everything...start fresh.
rm(list=ls(all=T));cat("\014");dev.off()

## Libraries
#devtools::install_github("SwampThingPaul/AnalystHelper")
library(AnalystHelper);
library(plyr)
library(reshape)
library(openxlsx)

## Paths
wd="C:/Julian_LaCie/_Github/CRE_Conditions"

paths=paste0(wd,c("/Plots/","/Export/","/Data/","/src"))
# Folder.Maker(paths);#One and done. Creates folders in working directory.
plot.path=paths[1]
export.path=paths[2]
data.path=paths[3]

dates=date.fun(c("2001-05-01","2021-04-30"))

# -------------------------------------------------------------------------
comp.dbkey=data.frame(DBKEY=c("15611","06832"),Priority=c("P1","P2"))

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
LakeO.xtab

LakeO.xtab$WY=WY(LakeO.xtab$DATE)

boxplot(Mean~WY,LakeO.xtab)

stg.rng=ddply(LakeO.xtab,"WY",summarise,min.val=min(Mean,na.rm=T),max.val=max(Mean,na.rm=T))
stg.rng$min.11.val=with(stg.rng,ifelse(min.val<11,1,NA))
stg.rng$max.16.val=with(stg.rng,ifelse(max.val>16,2,NA))
stg.rng$max.17.val=with(stg.rng,ifelse(max.val>17,2,NA))
stg.rng$rng=with(stg.rng,max.val-min.val)

barplot(t(stg.rng[,c("min.val","rng")]),ylim=c(8,18),col=c(NA,"grey"),border=c(NA,"black"),space=0)

ylim.val=c(0.5,2.5)
xlim.val=c(2002,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)

plot(min.11.val~WY,stg.rng,ylim=ylim.val,xlim=xlim.val)
points(max.17.val~WY,stg.rng,pch=21,bg="red")

## SAV
SAV=read.xlsx(paste0(data.path,"SAV_Graph_PJ.xlsx"))
SAV=SAV[,c("Year","Vasc","Non","Mixed")]
SAV$WY=SAV$Year+1
SAV=subset(SAV,WY<2022)

ylim.val=c(0.5,2.5)
xlim.val=c(2002,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)

x=barplot(stg.rng$max.17.val,ylim=ylim.val,border=NA,col=NA,xpd=F,ann=F,axes=F,space=0)
abline(h=c(1,2),v=x[seq(1,length(x),by.x)],lty=2,col="grey")
x=barplot(stg.rng$max.17.val,ylim=ylim.val,border=NA,col=NA,xpd=F,ann=F,axes=F,space=0,add=T)
points(x,stg.rng$min.11.val,pch=21,bg="khaki",cex=1.5)
points(x,stg.rng$max.16.val,pch=21,bg="darkolivegreen3",cex=1.5)
axis_fun(1,x[seq(1,length(x),by.x)],x,NA)
axis_fun(2,c(1,2),c(1,2),c("Stage < 11 Ft","Stage > 16 Ft"))
box(lwd=1)

# png(filename=paste0(plot.path,"LOK_stg_SAV.png"),width=6.5,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,3,0.5,0.5),oma=c(2,3,0.25,0.25));
layout(matrix(1:2,2,1),heights = c(0.5,1))

ylim.val=c(8,18);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(2002,2021);by.x=4;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
cols=adjustcolor(c(NA,"slategray1"),0.75)
brd.cols=c(NA,"grey40")
x=barplot(t(stg.rng[,c("min.val","rng")]),ylim=ylim.val,col=NA,border=NA,ann=F,axes=F,space=0)
abline(h=ymaj,v=x[seq(1,length(x),by.x)],lty=2,col=adjustcolor("grey",0.5))
x=barplot(t(stg.rng[,c("min.val","rng")]),ylim=ylim.val,col=cols,border=brd.cols,ann=F,axes=F,space=0,add=T)
abline(h=c(11,17),lty=2,col="red")
axis_fun(1,x[seq(1,length(x),by.x)],x,NA)
axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)


ylim.val=c(0,65000);by.y=10000;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
cols=adjustcolor(c(rgb(243/255,120/255,37/255),rgb(79/255,149/255,213/255),"white"),0.75)

# x=barplot(t(SAV[,2:4]),col=cols,border="grey40",ylim=ylim.val,space=0)
x=barplot(t(SAV[,2:4]),ylim=ylim.val,border=NA,col=NA,xpd=F,ann=F,axes=F,space=0,names.arg=rep(NA,nrow(SAV)))
abline(h=ymaj,v=x[seq(1,length(x),by.x)],lty=2,col="grey")
x=barplot(t(SAV[,2:4]),ylim=ylim.val,border="grey40",col=cols,xpd=F,ann=F,axes=F,space=0,names.arg=rep(NA,nrow(SAV)),add=T)
axis_fun(1,x[seq(1,length(x),by.x)],x,xmaj)
axis_fun(2,ymaj,ymin,format(ymaj,big.mark=","))
box(lwd=1)
legend("topright",legend=c("Vascular","Non-Vascular","Mixed"),
       pch=22,pt.bg=cols,pt.cex=1.5,
       lty=NA,lwd=1,col="grey40",
       ncol=1,cex=0.9,bty="n",y.intersp=1,x.intersp=1,xpd=NA,xjust=0,yjust=1)



# png(filename=paste0(plot.path,"LOK_SAV.png"),width=6,height=3,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2.5,0.5,0.5),oma=c(2.5,3,0.25,0.25));

xlim.val=c(2002,2020);by.x=4;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
ylim.val=c(0,65000);by.y=10000;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
cols=adjustcolor(c(rgb(243/255,120/255,37/255),rgb(79/255,149/255,213/255),"white"),0.75)

# x=barplot(t(SAV[,2:4]),col=cols,border="grey40",ylim=ylim.val,space=0)
x=barplot(t(SAV[,2:4]),ylim=ylim.val,border=NA,col=NA,xpd=F,ann=F,axes=F,space=0,names.arg=rep(NA,nrow(SAV)))
abline(h=ymaj,v=x[seq(1,length(x),by.x)],lty=3,col=adjustcolor("grey",0.5))
x=barplot(t(SAV[,2:4]),ylim=ylim.val,border="grey40",col=cols,xpd=F,ann=F,axes=F,space=0,names.arg=rep(NA,nrow(SAV)),add=T)
axis_fun(1,x[seq(1,length(x),by.x)],x,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,format(ymaj,big.mark=","))
box(lwd=1)
mtext(side=2,line=3.5,"SAV Coverage (acres)")
mtext(side=1,line=2,"Water Year")
legend("topright",legend=c("Vascular","Non-Vascular","Mixed"),
       pch=22,pt.bg=cols,pt.cex=1.5,
       lty=NA,lwd=1,col="grey40",
       ncol=1,cex=0.9,bty="n",y.intersp=1,x.intersp=1,xpd=NA,xjust=0,yjust=1)
dev.off()


# png(filename=paste0(plot.path,"LOK_stg_panel.png"),width=6,height=2,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(0.5,2.5,0.5,0.5),oma=c(0.5,3,0.25,0.25));

ylim.val=c(8,19);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
cols=adjustcolor(c(NA,"slategray1"),0.75)
brd.cols=c(NA,"grey40")
x=barplot(t(stg.rng[,c("min.val","rng")]),ylim=ylim.val,col=NA,border=NA,ann=F,axes=F,space=0)
abline(h=ymaj,v=x[seq(1,length(x),by.x)],lty=3,col=adjustcolor("grey",0.5))
x=barplot(t(stg.rng[,c("min.val","rng")]),ylim=ylim.val,col=cols,border=brd.cols,ann=F,axes=F,space=0,add=T)
abline(h=c(11,17),lty=2,col="red")
axis_fun(1,x[seq(1,length(x),by.x)],x,NA)
axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
mtext(side=2,line=3.5,"Stage\n(Ft, NGVD29)")
box(lwd=1)
legend("bottomright",legend=c("Annual Range"),
       pch=22,pt.bg=adjustcolor("slategray1",0.75),pt.cex=1,
       lty=NA,lwd=1,col="grey40",
       ncol=1,cex=0.8,bty="n",y.intersp=1,x.intersp=1,xpd=NA,xjust=0,yjust=1)
dev.off()


# png(filename=paste0(plot.path,"LOK_stg_SAV.png"),width=6.5,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,2.5,0.5,0.5),oma=c(2.5,3,0.25,0.25));
layout(matrix(1:2,2,1),heights = c(0.5,1))

ylim.val=c(8,19);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
cols=adjustcolor(c(NA,"slategray1"),0.75)
brd.cols=c(NA,"grey40")
x=barplot(t(stg.rng[,c("min.val","rng")]),ylim=ylim.val,col=NA,border=NA,ann=F,axes=F,space=0)
abline(h=ymaj,v=x[seq(1,length(x),by.x)],lty=3,col=adjustcolor("grey",0.5))
x=barplot(t(stg.rng[,c("min.val","rng")]),ylim=ylim.val,col=cols,border=brd.cols,ann=F,axes=F,space=0,add=T)
abline(h=c(11,17),lty=2,col="red")
axis_fun(1,x[seq(1,length(x),by.x)],x,NA)
axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
mtext(side=2,line=3.5,"Stage\n(Ft, NGVD29)")
box(lwd=1)
legend("bottomright",legend=c("Annual Range"),
       pch=22,pt.bg=adjustcolor("slategray1",0.75),pt.cex=1,
       lty=NA,lwd=1,col="grey40",
       ncol=1,cex=0.8,bty="n",y.intersp=1,x.intersp=1,xpd=NA,xjust=0,yjust=1)


xlim.val=c(2002,2020);by.x=4;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
ylim.val=c(0,65000);by.y=10000;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
cols=adjustcolor(c(rgb(243/255,120/255,37/255),rgb(79/255,149/255,213/255),"white"),0.75)

# x=barplot(t(SAV[,2:4]),col=cols,border="grey40",ylim=ylim.val,space=0)
x=barplot(t(SAV[,2:4]),ylim=ylim.val,border=NA,col=NA,xpd=F,ann=F,axes=F,space=0,names.arg=rep(NA,nrow(SAV)))
abline(h=ymaj,v=x[seq(1,length(x),by.x)],lty=3,col=adjustcolor("grey",0.5))
x=barplot(t(SAV[,2:4]),ylim=ylim.val,border="grey40",col=cols,xpd=F,ann=F,axes=F,space=0,names.arg=rep(NA,nrow(SAV)),add=T)
axis_fun(1,x[seq(1,length(x),by.x)],x,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,format(ymaj,big.mark=","))
box(lwd=1)
mtext(side=2,line=3.5,"SAV Coverage (acres)")
mtext(side=1,line=2,"Water Year")
legend("topright",legend=c("Vascular","Non-Vascular","Mixed"),
       pch=22,pt.bg=cols,pt.cex=1.5,
       lty=NA,lwd=1,col="grey40",
       ncol=1,cex=0.9,bty="n",y.intersp=1,x.intersp=1,xpd=NA,xjust=0,yjust=1)

dev.off()