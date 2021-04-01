## 
## https://michaeltoth.me/you-need-to-start-branding-your-graphs-heres-how-with-ggplot.html

lwd.val=1
ylim.val=c(-2,2);by.y=1
ymaj=c(0,seq(ylim.val[1],ylim.val[2],by.y));ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.vals=as.POSIXct(strptime(c(as.Date(paste(CurWY-1,05,01,sep="-")),as.Date(paste(CurWY,05,01,sep="-"))),"%Y-%m-%d"),tz="EST")
#xlim.vals=as.POSIXct(strptime(dates,"%Y-%m-%d"),tz="EST")
xmaj=seq(xlim.vals[1],xlim.vals[2],by="2 months");xmin=seq(xlim.vals[1],xlim.vals[2],by="1 months")

#LakeO.xtab$recess_7day=with(LakeO.xtab,ifelse(is.na(recess_7day)==T,0,recess_7day))
#LakeO.xtab$recess_30day=with(LakeO.xtab,ifelse(is.na(recess_30day)==T,0,recess_30day))

par(family="serif",cex.axis=1,mar=c(2,2,1,1),oma=c(3,3,1,1));
# layout(matrix(1:2,2,1),heights=c(1,0.25))
plot(recess_30day~DATE,LakeO.xtab,ylim=ylim.val,xlim=xlim.vals,type="n",lwd=2,ylab=NA,xlab=NA,yaxs="i",xaxs="i",xaxt="n",yaxt="n")
abline(v=xmin,h=ymin,lwd=1,col="grey",lty=3)
abline(h=0);
#with(LakeO.xtab,lines(DATE,recess_7day,lwd=1.5,col="black",lty=1))
# with(LakeO.xtab,shaded.range(DATE,rep(0,length(DATE)),recess_30day,"dodgerblue1",lty=1))
with(LakeO.xtab,lines(DATE,recess_30day,col="dodgerblue1",lty=1,lwd=1.5))
# with(subset(LakeO.xtab,WY==2019),lines(plot.dat,recess_30day,col="grey",lty=1,lwd=2))
with(LakeO.xtab,lines(DATE,recess_7day,col="indianred1",lty=2,lwd=1.5))
points(YEST,subset(LakeO.xtab,DATE==YEST)$recess_30day,pch=21,bg="dodgerblue1",cex=1.25)
text.x=if(YEST>date.fun(paste0(CurWY,"-04-01"))){YEST-ddays(15)}else{YEST+ddays(15)}
if(subset(LakeO.xtab,DATE==YEST)$recess_7day!=0){text(text.x,subset(LakeO.xtab,DATE==YEST)$recess_30day,round(subset(LakeO.xtab,DATE==YEST)$recess_30day,2),cex=0.8)}
points(YEST,subset(LakeO.xtab,DATE==YEST)$recess_7day,pch=21,bg="indianred1",cex=1.25)
text.x=if(YEST>date.fun(paste0(CurWY,"-04-01"))){YEST-ddays(15)}else{YEST+ddays(15)}
if(subset(LakeO.xtab,DATE==YEST)$recess_7day!=0){text(text.x,subset(LakeO.xtab,DATE==YEST)$recess_7day,round(subset(LakeO.xtab,DATE==YEST)$recess_7day,2),cex=0.8)}
abline(h=-0.5,lty=2,col="black",lwd=2); #max 30d recession - HAB deviation
axis_fun(1,xmaj,xmin,format(xmaj,"%b"),cex.axis=1)
axis_fun(2,ymaj,ymin,format(ymaj),cex.axis=1)
box(lwd=lwd.val)
mtext(side=1,"Month",line=2.25,cex=1.5)
mtext(side=2,"Recession/Ascension Rate",line=2.5,cex=1.5)

# plot(0:1,0:1,type = 'n', axes = F,xlab=NA, ylab=NA)
legend("topright",
       legend=c("Moving 7-day (Ft 7-days\u207B\u00B9)","Moving 30-day (Ft 30-days\u207B\u00B9)","Max 30-day Recession Rate\nHAB Deviation"),
       col=c("indianred1","dodgerblue1","black"),lty=c(2,1,2),lwd=c(1.5),ncol=1,cex=0.8,bty="n",y.intersp=1,x.intersp=0.5,xpd=NA,xjust=0.5)

logo=png::readPNG("c:/Julian_LaCie/_GitHub/CRE_Conditions/report/horiz_SCCF_Logo.png")
grid::grid.raster(logo,x=0.05,y=0.03,just=c("left","bottom"),width=unit(1.5,"inches"))

