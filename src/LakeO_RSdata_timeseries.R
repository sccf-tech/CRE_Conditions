## 
## BGAlgae remote sensing data animation
##
## Code was compiled by Paul Julian
## contact info: pauljulianphd@gmail.com

## BAD 
## https://www.tidyverse.org/articles/2017/12/workflow-vs-script/
## Clears Everything...start fresh.
rm(list=ls(all=T));cat("\014");dev.off()

## Libraries
#devtools::install_github("SwampThingPaul/AnalystHelper")
library(AnalystHelper);
library(plyr)
library(zoo)

# GIS libraries 
library(rgdal)
library(rgeos)
library(raster)

## Paths
wd="C:/Julian_LaCie/_GitHub/CRE_Conditions"

paths=paste0(wd,c("/Plots/","/Export/","/data/CyanoHAB_RS","/GIS"))
# Folder.Maker(paths);#One and done. Creates folders in working directory.
plot.path=paths[1]
export.path=paths[2]
data.path=paths[3]
GIS.path=paths[4]
GIS.path.gen="C:/Julian_LaCie/_GISData"

# Helper variables
nad83.pro=CRS("+init=epsg:4269")
utm17=CRS("+init=epsg:26917")

# Functions
decdate.fun=function(date){
  Y=as.numeric(format(date,"%Y"))
  start=date.fun(paste(Y,1,1,sep="-"))
  end=date.fun(paste(Y+1,1,1,sep="-"))
  sofar <- as.numeric(difftime(date, start, units = "secs"))
  total <- as.numeric(difftime(end, start, units = "secs"))
  Y + sofar / total
}
ci.reverse.scaling.fun=function(DN){
  10^(3.0 / 250.0 * DN - 4.2)
}
ci.scaling.fun=function(ci){
  round(83.3 * (log10(ci[ci>0]) + 4.2))
}

# -------------------------------------------------------------------------
# GIS Data
lakeO=readOGR("C:/Julian_LaCie/_GitHub/CRE_Conditions/report/GISData","LakeOkeechobee_general")
lakeO=spTransform(lakeO,utm17)

lakeO.lit=readOGR("C:/Julian_LaCie/_GitHub/CRE_Conditions/report/GISData","LOK_littoral")
lakeO.lit=spTransform(lakeO.lit,utm17)

LOK.area=area(lakeO)
YRS=c(2016:2021)
## 2016 -------------------------------------------------------------------
CI.files=c()

for( i in 1:length(YRS)){
  files=list.files(paste0(data.path,"/",YRS[i],"/"))
  tmp.files=files[grep("1_2.CIcyano.LakeOkee.",files)]
  CI.files=c(tmp.files,CI.files)
  print(i)
}

# files=list.files(paste0(data.path,"2016/"))
# CI.files=files[grep("1_2.CIcyano.LakeOkee.",files)]

date.vals=strsplit(sapply(CI.files,"[",1),"\\.")
yr.val.path=as.numeric(substr(sapply(date.vals,"[",2),1,4))

pb=txtProgressBar(min=0,max=length(CI.files),style=3)
cyano_area=data.frame()
for(i in 1:length(CI.files)){

tmp.raster=raster(paste(data.path,yr.val.path[i], CI.files[i],sep="/"))
tmp.raster=mask(tmp.raster,gBuffer(lakeO,width=500))

# remove cloud/no data values (see tif header)
# 0= nodetect
# 250 = saturated; 251 = ci adj; 252 = land; 253 = cloud;
# 254 = mixed pixel; 255 = no data

cloud.area=tmp.raster==253
cloud.area.raster=cloud.area
cloud.area=cellStats(cloud.area,sum)*raster::res(cloud.area)[1]*raster::res(cloud.area)[2]

other.area=tmp.raster%in%c(0,250,251,254,255)
other.area.raster=other.area
other.area=cellStats(other.area,sum)*raster::res(other.area)[1]*raster::res(other.area)[2]

vals=c(0,250,251,252,253,254,255)
tmp.raster[tmp.raster%in%vals]=NA

rev.scale=calc(tmp.raster,fun=ci.reverse.scaling.fun)*100000000

med.val=cellStats(rev.scale,median,na.rm=T)
mean.val=cellStats(rev.scale,mean,na.rm=T)
max.val=cellStats(rev.scale,max,na.rm=T)
min.val=cellStats(rev.scale,min,na.rm=T)
sd.val=cellStats(rev.scale,sd,na.rm=T)
N.val=N.obs(getValues(rev.scale))

area=rev.scale>0
val=cellStats(area,sum)*raster::res(area)[1]*raster::res(area)[2]


area2=rev.scale>(1000*1000)
val2=cellStats(area2,sum)*raster::res(area2)[1]*raster::res(area2)[2]

date.vals=strsplit(sapply(CI.files[i],"[",1),"\\.")

yr.val=as.numeric(substr(sapply(date.vals,"[",2),1,4))
month.val=as.numeric(substr(sapply(date.vals,"[",3),1,2))
day.val=as.numeric(substr(sapply(date.vals,"[",3),3,4))
date=as.Date(paste(yr.val,month.val,day.val,sep="-"))

tmp.rslt=data.frame(date=date,
                    cloud.area=cloud.area,
                    bloom.area.m2=val,
                    vis.bloom=val2,
                    median=med.val,
                    mean=mean.val,
                    min=min.val,
                    max=max.val,
                    sd=sd.val,
                    N=N.val)
cyano_area=rbind(cyano_area,tmp.rslt)
setTxtProgressBar(pb, i)
}

cyano_area$cloud.area.per=cyano_area$cloud.area/LOK.area
# cyano_area$bloom.area.m2.scn=with(cyano_area,ifelse(cloud.area.per>0.25,NA,bloom.area.m2))
cyano_area$bloom.area.mi2=cyano_area$bloom.area.m2*3.86102e-7
cyano_area$bloom.area.per=(cyano_area$bloom.area.m2/LOK.area)*100
cyano_area$date=date.fun(cyano_area$date)
cyano_area$date2=decdate.fun(cyano_area$date)
cyano_area$vis.bloom=cyano_area$vis.bloom*3.86102e-7

# write.csv(cyano_area,paste0(export.path,"CIHAB_ts.csv"),row.names = F)
cyano_area=read.csv(paste0(export.path,"CIHAB_ts.csv"))
cyano_area$date=date.fun(cyano_area$date)

plot(mean~date,cyano_area)
plot(median~date,cyano_area)
plot(bloom.area.per~date,cyano_area)
beepr::beep(4)

cyano_area$DOY=as.numeric(format(cyano_area$date,'%j'))
cyano_area$CY=as.numeric(format(cyano_area$date,'%Y'))

plot(bloom.area.per~DOY,cyano_area)
plot(median~DOY,cyano_area,log="y")


## 
cyano_area2=cyano_area[order(cyano_area$date),]
cyano_area2=merge(cyano_area2,
                  data.frame(date=seq(min(cyano_area2$date),max(cyano_area2$date),"1 days"),
                             fill=1),
                  "date",all.y=T)
cyano_area2$DOY=as.numeric(format(cyano_area2$date,'%j'))
cyano_area2$CY=as.numeric(format(cyano_area2$date,'%Y'))

tmp2=ddply(cyano_area2,"DOY",summarise,
          min.val=min(ifelse(bloom.area.per==0,NA,bloom.area.per),na.rm=T),
          median.val=median(ifelse(bloom.area.per==0,NA,bloom.area.per),na.rm=T),
          max.val=max(ifelse(bloom.area.per==0,NA,bloom.area.per),na.rm=T),
          q1=quantile(ifelse(bloom.area.per==0,NA,bloom.area.per),na.rm=T,prob=0.25),
          q3=quantile(ifelse(bloom.area.per==0,NA,bloom.area.per),na.rm=T,prob=0.75))
tmp2$max.MA=with(tmp2,c(rep(NA,29),rollapply(max.val,width=30,FUN=function(x)mean(x,na.rm=T))))
tmp2$min.MA=with(tmp2,c(rep(NA,29),rollapply(min.val,width=30,FUN=function(x)mean(x,na.rm=T))))
tmp2$median.MA=with(tmp2,c(rep(NA,29),rollapply(median.val,width=30,FUN=function(x)mean(x,na.rm=T))))

plot(q1~DOY,tmp2,ylim=c(0,70),type="l")
lines(q3~DOY,tmp2)
lines(median.val~DOY,tmp2)
with(tmp2,shaded.range(DOY,q1,q3,"grey",lty=1))


ylim.val=c(0,70);by.y=20;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(1,366);by.x=60;xmaj=c(1,60,120,180,240,300,360);xmin=seq(0,xlim.val[2],by.x/2)

plot(q1~DOY,tmp2,axes=F,ann=F,type="n",ylim=ylim.val,xlim=xlim.val,yaxs="i")
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.75)
with(tmp2,shaded.range(DOY,min.MA,max.MA,"limegreen",lty=1))
lines(median.mean~DOY.date,tmp2,lty=1,col="forestgreen")
lines(median.MA~DOY,tmp2,lwd=2)
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
box(lwd=1)
mtext(side=1,line=1.5,"DOY")
mtext(side=2,line=2.5,"Cyanobacteria Algal Bloom\ncoverage (% LOK)")
legend("topleft",legend=c("2016 - 2021 30d MA range","2016 - 2021 30d MA median"),
       lty=c(NA,1,NA),lwd=c(0.1,1,0.1),col=c("limegreen","forestgreen","black"),
       pch=c(22,NA,21),pt.bg=c(adjustcolor("limegreen",0.5),NA,"indianred1"),
       pt.cex=1.25,ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0)
mtext(side=3,adj=1,"Data Source: NOAA NCCOS",font=3)
mtext(side=3,adj=0,"Lake Okeechobee",font=3)


cyano_area2$area.per.MA=with(cyano_area2,c(rep(NA,29),
                                           rollapply(
                                             ifelse(bloom.area.per==0&cloud.area.per>0.4,NA,bloom.area.per),
                                             width=30,FUN=function(x)mean(x,na.rm=T))))


tmp=ddply(cyano_area2,"DOY",summarise,
          min.mean=min(area.per.MA,na.rm=T),
          median.mean=median(area.per.MA,na.rm=T),
          max.mean=max(area.per.MA,na.rm=T))

plot(max.mean~DOY,tmp,type="l",ylim=c(0,50))
lines(min.mean~DOY,tmp)
with(tmp,shaded.range(DOY,min.mean,max.mean,"grey"))


data.path2="C:/Julian_LaCie/_GitHub/CRE_Conditions/report/RSdata"
fnames=list.files(data.path2)


yr.val=substr(fnames,1,4)
month.val=substr(fnames,5,6)
day.val=substr(fnames,7,8)
date=as.Date(paste(yr.val,month.val,day.val,sep="-"))

cyano_area_2022=data.frame()
for(i in 1:length(fnames)){
  tmp.raster=raster(paste(data.path2, fnames[i],sep="/"))
  
  # plot(tmp.raster)
  # plot(mask(tmp.raster,gBuffer(lakeO,width=500)))
  tmp.raster=mask(tmp.raster,gBuffer(lakeO,width=500))
  
  # plot(tmp.raster)
  # crs(tmp.raster)
  # tmp.raster>250
  # tmp.raster%in%c(250,251,253,254,255)
  
  cloud.area=tmp.raster==253
  # cloud.area=tmp.raster%in%c(253)
  cloud.area.raster=cloud.area
  # cloud.area=cloud.area==1
  cloud.area=cellStats(cloud.area,sum)*raster::res(cloud.area)[1]*raster::res(cloud.area)[2]
  
  # remove cloud/no data values (see tif header)
  # 0= nodetect
  # 250 = saturated; 251 = ci adj; 252 = land; 253 = cloud;
  # 254 = mixed pixel; 255 = no data
  
  vals=c(0,250,251,252,253,254,255)
  tmp.raster[tmp.raster%in%vals]=NA
  
  tmp.raster=calc(tmp.raster,fun=function(x) x*1)
  # ci.scale=calc(tmp.raster,fun=function(x) ci.scaling.fun(x))
  rev.scale=calc(tmp.raster,fun=ci.reverse.scaling.fun)*100000000
  
  area=rev.scale>0
  val=cellStats(area,sum)*raster::res(area)[1]*raster::res(area)[2]
  # val*3.86102e-7 # converts from m2 to mi2
  
  area2=rev.scale>(1000*1000)
  val2=cellStats(area2,sum)*raster::res(area2)[1]*raster::res(area2)[2]
  
  tmp.rslt=data.frame(date=date[i],
                      cloud.area=cloud.area,
                      bloom.area.m2=val,
                      vis.bloom=val2)
  cyano_area_2022=rbind(cyano_area_2022,tmp.rslt)
}

cyano_area_2022$cloud.area.per=cyano_area_2022$cloud.area/LOK.area
# cyano_area_2022$bloom.area.m2.scn=with(cyano_area_2022,ifelse(cloud.area.per>0.25,NA,bloom.area.m2))
cyano_area_2022$bloom.area.mi2=cyano_area_2022$bloom.area.m2*3.86102e-7
cyano_area_2022$bloom.area.per=(cyano_area_2022$bloom.area.m2/LOK.area)*100
cyano_area_2022$date=date.fun(cyano_area_2022$date)
cyano_area_2022$date2=decdate.fun(cyano_area_2022$date)
cyano_area_2022$vis.bloom=cyano_area_2022$vis.bloom*3.86102e-7

cyano_area_2022_2=cyano_area_2022[order(cyano_area_2022$date),]
cyano_area_2022_2=merge(cyano_area_2022_2,
                  data.frame(date=seq(min(cyano_area_2022_2$date),max(cyano_area_2022_2$date),"1 days"),
                             fill=1),
                  "date",all.y=T)
cyano_area_2022_2$DOY=as.numeric(format(cyano_area_2022_2$date,"%j"))
cyano_area_2022_2$area.per.MA=with(cyano_area_2022_2,c(rep(NA,29),
                                           rollapply(
                                             ifelse(bloom.area.per==0&cloud.area.per>0.4,NA,bloom.area.per),
                                             width=30,FUN=function(x)mean(x,na.rm=T))))


plot(max.mean~DOY,tmp,type="l",ylim=c(0,50))
lines(min.mean~DOY,tmp)
with(tmp,shaded.range(DOY,min.mean,max.mean,"grey"))

lines(bloom.area.per~DOY,cyano_area_2022,lwd=2)
lines(area.per.MA~DOY,cyano_area_2022_2,lwd=2)

tmp2$DOY[tmp2$DOY==366]=NA
tmp2$DOY.date=date.fun(as.Date(tmp2$DOY,origin="2022-01-01"))
tmp2=subset(tmp2,is.na(DOY)==F)

# png(filename=paste0(plot.path,"CiCyano_area_ts.png"),width=6.5,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,3,0.5,1),oma=c(2,2,0.5,0.5))
xlim.val=c(date.fun("2022-04-01"),date.fun(Sys.Date()+lubridate::duration(1,"months")));xmaj=seq(xlim.val[1],xlim.val[2],"1 months");xmin=seq(xlim.val[1],xlim.val[2],"1 days")
ylim.val=c(0,40);by.y=10;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

plot(max.mean~DOY.date,tmp,axes=F,ann=F,type="n",ylim=ylim.val,xlim=xlim.val,yaxs="i")
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.75)
with(tmp,shaded.range(DOY.date,min.mean,max.mean,"limegreen",lty=1))
lines(median.mean~DOY.date,tmp,lty=1,col="forestgreen")
lines(area.per.MA~date,cyano_area_2022_2,lwd=2)
with(cyano_area_2022_2,pt_line(date,area.per.MA,2,"black",1,21,"indianred1",cex=0.8))
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,xmaj,xmin,format(xmaj,"%b-%Y"),line=-0.5)
box(lwd=1)
mtext(side=1,line=1.5,"Date (Month-Year)")
mtext(side=2,line=2.5,"Cyanobacteria Algal Bloom\ncoverage (% LOK)")
legend("topleft",legend=c("2016 - 2021 30d MA range","2016 - 2021 30d MA median","30d MA coverage"),
       lty=c(NA,1,NA),lwd=c(0.1,1,0.1),col=c("limegreen","forestgreen","black"),
       pch=c(22,NA,21),pt.bg=c(adjustcolor("limegreen",0.5),NA,"indianred1"),
       pt.cex=1.25,ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0)
mtext(side=3,adj=1,"Data Source: NOAA NCCOS",font=3)
mtext(side=3,adj=0,"Lake Okeechobee",font=3)
dev.off()



# bloom duration metric ---------------------------------------------------

for(j in 1:length(YRS)){
files=list.files(paste0(data.path,"/",YRS[j],"/"))
tmp.files=files[grep("1_2.CIcyano.LakeOkee.",files)]
CI.files=tmp.files

i=1
date.vals=strsplit(sapply(tmp.files,"[",1),"\\.")
yr.val.path=as.numeric(substr(sapply(date.vals,"[",2),1,4))

tmp.raster=raster(paste(data.path,yr.val.path[i], CI.files[i],sep="/"))
tmp.raster=mask(tmp.raster,gBuffer(lakeO,width=500))
vals=c(0,250,251,252,253,254,255)
tmp.raster[tmp.raster%in%vals]=NA
rev.scale.1=calc(tmp.raster,fun=ci.reverse.scaling.fun)*100000000

i=2
tmp.raster=raster(paste(data.path,yr.val.path[i], CI.files[i],sep="/"))
tmp.raster=mask(tmp.raster,gBuffer(lakeO,width=500))
tmp.raster[tmp.raster%in%vals]=NA
rev.scale.2=calc(tmp.raster,fun=ci.reverse.scaling.fun)*100000000
rast.stack=stack(rev.scale.1,rev.scale.2)

for(i in 3:length(CI.files)){
  tmp.raster=raster(paste(data.path,yr.val.path[i], CI.files[i],sep="/"))
  tmp.raster=mask(tmp.raster,gBuffer(lakeO,width=500))
  
  tmp.raster[tmp.raster%in%vals]=NA
  
  rev.scale.2=calc(tmp.raster,fun=ci.reverse.scaling.fun)*100000000
  
  rast.stack=stack(rast.stack,rev.scale.2)
  print(i)
}

tmp=sum(rast.stack>0,na.rm=T)
tmp=tmp/nlayers(rast.stack>0)
assign(paste0("bloom",YRS[j]),tmp)
print(j)
}


layout(matrix(1:6,2,3,byrow=T))

b=seq(0,0.5,0.1)
cols=viridisLite::magma(length(b),direction=1)
plot(lakeO,lwd=0.05)
image(mask(bloom2016,lakeO),add=T,col = cols)
plot(lakeO.lit,lwd=0.05,col=adjustcolor("honeydew2",0.5),border=NA,add=T)
plot(lakeO,lwd=0.05,add=T,border="white")
mtext(side=3,"2016")

plot(lakeO,lwd=0.05)
image(mask(bloom2017,lakeO),add=T,col = cols)
plot(lakeO.lit,lwd=0.05,col=adjustcolor("honeydew2",0.5),border=NA,add=T)
plot(lakeO,lwd=0.05,add=T,border="white")
mtext(side=3,"2017")

plot(lakeO,lwd=0.05)
image(mask(bloom2018,lakeO),add=T,col = cols)
plot(lakeO.lit,lwd=0.05,col=adjustcolor("honeydew2",0.5),border=NA,add=T)
plot(lakeO,lwd=0.05,add=T,border="white")
mtext(side=3,"2018")

plot(lakeO,lwd=0.05)
image(mask(bloom2019,lakeO),add=T,col = cols)
plot(lakeO.lit,lwd=0.05,col=adjustcolor("honeydew2",0.5),border=NA,add=T)
plot(lakeO,lwd=0.05,add=T,border="white")
mtext(side=3,"2019")

plot(lakeO,lwd=0.05)
image(mask(bloom2020,lakeO),add=T,col = cols)
plot(lakeO.lit,lwd=0.05,col=adjustcolor("honeydew2",0.5),border=NA,add=T)
plot(lakeO,lwd=0.05,add=T,border="white")
mtext(side=3,"2020")

plot(lakeO,lwd=0.05)
image(mask(bloom2021,lakeO),add=T,col = cols)
plot(lakeO.lit,lwd=0.05,col=adjustcolor("honeydew2",0.5),border=NA,add=T)
plot(lakeO,lwd=0.05,add=T,border="white")
mtext(side=3,"2021")

plot(bloom2016)
plot(bloom2017)
plot(bloom2018)
plot(bloom2019)
plot(bloom2020)
plot(bloom2021)
