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

# GIS libraries 
library(rgdal)
library(rgeos)
library(raster)

## Paths
wd="C:/Julian_LaCie/_GitHub/CRE_Conditions"

paths=paste0(wd,c("/Plots/","/Export/","/report/RSdata/","/GIS"))
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

## 
list.files(data.path)
noaa.HAB=readLines("https://products.coastalscience.noaa.gov/habs_explorer/index.php?path=ajZiOVoxaHZNdE5nNytEb3RZdU5iYjNnK3AvTWRrYmNWbXU0K0YvMlA1UlBtTWZlRFV3R1RicVRYb2pxeVJBUA==")

vals=grep("<section class='onecol habonecol'><a href='https://products.coastalscience.noaa.gov/habs_explorer/index.ph",noaa.HAB)

noaa.image.inventory=data.frame()
# for(i in 1:6){
for(i in 1:length(vals)){
  tmp=noaa.HAB[vals[i]]
  tmp2=strsplit(tmp,"<a|</a>")[[1]]
  tmp3=strsplit(tmp2[2],"href=|title=|>")
  tmp4=strsplit(tmp2[3],"</section>")
  
  dat=data.frame(fileadd=sapply(tmp3,"[",2),filename=sapply(tmp4,"[",1))
  
  fname.vals=strsplit(dat$filename,"_")
  if(trimws(substr(sapply(fname.vals,"[",1),1,9))!="sentinel"){next}else{
    dat$data.product=sapply(fname.vals,"[",length(fname.vals[[1]]))
    
    date.vals=strsplit(sapply(fname.vals,"[",1),"\\.")
    yr.val=as.numeric(substr(sapply(date.vals,"[",2),1,4))
    month.val=as.numeric(substr(sapply(date.vals,"[",3),1,2))
    day.val=as.numeric(substr(sapply(date.vals,"[",3),3,4))
    dat$date=as.Date(paste(yr.val,month.val,day.val,sep="-"))
    
    noaa.image.inventory=rbind(dat,noaa.image.inventory)
  }
}

noaa.HAB.image=subset(noaa.image.inventory,data.product=="3.CIcyano.LakeOkee.tif"&date==max(noaa.image.inventory$date))

noaa.HAB.image=subset(noaa.image.inventory,data.product=="3.CIcyano.LakeOkee.tif")
# noaa.HAB.image$filename
noaa.HAB.image$fnames=with(noaa.HAB.image,paste0(format(date,"%Y%m%d"),"_LOK_CIcyano.tif"))
# fnames=with(noaa.HAB.image,paste0(format(date,"%Y%m%d"),"_LOK_CIcyano.tif"))

# date.range=seq(date.fun("2022-05-01"),date.fun(max(noaa.HAB.image$date)),"1 days")


date.range=seq(date.fun(max(noaa.HAB.image$date)-lubridate::duration(1,"month")),date.fun(max(noaa.HAB.image$date)),"1 days")
noaa.HAB.image=subset(noaa.HAB.image,date.fun(date)%in%date.range)

fnames=noaa.HAB.image$fnames



for(i in 1:length(fnames)){
  tmp.raster=raster(paste(data.path, fnames[i],sep="/"))
  tmp.raster=mask(tmp.raster,gBuffer(lakeO,width=500))
  
  cloud.area=tmp.raster==253
  cloud.area.raster=cloud.area
  
  vals=c(0,250,251,252,253,254,255)
  tmp.raster[tmp.raster%in%vals]=NA
  tmp.raster=calc(tmp.raster,fun=function(x) x*1)
  
  rev.scale=calc(tmp.raster,fun=ci.reverse.scaling.fun)*100000000
  
  area=rev.scale>0
  val=cellStats(area,sum)*raster::res(area)[1]*raster::res(area)[2]
  # val*3.86102e-7 # converts from m2 to mi2
  
  if(cellStats(calc(rev.scale,fun=function(x) is.na(x)),min)==1){next}
  tmp.date=subset(noaa.HAB.image,fnames==fnames[i])$date
  png(filename=paste0(plot.path,"BGAlgae/",format(tmp.date,"%Y%m%d"),"_CiCyano.png"),width=6.5,height=5,units="in",res=200,type="windows",bg="white")
  
  par(family="serif",mar=c(0.5,0.5,0.5,0.5),oma=c(0.1,0.1,0.1,0.1));
  layout(matrix(1:2,1,2,byrow = T),widths=c(1,0.4))
  
  b=c(0,20,100,500,1000,6300)*1000
  cols=viridisLite::turbo(249,direction=1)
  plot(lakeO,lwd=0.05)
  plot(lakeO.lit,lwd=0.05,col=adjustcolor("honeydew2",0.5),border=NA,add=T)
  plot(lakeO,lwd=0.05,add=T)
  image(rev.scale,add=T,col = cols)
  image(cloud.area.raster,add=T,col=c(NA,"grey"))
  plot(rasterToContour(rev.scale,levels=b,nlevels=length(b)),col="black",lwd=2,add=T)
  mapmisc::scaleBar(utm17,"bottomright",bty="n",cex=1,seg.len=4,outer=F)
  mtext(side=3,line=-2.5,adj=0,paste("Date:",format(tmp.date,"%m-%d-%Y"),
                                   "\nData Source: NOAA NCCOS\nAlgae Coverage:",
                                   round(val*1e-6),"km\u00B2"))
  
  plot(0:1,0:1,ann=F,axes=F,type="n")
  b2=b/1000
  l.b=length(b2)
  labs=b2
  n.bks=length(b2) -1
  top.val=0.8
  bot.val=0.2
  mid.v.val=bot.val+(top.val-bot.val)/2
  x.max=0.3
  x.min=0
  mid.val=x.min+(x.max-x.min)/2
  txt.offset.val=-0.01
  lab.pos=seq(bot.val,top.val,length.out=l.b)
  legend_image=as.raster(matrix(rev(cols),ncol=1))
  rasterImage(legend_image,x.min,bot.val,x.max,top.val)
  text(x=x.max, y = lab.pos, labels = format(b2),cex=0.75,adj=0,pos=4,offset=0.5)
  segments(rep(x.min,l.b),lab.pos,rep(x.max,l.b),lab.pos,lwd=2)
  #add cloud
  legend_image=as.raster(matrix("grey",ncol=1))
  rasterImage(legend_image,x.min,bot.val-0.05,x.max,bot.val)
  text(x=x.max, y = bot.val-0.025, labels = "Clouds",cex=0.5,adj=0,pos=4,offset=0.5)
  
  legend_image=as.raster(matrix(adjustcolor("honeydew2",0.5),ncol=1))
  rasterImage(legend_image,x.min,bot.val-0.1,x.max,bot.val-0.05)
  text(x=x.max, y = bot.val-0.075, labels = "Littoral Zone",cex=0.5,adj=0,pos=4,offset=0.5)
  # bx.val= seq(bot.val,top.val,(top.val-bot.val)/n.bks)
  # rect(x.min,bx.val[1:n.bks],x.max,bx.val[2:(n.bks+1)],col=rev(col.rmp),lty=0)
  # text(y=bx.val[2:(n.bks+1)]-c(mean(diff(bx.val[2:(n.bks+1)]))/2), x = x.max, labels = rev(labs),cex=0.75,xpd=NA,pos=4,adj=0)
  text(x=mid.val,y=top.val,expression(paste("CI"["Cyano"]," (cells mL"^"-1","x1000)")),adj=0,cex=0.8,pos=3,xpd=NA)
  dev.off()
}



file.names=paste0(plot.path,"BGAlgae/",list.files(paste0(plot.path,"BGAlgae/")))

gifski::gifski(file.names, 
               delay = 70 / 100, 
               gif_file  = paste0(plot.path,"BGAlgae/LOK_BGAlgae_month_gifski.gif"),
               loop = T)
