

library(AnalystHelper)
library(sp)
library(rgdal)
library(rgeos)
library(raster)

wd="C:/Julian_LaCie/_GitHub/CRE_Conditions"
data.path=paste0(wd,"/Data/tmp/")
plot.path=paste0(wd,"/Plots/")

GIS.path.gen="C:/Julian_LaCie/_GISData"


utm17=CRS("+init=epsg:26917")

# -------------------------------------------------------------------------


lakeO=spTransform(readOGR(paste0(GIS.path.gen,"/SFWMD"),"LakeOkeechobee_general"),wkt(utm17))
plot(lakeO)
area(lakeO)

# -------------------------------------------------------------------------


#file="sentinel-3b.2021313.1109.1502C.L3.SF3.v950V20193_1_2.CIcyano.LakeOkee.tif"
file="20210916CIcyano.tiff"

gdalUtils::gdalinfo(paste0(data.path,file))

test.raster=raster(paste0(data.path,file))

plot(test.raster)
range(test.raster)
extent(test.raster)
crs(test.raster)


vals=c(0,250,251,252,253,254,255)
test.raster2=test.raster
test.raster2[test.raster2%in%vals]=NA
plot(test.raster2)

dev.off()

ci.reverse.scaling.fun=function(DN){
  10^(3.0 / 250.0 * DN - 4.2)
}
ci.scaling.fun=function(ci){
  round(83.3 * (log10(ci[ci>0]) + 4.2))
}
# plot(round(83.3*(log10(values(test.raster2)[values(test.raster2)>0])+4.2)))
# plot(10**(3.0/250.0*values(test.raster2)-4.2))

test.calc1=calc(test.raster2,fun=ci.reverse.scaling.fun)
plot(test.calc1)

plot(calc(test.calc1,ci.scaling.fun))
plot(calc(test.raster2,fun=function(x) x*1))





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
  dat$data.product=sapply(fname.vals,"[",length(fname.vals[[1]]))
  
  date.vals=strsplit(sapply(fname.vals,"[",1),"\\.")
  yr.val=as.numeric(substr(sapply(date.vals,"[",2),1,4))
  month.val=as.numeric(substr(sapply(date.vals,"[",3),1,2))
  day.val=as.numeric(substr(sapply(date.vals,"[",3),3,4))
  dat$date=as.Date(paste(yr.val,month.val,day.val,sep="-"))
  
  noaa.image.inventory=rbind(dat,noaa.image.inventory)
}

noaa.HAB.image=subset(noaa.image.inventory,data.product=="3.CIcyano.LakeOkee.tif"&date==max(noaa.image.inventory$date))

download.file(noquote(gsub("'", '', noaa.HAB.image$fileadd, fixed=TRUE)),paste0(data.path, noaa.HAB.image$filename),mode="wb")


test.raster=raster(paste0(data.path, noaa.HAB.image$filename))

plot(test.raster)
range(test.raster)
extent(test.raster)
crs(test.raster)

vals=c(0,250,251,252,253,254,255)
test.raster2=test.raster
test.raster2[test.raster2%in%vals]=NA
plot(test.raster2)

ci.reverse.scaling.fun
ci.scaling.fun

test.raster2=calc(test.raster2,fun=function(x) x*1)
plot(test.raster2)

ci.scale=calc(test.raster2,ci.scaling.fun)
plot(ci.scale)

rev.scale=calc(ci.scale,fun=ci.reverse.scaling.fun)
plot(rev.scale)

test=calc(test.raster2,ci.reverse.scaling.fun)
plot(test)
test2=calc(test,ci.scaling.fun)
plot(test2)


plot(test.raster2)

area=rev.scale>0
val=cellStats(area,sum)*raster::res(area)[1]*raster::res(area)[2]
val*3.86102e-7






# 2022 Tracking -----------------------------------------------------------
LOK.area=area(lakeO)

# From tif header tif
ci.reverse.scaling.fun=function(DN){
  10^(3.0 / 250.0 * DN - 4.2)
}
ci.scaling.fun=function(ci){
  round(83.3 * (log10(ci[ci>0]) + 4.2))
}

noaa.HAB.image=subset(noaa.image.inventory,data.product=="3.CIcyano.LakeOkee.tif")
noaa.HAB.image$filename

fnames=with(noaa.HAB.image,paste0(format(date,"%Y%m%d"),"_LOK_CIcyano.tif"))

for(i in 1:nrow(noaa.HAB.image)){
download.file(noquote(gsub("'", '', noaa.HAB.image$fileadd[i], fixed=TRUE)),paste0(data.path,"sentinel_2022/", fnames[i]),mode="wb")
  print(i)
}


i=1
cyano_area=data.frame()
for(i in 1:length(fnames)){
tmp.raster=raster(paste0(data.path,"sentinel_2022/", fnames[i]))

# plot(tmp.raster)
# plot(mask(tmp.raster,gBuffer(lakeO,width=500)))
tmp.raster=mask(tmp.raster,gBuffer(lakeO,width=500))

# plot(tmp.raster)
# crs(tmp.raster)

cloud.area=tmp.raster==253
cloud.area=cloud.area==1
cloud.area=cellStats(cloud.area,sum)*raster::res(cloud.area)[1]*raster::res(cloud.area)[2]

# remove cloud/no data values (see tif header)
vals=c(0,250,251,252,253,254,255)
tmp.raster[tmp.raster%in%vals]=NA

tmp.raster=calc(tmp.raster,fun=function(x) x*1)
ci.scale=calc(tmp.raster,ci.scaling.fun)
rev.scale=calc(tmp.raster,fun=ci.reverse.scaling.fun)

area=rev.scale>0
val=cellStats(area,sum)*raster::res(area)[1]*raster::res(area)[2]
# val*3.86102e-7 # converts from m2 to mi2

tmp.rslt=data.frame(date=noaa.HAB.image$date[i],
                    cloud.area=cloud.area,
                    bloom.area.m2=val)
cyano_area=rbind(cyano_area,tmp.rslt)
}
cyano_area$cloud.area.per=cyano_area$cloud.area/LOK.area
# cyano_area$bloom.area.m2.scn=with(cyano_area,ifelse(cloud.area.per>0.25,NA,bloom.area.m2))
cyano_area$bloom.area.mi2=cyano_area$bloom.area.m2*3.86102e-7

cyano_area$date=date.fun(cyano_area$date)

dev.off()
plot(bloom.area.mi2~date,cyano_area,type="l")

# png(filename=paste0(plot.path,"2022_LOK_Cyano.png"),width=6.5,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,3,0.5,1),oma=c(2,2,0.5,0.75))
layout(matrix(1:2,2,1),heights=c(0.5,1))

xlim.val=date.fun(c("2022-04-01","2022-06-01"));xmaj=seq(xlim.val[1],xlim.val[2],"1 months");xmin=seq(xlim.val[1],xlim.val[2],"1 days")
ylim.val=c(0,1);by.y=0.2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

plot(cloud.area.per~date,cyano_area,axes=F,ann=F,type="n",ylim=ylim.val,xlim=xlim.val,yaxs="i")
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.75)
with(cyano_area,pt_line(date,cloud.area.per,2,"grey",2,21,"grey"))
axis_fun(2,ymaj,ymin,ymaj*100)
# axis_fun(1,xmaj,xmin,format(xmaj,"%m-%Y"))
axis_fun(1,xmaj,xmin,NA)
box(lwd=1)
mtext(side=2,line=2.5,"Cloud Cover (%)")
mtext(side=3,adj=1,"Data Source: NOAA NCCOS",font=3)
mtext(side=3,adj=0,"Lake Okeechobee",font=3)

ylim.val=c(0,200);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(cloud.area.per~date,cyano_area,axes=F,ann=F,type="n",ylim=ylim.val,xlim=xlim.val,yaxs="i")
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.75)
with(cyano_area,pt_line(date,bloom.area.mi2,1,"dodgerblue1",2,21,"dodgerblue1",pt.lwd=0.1,cex=1.25))
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,xmaj,xmin,format(xmaj,"%b-%Y"),line=-0.5)
axis_fun(1,xmaj,xmin,NA)
box(lwd=1)
mtext(side=1,line=1.5,"Date (Month-Year)")
mtext(side=2,line=2.5,"Cyanobacteria Algal Bloom\ncoverage (mi\u00B2)")
dev.off()