

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
  83.3 * (log10(ci[ci>0]) + 4.2)
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

tmp.rslt=data.frame(date=noaa.HAB.image$date[i],
                    cloud.area=cloud.area,
                    bloom.area.m2=val)
cyano_area=rbind(cyano_area,tmp.rslt)
}

# png(filename=paste0(plot.path,"CiCyano.png"),width=6.5,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(0.5,0.5,0.5,0.5),oma=c(0.1,0.1,0.1,0.1));
layout(matrix(1:2,1,2,byrow = T),widths=c(1,0.4))

b=c(0,20,100,1000,6300)*1000
cols=viridisLite::turbo(249,direction=1)
plot(lakeO,lwd=0.05)
image(rev.scale,add=T,col = cols)
plot(rasterToContour(rev.scale,levels=b,nlevels=length(b)),col="black",lwd=2,add=T)
mapmisc::scaleBar(utm17,"bottomright",bty="n",cex=1,seg.len=4,outer=F)
mtext(side=3,line=-2,adj=0,paste("Date:",format(noaa.HAB.image$date[i],"%m-%d-%Y"),"\nData Source: NOAA NCCOS"))

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
# bx.val= seq(bot.val,top.val,(top.val-bot.val)/n.bks)
# rect(x.min,bx.val[1:n.bks],x.max,bx.val[2:(n.bks+1)],col=rev(col.rmp),lty=0)
# text(y=bx.val[2:(n.bks+1)]-c(mean(diff(bx.val[2:(n.bks+1)]))/2), x = x.max, labels = rev(labs),cex=0.75,xpd=NA,pos=4,adj=0)
text(x=mid.val,y=top.val,expression(paste("CI"["Cyano"]," (cells mL"^"-1","x1000)")),adj=0,cex=0.8,pos=3,xpd=NA)
dev.off()

cyano_area$cloud.area.per=cyano_area$cloud.area/LOK.area
# cyano_area$bloom.area.m2.scn=with(cyano_area,ifelse(cloud.area.per>0.25,NA,bloom.area.m2))
cyano_area$bloom.area.mi2=cyano_area$bloom.area.m2*3.86102e-7
cyano_area$bloom.area.per=(cyano_area$bloom.area.m2/LOK.area)*100
cyano_area$date=date.fun(cyano_area$date)
cyano_area$date2=lubridate::decimal_date(cyano_area$date)

dev.off()
plot(bloom.area.mi2~date,cyano_area,type="l")

# png(filename=paste0(plot.path,"2022_LOK_Cyano.png"),width=6.5,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,3,0.5,2),oma=c(2,2,0.5,2))
layout(matrix(1:2,2,1),heights=c(0.5,1))

xlim.val=c(date.fun("2022-04-01"),date.fun(Sys.Date()+lubridate::duration(1,"months")));xmaj=seq(xlim.val[1],xlim.val[2],"1 months");xmin=seq(xlim.val[1],xlim.val[2],"1 days")
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

ylim.val=c(0,max(cyano_area$bloom.area.mi2)*1.10);by.y=100;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(cloud.area.per~date,cyano_area,axes=F,ann=F,type="n",ylim=ylim.val,xlim=xlim.val,yaxs="i")
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.75)
with(cyano_area,pt_line(date,bloom.area.mi2,1,"dodgerblue1",2,21,"dodgerblue1",pt.lwd=0.1,cex=1.25))
with(subset(cyano_area,date==max(cyano_area$date)),
     text(date,bloom.area.mi2,paste0(round(bloom.area.mi2),"mi\u00B2\n(",round(bloom.area.per),"%)"),pos=4,offset=0.5,cex=0.75))
k.mod=loess(bloom.area.mi2~date2,subset(cyano_area,bloom.area.mi2>0))
x.val=seq(min(cyano_area$date2),max(cyano_area$date2),length.out=100)
pred.mod=predict(k.mod,data.frame(date2=x.val))
lines(date.fun(lubridate::date_decimal(x.val),form="%F %R"),pred.mod,col="red",lwd=2)
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,xmaj,xmin,format(xmaj,"%b-%Y"),line=-0.5)
axis_fun(1,xmaj,xmin,NA)
box(lwd=1)
mtext(side=1,line=1.5,"Date (Month-Year)")
mtext(side=2,line=2.5,"Cyanobacteria Algal Bloom\ncoverage (mi\u00B2)")
axis_fun(4,ymaj,ymin,round((ymaj/(LOK.area*3.861e-7))*100,0))
mtext(side=4,line=2.5,"Cyanobacteria Algal Bloom\ncoverage (% LOK)")
legend("topleft",legend=c("Smoothed Trend"),
       lty=c(1),lwd=c(1),col=c("red"),
       pch=c(NA),pt.bg=c(NA),
       pt.cex=1.25,ncol=1,cex=0.75,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0)

dev.off()