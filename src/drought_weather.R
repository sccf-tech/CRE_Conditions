## 
## Exploring climate data
## https://michaelpaulschramm.com/posts/2022-07-22-drought/
##
## Code was compiled by Paul Julian
## contact info: pauljulianphd@gmail.com / pjulian@sccf.org

## BAD 
## https://www.tidyverse.org/articles/2017/12/workflow-vs-script/
## Clears Everything...start fresh.
rm(list=ls(all=T));cat("\014");dev.off()

## Libraries
library(prism)
library(terra)


library(rgdal)
library(rgeos)


## Paths
wd="C:/Julian_LaCie/_GitHub/CRE_Conditions"

paths=paste0(wd,c("/Plots/","/Export/",NA,"/GIS"))
# Folder.Maker(paths);#One and done. Creates folders in working directory.
plot.path=paths[1]
export.path=paths[2]
GIS.path=paths[4]
GIS.path.gen="C:/Julian_LaCie/_GISData"


wgs84=CRS("+init=epsg:4326")
utm17=CRS("+init=epsg:26917")
shore=spTransform(readOGR("C:/Julian_LaCie/_GISData/FWC","FWC_Shoreline_simp"),utm17)
# -------------------------------------------------------------------------
## Rain

## download monthly normals
get_prism_normals(type = "ppt",
                  resolution = "4km",
                  mon = 1:12,
                  keepZip = FALSE)

## return the folders
ppt_norm_1 <- prism_archive_subset(type = "ppt",
                                   temp_period = "monthly normals",
                                   mon = 1:12,
                                   resolution = "4km")

## convert to terra
ppt_norm_1 <- pd_to_file(ppt_norm_1)
ppt_norm_1 <- rast(ppt_norm_1)

## change the layer names to something sensible
names(ppt_norm_1) <- month.name[1:12]

ppt_norm_2=terra::project(ppt_norm_1,"+proj=utm +zone=17 +datum=NAD83 +units=m +no_defs")

shore.buf=gBuffer(shore,width=5000)
shore.buf=SpatialPolygonsDataFrame(shore.buf,data.frame(id=1,row.names = "buffer"))

ppt_norm_2_FL=terra::crop(ppt_norm_2,shore.buf)
range(ppt_norm_2_FL)

bbox.lims=bbox(shore)
b=seq(0,300,1)
cols=khroma::colour("lapaz",reverse=T)(length(b))

# png(filename=paste0(plot.path,"PRISM_Florida.png"),width=8,height=6,units="in",res=200,type="windows",bg="white")
# layout(matrix(1:12,4,3,byrow = T))
par(family="serif",mar=c(0.05,0.05,0.05,0.05),oma=c(0.1,0.1,0.1,0.25),lwd=0.2);
layout(matrix(c(1:3,13,4:6,13,7:9,13,10:13),4,4,byrow = T),widths=c(1,1,1,0.75))

for(i in 1:12){
  plot(shore,ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],ann=F,axes=F,border=NA)
  plot(ppt_norm_2_FL[month.name[i]],breaks=b,
     col=cols,ann=F,axes=F,legend=F,add=T)
plot(shore,add=T,border="grey",lwd=0.25)
mtext(side=1,adj=0,paste0(" ",month.name[i]),line=-3)
box(lwd=1)
if(i==1){mapmisc::scaleBar(utm17,"bottomleft",bty="n",cex=0.75,seg.len=4,outer=F,xpd=F)}
}

plot(0:1,0:1,ann=F,axes=F,type="n")
b2=b
l.b=length(b2)
labs=c(paste0("< ",b2[2]),paste(b2[2:(l.b-2)],b2[3:(l.b-1)],sep=" - "),paste(paste0(">",b2[(l.b-1)])))
n.bks=length(b2)-1
top.val=0.8
bot.val=0.2
mid.v.val=0.3# bot.val+(top.val-bot.val)/2
x.max=0.4
x.min=0.1
mid.val=x.min+(x.max-x.min)/2
txt.offset.val=-0.01
legend_image=as.raster(matrix(rev(cols),ncol=1))
rasterImage(legend_image,x.min,bot.val,x.max,top.val)
segments(rep(x.min,4),seq(bot.val,top.val,length.out=4),
         rep(x.max,4),seq(bot.val,top.val,length.out=4),lwd=2)
text(x=x.max,
     y = seq(bot.val,top.val,length.out=4), 
     labels = format(seq(min(b),max(b),length.out=4)),cex=0.75,adj=0,pos=4,offset=0.5)
text(x=x.max, y = c(bot.val,top.val), labels = format(c(min(b2),max(b2))),cex=0.75,adj=0,pos=4,offset=0.5)
# bx.val= seq(bot.val,top.val,(top.val-bot.val)/n.bks)
# rect(x.min,bx.val[1:n.bks],x.max,bx.val[2:(n.bks+1)],col=rev(pal2),lty=0)
# text(y=bx.val[2:(n.bks+1)]-c(mean(diff(bx.val[2:(n.bks+1)]))/2), x = x.max, labels = rev(labs),cex=0.75,xpd=NA,pos=4,adj=0)
text(x=mid.val,y=top.val,"Precipitation (mm)",adj=0,cex=0.8,pos=3,xpd=NA)
mtext(side=1,line=-2,adj=1,cex=0.5,"1991-2020 (30-year)\nMonthly Normal Precipitation\nSource: PRISM Climate Group\n(https://www.prism.oregonstate.edu/)")
dev.off()

## 2022
get_prism_monthlys(type = "ppt",
                   year = 2022,
                   mon = 1:12,
                   keepZip = FALSE)

ppt_1 <- prism_archive_subset(type = "ppt",
                              temp_period = "monthly",
                              mon = 1:12)
n.rasts=length(ppt_1)
## convert to terra
ppt_1 <- pd_to_file(ppt_1)
ppt_1 <- rast(ppt_1)

## change the layer names to something sensible
names(ppt_1) <- month.name[1:n.rasts]

ppt_1_2=terra::project(ppt_1,"+proj=utm +zone=17 +datum=NAD83 +units=m +no_defs")

ppt_1_2_FL=terra::crop(ppt_1_2,shore.buf)
range(ppt_1_2_FL)

anomaly <- ((ppt_1_2_FL - ppt_norm_2_FL)/ppt_norm_2_FL)*100
names(anomaly) <- month.name[1:12]

bbox.lims=bbox(shore)
b=seq(0,800,1)
cols=scico::scico(n=length(b),palette="lapaz",direction=-1)
# png(filename=paste0(plot.path,"PRISM_Florida_2022RF.png"),width=8,height=6,units="in",res=200,type="windows",bg="white")
# layout(matrix(1:12,4,3,byrow = T))
par(family="serif",mar=c(0.05,0.05,0.05,0.05),oma=c(0.1,0.1,0.1,0.25),lwd=0.2);
layout(matrix(c(1:3,13,4:6,13,7:9,13,10:13),4,4,byrow = T),widths=c(1,1,1,0.75))

for(i in 1:6){
  plot(shore,ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],ann=F,axes=F,border=NA)
  plot(ppt_1_2_FL[month.name[i]],breaks=b,
       col=cols,ann=F,axes=F,legend=F,add=T)
  plot(shore,add=T,border="grey",lwd=0.25)
  mtext(side=1,adj=0,paste0(" ",month.name[i]),line=-3)
  box(lwd=1)
  if(i==1){mapmisc::scaleBar(utm17,"bottomleft",bty="n",cex=0.75,seg.len=4,outer=F,xpd=F)}
}
for(i in 7:12){
  plot(shore,ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],ann=F,axes=F,border="grey",lwd=0.25)
  mtext(side=1,adj=0,paste0(" ",month.name[i]),line=-3)
  box(lwd=1)
}

plot(0:1,0:1,ann=F,axes=F,type="n")
b2=b
l.b=length(b2)
labs=c(paste0("< ",b2[2]),paste(b2[2:(l.b-2)],b2[3:(l.b-1)],sep=" - "),paste(paste0(">",b2[(l.b-1)])))
n.bks=length(b2)-1
top.val=0.8
bot.val=0.2
mid.v.val=0.3# bot.val+(top.val-bot.val)/2
x.max=0.4
x.min=0.1
mid.val=x.min+(x.max-x.min)/2
txt.offset.val=-0.01
legend_image=as.raster(matrix(rev(cols),ncol=1))
rasterImage(legend_image,x.min,bot.val,x.max,top.val)
segments(rep(x.min,4),scales::rescale(seq(min(b),max(b),length.out=5),to=c(0.2,0.8)),
         rep(x.max,4),scales::rescale(seq(min(b),max(b),length.out=5),to=c(0.2,0.8)),lwd=2)
text(x=x.max,
     y = scales::rescale(seq(min(b),max(b),length.out=5),to=c(0.2,0.8)), 
     labels = format(seq(min(b),max(b),length.out=5)),cex=0.75,adj=0,pos=4,offset=0.5)
# text(x=x.max, y = c(bot.val,top.val), labels = format(c(min(b2),max(b2))),cex=0.75,adj=0,pos=4,offset=0.5)
# bx.val= seq(bot.val,top.val,(top.val-bot.val)/n.bks)
# rect(x.min,bx.val[1:n.bks],x.max,bx.val[2:(n.bks+1)],col=rev(pal2),lty=0)
# text(y=bx.val[2:(n.bks+1)]-c(mean(diff(bx.val[2:(n.bks+1)]))/2), x = x.max, labels = rev(labs),cex=0.75,xpd=NA,pos=4,adj=0)
text(x=mid.val,y=top.val,"Precipitation (mm)",adj=0,cex=0.8,pos=3,xpd=NA)
mtext(side=1,line=-2,adj=1,cex=0.5,"2022 Monthly Normal Precipitation\nSource: PRISM Climate Group\n(https://www.prism.oregonstate.edu/)")
dev.off()


b=c(-100,0,100,200,300,400)
b.sc=scales::rescale_mid(b,mid=0)
scico::scale_colour_scico(palette='vik',midpoint=0)

tmp=scico::scico(256,palette="vik")

cols.scale=scales::gradient_n_pal(colours=scico::scico(6,palette="vik"),values=b.sc,space="Lab")
cols.scale(b)


tmp=scico::scico(length(b.sc),palette="vik",begin=min(b.sc),end=max(b.sc))
cols=tmp
# cols=scico::scico(n=length(b),palette="vik")
# png(filename=paste0(plot.path,"PRISM_Florida_2022RFanomly.png"),width=8,height=6,units="in",res=200,type="windows",bg="white")
# layout(matrix(1:12,4,3,byrow = T))
par(family="serif",mar=c(0.05,0.05,0.05,0.05),oma=c(0.1,0.1,0.1,0.25),lwd=0.2);
layout(matrix(c(1:3,13,4:6,13,7:9,13,10:13),4,4,byrow = T),widths=c(1,1,1,0.75))

for(i in 1:6){
  plot(shore,ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],ann=F,axes=F,border=NA)
  plot(anomaly[month.name[i]],breaks=b,
       col=cols,ann=F,axes=F,legend=F,add=T)
  plot(shore,add=T,border="grey",lwd=0.25)
  mtext(side=1,adj=0,paste0(" ",month.name[i]),line=-3)
  box(lwd=1)
  if(i==1){mapmisc::scaleBar(utm17,"bottomleft",bty="n",cex=0.75,seg.len=4,outer=F,xpd=F)}
}
for(i in 7:12){
  plot(shore,ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],ann=F,axes=F,border="grey",lwd=0.25)
  mtext(side=1,adj=0,paste0(" ",month.name[i]),line=-3)
  box(lwd=1)
}

plot(0:1,0:1,ann=F,axes=F,type="n")
b2=b
l.b=length(b2)
labs=c(paste0("< ",b2[2]),paste(b2[2:(l.b-2)],b2[3:(l.b-1)],sep=" - "),paste(paste0(">",b2[(l.b-1)])))
n.bks=length(b2)-1
top.val=0.8
bot.val=0.2
mid.v.val=0.3# bot.val+(top.val-bot.val)/2
x.max=0.4
x.min=0.1
mid.val=x.min+(x.max-x.min)/2
txt.offset.val=-0.01
legend_image=as.raster(matrix(rev(cols),ncol=1))
rasterImage(legend_image,x.min,bot.val,x.max,top.val)
segments(rep(x.min,4),scales::rescale(c(-100,0,400),to=c(0.2,0.8)),
         rep(x.max,4),scales::rescale(c(-100,0,400),to=c(0.2,0.8)),lwd=2)
text(x=x.max,
     y = scales::rescale(c(-100,0,400),to=c(0.2,0.8)), 
     labels = format(c(-100,0,400)),cex=0.75,adj=0,pos=4,offset=0.5)
# text(x=x.max, y = c(bot.val,top.val), labels = format(c(min(b2),max(b2))),cex=0.75,adj=0,pos=4,offset=0.5)
# bx.val= seq(bot.val,top.val,(top.val-bot.val)/n.bks)
# rect(x.min,bx.val[1:n.bks],x.max,bx.val[2:(n.bks+1)],col=rev(pal2),lty=0)
# text(y=bx.val[2:(n.bks+1)]-c(mean(diff(bx.val[2:(n.bks+1)]))/2), x = x.max, labels = rev(labs),cex=0.75,xpd=NA,pos=4,adj=0)
text(x=mid.val,y=top.val,"Precipitation Anomaly\n(% difference)",adj=0,cex=0.8,pos=3,xpd=NA)
mtext(side=1,line=-2,adj=1,cex=0.5,"2022 Monthly Precipitation Anomalies\nSource: PRISM Climate Group\n(https://www.prism.oregonstate.edu/)")
dev.off()