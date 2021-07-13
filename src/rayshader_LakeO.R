## 
## Lake Okeechobee Bathymetry 
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
library(reshape)
library(openxlsx)

# GIS libraries 
# library(sp)
library(sp)
library(rgdal)
library(rgeos)
library(raster)
library(gstat)

#thin plate spline https://rspatial.org/raster/analysis/4-interpolation.html
library(fields)
library(raster)

## Paths
wd="C:/Julian_LaCie/_GitHub/CRE_Conditions"

paths=paste0(wd,c("/Plots/","/Export/","/Data/","/GIS"))
# Folder.Maker(paths);#One and done. Creates folders in working directory.
plot.path=paths[1]
export.path=paths[2]
data.path=paths[3]
GIS.path=paths[4]
GIS.path.gen=paste0(dirname(dirname(wd)),"/_GISData")

# Helper variables
# epsg.io
nad83=CRS(SRS_string ="EPSG:4269")
wgs84=CRS(SRS_string ="EPSG:4326")
utm17=CRS(SRS_string ="EPSG:26917")


# -------------------------------------------------------------------------
# lakeO=spTransform(readOGR(paste0(GIS.path.gen,"/SFWMD"),"LakeOkeechobee_general"),wkt(utm17))
wmd.mon=spTransform(readOGR(paste0(GIS.path.gen,"/SFWMD_Monitoring_20200221"),"Environmental_Monitoring_Stations"),wkt(utm17))


head(wmd.mon@data)
unique(wmd.mon$ACTIVITY_S)

vars=c("S77_T","S4_P","S3","S2_P","S308_S","S133_P",'S131_C','S127_P')
vars=c("S77_H","S4_P","S3","S2_P","S308_S","S133_T",'S131_T','S127_P')
struct=subset(wmd.mon,ACTIVITY_S%in%c("Flow","Stage")&STATION%in%vars)
struct2=struct@data[,c("SITE","LAT","LONG")]




## https://github.com/tylermorganwall/rayshader/
## https://wcmbishop.github.io/rayshader-demo/
library(rayshader)
library(elevatr)
library(magrittr)

bath=raster::raster(paste0(GIS.path.gen,"/LakeOkeechobee/LakeOkeechobee_Usace/spatial/export_raster/Bathym_50ft.tif"))
attributes(bath)$crs
# bath=projectRaster(bath,crs=utm17)
# attributes(bath)$crs
# proj4string(bath)<-utm17
plot(bath)

# par(family="serif",mar=c(0.5,0.5,0.5,0.5),oma=c(0.1,0.1,0.1,0.1));
# # layout(matrix(1:2,1,2,byrow=T),heights=c(1,0.2))
# plot(bath<8,col=c("grey",'red'),axes=F,ann=F,box=F,legend=F)
# # plot(0:1,0:1,type = 'n', axes = F,xlab=NA, ylab=NA)
# legend("topleft",
#        legend=c("< 8 Ft NGVD88","> 8 Ft NGVD88"),
#        pt.bg=c("red","grey"),pch=22,pt.cex = 2,col=NA,lty=NA,lwd=c(0.1),
#        ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=0.5,xpd=NA,xjust=0.5)

# convert from NGVD88 to NAVD29
bath=bath-(-1.32)
bath_100=raster::aggregate(bath,fact=2)
res(bath_100)
plot(bath_100)

bath_200=raster::aggregate(bath,fact=4)
res(bath_200)
plot(bath_200)
bath_200=projectRaster(bath_200,crs=utm17)
attributes(bath_200)$crs
#bath2=rayshader::raster_to_matrix(paste0(paths[4],"/LakeOkeechobee_Usace/spatial/export_raster/Bathym_50ft.tif"))
elmat=rayshader::raster_to_matrix(bath_200)

# elmat %>%
#   sphere_shade(sunangle = 45,texture = "unicorn") %>%
#   plot_map()

# test.text=create_texture("brown","green","forestgreen","khaki","brown")
# elmat %>%
#   sphere_shade(sunangle = 45,texture =test.text) %>%
#   plot_map()

# Ok.shadow = ray_shade(elmat, zscale = 50, lambert = FALSE)
# ok.amb = ambient_shade(elmat, zscale = 50)

# montereybay %>%
#   sphere_shade() %>%
#   plot_3d(montereybay,zscale=50, water=TRUE, waterlinecolor="white",
#           zoom=0.3,theta=-135,fov=70, phi=20) 
# # add labels at different heights (distances) 
# santa_cruz = c(36.962957, -122.021033)
# # behind focus
# render_label(montereybay,lat = santa_cruz[1], long = santa_cruz[2],
#              extent = attr(montereybay, "extent"),
#              altitude=2000, zscale=50, text = "Santa Cruz")
# Sys.sleep(0.2)
# render_snapshot()



places=data.frame(Name=c("S77","Coot Bay","Ritta Island","Kreamer Island","S308","C-38 Canal","Fisheating Creek","Observation Island","lakeCenter"),
           LAT=c(26.7607,26.7673,26.7218,26.7586,26.9841,27.1429,26.9677,26.838633,26.9245),
           LONG=c(-80.9178,-80.903,-80.8063,-80.7319,-80.6219,-80.8591,-81.1140,-80.963589,-80.7841))
places=subset(places,!(Name%in%c("S77","S308")))

places.shp=SpatialPointsDataFrame(places[,c("LONG","LAT")],places,proj4string = wgs84)
plot(places.shp)
places.shp=spTransform(places.shp,wkt(utm17))
coordinates(places.shp)

places.towns=data.frame(NAME=c("Okeechobee", "Buckhead Ridge","Moore Haven","Clewiston","Pahokee","Port Mayaca"),
                        LAT=c(27.206360,27.121495,26.840340,26.760773,26.825926,26.984051),
                        LONG=c(-80.797028,-80.895867,-81.085124,-80.917910,-80.667568,-80.620958))
places.towns.shp=SpatialPointsDataFrame(places.towns[,c("LONG","LAT")],places.towns,proj4string = wgs84)
plot(places.towns.shp)
places.towns.shp=spTransform(places.towns.shp,wkt(utm17))
coordinates(places.towns.shp)


# d <- paste(tempdir(), "/rayshader_plots", sep="/")
# dir.create(d, showWarnings = FALSE)


# z.vals=c(seq(10,17,1),rev(seq(10,16,1)))
z.vals=seq(10,17,1)
for(j in 1:length(z.vals)){
elmat %>%
  sphere_shade(zscale=0.04,texture = "imhof1") %>%
  plot_3d(elmat, zscale = 50, fov = 0, theta = -45, zoom = 0.6, phi = 45, windowsize = c(800, 600),
          water = TRUE, waterdepth = z.vals[j], wateralpha = 0.25, watercolor = "brown",
          waterlinecolor = "white", waterlinealpha = 0.5)

alt.vals=c(5000,2500,2500,5000,5000,2000,5000,5000);# was 5000
for(i in 1:nrow(struct@data)){
render_label(elmat,
             lat=as.numeric(coordinates(struct)[i,2]),
             long=as.numeric(coordinates(struct)[i,1]),
             altitude=alt.vals[i],
             zscale=20,extent=attr(bath_200,"extent"),
             text=struct@data$SITE[i],
             family="serif",
             textsize = 1.25, linewidth = 2.5)
}
for(i in c(1,3,5,6)){
  render_label(elmat,
               lat=as.numeric(coordinates(places.shp)[i,2]),
               long=as.numeric(coordinates(places.shp)[i,1]),
               altitude=1000,
               zscale=20,extent=attr(bath_200,"extent"),
               text=places.shp@data$Name[i],
               family="serif",
               textsize = 1, linewidth = 1.5,
               textcolor = "darkred", linecolor = "darkred")
}

render_label(elmat,
             lat=as.numeric(coordinates(places.shp)[7,2]),
             long=as.numeric(coordinates(places.shp)[7,1]),
             altitude=500,
             zscale=20,extent=attr(bath_200,"extent"),
             family="serif",
             text=paste0(z.vals[j]," Ft NGVD"),
             textsize = 1, linewidth = 1,
)
render_compass(position = "E",color_bevel ="grey",compass_radius=100)
# render_snapshot(clear=T)
rgl::snapshot3d(paste0(plot.path,"rayshader/",j,"_LOK_", z.vals[j],"ft.png"))
rgl::rgl.close()
print(j)
}
# render_highquality(filename=paste0(plot.path,"rayshade_lakeOBath_10Ft.png"))

## something to explore
# https://zappingseb.github.io/rayshaderanimate/


ray.files=c(paste0(plot.path,"rayshader/",1:8,"_LOK_", z.vals,"ft.png"),
            paste0(plot.path,"rayshader/",rev(1:7),"_LOK_", rev(z.vals[1:7]),"ft.png"))
gifski::gifski(ray.files, 
               delay = 40 / 100, 
               gif_file  = paste0(plot.path,"rayshader/LOK_rayshader_gifski.gif"),
               loop = T)

## Wednesday update
z.vals=c(9,17)
for(j in 1:2){
  elmat %>%
    sphere_shade(zscale=0.04,texture = "imhof1") %>%
    plot_3d(elmat, zscale = 50, fov = 0, theta = -45, zoom = 0.6, phi = 45, windowsize = c(800, 600),
            water = TRUE, waterdepth = z.vals[j], wateralpha = 0.25, watercolor = "brown",
            waterlinecolor = "white", waterlinealpha = 0.5)
  
  alt.vals=c(2500,2500,2500,5000,1500,2000);# was 5000
  for(i in 1:nrow(places.towns.shp@data)){
    render_label(elmat,
                 lat=as.numeric(coordinates(places.towns.shp)[i,2]),
                 long=as.numeric(coordinates(places.towns.shp)[i,1]),
                 altitude=alt.vals[i],
                 zscale=20,extent=attr(bath_200,"extent"),
                 text=places.towns.shp@data$NAME[i],
                 family="serif",
                 textsize = 1.25, linewidth = 2.5)
  }
  render_label(elmat,
               lat=as.numeric(coordinates(places.shp)[7,2]),
               long=as.numeric(coordinates(places.shp)[7,1]),
               altitude=500,
               zscale=20,extent=attr(bath_200,"extent"),
               family="serif",
               text=paste0(z.vals[j]," Ft NGVD"),
               textsize = 1.5, linewidth = 1,textcolor = "white",linecolor="white")

  render_compass(position = "E",color_bevel ="grey",compass_radius=100)
  # render_snapshot(clear=T)
  rgl::snapshot3d(paste0(plot.path,"rayshader/",j,"_LOK_wed_", z.vals[j],"ft.png"))
  rgl::rgl.close()
  print(j)
}








# http://www.pieceofk.fr/a-3d-tour-over-lake-geneva-with-rayshader/

z_scale=40
ambmat <- ambient_shade(elmat)
raysha <- ray_shade(elmat, zscale = z_scale, maxsearch = 300)
hillshade <- elmat %>%
  sphere_shade(texture = "imhof1", sunangle = 35) %>%
  add_shadow(raysha, 0.5) %>%
  add_shadow(ambmat, 0.5)

render_snapshot(clear=T)
