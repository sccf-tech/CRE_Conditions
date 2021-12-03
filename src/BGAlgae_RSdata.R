

library(AnalystHelper)
library(raster)

wd="C:/Julian_LaCie/_GitHub/CRE_Conditions"
data.path=paste0(wd,"/Data/tmp/")
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
  10**(3.0 / 250.0 * DN - 4.2)
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

     