
library(sp)
library(tmap)
library(httr)
library(jsonlite)
library(raster)

wgs84=CRS("+init=epsg:4326")
utm17=CRS("+init=epsg:26917")

HABext=extent(-82.5,
              -80.0,
              24.4,
              27.4)
# HABext=extent(-83.1,
#               -79.95,
#               24.4,
#               28.1)
HABext.poly=as(HABext,"SpatialPolygons")
proj4string(HABext.poly)=wgs84
tm_shape(HABext.poly)+tm_polygons(alpha=0.5)
HABext2=extent(HABext)

TBEP.ext=extent(-83.4,
                -82.08,
                27.0,
                28.2)
TBEP.ext.poly=as(TBEP.ext,"SpatialPolygons")
proj4string(TBEP.ext.poly)=wgs84
tm_shape(TBEP.ext.poly)+tm_polygons(alpha=0.5)

tm_shape(TBEP.ext.poly)+tm_polygons(alpha=0.5)+
  tm_shape(HABext.poly)+tm_polygons(alpha=0.5)

CRE.ext=extent(-82.3,
              -81.8,
              26.3,
              26.7)
CRE.ext=as(CRE.ext,"SpatialPolygons")
proj4string(CRE.ext)=wgs84
CRE.ext=SpatialPolygonsDataFrame(CRE.ext,data.frame(ID="CRE"))
tm_shape(CRE.ext)+tm_polygons(alpha=0.5)



# Red Tide ----------------------------------------------------------------
## south of Tampa Bay 
# Based on https://github.com/tbep-tech/piney-point/blob/141824adc082a57d79ba58ff38ac31d41dea4e44/R/dat_proc.R#L1262
path <- 'https://gis.ncdc.noaa.gov/arcgis/rest/services/ms/HABSOS_CellCounts/MapServer/0/query?'

request <- GET(
  url = path,
  query= list(       
    where= paste("LATITUDE <",round(HABext2[4],1),"AND LATITUDE >",round(HABext2[3],1),"AND LONGITUDE >",round(HABext2[1],1),"AND LONGITUDE <",round(HABext2[2],1)),
    outFields = 'DESCRIPTION,SAMPLE_DATE,LATITUDE,LONGITUDE,SALINITY,SALINITY_UNIT,WATER_TEMP,WATER_TEMP_UNIT,GENUS,SPECIES,CATEGORY,CELLCOUNT,CELLCOUNT_UNIT',
    f = 'pjson'
  )
)

response <- content(request, as = "text", encoding = "UTF-8")
results <- jsonlite::fromJSON(response,flatten=T)
results

kbrdat=results$features
vars=c("attributes.DESCRIPTION", "attributes.SAMPLE_DATE", "attributes.LATITUDE", 
       "attributes.LONGITUDE", "attributes.SALINITY", "attributes.SALINITY_UNIT", 
       "attributes.WATER_TEMP", "attributes.WATER_TEMP_UNIT", "attributes.GENUS", 
       "attributes.SPECIES", "attributes.CATEGORY", "attributes.CELLCOUNT", 
       "attributes.CELLCOUNT_UNIT", "geometry.x", "geometry.y")
vars=strsplit(vars,"\\.")
vars=sapply(vars,"[",2)

colnames(kbrdat)<-tolower(vars)

kbrdat$date=as.POSIXct(as.numeric(gsub('000$', '',format(kbrdat$sample_date,scientific=F))), 
                         origin = c('1970-01-01'), tz = 'UTC')
kbrdat$datetime=date.fun(kbrdat$date,form="%F %R")
kbrdat$date=date.fun(kbrdat$date)
kbrdat$year=as.numeric(format(kbrdat$date,"%Y"))
range(kbrdat$year,na.rm=T)
kbrdat=subset(kbrdat,is.na(year)==T|year!=153)

## 
TODAY=date.fun("2021-10-11")
sdate=date.fun(TODAY-lubridate::duration(30,"days"))

cat.abund.xwalk=data.frame(category=c("not observed","very low","low","medium","high"),
                           abund=c("not present/background (0-1,000)", "very low (>1,000-10,000)",
                                   "low (>10,000-100,000)", "medium (>100,000-1,000,000)", 
                                   "high (>1,000,000)"))

kbrdat=merge(kbrdat,cat.abund.xwalk,"category")
kbrdat$Abundance=factor(kbrdat$abund,levels=cat.abund.xwalk$abund)
kbrdat$category=factor(kbrdat$category,levels=cat.abund.xwalk$category)

kbrdat_30d=subset(kbrdat,date<TODAY&date>sdate)
kbrdat_30d.shp=SpatialPointsDataFrame(coords=kbrdat_30d[,c("longitude","latitude")],
                                          data=kbrdat_30d,
                                          proj4string = wgs84)

tm_shape(kbrdat_30d.shp)+tm_dots(col="Abundance",
                                 palette =c("white","grey","dodgerblue1","orange","indianred1"),
                                 size=0.03,
                                 id="date",
                                 popup.vars=c("date","cellcount","category","genus","species"),
                                 group="NOAA/FWC Redtide Data")+
  tm_shape(CRE.ext)+tm_borders(group="Greater Caloosahatchee Estuary")+
  tm_layout("Red Tide")


# CRE.inter=rgeos::gIntersects(CRE.ext,kbrdat_30d.shp,byid=T)
# plyr::ddply(kbrdat_30d.shp[as.vector(CRE.inter),]@data,"Abundance",summarise,N.val=N.obs(category))  
rslt =plyr::ddply(kbrdat_30d.shp[CRE.ext,]@data,"category",summarise,N.val=N.obs(category))  

rslt=merge(cat.abund.xwalk,rslt,"category",all.x=T)

rslt=rslt[match(cat.abund.xwalk$abund,rslt$abund),]
library(flextable)
library(magrittr)

rslt[,c("abund","N.val")]%>%
  flextable()%>%
  colformat_num(j=2,big.mark = "",digits=0,na_str = "---")%>%
  set_header_labels("abund"="Cell Abundance (Cells L\u207B\u00B9)",
                    "N.val"="Number\nof\nSamples")%>%
  align(j=2,align="center",part="all")%>%
  width(width=c(2.5,0.5))%>%
  font(fontname="Times New Roman",part="all")%>%
  padding(padding=2,part="all")


# Blue-Green Algae --------------------------------------------------------
path <- 'https://services1.arcgis.com/nRHtyn3uE1kyzoYc/arcgis/rest/services/VIEW_FL_Algal_Bloom_Site_Visits_1/FeatureServer/0/query?'

request <- GET(
  url = path,
  query= list(       
    where = "County='Lee' OR County='Collier' OR County='Hendry' OR County='Glades' OR County='Martin'",
    outFields = '*',
    f = 'pjson'
  )
)
response <- content(request, as = "text", encoding = "UTF-8")
results <- jsonlite::fromJSON(response,flatten=T)
# nrow(results$features)

bgdat=results$features

request <- GET(
  url = path,
  query= list(       
    where = "County='Okeechobee' OR County='PalmBeach' OR County='Broward' OR County='MiamiDade' OR County='Monroe'",
    outFields = '*',
    f = 'pjson'
  )
)
response <- content(request, as = "text", encoding = "UTF-8")
results <- jsonlite::fromJSON(response,flatten=T)
nrow(results$features)

bgdat=rbind(bgdat, results$features)

vars=c("attributes.objectid", "attributes.globalid", "attributes.SiteVisitDate", 
       "attributes.Location", "attributes.County", "attributes.Visitor", 
       "attributes.AlgaeObserved", "attributes.SampleTaken", "attributes.DepthDesc", 
       "attributes.SampleDepth", "attributes.AnalyzedBy", "attributes.Otherlab", 
       "attributes.Comments", "attributes.Latitude", "attributes.Longitude", 
       "attributes.AlgalID", "attributes.Microcystin", "attributes.OtherToxin", 
       "attributes.EditDate", "attributes.PicURL", "attributes.ToxinPresent", 
       "attributes.CyanobacteriaDominant", "geometry.x", "geometry.y"
)
vars=strsplit(vars,"\\.")
vars=sapply(vars,"[",2)
colnames(bgdat)<-tolower(vars)

bgdat$sitevisitdate=as.numeric(gsub('000$', '',format(bgdat$sitevisitdate,scientific=F)))
bgdat$datetime=as.POSIXct(bgdat$sitevisitdate,origin = c('1970-01-01'), tz = 'UTC')
bgdat$datetime=date.fun(bgdat$datetime,form="%F %R")
bgdat$date=date.fun(bgdat$datetime)

unique(bgdat$othertoxin)
vars=strsplit(bgdat$othertoxin,"\\;|\\:")
othertox=data.frame(tox1=sapply(vars,"[",1),tox1.rslt=sapply(vars,"[",2),
           tox2=sapply(vars,"[",3),tox2.rslt=sapply(vars,"[",4),
           tox3=sapply(vars,"[",5),tox3.rslt=sapply(vars,"[",6),
           tox4=sapply(vars,"[",7),tox4.rslt=sapply(vars,"[",8))
unique(othertox$tox1)
unique(othertox$tox2)
unique(othertox$tox3)

othertox.mdl=data.frame(tox=c("Anatoxin-a","Cylindrospermopsin"),
                        MDL=c(0.25))


vars=strsplit(bgdat$microcystin,"I")
# bgdat$microcystin.halfmdl=ifelse(sapply(vars,"[",1)=="not detected",0.5,as.numeric(sapply(vars,"[",1)))
bgdat$microcystin.val=ifelse(sapply(vars,"[",1)=="not detected",NA,as.numeric(sapply(vars,"[",1)))
bgdat[,c("microcystin","microcystin.halfmdl")]

bgdat_30d=subset(bgdat,date<TODAY&date>sdate)
bgdat_30d.shp=SpatialPointsDataFrame(coords=bgdat[,c("longitude","latitude")],
                                              data=bgdat,
                                              proj4string = wgs84)

tm_shape(bgdat_30d.shp)+tm_dots(col="toxinpresent",
                                palette =c("green","red"),
                                colorNA="grey",
                                size=0.03,
                                id="date",
                                popup.vars=c("date","visitor","depthdesc",
                                             "microcystin",
                                             "cyanobacteriadominant","toxinpresent",
                                             "algalid","othertoxin"),
                                group="FDEP Blue-green Algae Data",
                                title="Cyanobacteria dominate")+
  tm_shape(bgdat_30d.shp)+tm_bubbles(col="white",size="microcystin.val", id="date",group="Microcystin conc.")+
  tm_shape(bgdat.ext)+tm_borders(group="Area of Interest",lty="dashed",col="grey",lwd=1)

sum(bgdat_30d$cyanobacteriadominant=="Yes")
sum(bgdat_30d$toxinpresent=="Yes")
range(as.numeric(bgdat_30d$microcystin),na.rm = T)


## Species heat map idea
bgdat_30d$algalid
bgdat_30d$algaeobserved
bgdat_30d[,c("algalid","algaeobserved")]



bgdat_30d.algae=strsplit(bgdat_30d$algalid,"\\;|\\:| and")

algae.list=rbind(
cbind(bgdat_30d[,c("objectid","date")],data.frame(val=sapply(bgdat_30d.algae,"[",1))),
cbind(bgdat_30d[,c("objectid","date")],data.frame(val=sapply(bgdat_30d.algae,"[",2))),
cbind(bgdat_30d[,c("objectid","date")],data.frame(val=sapply(bgdat_30d.algae,"[",3)))
)

unique(algae.list$val)

algae.list=subset(algae.list,!(val%in%c("Dominant taxon",
                                  "Co-dominate taxa were","Co-dominant taxa were"
                                  ,NA,"no dominant species in sample",
                                  " no dominant species in sample")))


algae.list.plot=data.frame(val=c("mixed algae", " Microcystis aeruginosa", " Cylindrospermopsis raciborskii", 
                                 " Planktolyngbya limnetica", " Euglena sp."),
                           plot.y=1:5)
fill=expand.grid(date=date.fun(seq(min(algae.list$date),max(algae.list$date),"1 days")),
            val=algae.list.plot$val)

algae.list=merge(algae.list,fill,c("date","val"),all.y=T)
algae.list.sum=reshape::cast(algae.list,date~val,value="objectid",fun.aggregate=function(x) N.obs(x))

heatmap(as.matrix(algae.list.sum),Colv = NA, Rowv = NA, scale="column")

algae.list.sum=algae.list$val=as.factor(algae.list$val)




algae.list.sum=ddply(algae.list,c("date","val"),summarise,N.val=N.obs(objectid))
algae.list.sum=merge(algae.list.sum,fill,c("date","val"),all.y=T)
algae.list.sum=merge(algae.list.sum,algae.list.plot,"val",all.y=T)
algae.list.sum$N.val=with(algae.list.sum,ifelse(is.na(N.val)==T,0,N.val))

algae.list.sum$val.cat=as.factor(findInterval(algae.list.sum$val,c(0,1,3,5,10,15)))
algae.list.sum$date2=as.numeric(algae.list.sum$date)
cols.vir=c("grey80",rev(viridis::inferno(5)))
algae.list.sum=merge(algae.list.sum,
                     data.frame(val.cat=c(1:6),
                                val.cat.txt=c(0,1,"1 - 3","3 - 5","5 - 10","10 -15")),"val.cat")

plot(plot.y~date,algae.list.sum)

cols=c("1"=cols.vir[1],"2"=cols.vir[2],"3"=cols.vir[3],"4"=cols.vir[4],"5"=cols.vir[5],"6"=cols.vir[6])
library(lubridate)
x.seq.rect=date.fun(seq(min(algae.list.sum$date)-duration(0.5,"days"),
               max(algae.list.sum$date)+duration(0.5,"days"),"1 days"))
plot(plot.y~date,algae.list.sum)
for(i in 1:nrow(algae.list.plot)){
  tmp=subset(algae.list.sum,val==algae.list.plot$val[i])
  rect(x.seq.rect,i+0.5,max(x.seq.rect),i-0.5,col=cols[tmp$val.cat])
}


library(ggplot2)
library(cowplot)
cols.wes=rev(wesanderson::wes_palette("Zissou1",5,"continuous"))
cols.vir=rev(viridis::inferno(5))
cols=c("1"=cols.vir[1],"2"=cols.vir[2],"3"=cols.vir[3],"4"=cols.vir[4],"5"=cols.vir[5])
ggplot(algae.list.sum, aes(x = date2, y = plot.y, fill = val.cat)) +
  geom_tile(aes(group = val.cat), colour = 'black')+
  #geom_text(aes(label=format(round(TRF.in,1),nsmall=1),fontface = "bold"),size=2,colour=rf.dat.mon$txt.cols,family="serif",)+
  scale_y_reverse(expand = c(0, 0), breaks = algae.list.sum$date2) +
  # scale_x_discrete(expand = c(0, 0), position = 'top') +
  # scale_x_continuous(expand = c(0, 0), breaks = 1:12)+
  scale_fill_manual(values = cols,
                    name="Sample Counts",
                    breaks=1:5,
                    labels=c(1, 3 ,5, 10, 15)) +
  theme_bw() +
  theme(
    #legend.position = 'none',
    text=element_text(family="serif"),
    plot.title=element_text(size=12),
    plot.subtitle = element_text(color = "grey50",size=8),
    plot.caption = element_text(hjust = 0)
  )+
  labs(title = "Southwest Florida Rainfall",
       subtitle = "Total Monthly Rainfall",
       caption = paste0("Produced: ",format(Sys.Date(),"%d %b %Y")),
       x="Month",
       y="Year")
