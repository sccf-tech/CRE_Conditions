## 
## SW Florida rain
##
##
##
## Code was compiled by Paul Julian
## contact info: pjulian@sccf.org

## BAD 
## https://www.tidyverse.org/articles/2017/12/workflow-vs-script/
## Clears Everything...start fresh.
rm(list=ls(all=T));cat("\014");dev.off()

## Libraries
#devtools::install_github("SwampThingPaul/AnalystHelper")
library(AnalystHelper);
library(plyr)
library(reshape)
library(zoo)

library(lubridate)

## Paths
wd="C:/Julian_LaCie/_Github/CRE_Conditions"

paths=paste0(wd,c("/Plots/","/Export/","/Data/","/src","/GIS"))
# Folder.Maker(paths);#One and done. Creates folders in working directory.
plot.path=paths[1]
export.path=paths[2]
data.path=paths[3]
GIS.path=paths[5]

#Current WY
CurWY=WY(Sys.time())
CurWY


# -------------------------------------------------------------------------
dates=c(date.fun("1979-05-01"),date.fun(as.Date(Sys.time())-duration(1,"days")))

wx.sites=data.frame(SITE=c('FPWX','SLEE_R','S79_R','CORK_R','CORK_R','CRKSWPS_R',"S78_R","S78_R","DEVILS_R","PALMDALE_R","PALMDALE_R","S77_R","S77_R","S77_R"),
                    DBKEY=c('FZ598','06081','16414','DO541','VN012','63883',"06243","16625","IV150","06093","15786","05913","KD314","16415"))

rf.dat=data.frame()
for(i in 1:nrow(wx.sites)){
  tmp=DBHYDRO_daily(dates[1],dates[2],as.character(wx.sites$DBKEY[i]))
  tmp$DBKEY=as.character(wx.sites$DBKEY[i])
  rf.dat=rbind(rf.dat,tmp)
  print(i)
}

rf.dat=merge(rf.dat,wx.sites,"DBKEY")
range(rf.dat$Date,na.rm=T)
range(rf.dat$Data.Value,na.rm=T)
rf.dat$Data.Value[rf.dat$Data.Value<0]<-NA;# remove negative values
rf.dat$Data.Value[rf.dat$Data.Value>40]<-NA; # removed extreme values
unique(rf.dat$SITE)
# rf.dat$CY=as.numeric(format(rf.dat$Date,"%Y"))
# rf.dat$month=as.numeric(format(rf.dat$Date,"%m"))

rf.dat.da=ddply(rf.dat,"Date",summarise,mean.val=mean(Data.Value,na.rm=T),N.val=N.obs(Data.Value))
rf.dat.da$CY=as.numeric(format(rf.dat.da$Date,"%Y"))
rf.dat.da$month=as.numeric(format(rf.dat.da$Date,"%m"))

rf.dat.mon=ddply(rf.dat.da,c("CY","month"),summarise,TRF.in=sum(mean.val,na.rm=T))
rf.dat.mon=merge(rf.dat.mon,expand.grid(CY=1979:2021,month=1:12),c("CY","month"),all.y=T)
rf.dat.mon$RF.cat=as.factor(findInterval(rf.dat.mon$TRF.in,c(0,1,2.5,5,7.5,10,12.5,100)))
rf.dat.mon=merge(rf.dat.mon,
                 data.frame(RF.cat=c(NA,1:7),
                            RF.cat.txt=c("<NA>","<1","1 - 2.5","2.5 - 5","5 - 7.5","7.5 - 10","10 - 12.5",">12.5"),
                            txt.cols=c(NA,rep("black",3),rep("white",4))),
                 "RF.cat")
rf.dat.mon$month.f=month.abb[rf.dat.mon$month]
rf.dat.mon=rf.dat.mon[order(rf.dat.mon$CY,rf.dat.mon$month),]
head(rf.dat.mon)
cols.vir=rev(viridis::inferno(7))
cols=c("1"=cols.vir[1],"2"=cols.vir[2],"3"=cols.vir[3],"4"=cols.vir[4],"5"=cols.vir[5],"6"=cols.vir[6],"7"=cols.vir[7])

plot(CY~month,rf.dat.mon,xlim=c(0.5,12.5),xaxs="i",ylim=c(2021,1979))
for(i in 2021:1979){
tmp=subset(rf.dat.mon,CY==i)
rect(seq(0.5,12.5,1),i+0.5,12.5,i-0.5,col=cols[tmp$RF.cat])
}





library(ggplot2)
library(cowplot)
cols.wes=rev(wesanderson::wes_palette("Zissou1",7,"continuous"))
cols.vir=rev(viridis::inferno(7))
# cols=c("1"="red","2"="orange","3"="darkolivegreen1","4"="lightblue1","5"="deepskyblue","6"="royalblue","7"="black")
# cols=c("1"=cols.wes[1],"2"=cols.wes[2],"3"="darkolivegreen1","4"=cols.wes[5],"5"=cols.wes[6],"6"=cols.wes[7],"7"="black")
cols=c("1"=cols.vir[1],"2"=cols.vir[2],"3"=cols.vir[3],"4"=cols.vir[4],"5"=cols.vir[5],"6"=cols.vir[6],"7"=cols.vir[7])
rf_POR=ggplot(rf.dat.mon, aes(x = month, y = CY, fill = RF.cat)) +
  geom_tile(aes(group = RF.cat), colour = 'black')+
  geom_text(aes(label=format(round(TRF.in,1),nsmall=1),fontface = "bold"),size=2,colour=rf.dat.mon$txt.cols,family="serif",)+
  scale_y_reverse(expand = c(0, 0), breaks = rf.dat.mon$CY) +
  # scale_x_discrete(expand = c(0, 0), position = 'top') +
  scale_x_continuous(expand = c(0, 0), breaks = 1:12)+
  scale_fill_manual(values = cols,
                    name="Rainfall\nCategories\n(Inches)",
                    breaks=1:7,
                    labels=c("<1.0","1.0 - 2.5","2.5 - 5.0","5.0 - 7.5","7.5 - 10.0","10.0 - 12.5",">12.5")) +
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
rf_POR
# ggsave(paste0(plot.path,"rf_POR.png"),rf_POR,device="png",height =7,width=5,units="in")

rf.dat.mon=rf.dat.mon[order(rf.dat.mon$CY,rf.dat.mon$month),]
mon.mean=ddply(rf.dat.mon,"month",summarise, mean.val=mean(TRF.in,na.rm=T))

month.POR=ggplot()+
  geom_col(data=mon.mean,
           aes(x=month,y=mean.val,fill="POR Mean"),width=1,color='dodgerblue1',alpha=0.25)+
  scale_fill_manual(name = NULL, values = c("POR Mean" = "dodgerblue1"))+
  scale_x_continuous(limits = c(0.5,12.5),breaks=seq(1,12,1))+
  scale_y_continuous(limits = c(0,10), expand = c(0, 0)) +
  geom_point(data=subset(rf.dat.mon,CY==2021),
             aes(x=month,y=TRF.in,color="CY 2021"),
             size=2.5,fill="indianred1",shape=21)+
  scale_color_manual(name = NULL, values = c("CY 2021" = "indianred1"))+
  theme_bw() +
  theme(
    legend.position="bottom",
    #legend.position = 'none',
    text=element_text(family="serif"),
    plot.title=element_text(size=12),
    plot.subtitle = element_text(color = "grey50",size=8),
    plot.caption = element_text(hjust = 0)
  )+
  labs(x="Month",
       y="Rainfall (Inches)")+
  guides(fill=guide_legend(label.position="top"),color=guide_legend(label.position="top"))
month.POR

# GIS libraries 
library(rgdal)
library(rgeos)
library(raster)
library(tmap)
library(ggmap)
library(ggsn)
GIS.path.gen=paste0(dirname(dirname(wd)),"/_GISData")
# Helper variables
nad83.pro=CRS(SRS_string ="EPSG:4269")
utm17=CRS(SRS_string ="EPSG:26917")
## 
shoreline=spTransform(readOGR(paste0(GIS.path.gen,"/FWC"),"FWC_Shoreline"),utm17)
shoreline=gSimplify(shoreline,100)
shoreline2=sf::st_as_sf(shoreline)
shoreline.f=fortify(shoreline)

est_nnc_seg=spTransform(readOGR(paste0(GIS.path.gen,"/FDEP"),"Estuary_NNC"),wkt(utm17))
segs=c("Upper Caloosahatchee River Estuary","Middle Caloosahatchee River Estuary","Lower Caloosahatchee River Estuary","San Carlos Bay")
cre.nnc.segs=subset(est_nnc_seg,SEGMENT_NA%in%segs)

wmd.mon=spTransform(readOGR(paste0(GIS.path.gen,"/SFWMD_Monitoring_20200221"),"Environmental_Monitoring_Stations"),wkt(utm17))
rf.sites.shp=subset(wmd.mon,STATION%in%wx.sites$SITE&ACTIVITY_S=="Rain")
rf.sites.shp2=sf::st_as_sf(rf.sites.shp)

roads.all=spTransform(readOGR(paste0(GIS.path.gen,"/FDOT"),"FDOT_Roads"),utm17)
roads=sf::st_as_sf(roads.all)

# wbids=spTransform(readOGR(paste0(GIS.path.gen,"/FDEP"),"WBIDs"),wkt(utm17))
# wbids2=merge(wbids,
#              data.frame(PLANNING_U=c("West Caloosahatchee", 'East Caloosahatchee',"Caloosahatchee Estuary","Telegraph Swamp","Orange River"),Region=c("C43","C43","Tidal Basin","Tidal Basin","Tidal Basin")),
#              "PLANNING_U")
# plot(subset(wbids2,is.na(Region)==F))
# plot(subset(wbids,GROUP_NAME=="Caloosahatchee"))
# 
# wbids2.dis=gUnaryUnion(wbids2,id=wbids2@data$Region)
# IDlist <- data.frame(ID=sapply(slot(wbids2.dis, "polygons"), function(x) slot(x, "ID")))
# rownames(IDlist)  <- IDlist$ID
# wbids2.dis=SpatialPolygonsDataFrame(wbids2.dis,IDlist)
# 
# plot(wbids2.dis)
# writeOGR(wbids2.dis,GIS.path,"Caloosa_WBIDs_dis",driver="ESRI Shapefile")

wbids.dis=spTransform(readOGR(GIS.path,"Caloosa_WBIDs_dis"),wkt(utm17))
wbids.dis2=sf::st_as_sf(wbids.dis)

library(tmap)
tmap_mode("view")

tm_shape(wbids)+tm_polygons(alpha=0.5)



# http://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html
# https://timogrossenbacher.ch/2016/12/beautiful-thematic-maps-with-ggplot2-only/
theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "serif", color = "#22211d"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      # panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      # plot.background = element_rect(fill = "lightblue", color = NA),
      panel.background = element_rect(fill = "lightblue", color = NA),
      # legend.background = element_rect(fill = "#f5f5f2", color = NA),
      plot.background = element_blank(),
      # panel.background = element_blank(),
      legend.background = element_blank(),
      panel.border = element_blank(),
      plot.title=element_text(size=12),
      plot.subtitle = element_text(color = "grey50",size=8),
      plot.caption = element_text(hjust = 0),
      ...
    )
}

# Scale bar
# https://stackoverflow.com/questions/39067838/parsimonious-way-to-add-north-arrow-and-scale-bar-to-ggmap

# northSymbols()

bbox.lims3=bbox(gBuffer(wbids.dis,width=2500))
bbox.lims2=bbox(gBuffer(cre.nnc.segs,width=5000))
bbox.lims=bbox(gBuffer(rf.sites.shp,width=5000))


map=ggplot()+
  # geom_polygon(data=shoreline.f,
  #              aes(long,lat,group=group),
  #              fill="cornsilk",colour="grey")+
  geom_sf(data=shoreline2,fill="cornsilk",colour="grey",size=0.1)+
  geom_sf(data=roads,lty=1,colour="grey",size=0.5,alpha=0.5)+
  geom_sf(data=wbids.dis2,fill="grey",alpha=0.25)+
  geom_sf(data=rf.sites.shp2,size=2,shape=21,fill="dodgerblue1")+
  geom_sf_text(data=rf.sites.shp2,aes(label=SITE),
            nudge_x = c(-4000,-4000,4000,-4000,-4000,4000,-4000,0,-4000),
            nudge_y = c(0,0,0,0,0,0,0,-3000,0),
            family="serif",size=3.5)+
  theme_map()+
  coord_sf(xlim=c(bbox.lims3[1,1],bbox.lims3[1,2]),ylim=c(bbox.lims[2,1],bbox.lims[2,2]))+
  # coord_sf(xlim=c(bbox.lims2[1,1],bbox.lims[1,2]),ylim=c(bbox.lims[2,1],bbox.lims[2,2]))+
  labs(subtitle = "Rainfall monitoring locations")
map      



comboplot=plot_grid(
  rf_POR,map,
  ncol=1,
  rel_heights = c(3,1),
  rel_widths = c(2,0.75))
comboplot
# ggsave(paste0(plot.path,"rf_POR_map.png"),comboplot,device="png",height =8,width=4.5,units="in")


map.bar=plot_grid(
  map,month.POR,
  ncol=1,
  rel_heights = c(1,1.5))
map.bar

comboplot2=plot_grid(
  rf_POR,map.bar,
  ncol=2,
  rel_widths = c(1.5,1)
)
comboplot2

# ggsave(paste0(plot.path,"rf_POR_map2.png"),comboplot2,device="png",height =8,width=7,units="in")



# Discharge ---------------------------------------------------------------

Q.dbkeys=data.frame(SITE=c("S79",rep("S78",3),rep("S77",2)),
                    DBKEY=c("00865","00857","WN161","DJ236","15635","DJ235"))

q.dat=data.frame()
for(i in 1:nrow(Q.dbkeys)){
  tmp=DBHYDRO_daily(dates[1],dates[2],Q.dbkeys$DBKEY[i])
  tmp$DBKEY=as.character(Q.dbkeys$DBKEY[i])
  q.dat=rbind(q.dat,tmp)
  print(i)
}

q.dat=merge(q.dat,Q.dbkeys,"DBKEY")
q.dat$Date.EST=date.fun(q.dat$Date)
q.dat$CY=as.numeric(format(q.dat$Date.EST,"%Y"))
q.dat$wknum=as.numeric(format(q.dat$Date.EST,"%j"))%/%7L+1L
unique(q.dat$wknum)
# q.dat$wknum=as.numeric(format(q.dat$Date.EST,"%V"))
# q.dat$wknum=as.numeric(lubridate::isoweek(q.dat$Date.EST));# isoweek

range(q.dat$Data.Value,na.rm=T)
q.dat$Data.Value[q.dat$Data.Value<0]=0
q.dat$Data.Value[is.na(q.dat$Data.Value)==T]=0

q.dat.damean=ddply(q.dat,c("SITE","Date.EST","CY","wknum"),summarise,flow.cfs=mean(Data.Value,na.rm=T),N.flow=N.obs(Data.Value))

q.dat.wk.mean=ddply(q.dat.damean,c("SITE","CY","wknum"),summarise,mean.flow=mean(flow.cfs))
range(q.dat.wk.mean$mean.flow,na.rm=T)

q.dat.wk.mean$Q.cat=as.factor(findInterval(q.dat.wk.mean$mean.flow,c(0,457,750,2100,2600,6500,20000)))
q.dat.wk.mean=merge(q.dat.wk.mean,
                 data.frame(Q.cat=c(NA,1:6),
                            Q.cat.txt=c("<NA>","< 457","457 - 750", "750 - 2100","2100 - 2600","2600 - 6500",">6500"),
                            txt.cols=c(NA,rep("black",3),rep("white",3))),
                 "Q.cat")
unique(q.dat.wk.mean$Q.cat)
ddply(subset(q.dat.wk.mean,SITE=="S79"),"CY",summarise,N.week=N.obs(wknum))

test=subset(q.dat.wk.mean,SITE=='S79')
test=test[order(test$CY,test$wknum),]
cols.wes=rev(wesanderson::wes_palette("Zissou1",7,"continuous"))
cols.vir=rev(viridis::inferno(6))
# cols=c("1"="red","2"="orange","3"="darkolivegreen1","4"="lightblue1","5"="deepskyblue","6"="royalblue","7"="black")
# cols=c("1"=cols.wes[1],"2"=cols.wes[2],"3"="darkolivegreen1","4"=cols.wes[5],"5"=cols.wes[6],"6"=cols.wes[7],"7"="black")
cols=c("1"=cols.vir[1],"2"=cols.vir[2],"3"=cols.vir[3],"4"=cols.vir[4],"5"=cols.vir[5],"6"=cols.vir[6])
S79_POR=ggplot(subset(q.dat.wk.mean,SITE=='S79'), aes(x = wknum, y = CY, fill = Q.cat)) +
  geom_tile(aes(group = Q.cat), colour = 'black')+
  #geom_text(aes(label=format(round(mean.flow,0),nsmall=0),fontface = "bold"),size=2,colour=q.dat.wk.mean$txt.cols,family="serif",)+
  scale_y_reverse(expand = c(0, 0), breaks = q.dat.wk.mean$CY) +
  # scale_x_discrete(expand = c(0, 0), position = 'top') +
  scale_x_continuous(expand = c(0, 0), breaks = seq(1,53,4),labels=seq(1,53,4))+
  scale_fill_manual(values = cols,
                    name="Weekly Average\nDischarge\nCategories\n(CFS)",
                    breaks=1:6,
                    labels=c("< 457","457 - 750", "750 - 2100","2100 - 2600","2600 - 6500",">6500")) +
  theme_bw() +
  theme(
    #legend.position = 'none',
    text=element_text(family="serif"),
    plot.title=element_text(size=12),
    plot.subtitle = element_text(color = "grey50",size=8),
    plot.caption = element_text(hjust = 0)
  )+
  labs(title = "Caloosahatchee River Estuary (S79)",
       subtitle = "Average weekly discharge",
       caption = paste0("Produced: ",format(Sys.Date(),"%d %b %Y")),
       x="Week",
       y="Year")

# ggsave(paste0(plot.path,"S79_POR.png"),S79_POR,device="png",height =7,width=5,units="in")

S77_POR=ggplot(subset(q.dat.wk.mean,SITE=='S77'), aes(x = wknum, y = CY, fill = Q.cat)) +
  geom_tile(aes(group = Q.cat), colour = 'black')+
  #geom_text(aes(label=format(round(mean.flow,0),nsmall=0),fontface = "bold"),size=2,colour=q.dat.wk.mean$txt.cols,family="serif",)+
  scale_y_reverse(expand = c(0, 0), breaks = q.dat.wk.mean$CY) +
  # scale_x_discrete(expand = c(0, 0), position = 'top') +
  scale_x_continuous(expand = c(0, 0), breaks = seq(1,53,4),labels=seq(1,53,4))+
  scale_fill_manual(values = cols,
                    name="Weekly Average\nDischarge\nCategories\n(CFS)",
                    breaks=1:6,
                    labels=c("< 457","457 - 750", "750 - 2100","2100 - 2600","2600 - 6500",">6500")) +
  theme_bw() +
  theme(
    #legend.position = 'none',
    text=element_text(family="serif"),
    plot.title=element_text(size=12),
    plot.subtitle = element_text(color = "grey50",size=8),
    plot.caption = element_text(hjust = 0)
  )+
  labs(title = "Lake Okeechobee (S77 - Moorehaven Lock)",
       subtitle = "Average weekly discharge",
       caption = paste0("Produced: ",format(Sys.Date(),"%d %b %Y")),
       x="Week",
       y="Year")

S77_POR




# Lake Stage --------------------------------------------------------------
dates=date.fun(c("1978-01-01","2021-12-03"))

stg.dat=DBHYDRO_daily(dates[1],dates[2],c("00268","06832"))
subset(stg.dat,is.na(Data.Value))
range(stg.dat$Data.Value)

stg.dat=ddply(stg.dat,c("Date"),summarise,Data.Value=mean(Data.Value,na.rm=T))
stg.dat$stgchange_7day=with(stg.dat,c(rep(NA,7),diff(Data.Value,lag=7)))

stg.dat$DoY=as.numeric(format(stg.dat$Date,"%j"))
stg.dat$CY=as.numeric(format(stg.dat$Date,"%Y"))
max(stg.dat$Date)

subset(stg.dat,Data.Value==max(stg.dat$Data.Value,na.rm=T))
subset(stg.dat,Data.Value==min(stg.dat$Data.Value,na.rm=T))

plot(Data.Value~DoY,subset(stg.dat,CY==1995),ylim=c(8,19))
with(subset(stg.dat,CY==2007),lines(Data.Value~DoY))
with(subset(stg.dat,CY==2019),lines(Data.Value~DoY,col="darkgreen"))
with(subset(stg.dat,CY==2020),lines(Data.Value~DoY,col="red"))
with(subset(stg.dat,CY==2021),lines(Data.Value~DoY,col="blue"))


date.fill=data.frame(expand.grid(CY=seq(min(stg.dat$CY),max(stg.dat$CY),1),
            DoY=1:366))

recess.dat=stg.dat[,c("CY","DoY","Data.Value","stgchange_7day")]
recess.dat$recess_7day=with(recess.dat,ifelse(stgchange_7day<=0,abs(stgchange_7day),NA))

bks=c(0,0.05,0.16)
recess.dat$cat=as.factor(findInterval(recess.dat$recess_7day,bks,rightmost.closed = T,left.open = T))
range(stg.dat$stgchange_7day,na.rm=T)
bks=c(min(stg.dat$stgchange_7day,na.rm=T),-0.16,-0.05,0.05,0.25,max(stg.dat$stgchange_7day,na.rm=T))
recess.dat$cat2=as.factor(findInterval(recess.dat$stgchange_7day,bks,rightmost.closed = T,left.open = T))
recess.dat$cat2=with(recess.dat,ifelse(stgchange_7day>=-0.05&stgchange_7day<=0.05,1,
                                       ifelse(stgchange_7day<(-0.05)&stgchange_7day>(-0.16),2,
                                              ifelse(stgchange_7day<=(-0.16),3,
                                                     ifelse(stgchange_7day>0.05&stgchange_7day<=0.25,4,
                                                            ifelse(stgchange_7day>0.25,5,NA))))))
recess.dat$cat2=as.factor(recess.dat$cat2)
ddply(recess.dat,"cat2",summarise,min.val=min(stgchange_7day,na.rm=T),max.val=max(stgchange_7day,na.rm=T))


recess.dat=merge(recess.dat,date.fill,by=c("CY",'DoY'),all.y=T)

recess.dat=recess.dat[order(recess.dat$CY,recess.dat$DoY),]

library(ggplot2)
cols=c("1"="green","2"="yellow","3"="red","NA"="white")
rec.plot=ggplot(recess.dat, aes(x = DoY, y = CY, fill = cat)) +
  geom_tile(aes(group = cat), colour = "black",size=0.05)+
  #geom_text(aes(label=format(round(mean.flow,0),nsmall=0),fontface = "bold"),size=2,colour=q.dat.wk.mean$txt.cols,family="serif",)+
  scale_y_reverse(expand = c(0, 0), breaks = recess.dat$CY) +
  # scale_x_discrete(expand = c(0, 0), position = 'top') +
  scale_x_continuous(expand = c(0, 0), breaks = seq(1,366,30),labels= seq(1,366,30))+
  scale_fill_manual(values = cols,
                    name="Weekly Recession\nRate (ft/wk)",
                    breaks=c(1:3),
                    labels=c("< 0.05","\u2265 0.05 & < 0.16","\u2265 0.16"),
                    na.value="white") +
  theme_bw() +
  theme(
    #legend.position = 'none',
    text=element_text(family="serif"),
    plot.title=element_text(size=12),
    plot.subtitle = element_text(color = "grey50",size=8),
    plot.caption = element_text(hjust = 0)
  )+
  labs(title = "Lake Okeechobee Recession Rate",
       subtitle = "Weekly Recession Rate for Snail Kite",
       caption = paste0("Produced: ",format(Sys.Date(),"%d %b %Y")),
       x="Day of Year",
       y="Year")
# ggsave(paste0(plot.path,"Lake_recess.png"),rec.plot,device="png",height =7,width=6,units="in")

unique(recess.dat$cat2)
cols=c("1"="red","2"="yellow","3"="green","4"="goldenrod2","5"="darkred")
#  cols=c("1"="green","2"="yellow","3"="red","4"="goldenrod2","5"="darkred")
rec.plot2=ggplot(recess.dat, aes(x = DoY, y = CY, fill = cat2)) +
  geom_tile(aes(group = cat2), colour = "black",size=0.05)+
  #geom_text(aes(label=format(round(mean.flow,0),nsmall=0),fontface = "bold"),size=2,colour=q.dat.wk.mean$txt.cols,family="serif",)+
  scale_y_reverse(expand = c(0, 0), breaks = recess.dat$CY) +
  # scale_x_discrete(expand = c(0, 0), position = 'top') +
  scale_x_continuous(expand = c(0, 0), breaks = seq(1,366,30),labels= seq(1,366,30))+
  scale_fill_manual(values = cols,
                    name="Weekly Recession\nRate (ft/wk)",
                    breaks=c(1:5),
                    # labels=c("\u2265 0.05 & \u2264 -0.05","< -0.05 & > -0.16","\u2264 -0.16", "> 0.05 & \u2264 0.25", "> 0.25"),
                    labels=c("\u2264 -0.16","> -0.16 & < -0.05","> -0.05 & \u2264 0.05","> 0.05 & \u2264 0.25","> 0.25"),
                    na.value="white") +
  theme_bw() +
  theme(
    #legend.position = 'none',
    text=element_text(family="serif"),
    plot.title=element_text(size=12),
    plot.subtitle = element_text(color = "grey50",size=8),
    plot.caption = element_text(hjust = 0)
  )+
  labs(title = "Lake Okeechobee Recession/Accession Rate",
       subtitle = "Observed Weekly Recession/Accession Rates",
       caption = paste0("Produced: ",format(Sys.Date(),"%d %b %Y")),
       x="Day of Year",
       y="Year")
rec.plot2
# ggsave(paste0(plot.path,"Lake_recess2.png"),rec.plot2,device="png",height =7,width=6,units="in")