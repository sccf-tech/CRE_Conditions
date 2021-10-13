
library(AnalystHelper)
library(dataRetrieval)
library(reshape)

CurWY=as.numeric(ifelse(as.numeric(format(as.Date(Sys.time()),"%m"))>4,as.numeric(format(as.Date(Sys.time()),"%Y"))+1,format(as.Date(Sys.time()),"%Y")))

Start.Date=as.Date(paste(CurWY-2,05,01,sep="-"))
End.Date=as.Date(Sys.time())+duration(1,"days")
TODAY=as.POSIXct(strptime(Sys.time(),"%F"),tz="EST") # -duration(1,"days")


charhar.sites=data.frame(SITE=c("02298202","02296750","02298880","02297100","02299472"),
                         SiteName.abbr=c("Shell","Peace","Myakka","Joshua","BigSlough"),
                         Site.Lab=c("Shell Creek","Peace River","Myakka River","Joshua Creek","Big Slough"))
charhar.sites=charhar.sites[match(c("Myakka","BigSlough","Peace","Joshua","Shell"),charhar.sites$SiteName.abbr),]

q.dat=readNWISdv(charhar.sites$SITE,c("00060"),format(Start.Date,"%Y-%m-%d"),format(End.Date,"%Y-%m-%d"))
q.dat=renameNWISColumns(q.dat)
q.dat$Date=date.fun(q.dat$Date)
q.dat=merge(q.dat,charhar.sites,by.x="site_no",by.y="SITE")
q.dat$Flow.acft=cfs.to.acftd(q.dat$Flow)

fill=expand.grid(Date=seq(date.fun(Start.Date),max(q.dat$Date),"1 days"),
                 SITE=charhar.sites$SITE)
fill=merge(fill,charhar.sites,"SITE")
q.dat=merge(q.dat,fill,by.x=c("site_no","Date","SiteName.abbr"),by.y=c("SITE","Date","SiteName.abbr"),all.y=T)

q.dat$flow.fill=with(q.dat,ave(Flow.acft,site_no,FUN=function(x) zoo::na.approx(x)))


range(q.dat$Flow)
#q.dat.xtab=cast(q.dat,Date~SiteName.abbr,value="Flow",fun.aggregate=function(x) sum(cfs.to.acftd(x),na.rm=T))
# q.dat.xtab=cast(q.dat,Date~SiteName.abbr,value="Flow",sum,na.rm=T)
q.dat.xtab=cast(q.dat,Date~SiteName.abbr,value="flow.fill",sum, na.rm=T)
charhar.sites2=readNWISsite(charhar.sites$SITE)
q.dat.xtab$Tflow=rowSums(q.dat.xtab[,2:ncol(q.dat.xtab)])

plot(Myakka~Date,q.dat.xtab)
plot(Peace~Date,q.dat.xtab)
plot(Joshua~Date,q.dat.xtab)
plot(Shell~Date,q.dat.xtab)


plot(Tflow~Date,q.dat.xtab)
with(q.dat.xtab,shaded.range(Date,rep(0,length(Date)),Myakka,lty=1,bg="grey"))
with(q.dat.xtab,shaded.range(Date,Myakka,Myakka+Peace,lty=1,bg="red"))
with(q.dat.xtab,shaded.range(Date,Myakka+Peace,Myakka+Peace+Joshua,lty=1,bg="blue"))
with(q.dat.xtab,shaded.range(Date,Myakka+Peace+Joshua,Myakka+Peace+Joshua+Shell,lty=1,bg="green"))



xlim.val=date.fun(c(TODAY-ddays(30),TODAY+ddays(3)));xmaj=seq(xlim.val[1],xlim.val[2],by="7 days");xmin=seq(xlim.val[1],xlim.val[2],by="1 days")
cols=c("#3B9AB2", "#78B7C5", "#EBCC2A", "#E1AF00", "#F21A00")

layout(matrix(1:6,6,1,byrow=F))
par(family="serif",cex.axis=1.2,mar=c(1.5,2,0.5,1),oma=c(2.5,3,1,1));
for(i in 1:5){
  tmp=subset(q.dat,site_no==charhar.sites$SITE[i])
  n.mt=sum(is.na(subset(tmp,Date%in%seq(TODAY-ddays(30),TODAY+ddays(3),"1 days"))$Flow.acft))
  
  max.q=max(subset(tmp,Date%in%seq(TODAY-ddays(30),TODAY+ddays(3),"1 days"))$Flow.acft,na.rm=T)
  ylim.max=round(max.q+max.q*0.25,-3)
  
  ylim.val=c(0,ylim.max);by.y=ylim.max/2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  plot(Flow.acft~Date,tmp,type="n",ylim=ylim.val,xlim=xlim.val,xaxs="i",yaxs="i",xaxt="n",yaxt="n",xlab=NA,ylab=NA)
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  if(n.mt>0){with(tmp,lines(Date,flow.fill,col="grey",lwd=1,lty=2))}
  with(tmp,pt_line(Date,Flow.acft,2,cols[i],1.5,21,cols[i]))
  
  # if(i==4){axis_fun(1,xmaj,xmin,format(xmaj,"%m/%d/%Y"),line=-0.5)}else{axis_fun(1,xmaj,xmin,NA,line=-0.5)}
  axis_fun(1,xmaj,xmin,NA,line=-0.5)
  axis_fun(2,ymaj,ymin,format(ymaj/1000,nsmall=1),cex.axis=1)
  box(lwd=1)
  with(charhar.sites[i,],mtext(side=3,adj=0,paste(Site.Lab," (USGS Site: ",SITE,")",sep="")),cex=0.75)
  
}


max.q=max(subset(q.dat.xtab,Date%in%seq(TODAY-ddays(30),TODAY+ddays(3),"1 days"))$Tflow,na.rm=T)
ylim.max=round(max.q+max.q*0.25,-3)

ylim.val=c(0,ylim.max);by.y=ylim.max/4;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

plot(Tflow~Date,q.dat.xtab,type="n",ylim=ylim.val,xlim=xlim.val,xaxs="i",yaxs="i",xaxt="n",yaxt="n",xlab=NA,ylab=NA)
abline(h=ymaj,v=xmaj,lty=3,col="grey")
with(q.dat.xtab,shaded.range(Date,rep(0,length(Date)),Myakka,lty=1,bg=cols[1]))
with(q.dat.xtab,shaded.range(Date,Myakka,Myakka+BigSlough,lty=1,bg=cols[2]))
with(q.dat.xtab,shaded.range(Date,Myakka+BigSlough,Myakka+BigSlough+Peace,lty=1,bg=cols[3]))
with(q.dat.xtab,shaded.range(Date,Myakka+BigSlough+Peace,Myakka+BigSlough+Peace+Joshua,lty=1,bg=cols[4]))
with(q.dat.xtab,shaded.range(Date,Myakka+BigSlough+Peace+Joshua,Myakka+BigSlough+Peace+Joshua+Shell,lty=1,bg=cols[5]))
axis_fun(1,xmaj,xmin,format(xmaj,"%m/%d/%Y"),line=-0.5)
axis_fun(2,ymaj,ymin,format(ymaj/1000,nsmall=1),cex.axis=1)
box(lwd=1)
mtext(side=3,adj=0,"Daily Total")
mtext(side=2,line=1.25,outer=T,"Discharge (kAc-Ft D\u207B \u00B9)")
mtext(side=1,line=2,"Date (MM/DD/YYYY)")



library(httr)
library(sf)
library(tmap)

url <- parse_url("https://services.arcgis.com/P3ePLMYs2RVChkJx/arcgis/rest/services")
url$path <- paste(url$path, "USA_Counties/FeatureServer/0/query", sep = "/")
url$query <- list(where = "STATE_NAME = 'Florida'",  
                  outFields = "*",          
                  returnGeometry = "true",      
                  f = "geojson")
request <- build_url(url)

Florida_County <- st_read(request)
tmap_mode(mode = "view")
tm_shape(Florida_County)+tm_borders()#+tm_shape(NWIS.siteinfo.shp)+tm_dots()

url <- parse_url("https://services.arcgis.com/P3ePLMYs2RVChkJx/arcgis/rest/services")
url$path <- paste(url$path, "USA_Counties/FeatureServer/0/query", sep = "/")
url$query <- list(where = "STATE_NAME = 'Florida'",  
                  outFields = "*",          
                  returnGeometry = "true",      
                  f = "geojson")
request <- build_url(url)

Florida_County <- st_read(request)
tmap_mode(mode = "view")
tm_shape(Florida_County)+tm_borders()#+tm_shape(NWIS.siteinfo.shp)+tm_dots()
