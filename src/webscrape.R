## Webscraping for CRE conditions report

#Webscraping
library(rvest)
library(readr)
library(rjson)
library(jsonlite)

#Table
library(kableExtra)
library(knitr)

#Standard analysis
library(AnalystHelper)
library(plyr)
library(reshape)
library(lubridate)
library(dataRetrieval)
library(zoo)
library(RCurl)
# library(data.table)

###
WMD.json=function(path,type="1"){
  if(type=="1"){
    dat=rjson::fromJSON(file=path)
    dat=data.frame(Date= sapply(dat, "[", 1), Value = sapply(dat, "[", 2))
  }
  if(type=="2"){
    dat=jsonlite::fromJSON(path)
    dat=data.frame(Date=dat[,1],Value=dat[,2])
  }
  dat$Date=date.fun(as.POSIXct(dat$Date/1000,origin="1970-01-01",tz="GMT"))
  return(dat)
}
ColumnRename=function(rawData){
  Conv.df=data.frame(conv.vals=c(paste("p",c("00010","00095","00480","00060"),sep="")),conv.defs=c("Wtemp","SpCond","Sal","Flow"))
  Cnames=names(rawData)
  dataColumns <- c(grep("X_", Cnames), grep("X\\d{2}", Cnames))
  
  for(i in dataColumns){
    chunks <- strsplit(Cnames[i], "_")[[1]]
    chunks=unlist(strsplit(chunks,".",fixed=T))
    loc=toupper(chunks[chunks%in%c("TOP","Top","BOTTOM","Bottom")==T])
    param=paste0("p",chunks[paste0("p",chunks) %in% Conv.df$conv.vals])
    param.def=as.character(subset(Conv.df,conv.vals==param)$conv.defs)
    flag=if(length(chunks[(chunks=="cd")])==0){NA}else{chunks[(chunks=="cd")]}
    Cnames[i]=if(is.na(flag)==F){paste(loc,param.def,flag,sep="_")}else{paste(loc,param.def,sep="_")}
  }
  Cnames <- gsub("X_", "", Cnames)
  names(rawData) <- Cnames
  return(rawData)
}


# -------------------------------------------------------------------------
library(rvest)
library(kableExtra)

LO.url="https://w3.saj.usace.army.mil/h2o/reports/r-oke.html"
webpage=read_html(LO.url)
node.val=html_nodes(webpage,"pre")

LO.text.vals=html_text(node.val)
LO.report=strsplit(as.character(LO.text.vals),"\n")
LO.report

tmp1=data.frame(t(sapply(strsplit(LO.report[[1]][266:279],"\\s+"),c)))
colnames(tmp1)=c("day","month","year","S77","S77_ds","S78","S79")
tmp1$Date=with(tmp1,date.fun(paste(year,month,day),form="%Y %B %d"))
tmp1[,4:7]=sapply(tmp1[,4:7],as.numeric)

tmp2=data.frame(t(sapply(strsplit(LO.report[[1]][285:298],"\\s+"),c)))
colnames(tmp2)=c("day","month","year","S310","S351","S352","S354","L8")
tmp2$Date=with(tmp2,date.fun(paste(year,month,day),form="%Y %B %d"))
tmp2[,4:8]=sapply(tmp2[,4:8],as.numeric)

tmp3=data.frame(t(sapply(strsplit(LO.report[[1]][304:317],"\\s+"),c)))
colnames(tmp3)=c("day","month","year","S308","S308_ds","S80")
tmp3$Date=with(tmp3,date.fun(paste(year,month,day),form="%Y %B %d"))
tmp3[,4:6]=sapply(tmp3[,4:6],as.numeric)

q.dat=cbind(tmp1[,c(8,4:7)],tmp2[,c(4:8)],tmp3[,c(4:6)])

##
str.url="https://w3.saj.usace.army.mil/h2o/reports/r-lonin.html"
webpage=read_html(str.url)
node.val=html_nodes(webpage,"pre")

str.text.vals=html_text(node.val)
str.report=strsplit(as.character(str.text.vals),"\n")
str.report

pref.struct=data.frame(t(sapply(strsplit(str.report[[1]][21:34],"\\|"),c)))
pref.struct
colnames(pref.struct)=c("Date","LONIN_cfs","STORAGE_cfs","S77","S308","S351","S352","S354","L8")
pref.struct[,2:9]=sapply(pref.struct[,2:9],as.numeric)
pref.struct$Date=date.fun(pref.struct$Date,form="%d %B %Y")

## Discharge data for lake and estuaries
q.dat=merge(pref.struct[,c("Date","S77","S308","L8",'S351',"S352","S354")],
q.dat[,c("Date","S78","S79","S310","S80")],"Date")

##
library(lubridate)
dates=seq(date.fun(date.fun("2021-03-31")-ddays(13)),date.fun("2021-03-31"),"1 days")

map.q=data.frame()
for(i in 1:length(dates)){
  map.url=paste0("https://w3.saj.usace.army.mil/h2o/reports/StatusDaily/archive/",format(dates[i],"%m%d"),"/StatusDaily.htm")
  mapdata=readLines(map.url)
  val=grep("CA1IN",mapdata)
  WCA1=strsplit(strsplit(mapdata[val],"\\s+")[[1]][13],"</div>")[[1]][1]
  
  val=grep("CA2IN",mapdata)
  WCA2=strsplit(strsplit(mapdata[val],"\\s+")[[1]][13],"</div>")[[1]][1]
  
  val=grep("CA3IN",mapdata)
  WCA3=strsplit(strsplit(mapdata[val],"\\s+")[[1]][13],"</div>")[[1]][1]
  
  val=grep("S12",mapdata)
  ENP=strsplit(strsplit(mapdata[val],"\\s+")[[1]][8],"</div>")[[1]][1]
  
  date.val=dates[i]-ddays(1)
  rslt=data.frame(Date=date.val,WCA1=as.numeric(WCA1),WCA2=as.numeric(WCA2),WCA3=as.numeric(WCA3),ENP=as.numeric(ENP))
  map.q=rbind(map.q,rslt)
  print(i)
}

map.q[,2:5]=cfs.to.acftd(map.q[,2:5])

q.dat=merge(q.dat,map.q,"Date")

vars=c("Date","S77","S78","S79","S310","S351","S352","S354","L8","S308","S80","WCA1","WCA2","WCA3","ENP")
date.7day=seq(date.fun(date.fun("2021-03-29")-ddays(6)),date.fun("2021-03-29"),"1 days")

q.dat.7day=subset(q.dat,Date%in%date.7day)[,vars]
meanQ=cbind(data.frame(Statistic="Average"),data.frame(t(apply(q.dat.7day[,2:15],2,FUN=function(x)format(round(mean(x),0))))))
totalQ=cbind(data.frame(Statistic="Total"),data.frame(t(apply(q.dat.7day[,2:15],2,FUN=function(x)format(round(sum(x),0))))))

q.dat.7day=q.dat.7day[match(q.dat.7day$Date,rev(q.dat.7day$Date)),]

library(flextable)
library(magrittr)

q.dat.7day%>%
  flextable()%>%
  colformat_datetime(j=1,fmt_date="%m-%d")%>%
  colformat_double(j=2:15,big.mark = "",digits=0)
  
rbind(meanQ,totalQ)%>%
  flextable()
