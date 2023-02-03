## Title:      Caloosahatchee Data Scraping
##             
## Created by: Paul Julian (pauljulianphd@gmail.com)
## Created on: 01/24/2023

## BAD ## https://www.tidyverse.org/articles/2017/12/workflow-vs-script/
#Clears Everything...start fresh.
rm(list=ls(all=T));cat("\014");dev.off()

## Libraries
### Function to check, install and load packages 
### If package is installed, then the function moves to the next.
### Should only need to do this once

check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

pkg=c("remotes","jsonlite","httr","rvest","lubridate","RcppRoll")
check.packages(pkg)
warnings()

## For GitHub hosted package only
check.GH.packages <- function(pkg){
  pkg.only=sapply(strsplit(pkg,"/"),"[",2)
  new.GHpkg <- pkg[!(pkg.only %in% installed.packages()[, "Package"])]
  if (length(new.GHpkg)) 
    remotes::install_github(new.GHpkg, dependencies = TRUE)
  sapply(pkg.only, require, character.only = TRUE)
}
check.GH.packages("SwampThingPaul/AnalystHelper")


## If already installed just load packages
library(AnalystHelper);

library(remotes)
library(httr)
library(rvest)
library(jsonlite)
library(lubridate)
library(RcppRoll)

## extra functions
acftd.to.cfs=function(x) x*0.5041669

consec.startend=function(var){
  runs=rle(var)
  myruns = which(runs$values == TRUE)
  runs.lengths.cumsum = cumsum(runs$lengths)
  ends = runs.lengths.cumsum[myruns]
  newindex = ifelse(myruns>1, myruns-1, 0)
  starts = runs.lengths.cumsum[newindex] + 1
  if (0 %in% newindex) starts = c(1,starts)
  rslt=list(starts=starts,ends=ends)
  return(rslt)
}

# Data --------------------------------------------------------------------
## Lake Okeechobee Report
LO.url="https://w3.saj.usace.army.mil/h2o/reports/r-oke.html"
# download.file(LO.url, destfile = "LOK_scrapedpage.html", quiet=TRUE)
webpage=read_html(LO.url)
node.val=html_nodes(webpage,"pre")

LO.text.vals=html_text(node.val)
LO.report=strsplit(as.character(LO.text.vals),"\n")
LO.report

# Finds LOK Elevation line
# grep("*Okeechobee Lake Elevation",LO.report[[1]])

## 
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

## Converts Acft/D to CFS
q.dat[,2:13]=sapply(q.dat[,2:13],acftd.to.cfs)

## Preferred Structures
str.url="https://w3.saj.usace.army.mil/h2o/reports/r-lonin.html"
# download.file(str.url, destfile = "struct_scrapedpage.html", quiet=TRUE)
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
pref.struct[,2:9]=cfs.to.acftd(pref.struct[,2:9])

## Discharge data for lake and estuaries
q.dat=merge(pref.struct[,c("Date","S77","S308","L8",'S351',"S352","S354")],
            q.dat[,c("Date","S78","S79","S310","S80")],"Date")

## Maps and Archived data
## Uses system time minus 13 days
dates=seq(date.fun(date.fun(Sys.Date())-ddays(13)),date.fun(Sys.Date()),"1 days")

map.q=data.frame()
map.stg=data.frame()
for(i in 1:(length(dates)-1)){
  map.url=paste0("https://w3.saj.usace.army.mil/h2o/reports/StatusDaily/archive/",format(dates[i],"%m%d"),"/StatusDaily.htm")
  # download.file(map.url, destfile = "map_dat.html", quiet=TRUE)
  mapdata=readLines(map.url)
  val=grep("CA1IN",mapdata)
  WCA1=strsplit(strsplit(mapdata[val],"\\s+")[[1]][13],"</div>")[[1]][1]
  
  val=grep("CA2IN",mapdata)
  WCA2=strsplit(strsplit(mapdata[val],"\\s+")[[1]][13],"</div>")[[1]][1]
  
  val=grep("CA3IN",mapdata)
  WCA3=strsplit(strsplit(mapdata[val],"\\s+")[[1]][13],"</div>")[[1]][1]
  
  val=grep("S12",mapdata)
  S12s=as.numeric(strsplit(strsplit(mapdata[val],"\\s+")[[1]][8],"</div>")[[1]][1])
  
  val=grep("S333",mapdata)
  S333=as.numeric(strsplit(mapdata[val],"<br>|\\s+")[[1]][12])
  S333N=as.numeric(strsplit(strsplit(mapdata[val],"<br>|\\s+")[[1]][14],"</div>")[[1]][1])
  
  val=grep("S356",mapdata)
  S356=as.numeric(strsplit(strsplit(mapdata[val],"\\s+")[[1]][8],"</div>")[[1]][1])
  
  ENP=S12s+(S333+S333N)-S356
  
  val=grep("Istokpoga</a>",mapdata)
  Istok=strsplit(strsplit(mapdata[val],"\\s+")[[1]][6],"</div>")[[1]][1]
  
  val=grep("S-65E</a>",mapdata)
  S65E=strsplit(strsplit(mapdata[val],"\\s+")[[1]][6],"<br>")[[1]][1]
  
  val=grep("S-65EX1</a>",mapdata)
  S65EX1=strsplit(strsplit(mapdata[val],"\\s+")[[1]][6],"<br>")[[1]][1]
  
  val=grep("Fisheating Creek",mapdata)
  FEC=strsplit(strsplit(mapdata[val],"\\s+")[[1]][7],"</div>")[[1]][1]
  
  val=grep("/plots/s79h.pdf",mapdata)
  S79=strsplit(strsplit(mapdata[val],"\\s+")[[1]][6],"</a>")[[1]][1]
  
  val=grep("/plots/s78h.pdf",mapdata)
  S78=strsplit(strsplit(mapdata[val],"\\s+")[[1]][6],"</a>")[[1]][1]
  
  val=grep("/plots/s77",mapdata)
  S77=strsplit(strsplit(mapdata[val],"\\s+")[[1]][6],"</a>")[[1]][1]
  
  val=grep("../plots/ok8hhp.pdf",mapdata)
  LakeStage=as.numeric(strsplit(mapdata[val[1]],"\\s+")[[1]][6])
  
  # date.val=dates[i]-ddays(1)
  date.val=dates[i]
  rslt=data.frame(Date=date.val,
                  FEC=as.numeric(FEC),
                  Istok=as.numeric(Istok),
                  S65E=as.numeric(S65E),
                  S65EX1=as.numeric(S65EX1),
                  WCA1=as.numeric(WCA1),
                  WCA2=as.numeric(WCA2),
                  WCA3=as.numeric(WCA3),
                  ENP=as.numeric(ENP),
                  S79.map=as.numeric(S79),
                  S78.map=as.numeric(S78),
                  S77.map=as.numeric(S77))
  map.q=rbind(map.q,rslt)
  rslt.stg=data.frame(Date=date.val,Stg=as.numeric(LakeStage))
  map.stg=rbind(map.stg,rslt.stg)
  print(i)
}

# map.q[,2:ncol(map.q)]=cfs.to.acftd(map.q[,2:ncol(map.q)])

q.dat=merge(q.dat,map.q,"Date")
q.dat$S79=with(q.dat,ifelse(is.na(S79)==T,round(S79.map,0),S79))
q.dat$S78=with(q.dat,ifelse(is.na(S78)==T,round(S78.map,0),S78))
q.dat$S77=with(q.dat,ifelse(is.na(S77)==T,round(S77.map,0),S77))
q.dat
vars=c("Date", "S77", "S308", "L8", "S351", "S352", "S354", "S78", 
       "S79", "S310", "S80", "FEC", "Istok", "S65E", "S65EX1", "WCA1", 
       "WCA2", "WCA3", "ENP")
q.dat=q.dat[,vars]

# q.dat[q.dat<0]<-NA

q.dat$NthLake=rowSums(q.dat[,c("FEC","Istok","S65E","S65EX1")],na.rm=T)


# Export file -------------------------------------------------------------

## Will open dialog box to save file 
write.csv(q.dat,file.choose(),row.names = F)

# or can write path and run write.csv(...)

# path="C:/Julian/CRE_Report/DischargeData.csv"
# write.csv(q.dat,path,row.names=F)

# Calculations ------------------------------------------------------------
## 
vars=c("Date","S77","S78","S79","S310","S351","S352","S354","L8","S308","S80","NthLake","WCA1","WCA2","WCA3","ENP")
date.7day=seq(date.fun(date.fun(Sys.Date())-ddays(7)),date.fun(Sys.Date()),"1 days")

q.dat.7day=subset(q.dat,Date%in%date.7day)[,vars]

laketotalQ=q.dat.7day[,c("S77","S310","S351","S352","S354","L8","S308")]
q.from.Lake=sum(laketotalQ[laketotalQ>0],na.rm=T)
laketotalQ$EAA=rowSums(laketotalQ[,c("S351","S352","S354")],na.rm=T)
S77q=sum(laketotalQ[laketotalQ$S77>0,c("S77")],na.rm=T)
S77q.per=if(q.from.Lake==0){0}else if((S77q/q.from.Lake)*100<1){"< 1"}else{round((S77q/q.from.Lake)*100,0)}

S308q=sum(laketotalQ[,c("S308")],na.rm=T)
S308q.pos=sum(laketotalQ[laketotalQ$S308>0,c("S308")],na.rm=T)
S308q.per=ifelse(S308q<0,NA,round((S308q/q.from.Lake)*100,0))

S310q=sum(laketotalQ[,c("S310")],na.rm=T)
S310q.pos=sum(laketotalQ[laketotalQ$S310>0,c("S310")],na.rm=T)
S310q.per=ifelse(S310q<0,NA,round((S310q/q.from.Lake)*100,0))

L8q=sum(laketotalQ[,c("L8")],na.rm=T)
L8q.pos=sum(laketotalQ[laketotalQ$L8>0,c("L8")])
L8q.per=ifelse(L8q<0,NA,round((L8q/q.from.Lake)*100,0))

EAAq=sum(laketotalQ[,c("EAA")],na.rm=T)
EAAq.pos=sum(laketotalQ[laketotalQ$EAA>0,c("EAA")],na.rm=T)
EAAq.per=ifelse(EAAq<0,NA,round((EAAq/q.from.Lake)*100,0))

laketotalQ2=q.dat.7day[,c("S77","S310","S351","S352","S354","L8","S308")]
tmp=apply(laketotalQ2,2,min,na.rm=T)

q.BF.Lake=sum(abs(laketotalQ2[laketotalQ2<0]),na.rm=T)
q.in.Lake=sum(q.dat.7day[,"NthLake"],na.rm=T)



Q.dat.xtab=q.dat.7day[,c("Date","S79","S78","S77")]
Q.dat.xtab=Q.dat.xtab[order(Q.dat.xtab$Date),]
Q.dat=Q.dat.xtab[,c("Date","S79")]
Q.dat$Q=Q.dat$S79

dates2=c(date.fun(min(Q.dat.xtab$Date)-duration(365,'days')),date.fun(min(Q.dat.xtab$Date)-duration(1,'days')))
S79dat2=DBHYDRO_daily(dates2[1],dates2[2],"DJ237")
S79dat2$Q=S79dat2$Data.Value
S79dat2=rbind(S79dat2[,c("Date","Q")],Q.dat[,c("Date","Q")])

# Number of 14-day events -------------------------------------------------
S79dat2$Q.14=with(S79dat2,roll_meanr(Q,n=14))
S79dat2$Low=with(S79dat2,ifelse(Q.14<750,1,0))
S79dat2$Opt=with(S79dat2,ifelse(Q.14>=750&Q.14<2100,1,0))
S79dat2$Stress=with(S79dat2,ifelse(Q.14>=2100&Q.14<2600,1,0))
S79dat2$Dam=with(S79dat2,ifelse(Q.14>=2600,1,0))
S79dat2$Date=date.fun(S79dat2$Date)

S79dat2$Low.consec=0
for(i in 2:nrow(S79dat2)){
  S79dat2$Low.consec[i]=with(S79dat2,ifelse(Low[i-1]==0&Low[i]>0,1,
                                            ifelse(Low[i-1]>0&Low[i]>0,1,0)))
}
Low.consec.val=consec.startend(S79dat2$Low.consec)
S79dat2$sum.Low.consec=0
if(length(Low.consec.val$ends)!=0){
  for(i in 1:length(Low.consec.val$ends)){
    S79dat2[Low.consec.val$ends[i],]$sum.Low.consec=with(S79dat2[c(Low.consec.val$starts[i]:Low.consec.val$ends[i]),],sum(Low.consec,na.rm=T))
  }
}
#
S79dat2$Opt.consec=0
for(i in 2:nrow(S79dat2)){
  S79dat2$Opt.consec[i]=with(S79dat2,ifelse(Opt[i-1]==0&Opt[i]>0,1,
                                            ifelse(Opt[i-1]>0&Opt[i]>0,1,0)))
}
Opt.consec.val=consec.startend(S79dat2$Opt.consec)
#
S79dat2$sum.Opt.consec=0
if(length(Opt.consec.val$ends)!=0){
  for(i in 1:length(Opt.consec.val$ends)){
    S79dat2[Opt.consec.val$ends[i],]$sum.Opt.consec=with(S79dat2[c(Opt.consec.val$starts[i]:Opt.consec.val$ends[i]),],sum(Opt.consec,na.rm=T))
  }
}
# 
S79dat2$Stress.consec=0
for(i in 2:nrow(S79dat2)){
  S79dat2$Stress.consec[i]=with(S79dat2,ifelse(Stress[i-1]==0&Stress[i]>0,1,
                                               ifelse(Stress[i-1]>0&Stress[i]>0,1,0)))
}
Stress.consec.val=consec.startend(S79dat2$Stress.consec)
S79dat2$sum.Stress.consec=0
if(length(Stress.consec.val$ends)!=0){
  for(i in 1:length(Stress.consec.val$ends)){
    S79dat2[Stress.consec.val$ends[i],]$sum.Stress.consec=with(S79dat2[c(Stress.consec.val$starts[i]:Stress.consec.val$ends[i]),],sum(Stress.consec,na.rm=T))
  }
}
#
S79dat2$Dam.consec=0
for(i in 2:nrow(S79dat2)){
  S79dat2$Dam.consec[i]=with(S79dat2,ifelse(Dam[i-1]==0&Dam[i]>0,1,
                                            ifelse(Dam[i-1]>0&Dam[i]>0,1,0)))
}
Dam.consec.val=consec.startend(S79dat2$Dam.consec)
S79dat2$sum.Dam.consec=0
if(length(Dam.consec.val$ends)!=0){
  for(i in 1:length(Dam.consec.val$ends)){
    S79dat2[Dam.consec.val$ends[i],]$sum.Dam.consec=with(S79dat2[c(Dam.consec.val$starts[i]:Dam.consec.val$ends[i]),],sum(Dam.consec,na.rm=T))
  }
}
# S79dat2=subset(S79dat2,is.na(Q.14)==F)
salenv.vals=subset(S79dat2,Date==max(S79dat2$Date))

salenv.vals1=salenv.vals[,paste("sum",c("Low","Opt","Stress","Dam"),"consec",sep=".")]
colnames(salenv.vals1)=c("low","optimal","stress","damaging")
salenv.vals1