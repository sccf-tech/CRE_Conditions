## 
## C43 canal water level
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
library(reshape2)
library(zoo)
library(lubridate)

## Paths
wd="C:/Julian_LaCie/_Github/CRE_Conditions"

paths=paste0(wd,c("/Plots/","/Export/","/Data/","/src"))
# Folder.Maker(paths);#One and done. Creates folders in working directory.
plot.path=paths[1]
export.path=paths[2]
data.path=paths[3]


# -------------------------------------------------------------------------
dates=date.fun(c("2011-05-01","2022-04-30"))

DBKeys=data.frame(SITE=c("S235_T","S77_T","S79_H"),
                  DBKEY=c("38259","65461","AN786"))
WL.dat=data.frame()
for(i in 2:nrow(DBKeys)){
  tmp=DBHYDRO_daily(dates[1],dates[2],DBKeys$DBKEY[i])
  tmp$DBKEY=as.character(DBKeys$DBKEY[i])
  WL.dat=rbind(WL.dat,tmp)
  print(i)
}

WL.dat.xtab=reshape2::dcast(WL.dat,Date~Station,value.var = "Data.Value",mean)
WL.dat.xtab[(WL.dat.xtab$S77_T<10.47)==T,]
WL.dat.xtab$S77_T=with(WL.dat.xtab,ifelse(S77_T<10.47,NA,S77_T))
WL.dat.xtab$S79_H=with(WL.dat.xtab,ifelse(S79_H>3.71,NA,S79_H))

plot(S77_T~Date,WL.dat.xtab,type="l")

plot(S79_H~Date,WL.dat.xtab,type="l")

# write.csv(WL.dat,paste0(export.path,"20220523_WLData.csv"),row.names=F)

plot(Data.Value~Date,WL.dat,type="n",las=1)
lines(S77_T~Date,WL.dat.xtab,col="red")
lines(S79_H~Date,WL.dat.xtab,col="blue")

summary(WL.dat.xtab)
