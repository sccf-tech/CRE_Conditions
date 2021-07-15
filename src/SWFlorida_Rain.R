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

paths=paste0(wd,c("/Plots/","/Export/","/Data/","/src"))
# Folder.Maker(paths);#One and done. Creates folders in working directory.
plot.path=paths[1]
export.path=paths[2]
data.path=paths[3]

#Current WY
CurWY=WY(Sys.time())
CurWY


# -------------------------------------------------------------------------
dates=c(date.fun("1979-05-01"),date.fun(as.Date(Sys.time())-duration(1,"days")))

wx.sites=data.frame(SITE=c('FPWX','SLEE_R','S79_R','CORK_R','CORK_R','CRKSWPS_R'),
                    DBKEY=c('FZ598','06081','16414','DO541','VN012','63883'))

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

# rf.dat$CY=as.numeric(format(rf.dat$Date,"%Y"))
# rf.dat$month=as.numeric(format(rf.dat$Date,"%m"))

rf.dat.da=ddply(rf.dat,"Date",summarise,mean.val=mean(Data.Value,na.rm=T),N.val=N.obs(Data.Value))
rf.dat.da$CY=as.numeric(format(rf.dat.da$Date,"%Y"))
rf.dat.da$month=as.numeric(format(rf.dat.da$Date,"%m"))

rf.dat.mon=ddply(rf.dat.da,c("month","CY"),summarise,TRF.in=sum(mean.val,na.rm=T))
rf.dat.mon$RF.cat=as.factor(findInterval(rf.dat.mon$TRF.in,c(0,1,2.5,5,7.5,10,12.5,100)))
rf.dat.mon=merge(rf.dat.mon,
                 data.frame(RF.cat=1:7,
                            RF.cat.txt=c("<1","1 - 2.5","2.5 - 5","5 - 7.5","7.5 - 10","10 - 12.5",">12.5"),
                            txt.cols=c(rep("black",3),rep("white",4))),
                 "RF.cat")
rf.dat.mon$month.f=month.abb[rf.dat.mon$month]
library(ggplot2)

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
