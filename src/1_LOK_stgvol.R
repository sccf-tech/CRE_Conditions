## Title:      LOK Area to volume relationship
## Created by: Paul Julian (pjulian@evergladesfoundation.org)
## Created on: 02/13/2023

## BAD ## https://www.tidyverse.org/articles/2017/12/workflow-vs-script/
#Clears Everything...start fresh.
rm(list=ls(all=T));cat("\014");dev.off()

# Libraries
# devtools::install_github("SwampThingPaul/AnalystHelper")
library(AnalystHelper);
library(plyr)
library(reshape2)
library(openxlsx)

library(rgdal)
library(rgeos)
library(raster)

# Area/Volume -------------------------------------------------------------
# load("C:/Julian_LaCie/_Github/LakeO_Sediment/Export/LOK_stgvol_ft_mod.Rdata")

summary(stg.vol.ft)

stg1=predict(stg.vol.ft,data.frame(z=15.93),interval="confidence")
stg2=predict(stg.vol.ft,data.frame(z=13.5),interval="confidence")

(stg1[3]-stg2[1])/1000
