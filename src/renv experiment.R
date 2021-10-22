
library(renv)

## Libraries for Regional Conditions Report
library(AnalystHelper)
library(reshape)
library(plyr)
library(zoo)
library(rvest)
library(lubridate)
library(flextable)
library(magrittr)
library(grid)
library(RcppRoll)
library(downloadthis)
library(dataRetrieval)

library(jsonlite)
library(sp)
library(tmap)
library(httr)
library(raster)


install.packages(c("rmarkdown","knitr","reshape","plyr","zoo",
                   "rvest","lubridate","flextable","magrittr",
                   "grid","RcppRoll","downloadthis","dataRetrieval",
                   "RCurl","remotes","png","jsonlite","httr","sp",
                   "raster","tmap"))
remotes::install_github("SwampThingPaul/AnalystHelper")

renv::snapshot()

renv::deactivate()
