
Sys.setenv(PAGEDOWN_CHROME="C:/Users/Paul Julian/AppData/Local/Google/Chrome/Application/chrome.exe")
Sys.setenv(CHROMOTE_CHROME = "C:/Users/Paul Julian/AppData/Local/Google/Chrome/Application/chrome.exe")
library(xaringanBuilder)

wd="C:/Julian_LaCie/_GitHub/CRE_Conditions"

build_pdf(paste0(wd,"/slides/20220201_TBEPOpenSci.Rmd"),
          paste0(wd,"/slides/20220201_TBEPOpenSci.pdf"))
