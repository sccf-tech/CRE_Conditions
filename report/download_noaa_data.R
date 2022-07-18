
## Script to download geotiff images 
## Lake Okeechobee Cyano-HAB

data.path="./report/RSdata"

##
## Get list of files on NOAA site
noaa.HAB=readLines("https://products.coastalscience.noaa.gov/habs_explorer/index.php?path=ajZiOVoxaHZNdE5nNytEb3RZdU5iYjNnK3AvTWRrYmNWbXU0K0YvMlA1UlBtTWZlRFV3R1RicVRYb2pxeVJBUA==")

vals=grep("<section class='onecol habonecol'><a href='https://products.coastalscience.noaa.gov/habs_explorer/index.ph",noaa.HAB)

noaa.image.inventory=data.frame()
# for(i in 1:6){
for(i in 1:length(vals)){
  tmp=noaa.HAB[vals[i]]
  tmp2=strsplit(tmp,"<a|</a>")[[1]]
  tmp3=strsplit(tmp2[2],"href=|title=|>")
  tmp4=strsplit(tmp2[3],"</section>")
  
  dat=data.frame(fileadd=sapply(tmp3,"[",2),filename=sapply(tmp4,"[",1))
  
  fname.vals=strsplit(dat$filename,"_")
  if(trimws(substr(sapply(fname.vals,"[",1),1,9))!="sentinel"){next}else{
    dat$data.product=sapply(fname.vals,"[",length(fname.vals[[1]]))
    
    date.vals=strsplit(sapply(fname.vals,"[",1),"\\.")
    yr.val=as.numeric(substr(sapply(date.vals,"[",2),1,4))
    month.val=as.numeric(substr(sapply(date.vals,"[",3),1,2))
    day.val=as.numeric(substr(sapply(date.vals,"[",3),3,4))
    dat$date=as.Date(paste(yr.val,month.val,day.val,sep="-"))
    
    noaa.image.inventory=rbind(dat,noaa.image.inventory)
  }
}


# noaa.HAB.image=subset(noaa.image.inventory,data.product=="3.CIcyano.LakeOkee.tif"&date==max(noaa.image.inventory$date))

noaa.HAB.image=subset(noaa.image.inventory,data.product=="3.CIcyano.LakeOkee.tif")
# noaa.HAB.image$filename
noaa.HAB.image$fnames=with(noaa.HAB.image,paste0(format(date,"%Y%m%d"),"_LOK_CIcyano.tif"))
# fnames=with(noaa.HAB.image,paste0(format(date,"%Y%m%d"),"_LOK_CIcyano.tif"))
fnames=noaa.HAB.image$fnames

# paste0(data.path,"sentinel_2022/", fnames)
new.dat=fnames[fnames%in%list.files(data.path)==F]

# adjust timeout time
options(timeout = max(800, getOption("timeout")))

noaa.HAB.image2=subset(noaa.HAB.image,fnames%in%new.dat)
if(nrow(noaa.HAB.image2)!=0){
  for(i in 1:nrow(noaa.HAB.image2)){
    download.file(noquote(gsub("'", '', noaa.HAB.image2$fileadd[i], fixed=TRUE)),paste(data.path, noaa.HAB.image2$fnames[i],sep="/"),mode="wb",method="wininet")
    # print(i)
  }
}
