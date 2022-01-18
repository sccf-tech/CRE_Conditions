

library(geojsonio)
test=geojson_read("https://opendata.arcgis.com/datasets/dfd5e2914bd24944933650671cf86aa5_18.geojson",what="sp")

range(test@data$SAMPLE_DATE)

library(sp)
plot(test)


library(httr)
library(jsonlite)
path="https://atoll.floridamarine.org/arcgis/rest/services/FWC_GIS/OpenData_HAB/MapServer/18/query?outFields=*&where=1%3D1"


request<-GET(url=path)
request$status_code

response<-content(request,as="text",encoding="UTF-8")

df<-fromJSON(response,flatten=T)



test2=sf::read_sf("https://myfwc.com/media/26611/ge-04-23-2021.kmz")


## Experiment alt source
# fwc.pro=CRS("+init=epsg:3857")
# HABext2.pro=spTransform(HABext.poly,fwc.pro)
# HABext2.pro=extent(HABext2.pro)
# No need for different projection to query Map Server
path <- 'https://atoll.floridamarine.org/arcgis/rest/services/FWC_GIS/OpenData_HAB/MapServer/18/query'

request <- GET(
  url = path,
  query= list(       
    where= paste("LATITUDE <",round(HABext2[4],1),"AND LATITUDE >",round(HABext2[3],1),"AND LONGITUDE >",round(HABext2[1],1),"AND LONGITUDE <",round(HABext2[2],1)),
    f = 'pjson'
  )
)

response <- content(request, as = "text", encoding = "UTF-8")
results <- jsonlite::fromJSON(response,flatten=T)
