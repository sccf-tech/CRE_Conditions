

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
