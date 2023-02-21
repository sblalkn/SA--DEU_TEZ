library(dplyr)
library(leaflet)
library(lubridate)
library(chron)
library(leaflet.extras)

#LİMAN FİLTRELEME

#Charleston Limanı ENLEM BOYLAM 32.79487° / -79.914565°

charleston<-filter(TanKaD,LAT>=32.24 & LAT<=32.87)
charleston<-filter(charleston, LON<=-78.9 & LON>=-80)


paldata <- colorFactor(
  palette = rainbow(1000),
  domain = charleston$VesselName
)


m<-leaflet(charleston)  %>%
  addTiles() %>%
  addCircles(lat = ~LAT, lng = ~LON,weight = 2,radius = ~SOG, label =~paste(LAT,LON), color=~paldata(VesselName) ) %>%
  addLegend("topright", pal = paldata, values = ~VesselName,
            title = "Gemi İsimleri",
            labFormat = labelFormat(suffix = " " ),
            opacity = 2)
m

# ip gibi olan alanı kısıtla, bu alanda bulunan her gemi charleston limanına uğramıştır.

m<-leaflet(charleston)  %>%
  addTiles() %>%
  addCircles(lat = ~LAT, lng = ~LON,weight = 2,radius = ~SOG, label =~paste(LAT,LON,NewCOG),color = "red" )
m

m %>%
  addPolylines(c(-79.844333,-79.846233), c(32.740300,32.738067),
               fillColor = "transparent"
  ) %>%
  addPolylines(c(-79.68797,-79.6892), c(32.66623 , 32.66287),
               fillColor = "transparent"
  )


## Kesit Alınan Bölge

kesit<-charleston[charleston$LAT>=32.66287 & charleston$LAT<=32.740300,]
kesit<-kesit[kesit$LON>=-79.844333 & kesit$LON<=-79.6892,]

str(kesit)

paldata <- colorFactor(
  palette = rainbow(20),
  domain = kesit$Date
)

m<-leaflet(kesit)  %>%
  addTiles() %>%
  addCircles(lat = ~LAT, lng = ~LON,weight = 2,radius = ~SOG, label =~paste(LAT,LON,NewCOG), color=~paldata(Date) ) %>%
  addLegend("topright", pal = paldata, values = ~Date,
            title = "Tarih",
            labFormat = labelFormat(suffix = " " ),
            opacity = 2)
m

#TEK Renk
m<-leaflet(kesit)  %>%
  addTiles() %>%
  addCircles(lat = ~LAT, lng = ~LON,weight = 2,radius = ~SOG, label =~paste(LAT,LON,NewCOG), color="red")

m

#Kesit'in Eşik Değer için İncelenmesi

summary(kesit)
hist(kesit$NewCOG,breaks = seq(from=100, to=300, by=2),xlab = "COG Açıları",ylab = "Frekans Değerleri",main = "COG Açılarının Dağılımı")
hist(kesit$NewCOG,breaks=seq(min(kesit$NewCOG), max(kesit$NewCOG), length.out =100))
hist(kesit$Heading,breaks=seq(from=100, to=350, by=2),xlab = "Heading Açıları",ylab = "Frekans Değerleri",main = "Heading Açılarının Dağılımı")
summary(kesit$NewCOG)
summary(kesit$Heading)

#Veriyi Yön olarak etiketleme
kesit$Direction<-ifelse(kesit$NewCOG>=190,"Limana Giriş","Limandan Ayrılış")

kesit %>%
  group_by(Direction) %>%
  summarize(quantile(NewCOG))

#Kesit Alınan Bölge Direction'a Göre

paldata <- colorFactor(
  palette = c("red","darkblue"),
  domain = kesit$Direction
)


m<-leaflet(kesit)  %>%
  addTiles() %>%
  addCircles(lat = ~LAT, lng = ~LON,weight = 2,radius = ~SOG, label =~paste(LAT,LON,NewCOG), color=~paldata(Direction) )  %>%
  addLegend("topright", pal = paldata, values = ~Direction,
            title = " ",
            labFormat = labelFormat(suffix = " " ),
            opacity = 2)
m


kesit %>%
  group_by(Direction) %>%
  summarize(min=min(NewCOG),max=max(NewCOG),n=n())

kesit %>%
  group_by(Direction) %>%
  summarize(min=min(Heading),max=max(Heading),n=n())

#Giris ve cikis olarak iki dateframe

kesitG<-kesit[kesit$Direction=="Limana Giriş",]
kesitC<-kesit[kesit$Direction=="Limandan Ayrılış",]

m<-leaflet(c(kesitG,kesitC))  %>%
  addTiles() %>%
  addCircleMarkers(lng = kesitG$LON, lat = kesitG$LAT,label= ~paste(NewCOG,MMSI,BaseDateTime), radius=4,color = 'red', group = 'Limana Giriş(Kırmızı)',weight = 0.2) %>%
  addCircleMarkers(lng = kesitC$LON, lat = kesitC$LAT,label= ~paste(NewCOG,MMSI,BaseDateTime), radius=4, color = 'blue', group = 'Limandan Ayrılış(Mavi)',weight = 0.2) %>%
  addLayersControl(
    overlayGroups = c('Limana Giriş(Kırmızı)','Limandan Ayrılış(Mavi)'),
    options = layersControlOptions(collapsed = F))
m

m %>%
  addPolygons(
    lng = c(-79.591750,-79.590133,-79.844233,-79.846283),lat = c(32.616750, 32.619167,32.740250 ,  32.738050),color = "red",opacity = 0.1)


#KesitG data frame üzerinde algoritma uygulanacaktır
paldata <- colorFactor(
  palette = rainbow(100),
  domain = kesitG$MMSI
)

m<-leaflet(kesitG)  %>%
  addTiles() %>%
  addCircles(lat = ~LAT, lng = ~LON,weight = 2,radius = ~SOG, label =~paste(LAT,LON,NewCOG,MMSI), color=~paldata(MMSI) ) 

m