#ST-DBSCAN ile Anomali Belirleme

library(leaflet)
library(dplyr)

kesitG<-kesit[kesit$Direction=="Limana Giriş",]
kesitC<-kesit[kesit$Direction=="Limandan Ayrılış",]

paldata <- colorFactor(
  palette = rainbow(100),
  domain = kesitG$MMSI
)

m<-leaflet(kesitG)  %>%
  addTiles() %>%
  addCircles(lat = ~LAT, lng = ~LON,weight = 2,radius = ~SOG, label =~paste(LAT,LON,NewCOG,MMSI), color=~paldata(MMSI) ) 

m

m %>%
  addPolygons(
    lng = c(-79.591750,-79.590133,-79.844233,-79.846283),lat = c(32.616750, 32.619167,32.740250 ,  32.738050),color = "blue",opacity = 0.2)

#Kanal sınırına yakın gemiler seçilmiş ve rotaları üzerinde oynanmıştır

gemi<-kesitG[kesitG$MMSI==563076200,]

m<-leaflet(gemi)  %>%
  addTiles() %>%
  addCircles(lat = ~LAT, lng = ~LON,weight = 5, label =~paste(LAT,LON,BaseDateTime,NewCOG,MMSI),color = "red" ) 

m

m %>%
  addPolygons(
    lng = c(-79.591750,-79.590133,-79.844233,-79.846283),lat = c(32.616750, 32.619167,32.740250 ,  32.738050),color = "blue",opacity = 0.2)


#17 adet anomali nokta var(Sınıra çok yakın-Sınırın Ortasında)
anmli<-kesitG[kesitG$MMSI==218629000,]#sağ kenara yakın hareket
anmli<-anmli[with(anmli, order(BaseDateTime)),]
anmli$LAT[8:35]<-anmli$LAT[8:35]+0.0003
anmli$LON[8:35]<-anmli$LON[8:35]+0.0003
anmli$LAT[26:33]<-anmli$LAT[26:33]-0.0003
anmli$LON[26:33]<-anmli$LON[26:33]-0.0003
#anmli$LAT<-anmli$LAT+0.0003
#anmli$LON<-anmli$LON+0.0003

anmli$label<-"normal"
anmli$label[c(8:25)]<-"anomali"
anmli$label[c(35,34)]<-"anomali"


#5 adet anomali nokta(sınırın üstünde yada değiyor)
anmli1<-kesitG[kesitG$MMSI==538005309,]
anmli1<-anmli1[with(anmli1, order(BaseDateTime)),]
anmli1$LAT[25:29]<-anmli1$LAT[25:29]+0.00055
anmli1$LON[25:29]<-anmli1$LON[25:29]+0.00055
anmli1$LAT[24:22]<-anmli1$LAT[24:22]+0.00065
anmli1$LON[24:22]<-anmli1$LON[24:22]+0.00065
anmli1$LAT[4:8]<-anmli1$LAT[4:8]+0.0003
anmli1$LON[4:8]<-anmli1$LON[4:8]+0.0003
anmli1$LAT[5]<-anmli1$LAT[5]-0.0003
anmli1$LON[5]<-anmli1$LON[5]-0.0003
anmli1$label<-"normal"
anmli1$label[c(6,7,25,26,27)]<-"anomali"

# 14 adet anomali nokta
anmli2<-kesitG[kesitG$MMSI==538007698,]
anmli2<-anmli2[with(anmli2, order(BaseDateTime)),]
anmli2$LAT<-anmli2$LAT+0.0003
anmli2$LON<-anmli2$LON+0.0003
anmli2$LAT[17:22]<-anmli2$LAT[17:22]-0.0004
anmli2$LON[17:22]<-anmli2$LON[17:22]-0.0004
anmli2$LAT[4:7]<-anmli2$LAT[4:7]-0.0004
anmli2$LON[4:7]<-anmli2$LON[4:7]-0.0004
anmli2$label<-"normal"
anmli2$label[c(1,2,3,8,9,10,11,12,13,14,15,16,17,23,24)]<-"anomali"

#13 adet anomali nokta
anmli3<-kesitG[kesitG$MMSI==303210000,]
anmli3<-anmli3[with(anmli3, order(BaseDateTime)),]
anmli3$LAT<-anmli3$LAT+0.0004
anmli3$LON<-anmli3$LON+0.0004
anmli3$LON[18:20]<-anmli3$LON[18:20]-0.0004 #sonradan yazıldı
anmli3$LAT[18:20]<-anmli3$LAT[18:20]-0.0004#sonradan yazıldı

anmli3$label<-"normal"
anmli3$label[c(1:17)]<-"anomali"


#4 adet anomali nokta
anmli4<-kesitG[kesitG$MMSI==636017972,]
anmli4<-anmli4[with(anmli4, order(BaseDateTime)),]
#anmli4$LAT[4:9]<-anmli4$LAT[4:9]+0.0003
#anmli4$LON[4:9]<-anmli4$LON[4:9]+0.0003
anmli4$LAT[6:9]<-anmli4$LAT[6:9]+0.0003
anmli4$LON[6:9]<-anmli4$LON[6:9]+0.0003
anmli4$LAT[10:23]<-anmli4$LAT[10:23]-0.0003
anmli4$LON[10:23]<-anmli4$LON[10:23]-0.0003
anmli4$label<-"normal"
anmli4$label[c(6:9)]<-"anomali"

#7adet anomali nokta var
anmli5<-kesitG[kesitG$MMSI==636017062,]
anmli5<-anmli5[with(anmli5, order(BaseDateTime)),]
anmli5$LAT<-anmli5$LAT-0.0004
anmli5$LON<-anmli5$LON-0.0004
anmli5$LAT[c(1:8)]<-anmli5$LAT[c(1:8)]+0.0004#bu satır yoktu
anmli5$LON[c(1:8)]<-anmli5$LON[c(1:8)]+0.0004#bu satır yoktu
anmli5$LAT[c(17:18)]<-anmli5$LAT[c(17:18)]+0.0004#bu satır yoktu
anmli5$LON[c(17:18)]<-anmli5$LON[c(17:18)]+0.0004#bu satır yoktu
anmli5$label<-"normal"
anmli5$label[c(9:16)]<-"anomali"


#8 adet anomali nokta var
anmli6<-kesitG[kesitG$MMSI==636018018,]
anmli6<-anmli6[with(anmli6, order(BaseDateTime)),]
#anmli6$LAT<-anmli6$LAT-0.0006
#anmli6$LON<-anmli6$LON-0.0006
anmli6$LAT[9:17]<-anmli6$LAT[9:17]-0.0006
anmli6$LON[9:17]<-anmli6$LON[9:17]-0.0006
anmli6$label<-"normal"
anmli6$label[c(9:17)]<-"anomali"

anmli7<-kesitG[kesitG$MMSI==563076200,]
anmli7<-anmli7[with(anmli7, order(BaseDateTime)),]
anmli7$LAT[1:7]<-anmli7$LAT[1:7]-0.0004
anmli7$LON[1:7]<-anmli7$LON[1:7]-0.0004
anmli7$label<-"normal"

rm(kesitGG)
kesitGG<-kesitG[!(kesitG$MMSI==636017062),]
kesitGG<-kesitGG[!(kesitGG$MMSI==636017972),]
kesitGG<-kesitGG[!(kesitGG$MMSI==303210000),]
kesitGG<-kesitGG[!(kesitGG$MMSI==538007698),]
kesitGG<-kesitGG[!(kesitGG$MMSI==538005309),]
kesitGG<-kesitGG[!(kesitGG$MMSI==218629000),]
kesitGG<-kesitGG[!(kesitGG$MMSI==636018018),]
kesitGG<-kesitGG[!(kesitGG$MMSI==563076200),]
kesitGG$label<-"normal"
rm(deneme)
deneme<-rbind(kesitGG,anmli,anmli1,anmli2,anmli3,anmli4,anmli5,anmli6,anmli7)
deneme<-deneme[with(deneme, order(BaseDateTime)),]

sum(deneme$label=="anomali")

m<-leaflet(anmli6)  %>%
  addTiles() %>%
  addCircles(lat = ~LAT, lng = ~LON,weight = 5, label =~paste(LAT,LON,BaseDateTime,NewCOG,MMSI) ) 

m

paldata <- colorFactor(
  palette = c("red","black"),
  domain = anmli7$label
)

m<-leaflet(anmli7)  %>%
  addTiles() %>%
  addCircles(lat = ~LAT, lng = ~LON,weight = 2, label =~paste(LAT,LON,NewCOG,MMSI), color=~paldata(label) ) 

m

m %>%
  addPolygons(
    lng = c(-79.591750,-79.590133,-79.844233,-79.846283),lat = c(32.616750, 32.619167,32.740250 ,  32.738050),color = "blue",opacity = 0.0)

#Anomali senaryosunun ilave edildiği yeni data frame deneme adını almıştır
paldata <- colorFactor(
  palette = c("red","black"),
  domain = deneme$label
)

m<-leaflet(deneme)  %>%
  addTiles() %>%
  addCircles(lat = ~LAT, lng = ~LON,weight = 3, label =~paste(LAT,LON,NewCOG,MMSI), color=~paldata(label))  %>%
  addLegend("topright", pal = paldata, values = ~label,
            title = "Etiket",
            labFormat = labelFormat(suffix = " " ),
            opacity = 2)

m

gemi<-kesitG[kesitG$MMSI==563076200,]

m<-leaflet(gemi)  %>%
  addTiles() %>%
  addCircles(lat = ~LAT, lng = ~LON,weight = 5, label =~paste(LAT,LON,BaseDateTime,NewCOG,MMSI),color = "red" ) 

m

#STDBSCAN
str(deneme)

x=deneme$LAT
y=deneme$LON
time=deneme$BaseDateTime

#MinPts Seçimi
log(length(x))

#Eps1 enlem boylam için parametre seçimi 
#par(mfrow=c(1,2))
X=cbind(x,y)
dbscan::kNNdistplot(X, k = 7)
abline(h = 0.00075, lty = 2)

#bir 
eps=0.0009
eps2=1285421
minpts=8
#iki
eps=0.00085
eps2=1285421
minpts=8
#üç 
eps=0.0008
eps2=1285421
minpts=8
#dört
eps=0.0009
eps2=1285421
minpts=9
#bes
eps=0.0009
eps2=1285421
minpts=7
#altı
eps=0.00085
eps2=1285421
minpts=9
#yedi
eps=0.00085
eps2=1285421
minpts=7
#sekiz
eps=0.00080
eps2=1285421
minpts=7
#dokuz
eps=0.00080
eps2=1285421
minpts=9
#on
eps=0.00090
eps2=1285421
minpts=10
#onbir
eps=0.00085
eps2=1285421
minpts=10
#oniki
eps=0.00080
eps2=1285421
minpts=10

##onüc
eps=0.0007
eps2=1285421
minpts=6
#ondört
eps=0.0007
eps2=1285421
minpts=7

##onbes
eps=0.0007
eps2=1285421
minpts=8

##onalti
eps=0.0007
eps2=1285421
minpts=9

##onyedi)
eps=0.0007
eps2=1285421
minpts=10


##xxx
eps=0.00075
eps2=1285421
minpts=7

eps=0.00075
eps2=1285421
minpts=8

eps=0.00075
eps2=1285421
minpts=9

eps=0.00075
eps2=1285421
minpts=10


eps=0.0007
eps2=1285421
minpts=6

eps=0.00075
eps2=1285421
minpts=6

eps=0.0008
eps2=1285421
minpts=6

eps=0.00085
eps2=1285421
minpts=6

eps=0.0009
eps2=1285421
minpts=6


eps=0.00078
eps2=1285421
minpts=6

st<-stdbscan(x,y,time,eps ,eps2 ,minpts)


deneme$cluster<-(st[["cluster"]])
dim(deneme[(deneme$cluster==0),])

pal <- colorFactor(
  palette = rainbow(7),
  domain =deneme$cluster
)

m<-leaflet(deneme)  %>%
  addTiles() %>%
  addCircles(lat = ~LAT, lng = ~LON,weight = 5, label =~paste(LAT,LON,SOG,NewCOG,MMSI), color=~pal(cluster) ) %>%
  addLegend("topright", pal = pal, values = ~cluster,
            title = "Küme Sayısı",
            labFormat = labelFormat(suffix = " " ),
            opacity = 2)
m


#-------------------- Küme İnceleme ------------------

deneme %>%
  group_by(cluster) %>%
  summarize(quantile(NewCOG))

kesitG %>%
  group_by(cluster) %>%
  summarize(mean(NewCOG))

kesitG %>%
  group_by(cluster) %>%
  summarize(sd(NewCOG))

m %>%
  addPolygons(
    lng = c(-79.591750,-79.590133,-79.844233,-79.846283),lat = c(32.616750, 32.619167,32.740250 ,  32.738050),color = "blue",opacity = 0.2)


