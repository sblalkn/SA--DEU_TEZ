library(dplyr)
library(readxl)
library(dplyr)
library(lubridate)
library(readxl)
library(chron)

#Günlük Verilerin Içe Aktarılması
data<-read.csv("data/AIS_2020_01_01.csv")
data1<-read.csv("data/AIS_2020_01_02.csv")
data2<-rdaead.csv("data/AIS_2020_01_03.csv")
data3<-read.csv("data/AIS_2020_01_04.csv")
data4<-read.csv("data/AIS_2020_01_05.csv")
data5<-read.csv("data/AIS_2020_01_06.csv")
data6<-read.csv("data/AIS_2020_01_07.csv")
data7<-read.csv("data/AIS_2020_01_08.csv")
data8<-read.csv("data/AIS_2020_01_09.csv")
data9<-read.csv("data/AIS_2020_01_10.csv")
data10<-read.csv("data/AIS_2020_01_11.csv")
data11<-read.csv("data/AIS_2020_01_12.csv")
data12<-read.csv("data/AIS_2020_01_13.csv")
data13<-read.csv("data/AIS_2020_01_14.csv")
data14<-read.csv("data/AIS_2020_01_15.csv")

# iki haftalık tüm gemi tiplerine ait AIS sinyalleri
TanKaD<-rbind(data,data1,data2,data3,data4,data5,data6,data7,data8,data9,data10,data11,data12,data13,data14)

summary(TanKaD)
str(TanKaD)

# Yanlızca Kargo ve Tanker Gemileri
TankaD<-TankaD %>%
  filter(VesselType>=70 & VesselType<=89) %>%
  select_all()

#Zaman düzenleme

TanKaD$BaseDateTime<-ymd_hms(TanKaD$BaseDateTime)#tarih saat formatı
TanKaD<-TanKaD[with(TanKaD, order(BaseDateTime)),]#zamana göre sıralandı.
TanKaD$Date<-substring(TanKaD$BaseDateTime,1,10)#Sadece tarih yazılı bir sütun oluşturuldu.
TanKaD$Time<-substring(TanKaD$BaseDateTime,12,19)#Sadece saat yazılı bir sütun oluşturuldu.
TanKaD$Time <- chron(times=TanKaD$Time)#format ayarlama
TanKaD$Date<-as.Date(TanKaD$Date)#format ayarlama

#COG düzeltmesi

TanKaD$NewCOG<-ifelse(TanKaD$COG<=0,TanKaD$COG+360,TanKaD$COG)




