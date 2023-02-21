#ETİKETLEME
#0 anomali 1 normal
library(caret)

reel<-ifelse(deneme$label=="anomali",0,1)
predict<-ifelse(st$cluster==0,0,1)

cf<-cbind(reel,predict)
as.table(cf)

xxx<-confusionMatrix(as.factor(predict),as.factor(reel))

xxx[["byClass"]][["F1"]]
xxx[["byClass"]][["Recall"]]
xxx[["byClass"]][["Precision"]]

