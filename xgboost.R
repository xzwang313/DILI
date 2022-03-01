
#### Based on 1574 genes ####
data<-read.csv("C:/Users/xzwang313/Desktop/Paper/XGBoost/training dataset_1574.csv",header=F)

data2<-as.matrix(data)

model1<-xgboost(data=data2[,1:1574],label=data2[,1575],max_depth = 6, eta = 1,nrounds = 15, objective="binary:logistic")

prediction1<-predict(model1,newdata=data2[,1:1574])
 
prediction11<-ifelse(prediction1>=0.5,1,0)

data3<-read.csv("Desktop/testing_dataset.csv",header=F)

data4<-as.matrix(data3)

prediction2<-predict(model1,newdata=data4[,1:1574])
prediction22<-ifelse(prediction2>=0.5,1,0)

maxdepth=c(2,4,6,8,10,12,14,16,18,20)
eta=c(0.1,0.5,0.8,1,1.1,1.2,1.4,1.5,1.8,2.0)
nrounds=c(2,5,10,15,20,100)
t<-c()

for(i in 1:length(maxdepth))
  for(j in 1:length(eta))
    for(k in 1:length(nrounds))
      {
      	model<-xgboost(data=data2[,1:1574],label=data2[,1575],max_depth = maxdepth[i], eta = eta[j],nrounds = nrounds[k], objective="binary:logistic")
        prediction<-predict(model,newdata=data4[,1:1574])
        prediction0<-ifelse(prediction>=0.5,1,0)
        temp<-prediction0-data4[,1575]
        temp2<-table(temp==0)[[2]]/sum(table(temp==0))
        
        t<-rbind(t,c(i,j,k,temp2))
         
  }
#select the optimal combination of parameter values, and use the parameters in model1

imp1<-xgb.importance(model=model1)
xgb.plot.importance(imp1,top_n=20, main="Variable Importance based on 1574 Genes")

######################################################################
######### based on  375 genes #########
data5<-read.csv("Desktop/training_dataset2.csv",header=F)

data6<-as.matrix(data5)

model2<-xgboost(data=data6[,1:375],label=data6[,376],max_depth = 6, eta = 1,nrounds = 15, objective="binary:logistic")
 
data7<-read.csv("Desktop/testing_dataset2.csv",header=F)

data8<-as.matrix(data7)

prediction3<-predict(model2,newdata=data8[,1:375])

confusionMatrix(data8[,376],prediction3,cutoff=0.5)

imp2<-xgb.importance(model=model2)
xgb.plot.importance(imp2,top_n=20,main="Variable Importance based on 375 Genes")





