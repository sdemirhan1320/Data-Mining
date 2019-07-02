install.packages("caTools")
library("caTools")
getwd()
setwd("C:/Users/Sinan/Desktop/dersler/ie 425/HW1")
heartdata<- read.csv(file = "heart.csv",header=TRUE)
library(tree)
require(caTools)

str(heartdata)#the output attribute is in the form of integer.
heartdata$target=as.factor(as.character(heartdata$target))
set.seed(425)

data1= sample.split(heartdata,SplitRatio = 0.70)
train_heart=subset(heartdata,data1==TRUE)
test_heart=subset(heartdata,data1==FALSE)

acc<-0
c<-0
s<-0
u<-0
for(i in 2:60) {
    for(j in 1:20){
      if(j>=(i/2)){break}
      pbest<-tree(target~.,data=train_heart,mincut = j, minsize = i)
      ptest<-predict(pbest,newdata=test_heart,type="class")
      t11<-table(test_heart$target,ptest)
      acl<-100*(1-sum(diag(t11))/sum(t11))
      print(acl)
      if(acl>acc){
        acc<-acl
        c<-j
        s<-i
      }
      
    
  }
}

bestmincut<-c
bestminsize<-s


pstree<-tree(target~.,data = train_heart,mincut=bestmincut,minsize=bestminsize)
pstree
plot(pstree)
text(pstree,cex=0.6)
summary(pstree)
pred_train<-predict(pstree,type = "class")
pred_train
table(train_heart$target,pred_train)
pred_test=predict(pstree,newdata=test_heart,type="class")
table(test_heart$target,pred_test)



install.packages("rpart")
library(rpart)


set.seed(425)
train=sample(1:303,195)
rptrain=heartdata[train,]
rptest=heartdata[-train,]


rp = rpart(target~., data=rptrain, control=rpart.control(minsplit=3,minbucket=1,cp=0.001))
prediction<-predict(rp,newdata=rptest,type = "class")
prediction
table(rptest$target,prediction)





