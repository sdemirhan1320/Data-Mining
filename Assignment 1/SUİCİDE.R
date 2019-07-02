getwd()
setwd("C:/Users/Sinan/Desktop/dersler/ie 425/HW1")
suicide<- read.csv(file = "suicide-rate.csv",header=TRUE)
install.packages("tree")
library(tree)
str(suicide)

set.seed(492)
train=sample(1:27820,19474)
suitrain=suicide[train,]
suitest=suicide[-train,]


suitrain$country<- NULL
suitrain$country.year<- NULL
suitrain$gdp_for_year.... <- NULL
suitest$country<- NULL
suitest$country.year<- NULL
suitest$gdp_for_year.... <- NULL
suicide$country<- NULL
suicide$country.year<- NULL
suicide$gdp_for_year.... <- NULL



error<-0
thebesterror<-10^10

for (i in 1:150) {
  
    traintree<-tree (suicides.100k.pop~ .,data=suitrain,mincut=i)
    yhat = predict(traintree, newdata=suitest)
    sui_test = suicide[-train,"suicides.100k.pop"]
    error<-(mean((yhat-sui_test)^2))^0.5
    if(error<thebesterror){
      thebesterror<-error
      the_best<-error
      the_best_mincut<-i
    
  }
}

the_best
the_best_mincut

the_last_tree<-tree (suicides.100k.pop~ .,data=suitrain, mincut=the_best_mincut)
plot(the_last_tree)
text(the_last_tree,cex=0.6)
summary(the_last_tree)




install.packages('caret', dependencies = TRUE)
library(caret)
set.seed(492)

set.seed(492)
suicidets<-tree(suicides.100k.pop~ .,data=suicide, mincut=1)
cvpst=cv.tree(suicidets,K=10)
cvpst$size
cvpst$dev
plot(cvpst,pch=21,bg=8,type="p",cex=1.5)
