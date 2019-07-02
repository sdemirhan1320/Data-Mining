getwd()
setwd("C:/Users/Sinan/Desktop/HM2")
credit<-read.csv("Credit.csv",header = TRUE)

str(credit)
credit[,c(8:11)] <- lapply(credit[,c(8:11)] , factor)
linear<-lm(Balance~.,data=credit)
summary(linear)

levels(credit$Gender)


plot(x = credit$Gender, y =credit$Balance,ylab="credit card balance")

credit[Gender==" Male"]$Balance
credit[Gender==" Female"]$Balance

lineargender<-lm(Balance~Gender,data=credit)
summary(lineargender)

linearethnicity<-lm(Balance~Ethnicity,data=credit)
summary(linearethnicity)

Student_credit<-credit[Student=="Yes"]
Non_Student_credit<-credit[Student=="No"]

lm_Y<-lm(Balance~Income,data=Student_credit)
summary(lm_Y)

lm_N<-lm(Balance~Income,data=Non_Student_credit)
summary(lm_N)


linear_age_limit<-lm(Balance~Age+Limit,data=credit)
summary(linear_age_limit)

linear_rating_limit<-lm(Balance~Rating+Limit,data=credit)
summary(linear_rating_limit)

cor(credit$Limit,credit$Rating)
cor(credit$Limit,credit$Age)



getwd()
setwd("C:/Users/Sinan/Desktop/dersler/ie 425")
bank<-read.csv("UniversalBank.csv",header = TRUE)


library(caret)
library(class)

set.seed(4250)
trainIndex <- createDataPartition(bank$Personal.Loan, p = .8,list = FALSE,times = 1)

train_bank <- bank[ trainIndex,]
test_bank  <- bank[-trainIndex,]


train_bank[["Personal.Loan"]] = factor(train_bank[["Personal.Loan"]])
test_bank[["Personal.Loan"]] = factor(test_bank[["Personal.Loan"]])
str(train_bank)



trctrl <- trainControl(method = "cv", 
                       number = 10
                       )
trctrl
fit <- train(Personal.Loan ~ .,
             method     = "knn",
             tuneGrid   = expand.grid(k = 1:11),
             trControl  = trctrl,
             preProcess = c("center","scale"), 
             metric     = "Accuracy",
             data       = train_bank)


fit


preds<-predict(fit,test_bank)
confusionMatrix(preds,test_bank$Personal.Loan)

trainX<-train_bank[,-10]
trainY<-train_bank[,10]
testX<-test_bank[,-10]
testY<-test_bank[,10]
knn.pred.bank<-knn(trainX,testX,trainY,k=5)
table(testY,knn.pred.bank)

preds<-predict(fit,test_bank)
confusionMatrix(preds,test_bank$Personal.Loan)







