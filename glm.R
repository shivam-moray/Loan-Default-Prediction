data<-read.csv("C:/Users/hp/Desktop/Sem2/CA683 - Data Analytics and Data Mining/Assignment2_Documents/lending-club-loan-data/loan.csv")
install.packages("gmodels")
install.packages("lubridate")
install.packages("plyr")
install.packages("ggplot2")
install.packages("caTools")
install.packages("e1071")
install.packages("ROCR")
install.packages("caret")
install.packages("ROSE")
library(tidyverse)
library(gmodels)
library(lubridate)
library(plyr)
library(ggplot2)
library(caTools)
library(e1071)
library(ROCR)
library(caret)
library(ROSE)
dim(data)
classes(data)
class(data)
names(data)
head(data,n=20)
tail(data, n=20)
str(data)

loan <- data %>% select(loan_status, loan_amnt, int_rate, grade, emp_length, home_ownership, annual_inc, term)
data<-read.csv("C:/Users/hp/Desktop/Sem2/CA683 - Data Analytics and Data Mining/Assignment2_Documents/lending-club-loan-data/loan.csv")
library(tidyverse)

newdata <- data %>% select(loan_status, grade, sub_grade, open_acc,pub_rec, dti, delinq_2yrs,inq_last_6mths, emp_length, annual_inc, home_ownership,  purpose, addr_state,loan_amnt,int_rate, installment, issue_d, revol_bal, revol_util)
dim(newdata)
newdata <- newdata[!apply(newdata == "", 1, all),]

#Missing Values
summary(newdata$emp_length)
options(scipen = 50)
plot(newdata$emp_length, col="red")
newdata$emp_cat<-rep(NA, length(newdata$emp_length))
newdata$emp_cat[which(newdata$emp_length == "< 1 year")] <- "0-1"
newdata$emp_cat[which(newdata$emp_length == "1 year" | newdata$emp_length == "2 years" | newdata$emp_length == "3 years")] <- "0-1"
newdata$emp_cat[which(newdata$emp_length == "4 year" | newdata$emp_length == "5 years" | newdata$emp_length == "6 years")] <- "4-6"
newdata$emp_cat[which(newdata$emp_length == "7 year" | newdata$emp_length == "8 years" | newdata$emp_length == "9 years")] <- "7-9"
newdata$emp_cat[which(newdata$emp_length == "< 10 year")] <- "10+"
newdata$emp_cat[which(newdata$emp_length == "n/a")] <- "missing"
newdata$emp_cat <- as.factor(newdata$emp_cat)
plot(newdata$emp_cat, col="blue", main="Histogram of factorial variable emp_cat")
summary(newdata$emp_cat)
newdata$emp_length <- NULL


#Preparing data for analysis

#int_rate
class(newdata$int_rate)
newdata$int_rate <- as.numeric(sub("%","",newdata$int_rate))
newdata$int_rate<-newdata$int_rate/100
is.numeric(newdata$int_rate)
anyNA(newdata$int_rate)


#revol_util
class(newdata$revol_util)
newdata$revol_util <- as.numeric(sub("%","",newdata$revol_util))
newdata$revol_util<-newdata$revol_util/100
is.numeric(newdata$revol_util)
anyNA(newdata$revol_util)
index.NA<-which(is.na(newdata$revol_util))
newdata$revol_util[index.NA] <- median(newdata$revol_util, na.rm = TRUE)
anyNA(newdata$revol_util)

#revol_bal 
class(newdata$revol_bal)
newdata$revol_bal <- as.character(newdata$revol_bal)
newdata$revol_bal <- as.numeric(newdata$revol_bal)
anyNA(newdata$revol_bal)

#installment
class(newdata$installment)
newdata$installment <- as.character(newdata$installment)
newdata$installment <- as.numeric(newdata$installment)
is.numeric(newdata$installment)
anyNA(newdata$installment)

#loan amount
class(newdata$loan_amnt)
newdata$loan_amnt <- as.character(newdata$loan_amnt)
newdata$loan_amnt <- as.numeric(newdata$loan_amnt) 
is.numeric(newdata$loan_amnt)
anyNA(newdata$loan_amnt)

#annualincome
class(newdata$annual_inc)
newdata$annual_inc <- as.character(newdata$annual_inc)
newdata$annual_inc <- as.numeric(newdata$annual_inc)
is.numeric(newdata$annual_inc)
anyNA(newdata$annual_inc)
index.NA <- which(is.na(newdata$annual_inc))
newdata$annual_inc[index.NA] <- median(newdata$annual_inc, na.rm = TRUE)
anyNA(newdata$annual_inc)

#loan_status
class(newdata$loan_status)
newdata$loan_status <- as.character(newdata$loan_status)
is.character(newdata$loan_status)
arg <- newdata$loan_status=="Fully Paid" | newdata$loan_status=="Charged Off"
newdata <- subset(newdata, arg==TRUE)


#loan_status 0-chargedoff , 1- fullypaid
newdata$loan_status <- ifelse(newdata$loan_status=="Fully Paid",1,0)
newdata$loan_status <- as.integer(newdata$loan_status)
is.integer(newdata$loan_status)
anyNA(newdata$loan_status)

#dti
class(newdata$dti)
newdata$dti <- as.character(newdata$dti) 
newdata$dti <- as.numeric(newdata$dti)
is.numeric(newdata$dti)
anyNA(newdata$dti)


#openacc
class(newdata$open_acc)
newdata$open_acc <- as.character(newdata$open_acc)
newdata$open_acc <- as.numeric(newdata$open_acc)
is.numeric(newdata$open_acc)
anyNA(newdata$open_acc) 

#pub_rec
class(newdata$pub_rec)
newdata$pub_rec <- as.character(newdata$pub_rec)
newdata$pub_rec <- as.numeric(newdata$pub_rec)
is.numeric(newdata$pub_rec)
anyNA(newdata$pub_rec)

#delinq_2yrs
class(newdata$delinq_2yrs)
newdata$delinq_2yrs <- as.character(newdata$delinq_2yrs)
newdata$delinq_2yrs <- as.numeric(newdata$delinq_2yrs)
is.numeric(newdata$delinq_2yrs)
anyNA(newdata$delinq_2yrs)

#inq_last_6mths
class(newdata$inq_last_6mths)
newdata$inq_last_6mths <- as.character(newdata$inq_last_6mths)
newdata$inq_last_6mths <- as.numeric(newdata$inq_last_6mths)
is.numeric(newdata$inq_last_6mths)
anyNA(newdata$inq_last_6mths)
index.NA <- which(is.na(newdata$inq_last_6mths))
newdata$inq_last_6mths[index.NA] <- median(newdata$inq_last_6mths, na.rm = TRUE)
anyNA(newdata$inq_last_6mths)

str(newdata)


#EDA

summary(newdata$int_rate)
plot(newdata$int_rate, col="blue", main="Distribution of Interest rate")
int_rate<-newdata$int_rate
hist(int_rate, main="Distribution of Intrest rate", xlab="int_rate", col="orange")
table(newdata$loan_status, newdata$grade)
ggplot(newdata, aes(x = int_rate))+ geom_histogram(aes(fill = grade)) + facet_wrap(~loan_status, ncol = 1)
table(newdata$loan_status, newdata$l)


hist(newdata$int_rate, col = "orange", main = "Distribution of Intrest rate", xlab = "Interest rate")
summary(newdata$int_rate)


newdata$loan_status <- factor(newdata$loan_status)

plot1 <- ggplot(newdata,aes(x=grade, y=((..count..)/sum(..count..))*100))
plot1 <- plot1 + geom_histogram(aes(fill=loan_status), color="black", stat = "count", alpha=0.6)
plot1 <- plot1 + theme_light()

plot1 <- plot1 + scale_fill_manual("Loan Status",values = c("red", "green")) + labs(y="Percent", x="Loan Grades from A (best) to G (poor)")
plot1 <- plot1 + ggtitle("Distribution of Loans By Grading Scores and Loan Status")
plot1

CrossTable(newdata$grade, newdata$loan_status,prop.r = TRUE, prop.c = FALSE, prop.t = FALSE,prop.chisq = FALSE )
arg <- newdata$purpose == "credit_card" | newdata$purpose == "debt_consolidation" |newdata$purpose == "home_improvement" | newdata$purpose == "major_purchase" | newdata$purpose == "other"
j <- subset(newdata, arg==TRUE)

plot2 <- ggplot(j,aes(x=purpose, y=((..count..)/sum(..count..))*100))
plot2 <- plot2 + geom_bar(aes(fill=loan_status), position = "dodge", stat = "count")
plot2 <- plot2 + theme_bw()

plot2 <- plot2 + scale_fill_manual("Loan Status",values = c("red", "green")) + labs(y="Percent", x="Loan Purpose")
plot2 <- plot2 + ggtitle("Distribution of Loans By Purpose")
plot2

#DecisionTrees

index = createDataPartition(y = newdata$loan_status, p = 0.8)[[1]]
newdata.test<-newdata[-index,]
newdata.train<-newdata[index,]
index.rpart.0<-rpart(loan_status~.,data=newdata.train)
install.packages("rpart.plot")
library(rpart.plot)
index.rpart.0<-rpart(loan_status~.,data=newdata.train)
index.rpart.1<-rpart(loan_status~.,data=newdata.train, control=rpart.control(minsplit=10, minbucket = 3, cp=0.0006))
fancyRpartPlot(index.rpart.1)
library(rpart)
rpart.plot(index.rpart.1)


predictions.1 <- (predict(index.rpart.1, newdata.test))
confusionMatrix(predictions.1, newdata.test$loan_status)

CrossTable(newdata$purpose, newdata$loan_status,prop.r = TRUE, prop.c = FALSE, prop.t = FALSE,prop.chisq = FALSE )

plot3 <- ggplot(newdata, aes(x=int_rate, y=sub_grade)) + geom_point(aes(color=loan_status, alpha=0.4))
plot3 <- plot3 + theme_bw() + scale_fill_manual("Loan Status", values = c("red", "green")) +labs(y="Sub Grades", x="Interest Rates")
plot3


arg <- newdata$int_rate==0.06 & newdata$sub_grade=="G1"
newdata <- subset(newdata, arg==FALSE)

arg <- newdata$int_rate==0.06 & newdata$sub_grade=="F5"
newdata <- subset(newdata, arg==FALSE)

arg <- newdata$int_rate==0.06 & newdata$sub_grade=="F5"
newdata <- subset(newdata, arg==FALSE)


summary(newdata$annual_inc)
plot(newdata$annual_inc, ylab = "Annual Income")

index.outliers <- which(newdata$annual_inc > 1000000)
newdata <- newdata[-index.outliers,]
hist(newdata$annual_inc, col="orange", xlab = "Annual Income", main = "Histogram of Annual Income")


summary(newdata$dti)
plot(newdata$dti)
outliers_upperlimit <- quantile(as.numeric(newdata)$dti, 0.75,na.rm=TRUE) + 1.5 * IQR(newdata$dti)
index.outliers.dti <- which(newdata$dti > outliers_upperlimit | newdata$dti < 0 )

#Multicollinearity

cor(newdata[, sapply(newdata, class) != "factor"]) 

loan.model <- subset(newdata, select = c(1,2,4:11,13,14,17:19)) 
anyNA(loan.model)
index.NA <- which(is.na(newdata$loan.model))
newdata$loan.model[index.NA] <- median(newdata$loan.model, na.rm = TRUE)
anyNA(loan.model)
set.seed(123)

sample <- sample.split(loan.model$loan_status, 0.7)
library(caTools)
train.data <- subset(loan.model, sample==TRUE)
test.data <- subset(loan.model, sample==FALSE)
logistic.regressor <- glm(loan_status ~ ., family = binomial(link = 'logit'), data = train.data)
summary(logistic.regressor)

prob_pred <- predict(logistic.regressor, newdata = test.data, type = "response")
summary(prob_pred)

pred_cut_off <- ifelse(prob_pred > 0.5, 1,0)
t1<-table(test.data$loan_status,pred_cut_off )
pred <- prediction(pred_cut_off,test.data$loan_status)
library(ROCR)
perf <- performance(pred, "tpr", "fpr")
perf1 <- performance(pred, "auc")
print(perf1@y.values[[1]])
roc(test.data$loan_status, pred_cut_off, percent=T, boot.n=1000, ci.alpha=0.9, stratified=T, plot=TRUE, grid=TRUE, show.thres=TRUE, legacy.axes = TRUE, reuse.auc = TRUE, print.auc = TRUE, print.thres.col = "blue", ci=TRUE, ci.type="bars", print.thres.cex = 0.7, col = 'red', main = paste("The ROC-curve for Model with cut-off=0.5","(N = ",nrow(test),")") )
text(0.6,0.2,paste("AUC=0.51"))
confusionMatrix(t1 )
library(caret)

pred_cut_off <- ifelse(prob_pred > 0.8, 1,0)
t2<-table(test.data$loan_status,pred_cut_off )
pred <- prediction(pred_cut_off,test.data$loan_status)
perf <- performance(pred, "tpr", "fpr")
perf1 <- performance(pred, "auc")
print(perf1@y.values[[1]])
roc(test.data$loan_status, pred_cut_off, percent=T, boot.n=1000, ci.alpha=0.9, stratified=T, plot=TRUE, grid=TRUE, show.thres=TRUE, legacy.axes = TRUE, reuse.auc = TRUE, print.auc = TRUE, print.thres.col = "blue", ci=TRUE, ci.type="bars", print.thres.cex = 0.7, col = 'red', main = paste("The ROC-curve for Model with cut-off=0.8","(N = ",nrow(test),")") )
text(0.6,0.2,paste("AUC=0.646"))
confusionMatrix(t2)
options(scipen=20)
barchart(train.data$loan_status, main="Proportion of Fully Paid and Charged Off Loans (Training Set)", xlab="Number of Loans")


#RoseMethod

balanced.data <- ROSE(loan_status ~ ., data = train.data, seed = 1)$data
library(ROSE)
table(balanced.data$loan_status)

rose.regressor <- glm(loan_status ~ ., family = binomial(link = 'logit'), data = balanced.data)
summary(rose.regressor)

prob_pred_rose <- predict(rose.regressor, newdata = test.data, type="response")
hist(prob_pred_rose)

roc(test.data$loan_status, prob_pred_rose, percent=T, boot.n=1000, ci.alpha=0.9, stratified=T, plot=TRUE, grid=TRUE, show.thres=TRUE, legacy.axes = TRUE, reuse.auc = TRUE, print.auc = TRUE, print.thres.col = "blue", ci=TRUE, ci.type="bars", print.thres.cex = 0.7, col = 'red', main = paste("The ROC-curve for Model for Improved Model","(N = ",nrow(test),")") )
text(0.6,0.1,paste("AUC=0.70"))
