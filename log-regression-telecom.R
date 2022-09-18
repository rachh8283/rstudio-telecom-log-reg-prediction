Sys.time()

getwd()
telco <- read.csv("MIS470Telcocustomer.csv", stringsAsFactors = TRUE)
str(telco)

# uncomment install.packages("caret") if already installed
install.packages("caret")
library(caret)

# Prep data. Partition data into training and testing partitions
intrain<- createDataPartition(telco$Churn,p=0.7,list=FALSE)
set.seed(2017)
training<- telco[intrain,]
testing<- telco[-intrain,]

# Create a Logistic Model for Churn with training data
LogModel <-glm(Churn ~ .,family=binomial(link="logit"),data=training)
summary(LogModel)

# Evaluate model with testing data
testing$Churn <- as.character(testing$Churn)
testing$Churn[testing$Churn=="No"] <- "0"
testing$Churn[testing$Churn=="Yes"] <- "1"
fitted.results <- predict(LogModel,newdata=testing,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != testing$Churn)
misClasificError

print(paste('Logistic Regression Accuracy', 1-misClasificError))

table(fitted.results, testing$Churn)

# Create and evaluate the performance measures of 3 models using 3 predictors
LogModel.CPerf <- glm(Churn ~ Contract, family=binomial(link="logit"), data=training)
fitted.results.c <- predict(LogModel.CPerf,newdata=testing,type='response')
fitted.results.c <- ifelse(fitted.results.c > 0.5,1,0)
misClasificError.c <- mean(fitted.results.c != testing$Churn)
print(paste('Logistic Regression Accuracy for Contract, Paperless Billing, and Tenure Variables', 1-misClasificError.c))
table(fitted.results.c, testing$Churn)
summary(LogModel.CPerf)

LogModel.PPerf <- glm(Churn ~ PaperlessBilling, family=binomial(link="logit"), data=training)
fitted.results.p <- predict(LogModel.PPerf,newdata=testing,type='response')
fitted.results.p <- ifelse(fitted.results.p > 0.5,1,0)
misClasificError.p <- mean(fitted.results.p != testing$Churn)
print(paste('Logistic Regression Accuracy for Contract, Paperless Billing, and Tenure Variables', 1-misClasificError.p))
table(fitted.results.p, testing$Churn)
summary(LogModel.PPerf)

LogModel.TPerf <- glm(Churn ~ tenure_group, family=binomial(link="logit"), data=training)
fitted.results.t <- predict(LogModel.TPerf,newdata=testing,type='response')
fitted.results.t <- ifelse(fitted.results.t > 0.5,1,0)
misClasificError.t <- mean(fitted.results.t != testing$Churn)
print(paste('Logistic Regression Accuracy for Contract, Paperless Billing, and Tenure Variables', 1-misClasificError.t))
table(fitted.results.t, testing$Churn)
summary(LogModel.TPerf)


LogModel.Perf <- glm(Churn ~ Contract + PaperlessBilling + tenure_group, family=binomial(link="logit"), data=training)
fitted.results.cpt <- predict(LogModel.Perf,newdata=testing,type='response')
fitted.results.cpt <- ifelse(fitted.results.cpt > 0.5,1,0)
misClasificError.cpt <- mean(fitted.results.cpt != testing$Churn)
print(paste('Logistic Regression Accuracy for Contract, Paperless Billing, and Tenure Variables', 1-misClasificError.cpt))
table(fitted.results.cpt, testing$Churn)
summary(LogModel.Perf)
