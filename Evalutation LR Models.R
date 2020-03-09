##### INSTALLING PACKAGE ######
install.packages('caret') #Classification And REgression Training
require('caret')

##### BUILDING MODEL ######

mydata <- read.csv('https://stats.idre.ucla.edu/stat/data/binary.csv')

#devide data into 2 parts: 80% train and 20% test
split.data <- createDataPartition(mydata$admit, p=0.99, list = FALSE )
train<- mydata[ split.data, ]
test <- mydata[ -split.data, ]

### Training Data Set
#Using the training dataset, we will apply logistic regression method
#We model Admit as a function of three predictors: admit = gre/gpa/rank

train$rank <- as.factor(train$rank) 
#class(train$rank.factor)
#class(train$rank)

#model fit 1
logit <- glm(admit ~ gre + gpa + rank,data=train, family="binomial")

summary(logit)


#### Validating Predicted Values

#Classification Rate: we examinate the accuracy of Predicted Values using test data
test$rank <- as.factor(test$rank)
pred = predict(logit,test)

#create a table and checl if they match 
accuracy <- table(pred, test[,"admit"])
sum(diag(accuracy))/sum(accuracy)





############################# OTHERS

#### Whether our model is  good fit?

## confidence intervals using profiled log-likelihood
confint(logit)
## CIs using standard errors
confint.default(logit)

##
# model fit 2, reducing predictors
#reduced.logit <- glm(admit ~ gpa + rank,data=train, family="binomial")
#summary(reduced.logit)
#pred = predict(reduced.logit,test)
