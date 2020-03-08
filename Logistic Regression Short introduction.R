####### TRAINING ######

#Data acquisition
train.data <- read.csv('https://stats.idre.ucla.edu/stat/data/binary.csv')

#Predictors (aka criteria)
head(train.data,5) # so predictors are gre, gpa, rank

##optional step - summary data
summary(train.data) #mean of admission rate is less than 50%
##optional step - display the structure of data
str(train.data)  #data is either interger or number

#Check missing data
sum(is.na(train.data)) #make sure there is no empty cell, if there is NA value, then we need to tackle that issue
## optional - create new tabulation, counts rank/admits
xtabs(~admit + rank, data=train.data)

##optional - check rank column in train.data
train.data$rank #this is numeric
# Convert rank variable from numeric to factor, columns of a data frame is already a factor
train.data$rank.factor <- as.factor(train.data$rank) 
train.data$rank.factor #this is factor
#run logit function
logit <- glm(admit ~ gre+gpa+rank.factor, family = 'binomial', data =train.data)
summary(logit)
#### explaination the information logit provides
###number of Fisher Scroing iterations: https://webfocusinfocenter.informationbuilders.com/wfappent/TLs/TL_rstat/source/LogisticRegression43.htm


######### TESTING ###########

#Check the probability of getting admission of a student who has gre:800, gpa: 3.8, graduated from uni rank 1
test.data <- data.frame(gre=800,gpa=3.8,rank.factor=factor(1))
test.data #print test.data

#?predict() 
prob.admitted<- predict(logit,test.data)
prob.admitted
#prediction: he has a high probability of 87.69% to get the admission.


############################# OTHERS
## confidence intervals using profiled log-likelihood
confint(logit)
## CIs using standard errors
confint.default(logit)
