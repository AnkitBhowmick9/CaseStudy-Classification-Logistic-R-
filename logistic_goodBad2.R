# Logistic Regression - Binary, Ordered, Multinominal 
# Used when DV is discrete 
# Yes or No 
# Come back or not come back 
# Buy or nor buy 
# Renew or not-renew 
# Default or not-default 
# Win or loos 
# zero or one 
# good or bad 

# Compare linear & logistic regression 
# Y                = A + B*x1 + C*x2 + D*x3 + ........    Linear Regression 
# log(Y/(1-Y))     = A + B*x1 + C*x2 + D*x3 + ........    Logistic Regression
# log(Y/(1-Y))  = log of odds of DV = LOGIT OF DV 
# B - A unit change in the value of x1 will increase the LOGIT OF EVENT(1) by B times. 
# Exp(B) will talk in terms of ODD of EVENT & from odds you can easily get the prob 

goodbad<-read.csv("C:\\JS\\Logistic Regression/goodbad-class12.csv")
View(goodbad)
# 1 - Good Customer, 0 - Bad Customer 
table(goodbad$Good.Bad)
prop.table(table(goodbad$Good.Bad))

# Missing values 
colSums(is.na(goodbad))

sample(5,3)

# Split data into training and testing part 
sampling <- sort(sample(nrow(goodbad), nrow(goodbad)*.7))

sampling <- sort(sample(1000, 700))
sampling

# select training data 
train <- goodbad[sampling, ]
test  <- goodbad[-sampling, ]

nrow(train)
nrow(test)

# Build logisitic regression model 
myresult <- glm(Good.Bad ~ CreditHistory+Check_Account_Status+Duration, data = train, family = binomial)
summary(myresult)

# Function to convert logits into the probabilities 
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

logit2prob(coef(myresult))

# CreditHistoryA31 = -0.118830
# If a customer is moving from CreditHistoryA11 to CreditHistoryA31 the "logit of [1] non-default" will decrease by 0.11
exp(-0.118830)
# If a customer is moving from CreditHistoryA11 to CreditHistoryA31 the "odd of [1] non-default" will increase by 0.8879587
exp(-0.118830)/(1+exp(-0.118830))

# If a customer is moving from CreditHistoryA11 to CreditHistoryA31 the "prob of [1] non-default" will increase by 0.4703274
# AIC: 747.21 - Alkaiki Information Criterian 
# Complexity and explainability 

# Prediction from this model on the train data 
predicted <- myresult$fitted.values
predicted

predicted[1:10]
train$Good.Bad[1:10]


# Confusion matrix
predbkt <- ifelse(predicted>0.5, 'G', 'B')
table(predbkt, train$Good.Bad)

(79+442)/(79+46+133+442)

predbkt <- ifelse(predicted>0.2, 'G', 'B')
table(predbkt, train$Good.Bad)

(488+6)/(488+6+0+206)

predbkt <- ifelse(predicted>0.7, 'G', 'B')
table(predbkt, train$Good.Bad)

(154+326)/(154+326+58+162)

#Plotting ROC Curve
library(ROCR)
pred<-prediction(predicted,train$Good.Bad)
perf<-performance(pred,"tpr","fpr") 
plot(perf)

# Add colors
plot(perf, colorize=TRUE)
# Add threshold labels 
plot(perf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))


predbkt <- ifelse(predicted>0.6, 'G', 'B')
table(predbkt, train$Good.Bad)

# Prediction on test dataset 
predict(myresult, data = test, type = "response")

library(car)
vif(myresult)
