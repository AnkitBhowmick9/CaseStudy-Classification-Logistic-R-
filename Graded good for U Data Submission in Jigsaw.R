# Answer for question number 2 is ---TRUE
#ANSWER 3--> 8268 +9004 = 17272
#ANSWER 4--> 0.235300
#ANSWER 9--> 3117+10083 = 13200
#Answer 10 --> 8032+6517 = 14549
#Answer 11 --> 9258+4723 = 13981

## Explanations are given below:--

library(gains)
library(dplyr)
library(irr)
library(caret)

# Analysing data## 
dataset <- read.csv('goodforu-class12.csv')

# Sorting of useful column
dataset2 <-read.csv('goodforu-class12.csv')[,c('X2','X9','X16','X30','X23')]

#Filter Data for given condition:-
dataset2%>%filter(X23 <= '4')%>%nrow()  #12502

# Answer for question number 2 is ---TRUE 

#Factor that are good for Brand A
#Making dependent column  
dataset2$XP <- ifelse(dataset2$X23 >=5,1,0)
dataset2%>%select(-X23)->dataset2

## seggregation of data 
#install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset2$XP,SplitRatio = 0.75)
training_set = subset(dataset2,split == TRUE)
test_set     = subset( dataset2,split == FALSE)

#Fitting logistic regression to dataset
classifier = glm(formula= XP~.,
                 family = "binomial",
                 data= dataset2)
summary(classifier)

step(classifier,direction = "both")

classifier2 =glm(formula = XP ~ X2 + X9 + X16 + X30, family = "binomial", 
                 data = dataset2)
summary(classifier2)




#predicting the test result
Prob_pred2 = predict(classifier2,type = 'response',newdata = dataset2)
Prob_pred2

table(dataset2$XP)/nrow(dataset2)
y_pred2 = ifelse(Prob_pred2 >0.4957286 ,1,0)
y_pred2

kappa2(data.frame(dataset2$XP,y_pred2))

confusionMatrix(y_pred2,dataset2$XP,positive="1")

# Reference
# Prediction    0    1
# 0           8268 2950
# 1           3892 9004

#ANSWER 3--> 8268 +9004 = 17272

## Natural Oil Case
# Bad Respondents --13469
# Good  Respondents --10645
13469/24114 #0.5585552
10645/24114 #0.4414448


log(0.4414448/0.5585552)
#ANSWER 4--> 0.235300


##Q9 -11
dataset4 <- read.csv('goodforu-class12.csv')[,c('X2','X9','X16','X23')]

#Making dependent column  
dataset4$XP <- ifelse(dataset4$X23 >=5,1,0)
dataset4%>%select(-X23)->dataset4

classifier = glm(formula= XP~X2,
                 family = "binomial",
                 data= dataset4)
summary(classifier)

#predicting the test result
Prob_pred2 = predict(classifier,type = 'response',newdata = dataset4)
Prob_pred2

table(dataset4$XP)/nrow(dataset4)
y_pred2 = ifelse(Prob_pred2 >0.4957286 ,1,0)
y_pred2

kappa2(data.frame(dataset4$XP,y_pred2))

confusionMatrix(y_pred2,dataset4$XP,positive="1")

#Confusion Matrix and Statistics

#             Reference
# Prediction     0     1
# 0            3117  1871
# 1            9043 10083

#ANSWER 9--> 3117+10083 = 13200
########################

dataset4 <- read.csv('goodforu-class12.csv')[,c('X2','X9','X16','X23')]

#Making dependent column  
dataset4$XP <- ifelse(dataset4$X23 >=5,1,0)
dataset4%>%select(-X23)->dataset4

classifier = glm(formula= XP~X16,
                 family = "binomial",
                 data= dataset4)
summary(classifier)

#predicting the test result
Prob_pred2 = predict(classifier,type = 'response',newdata = dataset4)
Prob_pred2

table(dataset4$XP)/nrow(dataset4)
y_pred2 = ifelse(Prob_pred2 >0.4957286 ,1,0)
y_pred2

kappa2(data.frame(dataset4$XP,y_pred2))

confusionMatrix(y_pred2,dataset4$XP,positive="1")

# Confusion Matrix and Statistics
# 
#                  Reference
# Prediction    0    1
# 0             8032 5437
# 1             4128 6517


#Answer 10 --> 8032+6517 = 14549


###################################
dataset4 <- read.csv('goodforu-class12.csv')[,c('X2','X9','X16','X23')]

#Making dependent column  
dataset4$XP <- ifelse(dataset4$X23 >=5,1,0)
dataset4%>%select(-X23)->dataset4

classifier = glm(formula= XP~X9,
                 family = "binomial",
                 data= dataset4)
summary(classifier)

#predicting the test result
Prob_pred2 = predict(classifier,type = 'response',newdata = dataset4)
Prob_pred2

table(dataset4$XP)/nrow(dataset4)
y_pred2 = ifelse(Prob_pred2 >0.4957286 ,1,0)
y_pred2

kappa2(data.frame(dataset4$XP,y_pred2))

confusionMatrix(y_pred2,dataset4$XP,positive="1")

#              Reference
# Prediction    0    1
# 0          9258 7231
# 1          2902 4723

#Answer 11 --> 9258+4723 = 13981 



#C:\Jig16782\Graded\Predictive Analysis\Graded good for U Data Resubmit1

