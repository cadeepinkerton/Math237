a=function(u)
{
  b=exp(u)/(1+exp(u))
  return(b)
}

x=seq(-50,50,0.1)

plot(x,a(x),pch=16)


############# logistic regression in R 



attach(Education)
is.factor(Education[,"Gender"]) 
Gender=as.factor(Education[,"Gender"]) # force gender to be a factor variable
is.factor(Gender)

logistic.fit=glm(Gender~Math.Scaled.Scores.2013+Math.Scaled.Scores.2012
                 +Math.Scaled.Scores.2011,family=binomial)
summary(logistic.fit)
# When the math scores in 2011 of a student goes up one unit then the log odds in favor of category 1 will increase. Grades of 2012 and 2013 are in favor of category 0. 
# Males performed better in 2011, but in 2012 and 2013 females preformed better. 

####### cross-validation for logistic regression 

library(tidyverse)
library(caret)

set.seed(1234)
a=seq(1,1389,1)
b=sample(a,1000,replace = F)
Ed=data.frame(Education)
Ed$Gender=ifelse(Ed$Gender=="M",1,0) # if the column gender in Ed is a male assign a 1 otherwise assign a 0
Ed$Gender
head(Ed)
Ed.train=Ed[b,]
Ed.test=Ed[-b,]

logistic.fit.cv=glm(Gender~Math.Scaled.Scores.2013+Math.Scaled.Scores.2012
                 +Math.Scaled.Scores.2011,family=binomial,data=Ed.train)
predict.fit=logistic.fit.cv %>%  predict(Ed.test,type="response") # pipe operator run model on train and then run on test data
head(predict.fit)
length(predict.fit)
predicted.classes <- ifelse(predict.fit > 0.5, 1,0) # if in the predict fit data set if the number is greater than 0.5 then it is a 1 otherwise it is a 0 

counter=0
for(i in 1:389)
{
  if(predicted.classes[i] != Ed.test$Gender[i])
  {counter=counter+1}
}
# prediction not equal to actual gender
# if I predicted a 1 as 0 then I misclassified
counter
misclassification.rate=counter/389
misclassification.rate
# misclassification rate determines goodness of fit in logistic regression
#> misclassification.rate
# [1] 0.503856
