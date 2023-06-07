# Notes & Lecture by Akhona Njeje.
# Date 7 June 2023.
# Topic & Solution : NonLinear Models using Logistic R.

train = read.csv('D:/Users/NjejeA/Downloads/My Research Projects/Raw Solutions/R Solutions/Machine Learning/Part 4 Logistic Regression/titanic_train.csv')
print(head(train))
print('  ')
print(str(train))  # str in R DS structure. 



# EDA.
install.packages("Amelia")   # Amelia package we use it for Missing Values.
library(Amelia)   # help(Ameilia), explore your Missing Map on this documentation.

missmap(train, main = 'Mssing Map', col = c('Orange', 'Dark Blue'), legend = FALSE)   
# about 20% of the Age col data is missing.Orange = Missing Data.
   # Visualise those who passed vs survived.
library(ggplot2)
ggplot(train,aes(Survived)) + geom_bar() # 0 = Passed, 1 = Survived.
   # Wich passenger class had the most passing?
ggplot(train,aes(Pclass)) + geom_bar(aes(fill = factor(Pclass))) # 3rd Class has the highest number of deaths.
  
   # Wich Gender has the most passing?
ggplot(train,aes(Sex)) + geom_bar(aes(fill = factor(Sex))) # Male had the highest number of passing.
   
   # How where ages distributed?
ggplot(train,aes(Age)) + geom_histogram(bins = 20,alpha = 0.5, fill = 'blue') # Less older people.
  
   # Did people come with family members?
ggplot(train,aes(SibSp)) + geom_bar() # 0 = Lots of people didnt come with there Siblings onboard.Lots of Single people.

   # Fare distibution?
ggplot(train,aes(Fare)) + geom_histogram(fill = 'green', color = 'black', alpha= 0.5) # Most people paid for the lowest fared ticks. 



# Cleaning our Data.
   # We had missing Data in our Age column, lets fill in that data, using Evaragees.

pl = ggplot(train,aes(Pclass, Age))
pl = pl + geom_boxplot(aes(group=Pclass,fill=factor(Pclass),alpha=0.4))
pl + scale_y_continuous(breaks = seq(min(0),max(80), by=2))

   # IMPUTE of Age based on Class. Lets create a function.

impute = function(age,class){
  out = age
  for (i in 1:length(age)){
    
    if (is.na(age[i])){
      
      if (class[i] == 1){
        out[i] = 37
        
      }else if (class[i]==2){
        out[i] = 29
      
      }else{
        out[i] = 24
      }
    }else{
      out[i] = age[i]
    }
  }
  return(out)
}

fixed.ages = impute(train$Age,train$Pclass)
train$Age = fixed.ages
print(missmap(train,main = 'Impute Check', col=c('orange', 'black'), legend = FALSE)) # Impute check is all black this means we have cleaned our data & we ready to build our model :-).



# Model Developed.
# Lets remove final features that we no longer need.

str(train) # Theres some columns we wont use/need.E.g Passenger name,Ticket id etc.

library(dplyr)
train = select(train,-PassengerId,-Name,-Ticket,-Cabin) # removing all column we dont need for our model.
head(train)

   # Lets factor columns = Survived,Pclass,Parch & Sibsp.
train$Survived = factor(train$Survived)
train$Pclass = factor(train$Pclass)
train$Parch = factor(train$Parch)
train$SibSp = factor(train$SibSp)

str(train)

# Model
log.model = glm(Survived~., family = binomial(link = 'logit'),data = train) # glm = NonLinear model = Logistic.
summary(log.model) # *** = very important fts.

# Split Data.
library(caTools)
set.seed(101)
split = sample.split(train$Survived, SplitRatio = 0.8)
final.train = subset(train,split == TRUE)
final.test = subset(train, split == FALSE)

final.log.model = glm(Survived~., family = binomial(link = 'logit'),data = final.train)
summary(final.train)

# Prediction.
fitted.probabilities = predict(final.log.model,final.test, type = 'response')
fitted.result = ifelse(fitted.probabilities>0.5,1,0)
misClassError = mean(fitted.result != final.test$Survived)
print(1-misClassError) # = 76%.

# Confusion Matrix.
table(final.test$Survived,fitted.probabilities>0.5)
