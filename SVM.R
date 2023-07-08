# Collab, Notes & Lecture by Akhona Njeje.
# Date 8 July 2023.
# Topic & Solution : Support Vector Machine, ROI Algorithm for Investors.



# Load DATA.

loan = read.csv("C:/Users/User/Desktop/AI/Machine Learning/R/SVM/loan_data.csv")
print(loan)


# Structure of the loan data.

print(str(loan))


# Covert to categorical Data.

loan$credit.policy = factor(loan$credit.policy)
loan$inq.last.6mths = factor(loan$inq.last.6mths)
loan$delinq.2yrs = factor(loan$delinq.2yrs)
loan$pub.rec = factor(loan$pub.rec)
loan$not.fully.paid = factor(loan$not.fully.paid)


# EDA.

library(ggplot2)

### Histgram.

pl = ggplot(loan, aes(fico))
pl = pl + geom_histogram(aes(fill=not.fully.paid), color = 'black', bins = 40)
pl = pl + theme_bw()
pl

### Barplot.

pl = ggplot(loan, aes(x = factor(purpose)))
pl = pl + geom_bar(aes(fill=not.fully.paid), position = 'dodge')
pl = pl + theme_bw()
pl

### Scatterplot.

pl = ggplot(loan, aes(int.rate,fico)) + geom_point() + theme_bw()
pl


# Model Development.

### Train Test Split.

install.packages("caTools")
library(caTools)
set.seed(101)

sample = sample.split(loan$not.fully.paid, 0.8)
train = subset(loan, sample == TRUE)
test = subset(loan, sample == FALSE)

install.packages("e1071")
library(e1071)

model = svm(not.fully.paid ~ .,data = train)
print(summary(model))


### Model Performance.

predicted.values = predict(model,test[1:13])
table(predicted.values, test$not.fully.paid) # Our model is bad.

### Tune The model for high performance.

tuned.results = tune(svm, train.x = not.fully.paid ~ ., data=train,
                     kernel='radial', ranges = list(cost=c(100,200),
                                                    gamma=c(0.1)))
print(summary(tuned.results)) # Tuning takes a bit of time depends of your pc.
                              # Be patient while its processing results :-).
                              # Looks like model imporved :-), lets go again.
                              # Lets use the cost =100 & gamma = 0.1.
tuned.model = svm(not.fully.paid ~ ., data = train, cost=100, gamma = 0.1)
tuned.predictions = predict(tuned.model, test[1:13])
table(tuned.predictions, test$not.fully.paid) # Model Improved :-).