# Notes & Lecture by Akhona Njeje.
# Date 7 June 2023.
# Topic & Solution : NonLinear Models using Logistic R.
# Cleaning, Transformation & Model Building.

adult = read.csv("D:/Users/NjejeA/Downloads/My Research Projects/Raw Solutions/R Solutions/Machine Learning/Part 4 Logistic Regression/adult_sal.csv")
print(head(adult)) # Remove X column, we dont need it.

adult = select(adult, -X) # We deleted X column.
print(str(adult))
print(summary(adult))


### Cleaning. ###

table(adult$type_employer) # 1836 null values. Combine Never worked & Without pay & form one column called Unemployed.

unemployed = function(job){
  job = as.character(job)
  if (job == 'Never-worked' | job == 'Without-pay'){
    return('Unemployed')
  }else{
    return(job)
  }
}

adult$type_employer = sapply(adult$type_employer, unemployed)
print(table(adult$type_employer)) # We have created a new column called Unemployed using 2 column = Nver worked & Without pay.

   # Create a new column called Self Employed using =(Self-emp-inc & Self-emp-not-inc)

self_emp = function(job){
  job = as.character(job)
  if (job == 'Self-emp-inc' | job == 'Self-emp-not-inc'){
    return('Self Employed')
  }else{
    return(job)
  }
}

adult$type_employer = sapply(adult$type_employer, self_emp)
print(table(adult$type_employer))

# Create a new column called SL Employed using =(Local Gov & State Gov).

gov = function(job){
  job = as.character(job)
  if (job == 'Local-gov' | job == 'State-gov'){
    return('Goverment')
  }else{
    return(job)
  }
}

adult$type_employer = sapply(adult$type_employer, gov)
print(table(adult$type_employer))


# Create a new column called Married.

married = function(marr){
  marr = as.character(marr)
  if (marr == 'Separated' | marr == 'Divorced' | marr == 'Windowed'){
    return('Not-Married')
    
    # Never Married.
  }else if (marr == 'Never-married'){
    return(marr)
    
    # Married.
  }else{
    return('Married')
  }
}

adult$marital = sapply(adult$marital, married)
print(table(adult$marital))

# Group Countries together by Continents.

table(adult$country)

Asia <- c('China','Hong','India','Iran','Cambodia','Japan', 'Laos' ,
          'Philippines' ,'Vietnam' ,'Taiwan', 'Thailand')

North.America <- c('Canada','United-States','Puerto-Rico' )

Europe <- c('England' ,'France', 'Germany' ,'Greece','Holand-Netherlands','Hungary',
            'Ireland','Italy','Poland','Portugal','Scotland','Yugoslavia')

Latin.and.South.America <- c('Columbia','Cuba','Dominican-Republic','Ecuador',
                             'El-Salvador','Guatemala','Haiti','Honduras',
                             'Mexico','Nicaragua','Outlying-US(Guam-USVI-etc)','Peru',
                             'Jamaica','Trinadad&Tobago')
Other <- c('South')

adult$country = sapply(adult$country, group_country)
print(table(adult$country)) # We reduced Countries to 5 Groups.

str(adult) # Re factor old column to new ones.

adult$type_employer <- sapply(adult$type_employer,factor)
adult$country <- sapply(adult$country,factor)
adult$marital <- sapply(adult$marital,factor)
print(table(adult$country))

### Missing Values/Data ###

library(Amelia)

adult[adult == '?'] = NA
print(table(adult$type_employer)) # ? = 0.

adult$type_employer <- sapply(adult$type_employer,factor)
adult$country <- sapply(adult$country,factor)
adult$marital <- sapply(adult$marital,factor)
print(table(adult$country))

missmap(adult) # Shows us missing values.

# drop missing values.
adult = na.omit(adult) # Now we have dropped all our missing values :-).
missmap(adult)


### EDA.###

library(ggplot2)
library(dplyr)
ggplot(adult,aes(age)) + geom_histogram(aes(fill = income), color='black', binwidth=1) + theme_bw()

# Hours worked pw.
ggplot(adult,aes(hr_per_week)) + geom_histogram() + theme_bw() # 40hrs perweek is the highest.

# Rename the country column to Region.
adult = rename(adult,region= country)
print(head(adult))

# Regions with the highest Incomes.

ggplot(adult,aes(region)) + geom_bar(aes(fill=income),color='black' + theme_bw()

### Model Development : Classification ###.

library(caTools)
set.seed(101)                                    
 
# Split.
 
sample = sample.split(adult$income, SplitRatio = 0.8)
# Train Data.
train = subset(adult,sample == TRUE)
# Tests Data.
test = subset(adult,sample == FALSE)

model = glm(income ~ ., family = binomial(link = 'logit'), data = train) # This model will help us predict if most of the population makes 50k or less pa.
summary(model) # Error in eval(family$initialize) : y values must be 0 <= y <= 1.

# Removing Predicted variables from the model.
# We want to create different Regression models using the AIC, invented by a Japanese Statistician.

new.step.model = step(model).


