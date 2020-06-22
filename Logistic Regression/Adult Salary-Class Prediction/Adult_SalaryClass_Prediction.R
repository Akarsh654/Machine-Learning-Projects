setwd('C:/Users/akars/Desktop/Studies/EXTRA/R')
# The data is obtained from UCI adult dataset
library(dplyr)
library(Amelia)
library(ggplot2)
library(caTools)

adult <- read.csv("adult_sal.csv")
adult <- select(adult,-X)


#print(table(adult$type_employer))
# there are 1836 null values

# combine the 2 smallest groups are Never-worked and Without-pay
unemp <- function(job){
  job <- as.character(job)
  if (job=='Never-worked' | job=='Without-pay'){
    return('Unemployed')
  }else{
    return(job)
  }
}
adult$type_employer <- sapply(adult$type_employer,unemp)


# combine State and Local gov jobs and combine Self-employed jobs
group_emp <- function(job){
  if (job=='Local-gov' | job=='State-gov'){
    return('SL-gov')
  }else if (job=='Self-emp-inc' | job=='Self-emp-not-inc'){
    return('Self-emp')
  }else{
    return(job)
  }
}
adult$type_employer <- sapply(adult$type_employer,group_emp)


#print(table(adult$marital))
# reduce these into married, not-married, never-married
group_marital <- function(mar){
  mar <- as.character(mar)
  # Not-Married
  if (mar=='Separated' | mar=='Divorced' | mar=='Widowed'){
    return('Not-Married')
  }else if(mar=='Never-married'){
    return(mar)
  }else{
    return('Married')
  }
}
adult$marital <- sapply(adult$marital,group_marital)


#print(table(adult$country))
# group these countries by continent
Asia <- c('China','Hong','India','Iran','Cambodia','Japan', 'Laos' ,
          'Philippines' ,'Vietnam' ,'Taiwan', 'Thailand')

North_America <- c('Canada','United-States','Puerto-Rico' )

Europe <- c('England' ,'France', 'Germany' ,'Greece','Holand-Netherlands','Hungary',
            'Ireland','Italy','Poland','Portugal','Scotland','Yugoslavia')

Latin_and_South_America <- c('Columbia','Cuba','Dominican-Republic','Ecuador',
                             'El-Salvador','Guatemala','Haiti','Honduras',
                             'Mexico','Nicaragua','Outlying-US(Guam-USVI-etc)','Peru',
                             'Jamaica','Trinadad&Tobago')
Other <- c('South')

group_country <- function(ctry){
  if (ctry %in% Asia){
    return('Asia')
  }else if (ctry %in% North_America){
    return('North_America')
  }else if (ctry %in% Europe){
    return('Europe')
  }else if (ctry %in% Latin_and_South_America){
    return('Latin_and_South_America')
  }else{
    return('Other')      
  }
}
adult$country <- sapply(adult$country,group_country)


#print(table(adult$education))
# group the education 
group_education <- function(edu){
  edu <- as.character(edu)
  if(edu == "Assoc-acdm" | edu == "Assoc-voc"){
    return("Assoc")
  }else if(edu == "Bachelors" | edu == "Doctorate" | edu == "Masters" | edu == "Some-college"){
    return("University")
  }else{
    return("School")
  }
}
adult$education <- sapply(adult$education,group_education)


# ensure that the columns have factor levels
adult$type_employer <- sapply(adult$type_employer,factor)
adult$country <- sapply(adult$country,factor)
adult$marital <- sapply(adult$marital,factor)
adult$education <- sapply(adult$education,factor)

# convert ? to NA
adult[adult == '?'] <- NA

adult$type_employer <- sapply(adult$type_employer,factor)
adult$country <- sapply(adult$country,factor)
adult$marital <- sapply(adult$marital,factor)
adult$occupation <- sapply(adult$occupation,factor)

# missmap
#print(missmap(adult,y.at=c(1),y.labels = c(''),col=c('yellow','black')))

# histogram of ages colored by income
age_pl <- ggplot(adult,aes(age)) + geom_histogram(aes(fill=income),color='black',binwidth=1) + theme_bw()
#print(age_pl)

# histogram of hours worked per week 
work_pl <- ggplot(adult,aes(hr_per_week)) + geom_histogram(color="black",fill="blue",bins = 20) + theme_bw()
#print(work_pl)

# rename the country column to region column to better reflect the factor levels
adult <- rename(adult,region = country)

# bar plot with fill color determined by income class
income_pl <- ggplot(adult,aes(region)) + geom_bar(aes(fill=income),color='black')+theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#print(income_pl)

set.seed(101)
# Split the sample
sample <- sample.split(adult$income, SplitRatio = 0.70)

# Training Data
train = subset(adult, sample == TRUE)

# Testing Data
test = subset(adult, sample == FALSE)

model = glm(income ~ ., family = binomial(logit), data = train)

# use step to delete variables that do not significantly add to the fit
new_step_model <- step(model)

# Confusion Matrix
test$predicted_income = predict(new_step_model, newdata=test, type="response")
print(table(test$income, test$predicted_income > 0.5))

# accuracy
print((6402+1370)/(6402+521+924+1370))  # 0.8432245

# precision
print(6402/(6402+924))  # 0.8738739

# recall 
print(6402/(6402+521))  # 0.9247436


