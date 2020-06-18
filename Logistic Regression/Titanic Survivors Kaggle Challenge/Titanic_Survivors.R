
library(Amelia)
library(ggplot2)
library(dplyr)
library(caTools)
df_train <- read.csv("titanic_train.csv")

# check for missing values (na)
print(missmap(df_train,main="Missing Map",col=c('yellow','black'),legend=F))
# the missing map shows that there is a lot of missing data for ages

# bar plot of survived vs not survived to get a rough idea
print(ggplot(df_train,aes(Survived))+geom_bar())

# bar plot based on Pclass 
print(ggplot(df_train,aes(Pclass))+geom_bar(aes(fill=factor(Pclass))))

# bar plot based on gender
print(ggplot(df_train,aes(Sex))+geom_bar(aes(fill=factor(Sex))))

# histogram based on ages of people
print(ggplot(df_train,aes(Age))+geom_histogram(bins=20,alpha=0.5,fill="blue"))

# bar plot based on siblings and spouses
print(ggplot(df_train,aes(SibSp)) + geom_bar())

# histogram based on fare 
print(ggplot(df_train,aes(Fare)) + geom_histogram(bins=10,fill="green",color="black",alpha=0.5))

# boxplot of Age vs Pclass
pl <- ggplot(df_train,aes(Pclass,Age)) + geom_boxplot(aes(group=Pclass,fill=factor(Pclass),alpha=0.4))
pl <- pl + scale_y_continuous(breaks = seq(min(0),max(80),by=2)) + theme_bw()
print(pl)
# the boxplot shows that average age in first class is 37, second class is 29, third class is 24 


# discarding all the data with missing ages would lead to less accurate prediction
# fill in missing age values with average age by passenger class

impute_age <- function(age,class){
  out <- age
  for (i in 1:length(age)){
    
    if (is.na(age[i])){
      
      if (class[i] == 1){
        out[i] <- 37
        
      }else if (class[i] == 2){
        out[i] <- 29
        
      }else{
        out[i] <- 24
      }
    }else{
      out[i]<-age[i]
    }
  }
  return(out)
}

fixed_ages <- impute_age(df_train$Age,df_train$Pclass)
df_train$Age <- fixed_ages

# check if the function was successful
print(missmap(df_train,main="Imputation Check", col = c("yellow","black"),legend = F))

# remove features that wont be used
df_train <- select(df_train,-PassengerId,-Name,-Ticket,-Cabin)
print(head(df_train))

# change some columns from int to continuous variables
df_train$Survived <- factor(df_train$Survived)
df_train$Pclass <- factor(df_train$Pclass)
df_train$Parch <- factor(df_train$Parch)
df_train$SibSp <- factor(df_train$SibSp)

# train the model
log_model <- glm(formula=Survived ~.,family=binomial(link="logit"),data=df_train)
print(summary(log_model))

# split the data 
set.seed(101)
split <- sample.split(df_train$Survived, SplitRatio = 0.7)
final_train <- subset(df_train,split== T)
final_test <- subset(df_train,split== F)
final_log_model <- glm(formula = Survived ~.,family = binomial(link="logit"),data=final_train)
print(summary(final_log_model))

# predict
fitted_probabilities <- predict(final_log_model,final_test,type="response")
fitted_results <- ifelse(fitted_probabilities>0.5,1,0)

# Misclassification error and accuracy
missClassError <- mean(fitted_results != final_test$Survived)
print(1 - missClassError)

# Confusion Matrix
print(table(final_test$Survived,fitted_probabilities>0.5))
