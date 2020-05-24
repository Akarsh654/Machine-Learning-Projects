setwd('C:/Users/akars/Desktop/Studies/EXTRA/R')
# P. Cortez and A. Silva. Using Data Mining to Predict Secondary School Student Performance. In A. Brito and J. Teixeira Eds., 
# Proceedings of 5th FUture BUsiness TEChnology Conference (FUBUTEC 2008) pp. 5-12, Porto, Portugal, April, 2008, EUROSIS, ISBN 978-9077381-39-7
library(ggplot2)
library(ggthemes)
library(dplyr)
library(corrgram)
library(corrplot)
library(caTools)

# Read the data
df <- read.csv('student-mat.csv',sep=';')

# Clean data
print(any(is.na(df)))

# Num only
num.cols <- sapply(df,is.numeric)

# Filter
cor.data <- cor(df[,num.cols])
print(corrplot(cor.data,method = 'color'))
corrgram(df,order = T,lower.panel = panel.shade,upper.panel=panel.pie,text.panel = panel.txt)
ggplot(df,aes(x=G3)) + geom_histogram(bins=20,alpha=0.5,fill='blue')

# Set a Seed
set.seed(101)

# Split up sample
sample <- sample.split(df$G3,SplitRatio = 0.7)

# Training Data
train <- subset(df, sample==TRUE)
# Testing Data
test <- subset(df,sample==FALSE) 

# Train and build model
model <- lm(G3~.,data=train)
print(summary(model))

# Residual
res <- residuals(model)
res <- as.data.frame(res)
ggplot(res,aes(res)) + geom_histogram(fill='blue',alpha = 0.5)

# Predictions
G3.predictions <- predict(model,test)

results <- cbind(G3.predictions,test$G3)
colnames(results) <- c('predicted','actual')
results <- as.data.frame(results)

# Take care of negative values
to_zero <- function(x){
  if(x<0){
    return(0)
  }else{
    return(x)
  }
}

# Apply zero function
results$predicted <- sapply(results$predicted,to_zero)

# Mean Squared Error
mse <- mean((results$actual - results$predicted)^2)
print("MSE")
print(mse)

# RMSE
print("Squared Root of MSE")
print(mse^0.5)

# SSE and SST
SSE <- sum((results$predicted - results$actual)^2)
SST <- sum((mean(df$G3)- results$actual)^2)

# R-squared
R2 <- 1 - SSE/SST
print("R2")
print(R2)

