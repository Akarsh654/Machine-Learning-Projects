
library(ggplot2)
library(corrgram)
library(corrplot)
library(dplyr)

# read the file
bike <- read.csv("bikeshare.csv")

# plot scatter plots to discover patterns/trends
pl <- ggplot(bike,aes(temp,count)) + geom_point(alpha = 0.2,aes(color=temp)) + theme_bw()
print(pl)

bike$datetime <- as.POSIXct(bike$datetime)
pl2 <- ggplot(bike,aes(datetime,count)) + geom_point(aes(color=temp),alpha = 0.5) 
pl2 <- pl2 + scale_color_continuous(low='#55D8CE',high='#FF6E2E') + theme_bw()
print(pl2)

# correlation between temperature and count
correlation <-cor(bike[,c("temp","count")])
print(correlation)

# plot a box plot to explore the season data
season_plot <- ggplot(bike,aes(factor(season),count)) + geom_boxplot(aes(color = factor(season))) + theme_bw()
print(season_plot)

# feature engineering
time_stamp <- bike$datetime[4]
format(time_stamp, "%H")
bike$hour <- sapply(bike$datetime,function(x){format(x,"%H")})

# scatterplot of count vs hour for working days
w_pl <- ggplot(filter(bike,workingday==1),aes(hour,count))
w_pl <- w_pl + geom_point(position=position_jitter(w=1, h=0),aes(color=temp),alpha=0.5)
w_pl <- w_pl + scale_color_gradientn(colours = c('dark blue','blue','light blue','light green','yellow','orange','red'))
w_pl <- w_pl + theme_bw()
print(w_pl)

# scatterplot of count vs hour for non-working days
nw_pl <- ggplot(filter(bike,workingday==0),aes(hour,count)) 
nw_pl <- nw_pl + geom_point(position=position_jitter(w=1, h=0),aes(color=temp),alpha=0.8)
nw_pl <- nw_pl + scale_color_gradientn(colours = c('dark blue','blue','light blue','light green','yellow','orange','red'))
nw_pl + theme_bw()
print(nw_pl)

# build the model
temp_model <- lm(count~temp,bike)
print(summary(temp_model))

# predict the bike rentals when temp is 25
temp_test <- data.frame(temp=c(25))
print(predict(temp_model,temp_test))

bike$hour <- sapply(bike$hour,as.numeric)

# build another model
model <- lm(count ~ . -casual - registered -datetime -atemp,bike )
print(summary(model))

# Conclusion: A Linear Model won't be able to take into account seasonality of the data









