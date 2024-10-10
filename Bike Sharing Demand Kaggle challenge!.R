#install the packages with the library below if not already installed
library(caTools)
library(corrgram)
library(corrplot)
library(ggplot2)
library(ggthemes)
library(dplyr)

#importing the data
bike <- read.csv('bikeshare.csv')
#head(bike)

# -----exploratory data analysis
print(ggplot(bike,aes(x=temp,y=count)) +
        geom_point(aes(color=temp),alpha=0.2) +
        theme_bw())

#converting the datetime column to date format
bike$datetime <- as.POSIXct(bike$datetime)
print(ggplot(bike,aes(x=datetime,y=count)) + 
        geom_point(aes(color=temp),alpha=0.2) + 
        theme_bw() + 
        scale_color_continuous(low='#55A8EB',high='#AF6E2E'))

#creating a correlation between temp and count

cor.dat <- cor(bike[,c('temp','count')])
print(cor.dat)

#exploring the season data
print(ggplot(bike,aes(x=factor(season),y=count)) + 
        geom_boxplot(aes(color=factor(season))) + 
        theme_bw())


# -----FEATURE ENGINEERING


#lets create a new column hour
bike$hour <- format(bike$datetime, '%H')

#lets visualize for trend on workdays
print(ggplot(subset(bike,bike$workingday == 1),aes(hour,count))+
        geom_point(aes(color=temp),alpha=0.5,position = position_jitter(w=1,h=0))+
        scale_color_gradientn(colours=c('blue','lightblue','green','lightgreen','yellow','red'))+
        theme_bw())

#lets visualize for trend on non-workdays
print(ggplot(subset(bike,bike$workingday == 0),aes(hour,count))+
        geom_point(aes(color=temp),alpha=0.5,position = position_jitter(w=1,h=0))+
        scale_color_gradientn(colours=c('blue','lightblue','green','lightgreen','yellow','red'))+
        theme_bw())

#let now build a model
model <- lm(count ~ temp,data = bike)
summary(model)

#predict number of bike at a temp of 30 degree
temp_num <- data.frame(temp=c(30))

bike_predict <- predict(model,temp_num)
print(bike_predict)

#changing the hour column created to numeric values

bike$hour <- sapply(bike$hour,as.numeric)

allmodel <- lm(count ~ . -casual -registered - datetime - atemp, data = bike)
summary(allmodel)

