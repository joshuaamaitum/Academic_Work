setwd("D:/School/Research Methodology/Assignments/Assignment")


#ASSIGNMENT: Build a linear regression model which we can used for estimating total stem volume in the stand
#Model should be a function of basic stand characteristics
#Select models with different variables, including converted ones and select among them

#1. Import the modelling dataset
DATASET <- read.table('Group4.csv', header=TRUE, sep=',')
str(DATASET)
names(DATASET)  #Titles of the columns
cor(DATASET)

data <- read.table('Group16.csv', header=TRUE, sep=',')
str(data)
names(data)

#2. Calculate summary statistics
summary(DATASET)
apply(DATASET,2,sd) #Standard deviation for each column
nrow(DATASET)  #Determine the number of plots
#Can be compiled into a data frame

#Draw histograms & boxplots of total volume based on dominant species and different forest types
#Histogram
hist(DATASET$TOTAL_VOLUME,main='Total Volume', xlab='Total volume, m3/ha',freq=TRUE)
#Boxplot
boxplot(DATASET$TOTAL_VOLUME~DATASET$SP_GROUP, xlab='Species Group', ylab='Total Volume, m3', main='Total Volume by Species Group')
boxplot(DATASET$TOTAL_VOLUME~DATASET$FOREST_TYPE, xlab='Forest Type', ylab='Total Volume, m3', main='Total Volume by Forest Type') 

#EXPORT AN IMAGE FILE
jpeg('TSVPrecipitationScatter.jpg', width = 800, height = 600, res=100)

dev.off()

#3. Draw scatterplot to examine relationships
plot(DATASET$BA,DATASET$TOTAL_VOLUME,xlab='Basal Area',ylab='Total Stem Volume')
plot(DATASET$AGE,DATASET$TOTAL_VOLUME,xlab='AGE',ylab='Total Stem Volume')
plot(DATASET$D,DATASET$TOTAL_VOLUME,xlab='Diameter',ylab='Total Stem Volume')
plot(DATASET$H,DATASET$TOTAL_VOLUME,xlab='Height',ylab='Total Stem Volume')
plot(DATASET$P0,DATASET$TOTAL_VOLUME,xlab='Precipitation',ylab='Total Stem Volume')

#New findings
cor <- cor(DATASET)
ggcorplot(cor) #Save as a PNG file

library(corrplot)

M = cor(datanew)
corrplot(M, method='number')

#EXPORT AN IMAGE FILE
#jpeg('Correlation plot.jpg', width = 800, height = 600, res=100)

dev.off()

library(psych)

corrplot(DATASET)

#4. Model building
#Build a model estimating the dependent variable as a function of stand variables
mod.lm <- lm(TOTAL_VOLUME~X.1+X+LAT+LONG+SP_GROUP+AGE+P0+BA+D+H+YEAR+FOREST_TYPE,data=DATASET)
summary(mod.lm) #Examine outputs
plot(mod.lm)

#Check normality of residuals
plot(mod.lm$fitted.values,mod.lm$residuals, xlab='Fitted values', ylab='Residuals')
abline(h=0)

#Create a data set where I remove all the non ratio variables
names(DATASET)
datanew<-DATASET[, -c(1,2,5,11,12)]

#Begin to trial models (Make about 10 and include a table where you mention top 3)
m6 <- lm(TOTAL_VOLUME~BA*H, data=datanew)
m7 <- lm(TOTAL_VOLUME~BA*D, data=datanew)
m8 <- lm(TOTAL_VOLUME~BA*D*H, data=datanew)
m9 <- lm(sqrt(TOTAL_VOLUME)~sqrt(BA*H), data=datanew)
m10 <- lm(sqrt(TOTAL_VOLUME)~sqrt(BA*H)+ BA, data=datanew)
m9 <- lm(sqrt(TOTAL_VOLUME)~sqrt(BA*H), data=datanew)

#Trial varieties of m6
m11 <- lm(log(TOTAL_VOLUME)~(BA*H), data=datanew)
m12 <- lm(sqrt(TOTAL_VOLUME)~sqrt(BA*H), data=datanew) #CLOSEST TO PERFECTION
m13 <- lm(1/(TOTAL_VOLUME)~(BA*H), data=datanew)
m14 <- lm((TOTAL_VOLUME)~log(BA*H), data=datanew) #OUT
m15 <- lm(log(TOTAL_VOLUME)~log(BA*H), data=datanew)  

#Trial varieties of m7
m11 <- lm(log(TOTAL_VOLUME)~(BA*D), data=datanew)
m12 <- lm((TOTAL_VOLUME)^2~(BA*D)^2, data=datanew) #FAIRLY GOOD
m13 <- lm(1/(TOTAL_VOLUME)~(BA*D), data=datanew)
m14 <- lm((TOTAL_VOLUME)~log(BA*D), data=datanew)
m15 <- lm(log(TOTAL_VOLUME)~log(BA*D), data=datanew)  #CLOSE TO PERFECTION

#Trial varieties of m8
m11 <- lm(log(TOTAL_VOLUME)~(BA*D*H), data=datanew)
m12 <- lm(sqrt(TOTAL_VOLUME)~sqrt(BA*D*H), data=datanew) 
m13 <- lm(1/(TOTAL_VOLUME)~(BA*D*H), data=datanew)
m14 <- lm((TOTAL_VOLUME)~log(BA*D*H), data=datanew)
m15 <- lm(log(TOTAL_VOLUME)~log(BA*D*H), data=datanew)  #CLOSE TO PERFECTION

#The 3 selected outputs are: 
summary(m6)
summary(m7)
summary(m9)
summary(m8)
summary(m11)
summary(m12)
summary(m13)
summary(m14)
summary(m15)
summary(m16)


#Scatterplots
plot(datanew$BA,datanew$TOTAL_VOLUME)
plot(datanew$D,datanew$TOTAL_VOLUME)
plot(datanew$H,datanew$TOTAL_VOLUME)

#EXPORT AN IMAGE FILE
jpeg('Model16ResVsFit.jpg', width = 800, height = 600, res=100)

dev.off()

#Residual figures
plot(m6$fitted.values,m6$residuals)
plot(m7$fitted.values,m7$residuals,ylab='Residuals', xlab='Fitted values',main='1st Model Residuals vs Fitted')
plot(m8$fitted.values,m8$residuals)
plot(m9$fitted.values,m9$residuals)
plot(m11,ylab='Residuals', xlab='Fitted values',main='Model 1 Residuals vs Fitted')
plot(m12$fitted.values,m12$residuals,ylab='Residuals', xlab='Fitted values',main='2nd Model Residuals vs Fitted')
plot(m13$fitted.values,m13$residuals,ylab='Residuals', xlab='Fitted values',main='3rd Model Residuals vs Fitted')
plot(m14$fitted.values,m14$residuals,ylab='Residuals', xlab='Fitted values', main='Model 2 Residuals vs Fitted')
plot(m15$fitted.values,m15$residuals)
plot(m16$fitted.values,m16$residuals,ylab='Residuals', xlab='Fitted values', main='Model 3 Residuals vs Fitted')
abline(h=0)

#EXPORT AN IMAGE FILE
jpeg('HistResM16.jpeg', width = 800, height = 600, res=100)

dev.off()

hist(m6$residuals)
hist(m7$residuals,xlab='Residuals',main='1st Model Histogram of residuals')
hist(m8$residuals)
hist(m9$residuals)
hist(m10$residuals)
hist(m11$residuals,xlab='Residuals',main='Model 1 Histogram of residuals')
hist(m12$residuals,xlab='Residuals',main='2nd Model Histogram of residuals')
hist(m13$residuals,xlab='Residuals',main='3rd Model Histogram of residuals')
hist(m14$residuals,xlab='Residuals',main='Model 2 Histogram of residuals')
hist(m15$residuals)
hist(m16$residuals,xlab='Residuals',main='Model 3 Histogram of residuals')

#TOP 6 MODELS
m11 <- lm(log(TOTAL_VOLUME)~log(BA*H), data=datanew)  #Good histogram
m12 <- lm(sqrt(TOTAL_VOLUME)~sqrt(BA*H), data=datanew) #Histogram not good, has outliers
m13 <- lm(sqrt(TOTAL_VOLUME)~sqrt(BA*D*H), data=datanew)  #Perfect histogram #High intercept but ok #Not good residual plots
m14 <- lm(log(TOTAL_VOLUME)~log(BA*D), data=datanew)  #Left skewed histogram

m15 <- lm(log(TOTAL_VOLUME)~log(BA*D*H), data=datanew) #Not among best options
m16 <- lm(sqrt(TOTAL_VOLUME)~sqrt(BA*D), data=datanew) #Outliers in the histogram but good #Good constant variance #Good intercept value

#FINAL 3: m11, m14, and m16
#Eventually chose model m16

BA<-data$BA
D<-data$D
H<-data$H

model.final<-(1.079893 + 0.551283*(sqrt(BA*D)))^2
model.final<-(0.692651 + 0.662635*sqrt(BA*H))^2
m17<-lm(log(TOTAL_VOLUME)~log(BA*D*(H^2)),data)
m17<-10^(-0.37224+0.47955*(log(BA*D*(H^2))))

#The best model is m16 or m7 (Save it into a new object)
model.final<-lm(TOTAL_VOLUME~BA*D, data=datanew)
model.final<-lm(TOTAL_VOLUME~BA*H*D, data=datanew)
#We use our new model to predict volume and save as a new column
mod.pred <- predict((1.079893 + 0.551283*sqrt(BA*D))^2,DATASET)
mod.pred<-predict(model.final,data)
#Combine the observed and predicted data into a new object
obs.pred <- cbind(DATASET$TOTAL_VOLUME,mod.pred)
obs.pred <- cbind(data$TOTAL_VOLUME,mod.pred)
#Change column names to something more appropriate
colnames(obs.pred) <- c("Observed","Predicted")
head(obs.pred)

#Creating a new dataframe


#Attempt to plot the data
obs.pred<-data.frame(obs.pred)
#Make a scatter plot
plot(obs.pred$Observed, obs.pred$Predicted, main='Plot of modeled volume and reference volume',xlab='Reference volume',ylab='Modeled volume')
abline(lm(obs.pred$Predicted~obs.pred$Observed))
#OR
plot(DATASET$TOTAL_VOLUME,)

#EXPORT AN IMAGE FILE
jpeg('RefvsMod1.jpg', width = 800, height = 600, res=100)

dev.off()

#Histogram of residuals



#VALIDATING THE MODEL
#1. Import the data
VAL <- read.table('validating_data.csv', header=TRUE, sep=',')
str(VAL)

#2. Summary statistics of the data
summary(VAL)
apply(VAL,2,sd) #Standard deviation for each column
nrow(VAL)  #Determine the number of plots
#Can be compiled into a data frame

names(VAL)

#Draw histograms & boxplots of total volume based on dominant species and different forest types
#Histogram
hist(VAL$VOLUME,main='Total Volume', xlab='Total volume, m3/ha',freq=TRUE)
#Boxplot
boxplot(VAL$VOLUME~VAL$SP_GROUP, xlab='Species Group', ylab='Total Volume, m3', main='Total Volume by Species Group')
boxplot(VAL$VOLUME~VAL$FOREST_TYPE, xlab='Forest Type', ylab='Total Volume, m3', main='Total Volume by Forest Type') 

#3. Draw scatterplot to examine relationships
plot(VAL$BA,VAL$VOLUME,xlab='Basal Area',ylab='Total Stem Volume')
plot(DATASET$AGE,DATASET$TOTAL_VOLUME,xlab='AGE',ylab='Total Stem Volume')
plot(VAL$D,VAL$VOLUME,xlab='Diameter',ylab='Total Stem Volume')
plot(VAL$H,VAL$VOLUME,xlab='Height',ylab='Total Stem Volume')
plot(DATASET$P0,DATASET$TOTAL_VOLUME,xlab='Precipitation',ylab='Total Stem Volume')

library(ggcorrplot)
ggcorrplot(VAL)

#Now calculate stand volume estimate using the best model (m7)
model.final1<-lm(VOLUME~BA*D, data=VAL)        #Remember this is our model
model.final1<-lm(VOLUME~BA*D*H, data=VAL)
#We use our new model to predict volume and save as a new column
mod.pred1 <- predict(model.final,VAL)

#Add a new column to the validation dataset, where you calculate stand volume estimate
#In our case we don't add a new column, instead we create new data object based off the formula of our best model, m7

#Compare the new modeled stand volume with the original stand volume in the validation data
#Draw scatter plot between the modeled and original volumes
#Combine the original stand volume and modeled stand volume into a new object (using validation data)
obs.pred1 <- cbind(VAL$VOLUME,mod.pred1)
#Change column names to something more appropriate
colnames(obs.pred1) <- c("Original","Modeled")
head(obs.pred1)

#Attempt to plot the data
obs.pred1<-data.frame(obs.pred1)
#Make a scatter plot
plot(obs.pred1$Original, obs.pred1$Modeled,main='Plot of modeled volume and reference volume',xlab='Reference volume',ylab='Modeled volume')
abline(lm(obs.pred1$Modeled~obs.pred1$Original))

#EXPORT AN IMAGE FILE
jpeg('RefvsMod2.jpg', width = 800, height = 600, res=100)

dev.off()

#ABSOLUTE BIAS & ABSOLUTE RMSE
#RMSE
#Residuals are a measure of how far from the regression line data points are; RMSE is a measure of how spread out these residuals are. In other words, it tells you how concentrated the data is around the line of best fit (abline)
#we will create our own R formula obtained from this nice r-bloggers entry:

rmse <- function(error)
{
  sqrt(mean(error^2))
}

rmse(obs.pred1$Modeled - obs.pred1$Original)

#OR
sqrt(mean((obs.pred1$Modeled - obs.pred1$Original)^2))

#The larger the difference indicates a larger gap between the predicted and observed values, which means poor regression model fit. In the same way, the smaller RMSE that indicates the better the model.
#Based on RMSE we can compare the two different models with each other and be able to identify which model fits the data better.

#BIAS
t.test(obs.pred1$Original,obs.pred1$Modeled,paired=TRUE)
#Since p-value is not less than 0.05, the bias is not significant

library(hydroGOF)

pbias(obs.pred1$Modeled, obs.pred1$Original)

