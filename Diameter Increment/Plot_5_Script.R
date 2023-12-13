###########################################################################
#### Assignment script for Plot 5 by Joshua Amaitum and Jean Kemely
###########################################################################

#Load required libraries
library(corrplot)
library(ggplot2)


rm(list = ls())   #Clear R's memory


# Read the data and save it into an object
tab <- read.csv("LMEFData.csv",header=T,sep=",",dec=".")  
head(tab) # View top rows of the data
str(tab)  # Dislay the structure
summary(tab)  # Display summary


# Plot 5 is selected, so object created with only data that corresponds to it, similar operations as above conducted follow
mytab <- tab[tab$plotID==5,]
summary(mytab)
str(mytab)
head(mytab)


# Group the data by years for summary (Used in the report)
mytabsum1989 <- mytab[mytab$dateYr==1989,]
summary(mytabsum1989)
table(mytabsum1989$dead) # Determine number of living & dead

mytabsum1994 <- mytab[mytab$dateYr==1994,]
summary(mytabsum1994)
table(mytabsum1994$dead) # Determine number of living & dead

mytabsum2000 <- mytab[mytab$dateYr==2000,]
summary(mytabsum2000)
table(mytabsum2000$dead) #Determine number of living & dead


#On to the Statistical analysis: 
#Focus on question of do larger the trees have smaller diameter increments
#Visualize relationships between variables tree dbh and increment according to years
plot(mytabsum1989$dbhCm, mytabsum1989$diMm, col = "blue", pch = 16, xlim = c(0, 30), ylim = c(0, 30),main="Scatterplot of tree dbh and dbh increment over the growth interval",xlab = "Tree DBH", ylab = "Diameter Increment")
points(mytabsum1994$dbhCm, mytabsum1994$diMm, col = "red", pch = 18)
points(mytabsum2000$dbhCm, mytabsum2000$diMm, col = "green", pch = 10)
legend("bottomright", legend = c("1989", "1994","2000"), col = c("blue", "red","green"), pch = c(16, 18, 10), title = "Years")

#Visualize relationships between variables BALM2Ha (competition) and tree dbh according to years
plot(mytabsum1989$BALM2Ha, mytabsum1989$diMm, col = "blue", pch = 16, xlim = c(0, 30), ylim = c(0, 30),main="Scatterplot of BALM2Ha and tree dbh over the growth interval",xlab = "BALM2Ha (competition)", ylab = "Diameter Increment")
points(mytabsum1994$BALM2Ha, mytabsum1994$diMm, col = "red", pch = 18)
points(mytabsum2000$BALM2Ha, mytabsum2000$diMm, col = "green", pch = 10)
legend("bottomright", legend = c("1989", "1994","2000"), col = c("blue", "red","green"), pch = c(16, 18, 10), title = "Years")


#Next remove dead trees, as they have NA values for the response variable
mytab1<-mytab[mytab$dead==0,] #Leaves only the living trees
head(mytab1)  #View top rows


#Determine correlation between variables
mytab2<-mytab1[,-c(1,2,6,9)]  #Remove variables not necessary for the correlation analysis
head(mytab2)
summary(mytab2)
cor(mytab2$dbhCm,mytab2$diMm) # Correlation between tree size and diameter increment
cor(mytab2$BALM2Ha,mytab2$diMm) # Correlation between BALM2Ha (competition) and diameter increment

# Correlation plot for visual relationship
par(mfrow=c(1,1))
cor<-cor(mytab2)
corrplot(cor)


# Build a regression model
# See what species we have
table(mytab$species) 
round(prop.table(table(mytab$species))*100, digits = 2) # estimation of the percentage of each species
mytab<-mytab[mytab$species=="Fir",] # Remove unwanted species (Spruce and Others)
head(mytab)
table(mytab$species) # See what remains 


#Model for tree size and diameter increment
model<-lm(diMm~dbhCm,data=mytab)
summary(model)
hist(model$residuals, xlab='Residuals',main='Model Histogram of residuals') # Histogram of residuals 
par(mfrow=c(2,2))
plot(model)
par(mfrow=c(1,1))


#Model for competition and diameter increment
model<-lm(diMm~BALM2Ha,data=mytab)
summary(model)
hist(model$residuals, xlab='Residuals',main='Model Histogram of residuals') # Histogram of residuals 
par(mfrow=c(2,2))
plot(model)
par(mfrow=c(1,1))


#For a Year by year analysis of the data
#remove NA values
mytabsum1989<-na.omit(mytabsum1989)
mytabsum1994<-na.omit(mytabsum1994)
mytabsum2000<-na.omit(mytabsum2000)

#Merge
merged<-merge(mytabsum1989,mytabsum1994,by="treeID",all = FALSE)
merged_final<-merge(merged,mytabsum2000,by="treeID",all=FALSE)
merged_final<-merged_final[-c(10,24,25,46,51),] # Remove outlier values


df1989<-subset(merged_final,select = c(dbhCm.x,BALM2Ha.x,diMm.x))
df1994<-subset(merged_final,select = c(dbhCm.y,BALM2Ha.y,diMm.y))
df2000<-subset(merged_final,select = c(dbhCm,BALM2Ha,diMm))
summary(df1989)
summary(df1994)
summary(df2000)

par(mfrow=c(1,1))


# Calculate Pearson's correlation coefficient for each period
correlation_coefficient_period1 <- cor(df1989$dbhCm.x, df1989$diMm.x)
correlation_coefficient_period2 <- cor(df1994$dbhCm.y, df1994$diMm.y)
correlation_coefficient_period3 <- cor(df2000$dbhCm, df2000$diMm)

# Print the results
cat("Pearson's correlation coefficient for Period 1:", correlation_coefficient_period1, "\n")
cat("Pearson's correlation coefficient for Period 2:", correlation_coefficient_period2, "\n")
cat("Pearson's correlation coefficient for Period 3:", correlation_coefficient_period3, "\n")

# Linear regression model
regression_model1989 <- lm(diMm.x~dbhCm.x, data = df1989)
regression_model1994 <- lm(diMm.y~dbhCm.y , data = df1994)
regression_model2000 <- lm(diMm~dbhCm, data = df2000)


# Summary of the regression model
summary(regression_model1989)
summary(regression_model1994)
summary(regression_model2000)

#data exploration 
par(mfrow=c(1,1))
library(geometry)

ggplot() +
  geom_point(data = df1989, aes(x = dbhCm.x, y = diMm.x), color = "red") +
  geom_smooth(data = df1989, aes(x = dbhCm.x, y = diMm.x), method = "lm", se = FALSE, color = "red") +
  
  geom_point(data = df1994, aes(x = dbhCm.y, y = diMm.y), color = "blue") +
  geom_smooth(data = df1994, aes(x = dbhCm.y, y = diMm.y), method = "lm", se = FALSE, color = "blue") +
  
  geom_point(data = df2000, aes(x = dbhCm, y = diMm), color = "green") +
  geom_smooth(data = df2000, aes(x = dbhCm, y = diMm), method = "lm", se = FALSE, color = "green") +
  
  labs(title = "Regression Lines for Each Period",
       x = "Tree Size",
       y = "Diameter Increments") +
  
  scale_color_manual(values = c("1989" = "blue", "1994" = "red", "2000" = "green"),
                     name = "Period") +
  
  theme_minimal()+
theme(legend.position = "top")

hist(regression_model1989$residuals, xlab='Residuals',main='Model Histogram of residuals') 
hist(regression_model1994$residuals, xlab='Residuals',main='Model Histogram of residuals')  
hist(regression_model2000$residuals, xlab='Residuals',main='Model Histogram of residuals')  

par(mfrow=c(2,2))

plot(regression_model1989)
plot(regression_model1994)
plot(regression_model2000)


par(mfrow=c(1,1))


# Calculate Pearson's correlation coefficient for each period
correlation_competition89 <- cor(df1989$BALM2Ha.x, df1989$diMm.x)
correlation_competition94 <- cor(df1994$BALM2Ha.y, df1994$diMm.y)
correlation_competition00 <- cor(df2000$BALM2Ha, df2000$diMm)

# Print the results
cat("Pearson's correlation coefficient for Period 1:", correlation_competition89, "\n")
cat("Pearson's correlation coefficient for Period 2:", correlation_competition94, "\n")
cat("Pearson's correlation coefficient for Period 3:", correlation_competition00, "\n")

# Linear regression model for competition
regression_model89 <- lm(diMm.x~ BALM2Ha.x, data = df1989)
regression_model94 <- lm(diMm.y ~ BALM2Ha.y, data = df1994)
regression_model00 <- lm(diMm ~ BALM2Ha, data = df2000)


# Summary of the regression model
summary(regression_model89)
summary(regression_model94)
summary(regression_model00)

#data exploration 

ggplot() +
  geom_point(data = df1989, aes(x = BALM2Ha.x, y = diMm.x), color = "blue") +
  geom_smooth(data = df1989, aes(x = BALM2Ha.x, y = diMm.x), method = "lm", se = FALSE, color = "red") +
  
  geom_point(data = df1994, aes(x = BALM2Ha.y, y = diMm.y), color = "red") +
  geom_smooth(data = df1994, aes(x = BALM2Ha.y, y = diMm.y), method = "lm", se = FALSE, color = "blue") +
  
  geom_point(data = df2000, aes(x = BALM2Ha, y = diMm), color = "green") +
  geom_smooth(data = df2000, aes(x = BALM2Ha, y = diMm), method = "lm", se = FALSE, color = "green") +
  
  labs(title = "Regression Lines for Each Period",
       x = "Tree Size",
       y = "Diameter Increments") +
  
  scale_color_manual(values = c("1989" = "red", "1994" = "blue", "2000" = "green"),
                     name = "Period") +
  
  theme_minimal()+
  theme(legend.position = "top")


hist(regression_model89$residuals, xlab='Residuals',main='Model Histogram of residuals') 
hist(regression_model94$residuals, xlab='Residuals',main='Model Histogram of residuals')  
hist(regression_model00$residuals, xlab='Residuals',main='Model Histogram of residuals')  

par(mfrow=c(2,2))

par(mfrow=c(2,2))
plot(regression_model89)
plot(regression_model94)
plot(regression_model00)


