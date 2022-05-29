#Read csv
sleep <- read.csv("sleep.csv", na = "")
head(sleep)
#Changing column names
names(sleep)
names(sleep)[names(sleep) == 'sr'] <- 'snoring_rate'
names(sleep)[names(sleep) == 't'] <- 'body_temperature'
names(sleep)[names(sleep) == 'lm'] <- 'limb_movement'
names(sleep)[names(sleep) == 'bo'] <- 'blood_oxygen'
names(sleep)[names(sleep) == 'rem'] <- 'rapid_eyemove'
names(sleep)[names(sleep) == 'sr.1'] <- 'sleeping_hrs'
names(sleep)[names(sleep) == 'hr'] <- 'heart_rate'
names(sleep)[names(sleep) == 'sl'] <- 'stress_level'
names(sleep)[names(sleep) == 'rr'] <- 'respiration_rate'

summary(sleep)

#Histogram for the dependant variable
hist(sleep$blood_oxygen)
#Computing skewness for blood oxygen level
library(moments)
skewness(sleep$blood_oxygen, na.rm = TRUE)

#Transforming the blood_oxygen variable


new_oxygen <- caret::BoxCoxTrans(sleep$blood_oxygen)
print(new_oxygen)

sleep<- cbind(sleep, oxygen=predict(new_oxygen, sleep$blood_oxygen))

#Histogram of transformed blood oxygen
hist(sleep$oxygen)


#Correlation of variables with blood oxygen
cor.test(sleep$respiration_rate, sleep$oxygen)
cor.test(sleep$body_temperature, sleep$oxygen)
cor.test(sleep$rapid_eyemove , sleep$oxygen)
cor.test(sleep$limb_movement , sleep$oxygen)
cor.test(sleep$snoring_rate , sleep$oxygen)
cor.test(sleep$sleeping_hrs , sleep$oxygen)




#Splitting data into training and testing
set.seed(1)
dt = sort(sample(nrow(sleep), nrow(sleep)*.7))
train<-sleep[dt,]
test<-sleep[-dt,]



#Transforming the blood_oxygen variable


new_oxygen <- caret::BoxCoxTrans(sleep$blood_oxygen)
print(new_oxygen)


#Changing blood oxygen variable to the transfornmed variable
sleep<- cbind(sleep, oxygen=predict(new_oxygen, sleep$blood_oxygen))

#Regression using training data
reg1 <- lm(oxygen ~ respiration_rate + body_temperature + rapid_eyemove + 
             limb_movement + snoring_rate + sleeping_hrs
             ,data=train)
summary(reg1)
plot(reg1)


#Finding the cooks distance to find the influential variables and remove the outliers
cooksD <- cooks.distance(reg1)
influential <- cooksD[(cooksD > (3 * mean(cooksD, na.rm = TRUE)))]
influential

install.packages("magrittr") # package installations are only needed the first time you use it
install.packages("dplyr")    # alternative installation of the %>%
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%

names_of_influential <- names(influential)

#computing outliers
outliers <- train[names_of_influential,]

#Transforming data without outliers
data_without_outliers <- train%>% anti_join(outliers)

#Splitting the data into training and testing
set.seed(1)
dt = sort(sample(nrow(data_without_outliers), nrow(data_without_outliers)*.7))
train<-data_without_outliers[dt,]
test<-data_without_outliers[-dt,]



#Regression on the transformed data
reg2 <- lm(oxygen ~ respiration_rate + body_temperature + rapid_eyemove + 
     limb_movement + snoring_rate + sleeping_hrs
   ,data=train)
summary(reg2)
par(mar=c(1,1,1,1))
plot(reg2)


#Checking for multicollinearity
car::vif(reg2)

#Regression performed after removing correlated variables
reg3 <- lm(oxygen ~  respiration_rate + body_temperature + rapid_eyemove + sleeping_hrs  
           ,data=train)
summary(reg3)
 #Checking for multicollinearity of reg3
car::vif(reg3)




#Leap regression
install.packages("leaps")
library(leaps)
leaps <-regsubsets(oxygen ~  respiration_rate + body_temperature + rapid_eyemove + sleeping_hrs  
                   ,data=train ,nbest=4)
#Plot leap regression
plot(leaps, scale="adjr2")



#Computing homoskedasticity
install.packages("skedastic")
library(skedastic)
skedastic::white_lm(reg3)

#Resolving heteroskedasticity using GLS regression
train$resi <- reg3$residuals
varfunc.ols <- lm(log(resi^2) ~ respiration_rate + body_temperature + rapid_eyemove, 
             
                  data = train)
train$varfunc <- exp(varfunc.ols$fitted.values)
#GLS Regression
sleep.gls <- lm( oxygen ~  respiration_rate + body_temperature + rapid_eyemove  , 
                 weights = 1/sqrt(varfunc), data = train)
summary(sleep.gls)
summary(reg3)
plot(sleep.gls)

#Predicted model on testing data
predicted_model <- data.frame(predict(sleep.gls, test))
summary(predicted_model)
plot(predicted_model)

#Actual vs predicted data
values <- data.frame(actual= test$oxygen, predicted=predicted_model)
values
plot(values, xlab='Predicted Values',
     ylab='Actual Values',
     main='Predicted vs. Actual Values')
abline(a=0, b=1)

#Devising scenarios for model validation
df <- data.frame(respiration_rate = c(25), body_temperature = c(90), rapid_eyemove = c(85))
predicted_oxygen <- predict(sleep.gls, df)
predicted_oxygen

df <- data.frame(respiration_rate = c(25), body_temperature = c(90), rapid_eyemove = c(100))
predicted_oxygen <- predict(sleep.gls, df)
predicted_oxygen

#Inputting actual data into the model and observing the predictions

df <- data.frame(respiration_rate = c(25.680), body_temperature = c(91.84), rapid_eyemove = c(99.60))
predicted_oxygen <- predict(sleep.gls, df)
predicted_oxygen

df <- data.frame(respiration_rate = c(25.104), body_temperature = c(91.552), rapid_eyemove = c(98.88))
predicted_oxygen <- predict(sleep.gls, df)
predicted_oxygen



























