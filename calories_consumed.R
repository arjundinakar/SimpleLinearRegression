library(readr)
calories = read_csv("D:\\Arjun\\DATA_SCIENCE-ASSIGNMENTS\\Simple Linear Regression\\calories_consumed.csv")
View(calories)
attach(calories)

#EDA
boxplot(calories)###THE MEDIAN OF THE BOXPLOT SEEMS TO BE IN THE MIDDLE FOR I/P VARIABLE, SO NO SKEWNESS
hist(`Calories Consumed`) ###THERE SEEMS TO BE NO SKEWNESS IN THE I/P DATA
#FROM THE ABOVE I/P DATA SEEMS TO BE NORMALLY DISTRIBUTED



#scatter plot to check for direction & linearity
plot(`Calories Consumed`,`Weight gained (grams)`)
#Direction = +ve
#Graph is Linear




#Correlation Coefficient (r)
cor(`Calories Consumed`,`Weight gained (grams)`) 
#Strength = Based on co-relation co-efficient value (r)
#r=0.946991 which is >0.85. This means that Calories Consumed is a good input to predict output or else need to choose another variable
#It is strong positive co-relation
  
# Simple Linear Regression model
reg_model <- lm(`Weight gained (grams)` ~ `Calories Consumed`) # lm(Y ~ X)
summary(reg_model)
#Y=B0+B1X=-625.75236+(0.42016)X
#SINCE P VALUE <0.05 FOR BOTH B0 & B1, IT CAN BE USED IN THE PREDICTION MODEL. So, BO & B1 are both significant
#TO GUAGE THE STRENGTH OF THE MODEL, R-SQ VALUE SHD. BE >0.80. 
#R-SQ=0.8968 WHICH IS GOOD MODEL
#SO, WEIGHT GAINED=-625.75236+(0.42016)(CALORIES CONSUMED) IS THE MODEL FOR POINT ESTIMATE

#SINCE IT IS A  POINT ESTIMATE, THE PROBABILITY ASSOCIATED WITH THIS IS ZERO.SO, NEED TO COME UP WITH CONFIDENCE INTERVAL & PREDICTION INTERVAL
confint(reg_model,level=0.95) #CONF. INTERVAL FOR 95% CONFIDENCE
#LOWER LIMIT, WEIGHT GAINED=-845.4266546+0.3305064(CALORIES CONSUMED)
#UPPER LIMIT , WEIGHT GAINED=-406.0780569+0.3305064(CALORIES CONSUMED)

#PREDICTION MODEL
predict(reg_model,interval="predict")
#WEIGHT GAINED=-625.75236+(0.42016)(CALORIES CONSUMED).IF we substitute calories here ,we will get the fitted values
#PREDICTION VALUE - ACTUAL VALUE = ERROR

##CHECK FOR OTHER TRANSFORMATIONS IN THE MODEL, IF IT RESULTS IN BETTER RSQUARE VALUE
# Logarthmic transformation
reg_model_log <- lm(`Weight gained (grams)` ~ log(`Calories Consumed`))
summary(reg_model_log)
#R-squared:  0.8077 has decreased


#EXPONENTIAL MODEL
reg_model_exp <- lm(log(`Weight gained (grams)`) ~ log(`Calories Consumed`))
summary(reg_model_exp)
##R-squared:  0.8465 which is less 0.8968 that we got in the model without transformation

#QUADRATIC MODEL
calories[,"CC_sq"]=`Calories Consumed`*`Calories Consumed`
reg_model_quad=lm(`Weight gained (grams)`~`Calories Consumed`+I(`Calories Consumed`^2),data=calories)
summary(reg_model_quad)
##R-squared:  0.9521. There is a significant increase in R-square value


#CUBIC MODEL
reg_model_cubic=lm(`Weight gained (grams)`~`Calories Consumed`+I(`Calories Consumed`^2)+I(`Calories Consumed`^3),data=calories)
summary(reg_model_cubic)
# R-squared:  0.9811 WHICH SEEMS TO BE THE HIGHEST AMONG ALL OTHER MODELS

#######################################################################################
#WE CAN CONCLUDE THAT THE CUBIC-MODEL WILL PREDICT THE OUTPUT WHICH WILL BE 98.11 CORRECT 
#######################################################################################

