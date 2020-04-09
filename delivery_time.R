library(readr)
delivery_time= read_csv("D:\\Arjun\\DATA_SCIENCE-ASSIGNMENTS\\Simple Linear Regression\\delivery_time.csv")
View(delivery_time)
attach(delivery_time)
summary(delivery_time)

#EDA
boxplot(delivery_time)##THE MEDIAN OF THE BOXPLOT SEEMS TO BE IN THE MIDDLE FOR I/P VARIABLE, SO NO SKEWNESS IN THE DATA
hist(`Sorting Time`)#THERE IS NO SKEWNESS IN THE DATA FOR SORTING TIME

plot(`Sorting Time`,`Delivery Time`)#DIRECTION IS POSITIVE & LINEAR but scattered. So, It is moderate positive co-relation




#Correlation Coefficient (r)
cor(`Sorting Time`,`Delivery Time`) 
#r=0.8259973 which is close to 0.85 which means the co-relation between these 2 variables is moderate

# Simple Linear Regression model without transformation
reg_model <- lm(`Delivery Time` ~ `Sorting Time`) # lm(Y ~ X)
summary(reg_model)
#p-value is less than 0.05 for both variables which means they are both significant.Also, overall p-value:3.983e-06 which is less than 0.05
# R-squared:  0.6823 which is less and needs to be improved by applying transformations


########Logrithamic Model################
cor(log(`Sorting Time`),`Delivery Time`) #r=0.8339325 which is close to 0.85
reg_model_log <- lm(`Delivery Time` ~ log(`Sorting Time`))
summary(reg_model_log)
#log(delivery time)=1.160+9.043(Sorting time)
#R-squared:  0.6954 which can be improved


########## Exponential Model##################
cor(`Sorting Time`,log(`Delivery Time`)) # r=0.8431773 which is close to 0.85
reg_model_exp <- lm(log(`Delivery Time`) ~ `Sorting Time`)
summary(reg_model_exp)
#R-squared:  0.7109 which can be improved further
#log(delivery time)=2.12137+0.10555(Sorting time)
#to remove log of delivery time, add exponential at both sides. exp(log(delivery time))=exp(2.12137+0.10555(Sorting time))
#Therefore, Delivery_Time=exp(2.12137+0.10555(Sorting time))


##############################
# Polynomial model with 3 degree OR CUBIC MODEL
reg_model_cub <- lm(`Delivery Time` ~ `Sorting Time` + I(`Sorting Time`*`Sorting Time`)+ I(`Sorting Time`*`Sorting Time`*`Sorting Time`))
summary(reg_model_cub)
#p-value overall model is  9.586e-05 which less than 0.05
#R-squared:  0.7034 




###########################################################################
#That means, EXPONENTIAL model gives the best results which will predict the output which will be 71.09% accurate
###########################################################################

