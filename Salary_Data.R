library(readr)
salary_data = read_csv("D:\\Arjun\\DATA_SCIENCE-ASSIGNMENTS\\Simple Linear Regression\\Salary_Data.csv")
View(salary_data)
attach(salary_data)

#EDA
boxplot(salary_data)
hist(YearsExperience) ##THE I/P DATA SEEMS WITHOUT SKEWNESS

plot(YearsExperience,Salary) # IT IS MODERATELY POSITIVE



#Calculate Correlation coefficient value 
cor(YearsExperience,Salary)
#r=0.9782416 which is > 0.85. This means experience is a good  param to predict salary

# Simple Linear Regression model
reg_model=lm(Salary~YearsExperience)
summary(reg_model)
#Here , R-squared:  0.957,P-value <0.05, Salary=25792.2+945*(YearsExperience)


#LOGATHMIC MODEL
reg_model_log=lm(Salary~log(YearsExperience))
summary(reg_model_log)
#R-squared:  0.8539

#EXPONENTIAL MODEL
reg_model_exp=lm(log(Salary)~YearsExperience)
summary(reg_model_exp)
#R-squared:  0.932

#CUBIC MODEL
reg_model_cub=lm(Salary~YearsExperience+I(YearsExperience^2)+I(YearsExperience^3),data=salary_data)
summary(reg_model_cub)
#R-squared:  0.9636

#######################################################################################
#THE CUBIC MODEL WILL PREDICT THE OUTPUT WHICH WILL BE 96.36% ACCURATE 
#######################################################################################