library(readr)
emp_data = read_csv("D:\\Arjun\\DATA_SCIENCE-ASSIGNMENTS\\Simple Linear Regression\\emp_data.csv")
View(emp_data)
attach(emp_data)

#EDA
boxplot(emp_data)#THE MEDIAN FOR SALARY_HIKE IS IN THE MIDDLE, SO NO SKEWNESS
hist(emp_data$Salary_hike)##SEEMS SLIGHTLY RIGHT SKEWED 


plot(Salary_hike,Churn_out_rate) #as the salary_hike increases, churnout_rate increases





# Correlation coefficient value for Salary Hike and Churn_out_Date
cor(Salary_hike,Churn_out_rate)
#r=-0.9117216 which has a Strong Negative Co-Relation

# Simple model without using any transformation
reg_model=lm(Churn_out_rate~Salary_hike)
summary(reg_model)
#R-squared:  0.8312 which is >0.80 which is good. 
#Let us check whether it can be further increased

#LOGARTHMIC MODEL
reg_model_log=lm(Churn_out_rate~log(Salary_hike))
summary(reg_model_log)
#R-squared:  0.8486

# Exponential model 
reg_model_exp=lm(log(Churn_out_rate)~(Salary_hike))
summary(reg_model_exp)
#Churn_out_rate=log(6.6383000+(-0.0013963)*Salary_Hike)
#R-squared:  0.8735 which is also good


#QUADRATIC MODEL
emp_data[,"sh_sq"]=Salary_hike*Salary_hike
reg_model_quad=lm(Churn_out_rate~Salary_hike+I(Salary_hike^2),data=emp_data)
summary(reg_model_quad)
# R-squared:  0.9737

###########################################################################
#CAN CONCLUDE THAT, QUADRATIC MODEL will predict the output which will be 97.37% ACCURATE
###########################################################################
