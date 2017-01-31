#1a
retail = read.csv('retailsales.csv')
plot(retail$percent_chginGDP, retail$percent_chginRetailSales)

retailreg = lm(percent_chginRetailSales~percent_chginGDP, data=retail)
summary(retailreg)

#1b
predictions = predict(retailreg)
se = sqrt(sum((retail$percent_chginRetailSales-predictions)^2)/8)
# se = 1.601517
# ChangeInRetail = 3.4255 + 0.7306 * (changeInGDP) + 1.602

#1c
# estimated change per one percent increase in GDP is 0.7306

#1d
interval1 = 0.7306 + qt(0.025, 8) * 0.4301
interval2 = 0.7306 - qt(0.025, 8) * 0.4301
print(interval1)
print(interval2)
# 95 percent confidence interval = [-0.2612124, 1.722412]

#1e
interval1 = 0.7306 + qt(0.05, 8) * 0.4301
interval2 = 0.7306 - qt(0.05, 8) * 0.4301
print(interval1)
print(interval2)
# 90 percent confidence interval = [-0.06919161, 1.530392]

#1f
summary(retailreg)
#H0 : slope = 0
#H1: slope not equal to 0
# t value
0.7306/0.4301 
#p-value
2 * pt(-1.699, 8)
# cannot reject the null hypothesis, 0.1278 > 0.05

#2a
newdata=data.frame(percent_chginGDP=3.0)
predict(retailreg, newdata=newdata, se.fit=TRUE)
# 5.617346 percent change in Retail Sales

#2b
se = sqrt(0.5177329^2+ 1.601517^2)
#se = 1.683123
interval1 = 5.617346 + qt(0.025, 8) * se
interval2 = 5.617346 - qt(0.025, 8) * se
print(interval1)
print(interval2)
# 95 percent confidence level for estimate = [1.736057, 9.498635]

#2c
interval1 = 5.617346 + qt(0.01, 8) * se
interval2 = 5.617346 - qt(0.01, 8) * se
print(interval1)
print(interval2)
# 98 percent confidence level for estimate = [0.7422476, 10.49244]

#2d
1-pnorm(8.5, 5.617346, sd=1.683123)
# Probability that change in retail sales will be greater than 8.5 percent = 0.04338555 (or 4.338 %)

#2e
# Slope is not statistically significant.
# Furthermore, the regression SE is 1.601517 and results in a large confidence interval
# relative to estimated mean of 5.617346, introducing
# noise and suggesting that the model is not good for prediction.

#3a
salaries = read.csv('salaries.csv')
plot(salaries$Years_Experience, salaries$Salary)

salreg = lm(Salary~Years_Experience, data=salaries)
summary(salreg)

#3b
predictions = predict(salreg)
se = sqrt(sum((salaries$Salary-predictions)^2)/39)
#se = 20556.09
# Salary = 30256.0 + 1853.6 * (Years_Experience) + 20560

#3c 
# Effect of one more year of experience is $1853.6 more added to salary.

#3d
interval1 = 1853.6 + qt(0.025, 39) * 387.1
interval2 = 1853.6 - qt(0.025, 39) * 387.1
print(interval1)
print(interval2)
# 95 percent confidence interval = [1070.62, 2636.58]

#3e
interval1 = 1853.6 + qt(0.005, 39) * 387.1
interval2 = 1853.6 - qt(0.005, 39) * 387.1
print(interval1)
print(interval2)
# 99 percent confidence interval = [805.3668, 2901.833]

#3f
2 * pt(-4.789, 39)
#strong evidence against the null hypothesis: 2.431676e-05 < 0.05

#4a
newdata=data.frame(Years_Experience=9)
predict(salreg, newdata=newdata, se.fit=TRUE)
# Salary of worker with nine years' experience = $46938.54

#4b
se = sqrt(4306.287^2+ 20556.09^2)
interval1 = 46938.54 + qt(0.025, 39) * se
interval2 = 46938.54 - qt(0.025, 39) * se
print(interval1)
print(interval2)
# 95 percent confidence level for estimate = [4457.362, 89419.72]

#4c
se = sqrt(4306.287^2+ 20556.09^2)
interval1 = 46938.54 + qt(0.125, 39) * se
interval2 = 46938.54 - qt(0.125, 39) * se
print(interval1)
print(interval2)
# 75 percent confidence level for estimate = [22413.23, 71463.85]

#4d
se = sqrt(4306.287^2+ 20556.09^2)
interval1 = 46938.54 + qt(0.05, 39) * se
interval2 = 46938.54 - qt(0.05, 39) * se
print(interval1)
print(interval2)
# 90 percent confidence level for estimate = [11552.27, 82324.81]

#4e
# Slope is statistically significant according to the summary, so we can be fairly confident
# that work experience is related to salary.
# However, regression SE is 20556.09, resulting in very large confidence intervals around the predictions
# and creating a lot of noise. The model is not very good for making predictions due to the noise.

#5a
euros = read.csv('eurodata.csv')
plot(euros$DKUnemplRate, euros$DKWageGrowth)
DKreg = lm(DKWageGrowth~DKUnemplRate, data=euros)
plot(euros$BEUnemplRate, euros$BEWageGrowth)
BEreg = lm(BEWageGrowth~BEUnemplRate, data=euros)
summary(DKreg)
summary(BEreg)

#5b
#equation for Denmark
predictions = predict(DKreg)
se = sqrt(sum((euros$DKWageGrowth-predictions)^2)/40)
#se = 3.48
# DKWage = 11.8021 - 0.8270 * (DKUnempl) + 3.48 

#equation for Belgium
predictions = predict(BEreg)
se = sqrt(sum((euros$BEWageGrowth-predictions)^2)/40)
#se = 3.376
# BEWage = 11.6489 - 0.7329 * (BEUnempl) + 3.376

#5c
# Denmark:
# For every one percentage point increase in unemployment, there is a 0.8270 increase in wages.

# Belgium:
# For every one percentage point increase in unemployment, there is a 0.7329 increase in wages.

#5d
# Denmark: 
interval1 = 0.8270 + qt(0.025, 40) * 0.1958
interval2 = 0.8270 - qt(0.025, 40) * 0.1958
print(interval1)
print(interval2)
# 95 percent confidence interval for Denmark: [0.4312734, 1.222727]

# Belgium:
interval1 = 0.7329 + qt(0.025, 40) * 0.1585
interval2 = 0.7329 - qt(0.025, 40) * 0.1585
print(interval1)
print(interval2)
# 95 percent confidence interval for Belgium: [0.4125596, 1.05324]

#5e
newdata=data.frame(DKUnemplRate=3)
predict(DKreg, newdata=newdata, se.fit=TRUE)
# Wage growth predicted to increase by 9.321161.

newdata=data.frame(BEUnemplRate=3)
predict(BEreg, newdata=newdata, se.fit=TRUE)
# Wage growth predicted to increase by 9.45022.

#5f
# Denmark
se = sqrt(0.6161303^2+ 3.47998^2)
interval1 = 9.321161 + qt(0.05, 40) * se
interval2 = 9.321161 - qt(0.05, 40) * se
print(interval1)
print(interval2)
# 90 percent confidence interval = [3.37026, 15.27206] 

# Belgium
se = sqrt(0.7219863^2+ 3.375837^2)
interval1 = 9.45022 + qt(0.05, 40) * se
interval2 = 9.45022 - qt(0.05, 40) * se
print(interval1)
print(interval2)
# 90 percent confidence interval = [3.637265, 15.26318]


