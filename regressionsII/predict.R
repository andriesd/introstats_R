#1a
pizza = read.csv('pizzasales.csv')
plot(pizza$Competitor, pizza$Sales)
pizzareg = lm(Sales~Competitor, data=pizza)
summary(pizzareg)
# equation
# Sales = 1105.43 + -455.35 * (Competitor) + 310.3

# estimate for daily sales for store with no competition:
# Sales = 1105.43 - 455.35 * 0 + 310.3
# Sales = 1105.43 + 310.3
# Sales = 1415.73

#1b
# estimate for daily sales for store with competition
# set Competitor = 1
# Sales = 1105.43 -455.35 + 310.3
# Sales = 960.38

#1c
1415.73 - 960.38
# difference between estimates is 455.35
# this is equal to beta in the regression equation; beta is the difference of the two intercepts
# practically speaking, this gap indicates that the presence of competitors has a negative effect on sales

#1d
interval1 = -455.35 + qt(0.025, 48) * 88.92
interval2 = -455.35 - qt(0.025, 48) * 88.92
print(interval1)
print(interval2)
# 95 percent confidence interval for effect of competition on sales = [-634.1356, -276.5644]

#1e
# R squared statistic is 0.3533
# therefore 35.33 percent of variance in sales can be explained using only competitor variable

#2a
incomereg = lm(Sales~Income, data=pizza)
summary(incomereg)
# Sales = 48.7208 + 2.6972 * Income + 224.1

newdata=data.frame(Income = 200)
predict(incomereg, newdata=newdata, se.fit=TRUE)

# estimated daily sales for store with neighborhood income $200: $588.1696

#2b
newdata=data.frame(Income = 300)
predict(incomereg, newdata=newdata, se.fit=TRUE)

# estimated daily sales for store with neighborhood income $300: $857.8939


#2c
# $1 increase in income, sales go up $2.6972
# Therefore $100 increase in income, sales go up 100 * 2.6972
# Estimated impact of $100 difference in income on sales is $269.72

#2d
interval1 = 269.72 + qt(0.025, 48) * 27.78
interval2 = 269.72 - qt(0.025, 48) * 27.78
print(interval1)
print(interval2)
# 95 percent confidence level = [213.8646, 325.5754]

#2e
# R squared statistic is 0.6626
# therefore 66.26 percent of variance in sales can be explained using only income variable

#3
plot(pizza$Income, pizza$Sales)
abline(incomereg)

# Without the regression line, there is evidence that the points are grouped around two divergent lines 
# with two different slopes and intercepts here, suggesting that introducing 
# a dummy variable would improve the model (instead of just relying on the current regression for the model).

#4a
allreg = lm(Sales~Income+Competitor+Income*Competitor, data=pizza)
summary(allreg)
#Sales = 90.7005 + 3.2668 * Income - 11.1002 * Competitor - 1.2419 *(Income*Competitor) + 95.27

# the Competitor variable is not significant

#4b
newdata=data.frame(Income = 300, Competitor=0)
predict(allreg, newdata=newdata, se.fit=TRUE)
# Estimate for daily sales for store with no competition and neighborhood income $300 is $1070.75

#4c
newdata=data.frame(Income = 300, Competitor=1)
predict(allreg, newdata=newdata, se.fit=TRUE)
# Estimate for daily sales for store with competition and neighborhood income $300 is $687.0918

#5a
euro = read.csv('eurodata2a.csv')
euroreg = lm(WageGrowth~Unemployment+Belgium+Unemployment*Belgium, data=euro)
summary(euroreg)
# WageGrowth = 11.80212 - 0.82699 * Unemployment - 0.15320 * Belgium + 0.09409 *(Unemployment*Belgium) + 3.428

#5b
# equation for Belgium
# WageGrowth = 11.80212 - 0.82699 * Unemployment - 0.15320 + 0.09409 *(Unemployment) + 3.428
# WageGrowth = 11.64892 -0.7329 *Unemployment + 3.428

#5c
# equation for Denmark
# WageGrowth = 11.80212 - 0.82699 * Unemployment + 3.428

#5d
# One percentage point increase in employment rate in Belgium results in 0.7329 decrease in wage growth.

#5e
# One percentage point increase in employment rate in Denmark results in 0.82699 decrease in wage growth.

#5f
# Estimated difference in how unemployment rate relates to growth between countries is 0.09409

#5g
interval1 =  0.09409 + qt(0.025, 80) * 0.25122
interval2 =  0.09409 - qt(0.025, 80) * 0.25122
print(interval1)
print(interval2)
# 95 percent confidence interval = [-0.4058537, 0.5940337]

#5h 
newdata=data.frame(Unemployment = 3.0, Belgium = 1)
predict(euroreg, newdata=newdata, se.fit=TRUE)
# Estimate for growth rate in wages for Belgium in year with 3% unemployment is 9.45022 

newdata=data.frame(Unemployment = 3.0, Belgium = 0)
predict(euroreg, newdata=newdata, se.fit=TRUE)
# Estimate for growth rate in wages for Denmark in year with 3% unemployment is 9.321161 

#5i
#Belgium
se = sqrt(0.7332073^2+ 3.428304^2)
interval1 =  9.45022 + qt(0.05, 80) * se
interval2 =  9.45022 - qt(0.05, 80) * se
print(interval1)
print(interval2)
# 90 percent confidence interval = [3.616078, 15.28436]

#Denmark
se = sqrt(0.606981^2+ 3.428304^2)
interval1 =  9.321161 + qt(0.05, 80) * se
interval2 =  9.321161 - qt(0.05, 80) * se
print(interval1)
print(interval2)
# 90 percent confidence interval = [3.527308, 15.11501]

#6a
euro2 = read.csv('eurodata2b.csv')
euro2reg = lm(WageGrowth~Unemployment+Germany+Unemployment*Germany, data=euro2)
summary(euro2reg)

# WageGrowth = 20.3510 -1.0691*Unemployment -10.4729 * Germany + 0.1329 (Unemployment*Germany) + 4.205

#6b
# Germany 
# WageGrowth = 20.3510 -1.0691*Unemployment -10.4729 + 0.1329 (Unemployment) + 4.205
# WageGrowth = 9.8781  - 0.9362 * Unemployment + 4.205

#6c
# Greece
# WageGrowth = 20.3510 - 1.0691*Unemployment  + 4.205

#6d
# One percentage increase in unemployment rate results in decrease of 0.9362 in wage growth.

#6e
# One percentage increase in unemployment rate results in decrease of 1.0691 in wage growth.

#6f
# Estimated difference in how wage growth relates to unemployment between countries is 0.1329

#6g
interval1 =  0.1329 + qt(0.025, 80) * 0.3092
interval2 =  0.1329 - qt(0.025, 80) * 0.3092
print(interval1)
print(interval2)
# 95 percent confidence interval = [-0.4824276, 0.7482276]

#6h
newdata=data.frame(Unemployment = 3.0, Germany = 1)
predict(euro2reg, newdata=newdata, se.fit=TRUE)
# Estimate for growth rate in wages for Germany in year with 3% unemployment is 7.069501 

newdata=data.frame(Unemployment = 3.0, Germany = 0)
predict(euro2reg, newdata=newdata, se.fit=TRUE)
# Estimate for growth rate in wages for Greece in year with 3% unemployment is 17.14356 

#6i
# Germany
se = sqrt(0.710628^2+ 4.205486^2)
interval1 =  7.069501 + qt(0.05, 80) * se
interval2 =  7.069501 - qt(0.05, 80) * se
print(interval1)
print(interval2)
# 90 percent confidence interval = [-0.02816194, 14.16716]

# Greece
se = sqrt(0.9341422^2+ 4.205486^2)
interval1 =  17.14356 + qt(0.05, 80) * se
interval2 =  17.14356 - qt(0.05, 80) * se
print(interval1)
print(interval2)
# 90 percent confidence interval = [9.974536, 24.31258]