data_sat = read.table("./sat.txt", header=TRUE)
data_sat
summary(data_sat)
c(length(data_sat))
expend = data_sat$expend
ratio = data_sat$ratio
salary = data_sat$salary
takers = data_sat$takers
total = data_sat$total
# **a)** 
# Step-up method
# First round
summary(lm(total~expend)) # multipled R^2 = 0,1448, p-value = 0.00641
summary(lm(total~ratio)) # multipled R^2 = 0.006602, p-value = 0.575
summary(lm(total~salary)) # multipled R^2 = 0.1935, p-value = 0.00139
summary(lm(total~takers)) # multipled R^2 = 0.787, p-value = <2e-16 --> choose this one first
# Second round
summary(lm(total~takers+expend)) # multipled R^2 = 0.8195, p-value = 0.00553 --> choose this one second
summary(lm(total~takers+ratio)) # multipled R^2 = 0.7991, p-value = 0.0982
summary(lm(total~takers+salary)) # multipled R^2 = 0.8056, p-value = 0.0394
# Third round
summary(lm(total~takers+expend+ratio)) # multipled R^2 = 0.8227, p-value = 0.3629 
summary(lm(total~takers+expend+salary)) # multipled R^2 = 0.8196, p-value = 0.8527
# stop after second round as the p-values are both not significant
# # Fourth round
# summary(lm(total~takers+expend+ratio+salary)) # multipled R^2 = 0.8246, p-value = 0.496 --> stop, so do not add variable salary

# Step-down method
# First round
summary(lm(total~takers+expend+ratio+salary)) # remove expend since it has the highest p-value that is not significant
# Second round
summary(lm(total~takers+ratio+salary)) # all variables have a significant p-value, so stop removing

# Both have got three variables, but numbers step-up method are: multiple-r^2 = 0.8195 and p-value = < 2.2e-16
# For the step-down method it's like this: multiple-r^2 = 0.8239 and p-value = < 2.2e-16
# As the multiple-r^2 is higher for the step-down method and the p-value is significant, we would choose this one. 

# **b)** 
takers2=(data_sat$takers)^2
takers;takers2
# Step-up method
# First round
summary(lm(total~expend)) # multipled R^2 = 0,1448, p-value = 0.00641
summary(lm(total~ratio)) # multipled R^2 = 0.006602, p-value = 0.575
summary(lm(total~salary)) # multipled R^2 = 0.1935, p-value = 0.00139
summary(lm(total~takers)) # multipled R^2 = 0.787, p-value = <2e-16 --> choose this one first
summary(lm(total~takers2)) # multipled R^2 = 0.6578, p-value = 9.28e-13
# Second round
summary(lm(total~takers+expend)) # multipled R^2 = 0.8195, p-value = 0.00553 
summary(lm(total~takers+ratio)) # multipled R^2 = 0.7991, p-value = 0.0982
summary(lm(total~takers+salary)) # multipled R^2 = 0.8056, p-value = 0.0394
summary(lm(total~takers+takers2)) # multipled R^2 = 0.8732, p-value = 8.96e-07 --> choose this one second
# Third round
summary(lm(total~takers+takers2+expend)) # multipled R^2 = 0.8859, p-value = 0.0285 --> choose this oen third
summary(lm(total~takers+takers2+ratio)) # multipled R^2 = 0.8738, p-value = 0.634
summary(lm(total~takers+takers2+salary)) # multipled R^2 = 0.8858, p-value = 0.029 
# Fourth round
summary(lm(total~takers+takers2+expend+ratio)) # multipled R^2 = 0.8887, p-value = 0.2936
summary(lm(total~takers+takers2+expend+salary)) # multipled R^2 = 0.8873, p-value = 0.466
# --> stop after third round as both p-values are not significant

# Step-down method
# First round
summary(lm(total~takers+takers2+expend+salary+ratio)) # remove salary since it has the highest p-value that is not significant
# Second round
summary(lm(total~takers+takers2+expend+ratio)) # remove ratio since it has the highest p-value that is not significant
# Third round
summary(lm(total~takers+takers2+expend))
# --> stop here since all the remaining variables have significant p-values.

# It is useful I would say as it is included on both models.

# **c)** 
# The step-up method has got four variables while the step-down method has only got three variables. We would opt for a simpler model and thus the latter.
# For the step-up method it's like this: multiple-r^2 = 0.8859 and p-value = < 2.2e-16
# For the step-down method it's like this: multiple-r^2 = 0.8859 and p-value = < 2.2e-16
# The values are exactly the same, so we would opt for the simpler model.

# **d)** 
summary(lm(total~takers+takers2+expend))
chosen_model = lm(total~takers+takers2+expend)
# model = 1052 - 6.381 * takers + 0.04741 * takers2 + 7.914 * expend
newxdata = data.frame(expend=5, takers2=625, salary=36000, takers=25)
predict(chosen_model,newxdata,interval="prediction",level=0.95)
# Fit = 961.5703; lwr = 907.6003; upr = 1015.54;


# 
# bodyfat = read.table("/Users/rinusvangrunsven/Documents/GitHub/SSO-group26/assignment2/bodyfat.txt", header=TRUE)
# Fat = bodyfat$Fat
# Triceps = bodyfat$Triceps
# Thigh = bodyfat$Thigh
# Midarm = bodyfat$Midarm
# # bodyfat
# # summary(bodyfat)
# bodyfatlm = lm(Fat~Triceps+Thigh+Midarm,data=bodyfat)
# summary(bodyfatlm)
# SSE=sum(residuals(bodyfatlm)^2)
# SSYY=sum((bodyfat$Fat-mean(bodyfat$Fat))^2);(SSYY-SSE)/SSYY
# qqnorm(residuals(bodyfatlm))
# summary(bodyfatlm)
# lm(formula=Fat~Triceps+Thigh+Midarm,data=bodyfat)
# 
# confint(bodyfatlm)
# confint(bodyfatlm,level=0.9)
# 
# summary(lm(Fat~Triceps))
