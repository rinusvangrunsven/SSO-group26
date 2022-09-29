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
summary(lm(total~takers+expend+ratio)) # multipled R^2 = 0.8227, p-value = 0.0171 --> choose this third round
summary(lm(total~takers+expend+salary)) # multipled R^2 = 0.8196, p-value = 0.8527
# Fourth round
summary(lm(total~takers+expend+ratio+salary)) # multipled R^2 = 0.8246, p-value = 0.496 --> stop, so do not add variable salary

# Step-down method
# First round
summary(lm(total~takers+expend+ratio+salary)) # remove expend since it has the highest p-value which is not significant
# Second round
summary(lm(total~takers+ratio+salary)) # all variables have a significant p-value, so stop removing

# need to write interpretation

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
