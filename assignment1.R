##############################
######## Assignment 1 ######## 
##############################

#### Exercise 1.1 ####

data_read_1 = read.table("/Users/rinusvangrunsven/Documents/Study/UvA/SSO/week2/birthweight.txt",header=TRUE)
# head(data_read_1) how does the data looks like
birthweight = data_read_1$birthweight

# a)
# compute histogram and qq-plot
par(mfrow=c(1,2));hist(birthweight);qqnorm(birthweight)
# compute total summary to get an idea of the data 
summary = summary(birthweight); summary
# compute the mean
m = round(mean(birthweight),3); m

# b) (see slide 12, lecture 2)
# compute the length
n=length(birthweight); n
# compute the standard deviation
s=sd(birthweight); s
# compute test statistic
t=qt(0.95,df=n-1); t
# compute confidence interval
c(m-t*s/sqrt(n),m+t*s/sqrt(n)) 

# c) (see slide 22, lecture 2)
# H0 = mean birthweight <= 2800
# H1 = mean birthweight > 2800
# 1-pt(t,df=n-1) # p-value --> check again what this means!
# compute T-test
t.test(birthweight,mu=2800,alt="g") 
# p-value < 0,05 so reject H0 --> mean birthweight is indeed bigger than 2800

# d)
# It's different because in b) we calculated a 90% confidence interval while the 
# T-test calculated a 95% confidence interval.
# It is one-sided because we wanted to check if the mean birthweight is bigger than 2800.
# In order to check this, we are interested in the one value and the rest of the values (this goes until infinity).
# (check better explanation given in the lecture!)


#### Exercise 1.2 ####

# No mention of distribution, standard deviation, variance
# Same as example at slide 27 from lecture 2 so we will use that exercise as our leading example 

# a)
pbinom(140, 200,0.90, lower.tail = TRUE, log.p = FALSE)

binom.test(140,200,0.90)

library(Hmisc)
binconf(x=140, n=200, alpha=.05)

prop.test(x = 140, n = 200,conf.level = .99,correct = FALSE))

#define proportion
p <- 140/200
#define significance level
a <- .01
#calculate 99% confidence interval
p + c(-qnorm(1-a/2), qnorm(1-a/2))*sqrt((1/100)*p*(1-p))

#### Exercise 1.4 ####

data_read_4 = read.table("/Users/rinusvangrunsven/Documents/Study/UvA/SSO/week2/austen.txt",header=TRUE)
head(data_read_4) # how does the data looks like
data_read_4

# a)
# unclear by whom Sense and Emma are written. Sand1 is written by Austen and Sand2 is written by an admirer.
# Our guess is that Sense and Emma ar written by Austen as well and we will use this assumption throughout the other excercises also. 
# The presumed goal (at least that's what we think) is to check to what extend the novels are all written in the 
# same way. This is done by checking if the words are evenly distributed/proportionally used. A test for homogeneity
# would be the most suitable option. 
# Another option would be to run a test for homogeneity and see if the results show a difference between the Austen-columns 
# (sense, emma, and sand1) on one side, and the admirer-clumn (sand2) on the other hand. 

# b) (see slide 13, lecture 4)
sense = data_read_4$Sense
emma = data_read_4$Emma
sand1 = data_read_4$Sand1
x=as.data.frame(matrix(c(sense, emma, sand1),ncol=3,nrow=6)) 
dimnames(x)=list(c("a","an", "this", "that", "with", "without"),c("sense","emma", "sand1")) 
x
z = chisq.test(x)
z
# p-value (0,1609) > 5% so not reject H0; this means that there isn't sufficient evidence to state that there's a 
# dependency or the distribution is non the same amongst row-variables. This means that Austen was not consistent in her writing.
# (is this last sentence correct?)
z$expected
z$observed
X2=sum((z$observed-z$expected)^2/z$expected)
X2
1-pchisq(X2,df=(6-1)*(3-1)) # same is the same as the p-value we got from the Chi-squared test.
residuals(z)  

# c)
sand2 = data_read_4$Sand2
x=as.data.frame(matrix(c(sense, emma, sand1, sand2),ncol=4,nrow=6)) 
dimnames(x)=list(c("a","an", "this", "that", "with", "without"),c("sense","emma", "sand1", "sand2")) 
x
z = chisq.test(x)
z
# p-value (0,1208) > 5% so not reject H0;
z$expected
z$observed
X2=sum((z$observed-z$expected)^2/z$expected)
X2
1-pchisq(X2,df=(6-1)*(4-1))
residuals(z)

# check if correct with just two columns
sand2 = data_read_4$Sand2
x=as.data.frame(matrix(c(sand1, sand2),ncol=2,nrow=6)) 
dimnames(x)=list(c("a","an", "this", "that", "with", "without"),c("sand1", "sand2")) 
x
z = chisq.test(x)
z
# p-value (0,1308) > 5% so not reject H0;
z$expected
z$observed
X2=sum((z$observed-z$expected)^2/z$expected)
X2
1-pchisq(X2,df=(6-1)*(4-1))
residuals(z)
