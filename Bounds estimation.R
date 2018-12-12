test = rnorm(500, 100, 16)
test = sort(test)
test
no = rep(0,250)
yes = rep(1,250)
training = c(no,yes)
counterfactual = c(yes,no)
salary = test*100+training*15000+rnorm(500,0,3000)
plot(test,salary)

lm.no=lm(salary[1:250] ~ test[1:250]) 
lm.yes=lm(salary[251:500] ~ test[251:500]) 
abline(lm.no, lwd=3) 
abline(lm.yes, lwd=3)

cutoff=(test[25]+test[26])/2

lm.no$coefficients[1]

effect = lm.yes$coefficients[1]+cutoff*lm.yes$coefficients[2]-lm.no$coefficients[1]-cutoff*lm.no$coefficients[2]

countersalary=salary = test*100+counterfactual*15000+rnorm(500,0,3000)
plot(test,countersalary)

df <- data.frame(test,training,salary)

write.csv(df, file = 'data.csv')


#------------------------------------------------
x <- runif(1000,-1,1)
y <- 5 + 3 * x + 2*(x >= 0) + rnorm(1000)


d <- x > 0 
reg1 <- lm(y ~ x + d)
summary(reg1)
confint(reg1)

install.packages("rdrobust") 
library(rdrobust) 
rdplot(y,x) 
summary(rdrobust(y,x))

#---------------------------------
cache = rep(0, 100)
for (percentage in 1:100){
  tr_unemployed = 149*percentage*0.01
  tr_employed = 149-tr_unemployed
  
  c_unemployed = 22*percentage*0.01
  c_employed = 22-c_unemployed
  cache[percentage] = (143+tr_employed)/(143 + 81 + 149) - (55*c_employed)/(108+22)
}
length(cache)
plot(1:100, cache, ylab = "treatment effect", xlab = "Percentage unemployed amongst non-located")

(143)/(143 + 81 + 149) - (55)/(108+22)

