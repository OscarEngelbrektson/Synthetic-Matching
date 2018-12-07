library(foreign)
library(Matching)
foo <- read.dta("nsw_dw.dta")

Tr = foo$treat
Y = foo$re78
X= cbind(foo[,c(3:4)])

colnames(foo)

glm1 <- glm(treat ~ age + education, data = foo, family = binomial)

rr  <- Match(Y=Y, Tr=Tr, X= cbind(foo[,c(3:4)]), M=3, caliper = 0.5,replace = TRUE);
summary(rr)

mb  <- MatchBalance(treat ~ re74 + re75, data = foo, match.out=rr, nboots=10)

rr$index.treated[1:10]
rr$index.control[1:10]

length(which(foo$treat == 1))
which(foo$treat == 0)

cbind(rr$index.treated,rr$index.control)

foo1 <- rbind(foo[1,],foo[221,],foo[364,],foo[412,])

foo2 <- rbind(foo[1,],foo[221,],foo[364,],foo[412,])

a <- c(1,1,1,2,2,2)

dataprep.out <-
  dataprep(predictors = c("age","education") ,
           predictors.op = "mean" ,
           time.predictors.prior = 1,
           ),
           dependent = "gdpcap",
           unit.variable = "regionno",
           unit.names.variable = "regionname",
           time.variable = "year",
           treatment.identifier = 1,
           controls.identifier = 2:4,
           time.optimize.ssr = 1960:1969,
           time.plot = 1955:1997
  )
