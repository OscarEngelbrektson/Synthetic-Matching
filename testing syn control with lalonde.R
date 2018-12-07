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
foo1 <- foo1[rep(seq_len(nrow(foo1)), each=2),]

dummy_time <- c(1,2,1,2,1,2,1,2)
cbind(foo1,dummy_time)
foo1$data_id <- rownames(foo1)
foo1

dataprep.out <-
  dataprep(predictors = c("age","education") ,
           predictors.op = "mean" ,
           time.predictors.prior = 1,
           ),
           dependent = "re78",
           unit.variable = "data_id",
           unit.names.variable = "data_id",
           time.variable = "year",
           treatment.identifier = 1,
           controls.identifier = 3,5,7,
           time.optimize.ssr = 1,
  )
