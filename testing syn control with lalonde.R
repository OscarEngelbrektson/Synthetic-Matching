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

dummy_data_id <- as.character(c(1,1,221,221,364,364,412,412))
dummy_time <- c(1,2,1,2,1,2,1,2) #dummy time units
dummy_name <- as.numeric(c(1,1,2,2,3,3,4,4)) #dummy unts

foo1 <- cbind(dummy_data_id, dummy_name,foo1,dummy_time)



foo1$data_id <- as.character(rownames(foo1)) #id.nums NAMES
foo1

typeof(foo1$dummy_data_id)


dataprep.out <-
  dataprep(foo = foo1,
           predictors = c("age","education") ,
           predictors.op = "mean" ,
           time.predictors.prior = 1,
           dependent = "re78",
           unit.variable = "dummy_name",
           unit.names.variable = "data_id",
           time.variable = "dummy_time",
           treatment.identifier = 1,
           controls.identifier = 2:4,
           time.optimize.ssr = 1,
  )
