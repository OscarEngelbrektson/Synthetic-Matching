Tr = foo$treat
Y = foo$re78

colnames(foo1)

glm1 <- glm(treat ~ age + education + black + hispanic + married + nodegree, data = foo1, family = binomial)

rr  <- Match(Y=Y, Tr=Tr, X= cbind(foo[,c(3:4)]), M=5, caliper = 0.5,replace = TRUE);
summary(rr)


rr$index.treated[1:10]
rr$index.control[1:10]

cbind(rr$index.treated,rr$index.control)

foo2 <- rbind(foo[1,],foo[1,],foo[193,],foo[193,],foo[221,],foo[221,],foo[265,],foo[265,],foo[364,],foo[364,],foo[412,],foo[412,])
foo2

unit_num <- c(1,1,193,193,221,221,265,265,364,364,412,412)
year <- c(1974,1975,1974,1975,1974,1975,1974,1975,1974,1975,1974,1975) #dummy time units
name <- c("treat1","treat1","control1","control1","control2","control2","control3","control3","control4","control4","control5","control5") #dummy unts

foo2 <- cbind(foo2,unit_num, year,name)
foo2$name <- as.character(foo2$name)
foo2$unit_num <- as.numeric(foo2$unit_num)

foo2

dataprep.out <-
  dataprep(foo = foo2,
           predictors = c("age","black","hispanic","married") ,
           predictors.op = "mean",
           time.predictors.prior = c(1974:1975),
           dependent = "re78",
           unit.variable = "unit_num",
           unit.names.variable = "name",
           time.variable = "year",
           treatment.identifier = 1,
           controls.identifier = c(193,221,265,364,412),
           time.optimize.ssr = c(1974:1975)
  )


synth.out <- synth(data.prep.obj = dataprep.out,
                   method = "BFGS")

# Get result tables
synth.tables <- synth.tab(
  dataprep.res = dataprep.out,
  synth.res = synth.out
) 

# results tables:
print(synth.tables)
