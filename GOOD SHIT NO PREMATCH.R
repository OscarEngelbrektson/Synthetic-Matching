library(Synth)
data("synth.data")
View(synth.data)

foo <- read.dta("nsw_dw.dta")

foo
foo1 <- rbind(foo[1,],foo[1,],foo[186:190,],foo[186:190,])

unit_num <- c(1,1,186,186,187,187,188,188,189,189,190,190)
year <- c(1974,1975,1974,1975,1974,1975,1974,1975,1974,1975,1974,1975) #dummy time units
name <- c("treat1","treat1","control1","control1","control2","control2","control3","control3","control4","control4","control5","control5") #dummy unts

foo1 <- cbind(foo1,unit_num, year,name)
foo1

foo1$name <- as.character(foo1$name)
foo1$unit_num <- as.numeric(foo1$unit_num)

foo1

dataprep.out <-
  dataprep(foo = foo1,
           predictors = c("age","education") ,
           predictors.op = "mean",
           time.predictors.prior = c(1974:1975),
           dependent = "re78",
           unit.variable = "unit_num",
           unit.names.variable = "name",
           time.variable = "year",
           treatment.identifier = 1,
           controls.identifier = 186:190,
           time.optimize.ssr = c(1974:1975)
  )

###################################################
synth.out <- synth(data.prep.obj = dataprep.out,
                   method = "BFGS")

# Get result tables
synth.tables <- synth.tab(
  dataprep.res = dataprep.out,
  synth.res = synth.out
) 

#Create matrix of weights, one for each column
print(synth.tables)
weights_per_cunit = synth.tables$tab.w["w.weights"]
weights = matrix(weights_per_cunit[rep(1:nrow(weights_per_cunit), each=ncol(foo1)),], 
                 nrow=nrow(weights_per_cunit), ncol = ncol(foo1), byrow = T)
weights
foo1
typeof(foo1[1,2])
#foo1_covariates = foo1[which(typeof(foo1)!="character")]
#foo1_covariates = foo1[,c(-1, ncol(foo1))]
foo1_covariates = foo1[,-1]
foo1_covariates


weights*foo1_covariates

test = foo[186,]*0.496 



