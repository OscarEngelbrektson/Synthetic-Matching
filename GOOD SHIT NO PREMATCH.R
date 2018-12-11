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

#Take units used as potential controls
#Remove "character"-type colums to allow multiplication
foo_covariates = foo[186:190,-c(1, ncol(foo1))]

#Create matrix of weights, one for each column
weights_per_cunit = synth.tables$tab.w["w.weights"]
weights = matrix(weights_per_cunit[rep(1:nrow(weights_per_cunit), each=ncol(foo_covariates)),], 
                 nrow=nrow(weights_per_cunit), ncol = ncol(foo_covariates), byrow = T)

summed_weights = colSums(weights) #
summed_weights #adds to 1 for each - works well

dim(foo1_covariates)
dim(weights)

weighted_averages = weights*foo1_covariates[,-ncol(foo1_covariates)]  #Remove "name" to allow multiplication
synthetic_control = colSums(weighted_averages)
synthetic_control
