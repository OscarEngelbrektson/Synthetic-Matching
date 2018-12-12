library(Matching)
data("lalonde")
attach(lalonde)

library(rgenoud)

Y = lalonde$re78

Tr = lalonde$treat

genout <-  readRDS("~/Documents/GitHub/Synthetic-Matching/GeneticMatchingDiamond.rda")

mout = Match(Tr = Tr, X = X, M = 5, replace = TRUE, ties = FALSE, Weight.matrix = genout)

summary(mout)

length(mout$index.treated)
mout$index.control

#1st i = 1 + 5*(i-1)
#2nd i = 2 + 5*(i-1)
#3rd i = 3 + 5*(i-1)
#4th i = 4 + 5*(i-1)
#5th i = 5 + 5*(i-1)

#upper i = i*5



controls <- lalonde[mout$index.control[1:5],]
treat <- lalonde[mout$index.treated[1],]

fitness_func = function(w){
  loss = treat[,c(1:8,10:11)] - (w[1]*controls[1,c(1:8,10:11)] + w[2]*controls[2,c(1:8,10:11)] + w[3]*controls[3,c(1:8,10:11)] + w[4]*controls[4,c(1:8,10:11)] + w[5]*controls[5,c(1:8,10:11)])
  loss_squared <- loss*loss
  return(sum(loss_squared))
}

domain_matrix <- cbind(rep(0,5),rep(1,5))
genoud <- genoud(fitness_func, nvars = 5, Domains = domain_matrix, boundary.enforcement = 2,
       pop.size = 30,
       wait.generations = 5, hard.generation.limit = 20,)

genoud$par
head(lalonde)

#THIS IS IT
synthcontrol <- genoud$par[1]*controls[1,] + genoud$par[2]*controls[2,] + genoud$par[3]*controls[3,] + genoud$par[4]*controls[4,] + genoud$par[5]*controls[5,]

normalcontrol <- controls[1,]*0.20 + controls[2,]*0.20 +controls[3,]*0.20 +controls[4,]*0.20 +controls[5,]*0.20

synthcontrol
normalcontrol
treat

t.test(treat,synthcontrol)
t.test(treat,normalcontrol)





#Take units used as potential controls
#Remove "character"-type colums to allow multiplication
foo_covariates = foo[186:190,-c(1, ncol(foo1))]

#Create matrix of weights, one for each column
weights_per_cunit = synth.tables$tab.w["w.weights"]
weights = matrix(weights_per_cunit[rep(1:nrow(weights_per_cunit), each=ncol(foo_covariates)),], 
                 nrow=nrow(weights_per_cunit), ncol = ncol(foo_covariates), byrow = T)

summed_weights = colSums(weights)#Check weights per column sum to 1
summed_weights #adds to 0.999 for each - can we improve this rounding error?

dim(foo1_covariates) == dim(weights) #Check dimensions match

weighted_averages = weights*foo1_covariates  #Mutliply datasets to get weighted averages
synthetic_control = colSums(weighted_averages) #sum columns to generate synthetic control
synthetic_control #We know have a synthetic control. Bitches!
