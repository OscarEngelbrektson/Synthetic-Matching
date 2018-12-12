library(Matching)
data("lalonde")
attach(lalonde)

library(rgenoud)

Y = lalonde$re78

Tr = lalonde$treat

X <- cbind(age, educ, black, hisp, married, nodegr, re74, re75, u74, u75)
BalanceMatrix <- cbind(age, I(age^2), educ, I(educ^2), black, hisp,
                       + married, nodegr, re74, I(re74^2), re75, I(re75^2), u74, u75,
                       + I(re74 * re75), I(age * nodegr), I(educ * re74), I(educ * re75))

genout <- GenMatch(Tr = Tr, X = X, BalanceMatrix = BalanceMatrix,
                   pop.size = 1000, int.seed = 3818, unif.seed = 3527)


mout = Match(Y = Y, Tr = Tr, X = X, Weight.matrix = genout)

fitness_func = function(w1,w2,w3,w4,w5){
  loss = (treat[1,] - (w1*control1[1,] + w2*control2[1,] + w3*control3[1,] + w4*control4[1,] + w5*control5[1,]))^2
  return(loss)
}

genoud(fitness_func, 4, domains = , boundary.enforcement = 1)


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
