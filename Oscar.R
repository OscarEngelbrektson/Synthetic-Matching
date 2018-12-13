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

#genout <- GenMatch(Tr = Tr, X = X, BalanceMatrix = BalanceMatrix,
 #                  pop.size = 100, int.seed = 3818, unif.seed = 3527)

genout = readRDS("GeneticMatchingDiamond.rda")


mout = Match(Tr = Tr, X = X, M = 5, replace = TRUE, ties = FALSE, Weight.matrix = genout)

summary(mout)
mout$index.treated[1:10]
mout$index.control[1:10]
length(mout$index.treated)


synthetic.controls <- data.frame(matrix(NA, nrow = max(mout$index.treated), ncol = ncol(lalonde)))
head(synthetic.controls)

for (i in 1:length(mout$index.treated)){
  treat <- lalonde[mout$index.treated[i],]
  controls <- lalonde[mout$index.control[(1+5*(i-1)):(i*5)],]
  print(treat)
  print(controls)
  
  fitness_func = function(w){
    loss = treat[,c(1:11)] - (w[1]*controls[1,c(1:11)] + w[2]*controls[2,c(1:11)] + w[3]*controls[3,c(1:11)] + w[4]*controls[4,c(1:11)] + w[5]*controls[5,c(1:11)])
    loss_squared <- loss*loss
    return(sum(loss_squared))
  }
  
  domain_matrix <- cbind(rep(0,5),rep(1,5))
  genoud <- genoud(fitness_func, nvars = 5, Domains = domain_matrix, boundary.enforcement = 2,
                   pop.size = 30,
                   wait.generations = 5, 
                   hard.generation.limit = 20,
                   gradient.check = FALSE, print.level=0)
  
  this_control <- genoud$par[1]*controls[1,] + 
    genoud$par[2]*controls[2,] + 
    genoud$par[3]*controls[3,] + 
    genoud$par[4]*controls[4,] + 
    genoud$par[5]*controls[5,]
  print(this_control)
  
  synthetic.controls[i, ] <- this_control
  
}
head(synthcontrol)


normalcontrol <- controls[1,]*0.20 + controls[2,]*0.20 +controls[3,]*0.20 +controls[4,]*0.20 +controls[5,]*0.20

synthcontrol
normalcontrol
treat

t.test(treat,synthcontrol)
t.test(treat,normalcontrol)




synth <- matrix(nrow = 5, ncol = 12)
for (i in 1:5) {
  synth[i,] <- genoud$par[i]*controls[i,]
  synth <- colSums(synth)
}
genoud$par

#THIS IS IT
genoud$par[1]*controls[1,] + genoud$par[2]*controls[2,] + genoud$par[3]*controls[3,] + genoud$par[4]*controls[4,] + genoud$par[5]*controls[5,]
treat[1]



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
