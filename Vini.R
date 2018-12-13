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
                   #pop.size = 100, int.seed = 3818, unif.seed = 3527)

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
  
  fitness_func = function(w){
    loss = treat[,c(1:8,10:11)] - (w[1]*controls[1,c(1:8,10:11)] + w[2]*controls[2,c(1:8,10:11)] + w[3]*controls[3,c(1:8,10:11)] + w[4]*controls[4,c(1:8,10:11)] + w[5]*controls[5,c(1:8,10:11)])
    loss_squared <- loss*loss
    return(sum(loss_squared))
  }
  
  domain_matrix <- cbind(rep(0,5),rep(1,5))
  genoud <- genoud(fitness_func, nvars = 5, Domains = domain_matrix, boundary.enforcement = 2,
                   pop.size = 30,
                   wait.generations = 5, 
                   hard.generation.limit = 20,
                   gradient.check = FALSE)
  
  cat("Before")
  cat("The sum of weights is,", sum(genoud$par))
  print(genoud$par)
  genoud$par <- genoud$par / sum(genoud$par)
  cat("After")
  cat("The sum of weights is,", sum(genoud$par))
  genoud$par
  
  
  this_control <- genoud$par[1]*controls[1,] + 
    genoud$par[2]*controls[2,] + 
    genoud$par[3]*controls[3,] + 
    genoud$par[4]*controls[4,] + 
    genoud$par[5]*controls[5,]
  
  synthetic.controls[i, ] <- this_control
  
}

head(synthetic.controls,5)

saveRDS(synthetic.controls,"LalondeSyntheticControls.rda")

synths = readRDS("LalondeSyntheticControls.rda")
View(synths)

TreatedLalonde = subset(lalonde, lalonde$treat == 1)

colnames(synths) = colnames(lalonde)
SynthLalonde = rbind(TreatedLalonde, synths)

SynthLalonde = subset(SynthLalonde, is.na)
is.na(SynthLalonde)

Tr = SynthLalonde$treat
#attach(SynthLalonde)
X <- cbind(SynthLalonde$age, SynthLalonde$educ, SynthLalonde$black, 
           SynthLalonde$hisp, SynthLalonde$married, SynthLalonde$nodegr, 
           SynthLalonde$re74, SynthLalonde$re75, SynthLalonde$u74, SynthLalonde$u75)


mout = Match(Tr = Tr, X = X, M = 1, replace = F, ties = FALSE)

MatchBalance(Tr ~ age + I(age^2) + educ + I(educ^2) + black + hisp +
               + married + nodegr + re74 + I(re74^2) + re75 + I(re75^2) + u74 + u75 +
               + I(re74 * re75) + I(age * nodegr) + I(educ * re74) + I(educ * re75),
             data = SynthLalonde, match.out = mout, nboots = 1000)
mout$est

