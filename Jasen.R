library(Matching)
data("lalonde")
attach(lalonde)

library(rgenoud)

Y = lalonde$re78

Tr = lalonde$treat

genout <-  readRDS("~/Documents/GitHub/Synthetic-Matching/GeneticMatchingDiamond.rda")

mout = Match(Tr = Tr, X = X, M = 5, replace = TRUE, ties = FALSE, Weight.matrix = genout)

summary(mout)

max(mout$index.treated)
mout$index.control




controls <- lalonde[mout$index.control[1:5],]
treat <- lalonde[mout$index.treated[1],]

fitness_func = function(w){
  loss = treat[,c(1:8,10:11)] - (w[1]*controls[1,c(1:8,10:11)] + w[2]*controls[2,c(1:8,10:11)] + w[3]*controls[3,c(1:8,10:11)] + w[4]*controls[4,c(1:8,10:11)] + w[5]*controls[5,c(1:8,10:11)])
  loss_squared <- loss*loss
  return(sum(loss_squared))
}

domain_matrix <- cbind(rep(0,5),rep(1,5))
genoud <- genoud(fitness_func, nvars = 5, Domains = domain_matrix, boundary.enforcement = 2,
       pop.size = 100,
       wait.generations = 5, hard.generation.limit = 20,
       gradient.check = FALSE)

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

synthetic.controls <- data.frame(NA, nrow = max(mout$index.treated), ncol = ncol(lalonde))
for (i in 1:length(mout$index.treated)){
  treat <- lalonde[mout$index.treated[i],]
  controls <- lalonde[mout$index.control[1+5*(i-1):i*5],]
  
  fitness_func = function(w){
    loss = treat[,c(1:11)] - (w[1]*controls[1,c(1:11)] + w[2]*controls[2,c(1:11)] + w[3]*controls[3,c(1:11)] + w[4]*controls[4,c(1:11)] + w[5]*controls[5,c(1:11)])
    loss_squared <- loss*loss
    return(sum(loss_squared))
  }
  
  domain_matrix <- cbind(rep(0,5),rep(1,5))
  genoud <- genoud(fitness_func, nvars = 5, Domains = domain_matrix, boundary.enforcement = 2,
                   pop.size = 50,
                   wait.generations = 3, hard.generation.limit = 10,
                   gradient.check = FALSE)
  
  synthetic.controls[i,] <- genoud$par[1]*controls[1,] + 
                            genoud$par[2]*controls[2,] + 
                            genoud$par[3]*controls[3,] + 
                            genoud$par[4]*controls[4,] + 
                            genoud$par[5]*controls[5,]
  
}
