#Source: "Multivariate and Propensity Score Matching Software with Automated Balance Optimization: 
#The Matching package for R"


library(Matching)
data("lalonde")
attach(lalonde)

Y = lalonde$re78

Tr = lalonde$treat

X <- cbind(age, educ, black, hisp, married, nodegr, re74, re75, u74, u75)
BalanceMatrix <- cbind(age, I(age^2), educ, I(educ^2), black, hisp,
                          + married, nodegr, re74, I(re74^2), re75, I(re75^2), u74, u75,
                          + I(re74 * re75), I(age * nodegr), I(educ * re74), I(educ * re75))

genout <- GenMatch(Tr = Tr, X = X, BalanceMatrix = BalanceMatrix, wait.generations = 25, nboots = 100,
                     pop.size = 250, int.seed = 3818, unif.seed = 3527)

saveRDS(genout, "GeneticMatchingDiamond.rda")

mout = Match(Y = Y, Tr = Tr, X = X, Weight.matrix = genout)

MatchBalance(Tr ~ age + I(age^2) + educ + I(educ^2) + black + hisp +
               + married + nodegr + re74 + I(re74^2) + re75 + I(re75^2) + u74 + u75 +
               + I(re74 * re75) + I(age * nodegr) + I(educ * re74) + I(educ * re75),
              data = lalonde, match.out = mout, nboots = 1000)
mout$est
