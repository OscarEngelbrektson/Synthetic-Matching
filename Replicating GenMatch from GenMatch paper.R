library(Matching)
data("lalonde")
attach(lalonde)

Y = lalonde$re78

Tr = lalonde$treat

X <- cbind(age, educ, black, hisp, married, nodegr, re74, re75, u74, u75)
BalanceMatrix <- cbind(age, I(age^2), educ, I(educ^2), black, hisp,
                          + married, nodegr, re74, I(re74^2), re75, I(re75^2), u74, u75,
                          + I(re74 * re75), I(age * nodegr), I(educ * re74), I(educ * re75))
genout <- GenMatch(Tr = Tr, X = X, BalanceMatrix = BalanceMatrix,
                     pop.size = 1000)