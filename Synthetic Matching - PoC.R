library(purrr)
library(Matching)

#Get data
foo <- read.csv("https://course-resources.minerva.kgi.edu/uploaded_files/mke/00086677-3767/peace.csv")

# remove rows with missing data (there are better ways to handle missing data)
foo <- foo[-c(which(is.na(foo$pbs2l)), which(is.na(foo$pbs5l)), 
              which(is.na(foo$logcost)), which(is.na(foo$trnsfcap))), ]

#Define outcome
Y2 = foo$pbs2l #Lenient PB success 2 years after the war 
Y5 = foo$pbs5l #Lenient PB success 5 years after the war 

#Define Treatment
Tr <- rep(0, length(foo$untype))
Tr[which(foo$untype != "None")] <- 1

Xs = cbind(foo$wartype, foo$logcost, foo$wardur, foo$factnum, foo$factnum2, 
           foo$trnsfcap, foo$treaty, foo$develop, foo$exp, foo$decade)

#load.Rdata2( filename="genout_CI.RDATA", "genout" )
#a =load(file = "genout_CI.RDATA")

genout = GenMatch(Tr=Tr, X=Xs, estimand="ATT",
                  pop.size=100, max.generations=10, wait.generations=5, nboots = 10)

mout = Match(Y = Y2, Tr = Tr, X = Xs,
             Weight.matrix = genout,
             BiasAdjust = TRUE, 
             replace = TRUE,
             ties = TRUE,
             M = 5)

#length(mout$index.control) #165 w M=5, 33 w M=1

mout$index.control[1:]
mout$index.treated


for(i in 1:165){
  
  
  
}
