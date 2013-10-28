library(mvtnorm) #Generate MV Normals
library(coda) #MCMC datatype and Eff Samp Size
library(compiler) #Bytecode Compiler--speeds up for loops
library(MASS) #For Truehist
library(xtable) #To display Latex for variable selection

if (Sys.info()['sysname'] %in% c("Linux", "Darwin")) {
  setwd("~/Dropbox/sta250/Assignments/HW1/BayesLogit/")
} else { 
  stop("You're using a different architecture than I am. setwd() so that the code is in the pwd!")
}

# Read data corresponding to appropriate sim_num:
cancer = read.table("breast_cancer.txt", sep="", header=TRUE)


X = model.matrix(diagnosis ~ ., data=cancer) #Making the design matrix
p = ncol(X)
y = as.numeric(cancer$diagnosis)-1 #Benign=0, Malignant=1;
m = rep(1, length(y))

#################################################
# Set up the specifications:
beta.0 <- matrix(rep(0,p))
sigma.0 <- diag(p)*1000
niter <- 5E5
burnin = 1E4
print.every = niter/10
retune = 1E2
verbose = TRUE
#################################################

# Fit the Bayesian model:
source("./helper_functions.R")
Posterior_Draws = bayes.logreg.samples(m, y, X, beta.0, sigma.0, niter, burnin, print.every, retune, verbose)

chain = mcmc(Posterior_Draws)
ESS = effectiveSize(chain)
corrs = acf(chain, lag.max=1, plot=FALSE) #Compute lag-1 autocorrelations
diag_acf = numeric(p) 
for (i in 1:p) { diag_acf[i] = corrs$acf[2,i,i] } #Take only the non cross-autocorrs.

#Let's thin the draws to reduce the autocorrelation.
chain_thinned = mcmc(Posterior_Draws[seq.int(1, (niter-burnin), by=100), ])
ESS_thinned = effectiveSize(chain_thinned)
corrs_thinned = acf(chain_thinned, lag.max=1, plot=FALSE) #Compute lag-1 autocorrelations
diag_acf_thinned = numeric(p) 
for (i in 1:p) { diag_acf_thinned[i] = corrs_thinned$acf[2,i,i] } #Take only the non cross-autocorrs.

#Plotting the thinned, final chain.
plot(chain_thinned, density=FALSE, ask=FALSE)
Posterior_Quantiles = apply(chain_thinned, 2, quantile, probs=c(.025, .975))
xtable(Posterior_Quantiles, caption="95% Central Credible Interval", align="|cccccccccccc|")

#Making the Posterior Predictive Check graph.
PPC.Graphs(Y, X)