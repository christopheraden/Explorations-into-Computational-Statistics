library(mvtnorm)
library(coda)
library(compiler)
library(MCMCpack)

if (Sys.info()['sysname'] %in% c("Linux", "Mac")) {
  setwd("~/Dropbox/sta250/Assignments/HW1/BayesLogit/")
} else { 
  stop("You're using a different architecture than I am. setwd() so that the code is in the pwd!")
}
########################################################################################
########################################################################################
## Handle batch job arguments:
args <- commandArgs(TRUE) #1-indexed version is used now.

cat(paste0("Command-line arguments:\n"))
print(args)

####
# sim_start ==> Lowest simulation number to be analyzed by this particular batch job
###

#######################
sim_start <- 1000
length.datasets <- 200
beta.0=c(0,0)
sigma.0.inv = diag(2)
#######################

if (length(args)==0){
  sinkit <- FALSE
  sim_num <- sim_start + 1
  set.seed(1330931)
} else {
  # Sink output to file?
  sinkit <- TRUE
  # Decide on the job number, usually start at 1000:
  sim_num <- sim_start + as.numeric(args[1])
  # Set a different random seed for every job number!!!
  set.seed(762*sim_num + 1330931)
}

# Simulation datasets numbered 1001-1200

########################################################################################
########################################################################################

# Read data corresponding to appropriate sim_num:
Data = read.csv(paste("./data/blr_data_", sim_num, ".csv", sep=""))
pars = read.csv(paste("./data/blr_pars_", sim_num, ".csv", sep=""))

# Extract X and y:
X = as.matrix(Data[,3:4], ncol=2)
y = Data$y
m = Data$n

#################################################
# Set up the specifications:
p = ncol(X)
beta.0 <- matrix(rep(0,p))
sigma.0 <- sigma.0.inv <- diag(p)

niter <- 5E5
burnin = 1E4
print.every = niter/10
retune = 1E3
verbose = TRUE
local = FALSE #If running a cluster job, don't the piece to get raw samples--just quantiles.
thin = 100 #Take only every 100 observations to generate the quantiles.
#################################################

# Fit the Bayesian model:
source("./helper_functions.R")
if (local) {
Posterior_Draws = bayes.logreg.samples(m, y, X, beta.0, sigma.0, niter, burnin, print.every, retune, verbose)
chain = mcmc(Posterior_Draws) #Markov Chain objects, to make plots and ESS easy.
effectiveSize(chain) #Effective Sample Size for each variable.
plot(chain) #Convergence plots
}

# Extract posterior quantiles...
posterior_quantiles = bayes.logreg(m, y, X, beta.0, sigma.0.inv, niter, burnin, print.every, retune, verbose, thin)

# Write results to a (99 x p) csv file...
write.csv(posterior.quantiles, file=paste("posterior_quantiles_", sim_num, ".csv", sep=""))

# Go celebrate.
stop("You can't tell me what to do--this is America!")