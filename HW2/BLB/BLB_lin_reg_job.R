setwd("~/sta250/Explorations-into-Computational-Statistics/HW2/BLB/")

mini <- FALSE #Flag for mini dataset.

#============================== Setup for running on Gauss... ==============================#

args <- commandArgs(TRUE)
cat("Command-line arguments:\n", args)

####
# sim_start ==> Lowest possible dataset number
###

###################
sim_start <- 1000
###################

if (length(args)==0){
  sim_num <- sim_start + 1
  set.seed(121231)
} else {
  # SLURM can use either 0- or 1-indexing...
  # Lets use 1-indexing here...
  sim_num <- sim_start + as.numeric(args[1])
  sim_seed <- (762*(sim_num-1) + 121231)
}

cat(paste("\nAnalyzing dataset number ",sim_num,"...\n\n",sep=""))
#============================== Run the simulation study ==============================#
library(bigmemory)
library(biganalytics)

# I/O specifications:
datapath <- "/home/pdbaines/data/" #Location of data
outpath <- "output/" #Location of output
rootfilename = ifelse(mini, "blb_lin_reg_mini", "blb_lin_reg_data") # mini or full?

gam = .7 #Shrinking factor
s = 5 #Number of bootstrap subsets
r = 50 #Number of bootstrap replicates

# Find r and s indices:
s_index = rep(1:5, each=r)[sim_num - sim_start]
r_index = rep(1:50, s)[sim_num - sim_start]

# Attach big.matrix :
full.data = attach.big.matrix(dget(paste(datapath, rootfilename, '.desc', sep='')),backingpath=datapath)
n = describe(full.data)@description$totalRows #We've already computed n! This is an O(1) lookup instead of O(n).
b = n^gam #Size of each subset

load(paste("dump/sample_", s_index, '.Rdata', sep='')) #Make sure you've run the pre-file before this!
reduced = as.data.frame(reduced)

counts = rmultinom(1, n, rep(1/b,b) ) #Sample the b indeces n times.
names(reduced)[ncol(reduced)]="Y" #Change the last column name into Y, for ease of lm.

# Fit lm:
coefs = lm(Y ~ . - 1, data=reduced, weights=counts)$coefficients

# Output file:
outfile = paste0("output/","coef_",sprintf("%02d",s_index),"_",sprintf("%02d",r_index),".txt")

# Save estimates to file:
write.table(coefs, file=outfile, sep=',', row.names=FALSE, col.names=FALSE)
