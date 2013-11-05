setwd("~/sta250/Explorations-into-Computational-Statistics/HW2/BLB/")

pre_job=FALSE #Flag to produce sub-samples of big data.
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

# Load packages:
library(bigmemory)
library(biganalytics)
library(fastmatch) #Hashing functionality for subsetting the big dataset.

# I/O specifications:
datapath <- "/home/pdbaines/data/"
#datapath = "~/Dropbox/sta250/Assignments/HW2/BLB/"
outpath <- "output/"

# mini or full?
rootfilename = ifelse(mini, "blb_lin_reg_mini", "blb_lin_reg_data")

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

# Bootstrap dataset:
#Generate the s samples beforehand and store them.
if (pre_job) { 
  for (i in 1:s) {
    n.subset = sample(n, b, replace=FALSE)
    reduced = as.matrix( full.data[fmatch(n.subset, seq.int(n)), ])  #fmatch: Faster subsetting using a hash table.
    write.table(reduced, file=paste('dump/sample_', i, '.csv', sep=''), col.names=FALSE, row.names=FALSE, sep=',')
  }
    quit(save="no") #Finished writing samples--quit the program. Re-run with pre_job=FALSE.
}

reduced = read.table(paste("dump/sample_", s_index, '.csv', sep=''), header=FALSE, sep=',')
counts = rmultinom(1, n, rep(1/b,b) ) #Sample the b indeces n times.
names(reduced)[ncol(reduced)]="Y" #Change the last column name into Y, for ease of lm.

# Fit lm:
coefs = lm(Y ~ . - 1, data=reduced, weights=counts)$coefficients

# Output file:
outfile = paste0("output/","coef_",sprintf("%02d",s_index),"_",sprintf("%02d",r_index),".txt")

# Save estimates to file:
write.table(coefs, file=outfile, sep=',', row.names=FALSE, col.names=FALSE)
