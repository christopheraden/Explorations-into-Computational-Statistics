library(bigmemory)
library(biganalytics)
if (!require(fastmatch)) { install.packages("fastmatch", repos='http://cran.cnr.berkeley.edu/'); library(fastmatch)}
library(fastmatch) #Hashing functionality for subsetting the big dataset.

gam = .7 #Shrinking factor
s = 5 #Number of bootstrap subsets
r = 50 #Number of bootstrap replicates
mini <- FALSE #Flag for mini dataset.

datapath <- "/home/pdbaines/data/" #Location of data
outpath <- "output/" #Location of output
rootfilename = ifelse(mini, "blb_lin_reg_mini", "blb_lin_reg_data") # mini or full?

# Attach big.matrix :
full.data = attach.big.matrix(dget(paste(datapath, rootfilename, '.desc', sep='')),backingpath=datapath)
n = describe(full.data)@description$totalRows #We've already computed n! This is an O(1) lookup.  
b = n^gam #Size of each subset

#Generate the s samples beforehand and store them.
for (i in 1:s) {
  n.subset = sample(n, b, replace=FALSE)
  reduced = as.matrix( full.data[fmatch(n.subset, seq.int(n)), ])  #fmatch: Faster subsetting using a hash table.
  save(reduced, file=paste('dump/sample_', i, '.Rdata', sep=''))
}
quit(save="no") #Finished writing samples--quit the program.