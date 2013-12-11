#setwd("~/Dropbox/sta250/Assignments/Final")
library(msm)
library(coda)
library(compiler)
library(mvtnorm)
# source("sim_probit.R") #Generate the data
source("helper_functions.R")

nds = 5 #Number of datasets
MH_times = Gibbs_times = numeric(nds)
filenames = paste0("data_0", 1:nds, ".txt")
MH_samples = Gibbs_samples = list()
length(MH_samples) = length(Gibbs_samples) = nds

for (i in 1:nds){ #Times saved seperately so I could write the paper asyncronously. 
    Gibbs_times[i] = system.time({Gibbs_samples[[i]] = gibbs_probit(Sigma_0_inv=diag(8)/1E8, niter=3000, burnin=500, Datapath=filenames[i])})["elapsed"]
    MH_times[i] = system.time({MH_samples[[i]] = mh.probit(sigma.0=1E8 * diag(8), niter=24000, burnin=500, dataset=filenames[1])})["elapsed"]
}

save(Gibbs_times, file="Gibbs_times.Rdata")
save(MH_times, file="MH_times.Rdata")

#Convergence plots.
MH_obj = MH_samples[[2]]
Gibbs_obj = Gibbs_samples[[2]]

setEPS()
postscript('MH_Trace.eps')
plot(MH_obj, density=FALSE)
dev.off()

setEPS()
postscript('Gibbs_Trace.eps')
plot(Gibbs_obj, density=FALSE)
dev.off()

library(xtable)
(effectiveSize(MH_samples[[2]]) / effectiveSize(Gibbs_samples[[2]])) ^-1
xtable(cbind(MH = autocorr.diag(MH_obj)[2,] , Gibbs = autocorr.diag(Gibbs_obj)[2,]), digits=3)
xtable(cbind(MH_Eff = effectiveSize(MH_samples[[2]]), Gibbs_Eff = effectiveSize(Gibbs_samples[[2]])))
xtable(cbind(Gibbs=Gibbs_times, MH=MH_times))

setEPS()
postscript('Timings.eps')
plot(1:nds, log(Gibbs_times), xlab="Dataset Number", ylab="Log-Time", main="Probit Regression Runtime, Gibbs in Red, MH in Green", type='l', lty=4, col='red',
     ylim=c(min(log(c(Gibbs_times, MH_times))), max(log(c(Gibbs_times, MH_times))) ) )
lines(1:nds, log(MH_times), lty=3, col='green')
dev.off()
