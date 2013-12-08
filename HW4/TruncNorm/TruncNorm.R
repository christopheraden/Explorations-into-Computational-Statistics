#setwd("~/Dropbox/STA250/Assignments/HW4")
source("helper_functions.R")

N=1E4
mu=2; sigma=1
lower=0; upper=1.5

#Part C
truth = mu + (dnorm(lower, mu, sigma) - dnorm(upper, mu, sigma)) * sigma / (pnorm(upper, mu, sigma)-pnorm(lower, mu, sigma))
samples.mean.gpu = mean(tnorm.gpu(N, mu, sigma, lower, upper))
print(paste0("GPU Sample Mean: ", round(samples.mean.gpu, 6), ". True Mean: ", round(truth, 6)))

#Part D
samples.mean.cpu = mean(rtnorm(N, mu, sigma, lower, upper))
print(paste0("CPU Sample Mean: ", round(samples.mean.cpu, 6), ". True Mean: ", round(truth, 6)))

#Part E
logn = 7
GPU_time = CPU_time = numeric(logn)

for(i in 1:logn){
  paste0("Working on Dataset i= ", i)
  GPU_time[i] = system.time( tnorm.gpu(as.integer(10^i), mu, sigma, lower, upper) )["elapsed"]
  CPU_time[i] = system.time( rtnorm(   as.integer(10^i), mu, sigma, lower, upper) )["elapsed"]
}

setEPS()
postscript('TruncNorm_Timings.eps')
plot(1:logn, CPU_time, xlab="log(N)", ylab="Time", main="TruncNorm Runtime, CPU in Red, GPU in Green", type='l', col='red',
     ylim=c(min(c(CPU_time, GPU_time)), max(c(CPU_time, GPU_time)) ) )
lines(1:logn, GPU_time, type='l', col='green')
dev.off()
save(GPU_time, CPU_time, file="Timings.Rdata")

#Part F:
GPU.samps = tnorm.gpu(N, mu, sigma, a=-Inf, b=Inf)
CPU.samps = rtnorm(N, mu, sigma, lower=-Inf, upper=Inf)

setEPS()
postscript('No_TruncNorm_Hist.eps')
par(mfrow=c(2,1))
truehist(GPU.samps, main="Histogram of N(2,1) GPU Samples", xlab="")
truehist(CPU.samps, main="Histogram of N(2,1) CPU Samples", xlab="")
dev.off()

#Part G:
GPU.samps = tnorm.gpu(N=1E4, mu=0.0, sigma=1.0, a=-Inf, b=-10)
CPU.samps = rtnorm(n=1E4, mean=0.0, sd=1.0, lower=-Inf, upper=-10)

setEPS()
postscript('Tail_TruncNorm_Hist.eps')
par(mfrow=c(2,1))
truehist(GPU.samps, main="Histogram of N(0,1,-Inf,-10) GPU Samples", xlab="")
truehist(CPU.samps, main="Histogram of N(0,1,-Inf,-10) CPU Samples", xlab="")
dev.off()