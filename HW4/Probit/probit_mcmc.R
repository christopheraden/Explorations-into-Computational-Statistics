#setwd("~/Dropbox/STA250/Assignments/HW4")

#Figure out if on AWS. If yes, do GPU comp, else CPU.
AWS = ifelse(Sys.info()["user"]=="ec2-user", TRUE, FALSE)

if (AWS){ #Do GPU stuff
  source("helper_functions.R")
  probit_mcmc_gpu = function(y, X, beta_0 = NA, Sigma_0_inv=NA, niter=2000, burnin=500, maxtries=250, threads_per_block=512L, verbose=FALSE, Datapath=NA)
  { #Noninformative prior--don't need to specify prior variance or mean.
    if( !is.na(Datapath) ){
      Data = read.table(Datapath, header=TRUE)
      y=as.numeric(Data[,1])
      X=as.matrix(Data[,2:ncol(Data)])
    }
    p = ncol(X)
    n = nrow(X)
    if (all(is.na(beta_0))) beta_0 = rep(0, p)
    if (all(is.na(Sigma_0_inv))) Sigma_0_inv = matrix(0, p, p)
    z = numeric(p)
    beta = matrix(NA, nrow=niter, ncol=p)
    beta[1, ] = lm.fit(X,y)$coefficients #Start with OLS.
    
    uppers = ifelse(y==1, Inf, 0) #If y=1, then z>0, so trunc points were (0, Inf)
    lowers = ifelse(y==1, 0, -Inf) #If y=0, z<=0, so trunc points were (-Inf, 0)
    
    #If Flat Prior, posterior variance is (X'X)^-1 .
    beta_var = solve(Sigma_0_inv + t(X) %*% X)
    
    for (i in 1:(niter-1)) {
      #Sample (z|y,beta)
      z_mean = X %*% beta[i,]
      z = tnorm.gpu(as.integer(n), z_mean, 1, lowers, uppers)
      
      #Sample (beta | z, y)
      if (all(Sigma_0_inv==0)) {beta_mean = lm.fit(X, z)$coefficients} else{
        beta_mean = beta_var %*% (Sigma_0_inv %*% beta_0 + t(X) %*% z)}
      beta[i+1, ] = rmvnorm(1, mean=beta_mean, sigma=beta_var)
    }
    return(mcmc(beta[(burnin+1):niter, ]))
  }
  probit_mcmc_gpu = cmpfun(probit_mcmc_gpu)
} else {
  library(msm)
  library(coda)
  library(compiler)
  library(mvtnorm)
  probit_mcmc_cpu = function(y, X, beta_0 = NA, Sigma_0_inv=NA, niter=2000, burnin=500, Datapath=NULL)
  { #Noninformative prior--don't need to specify prior variance or mean.
    if( !is.na(Datapath) ){
      Data = read.table(Datapath, header=TRUE)
      y=as.numeric(Data[,1])
      X=as.matrix(Data[,2:ncol(Data)])
    }
    p = ncol(X)
    n = nrow(X)
    if (all(is.na(beta_0))) beta_0 = rep(0, p)
    if (all(is.na(Sigma_0_inv))) Sigma_0_inv = matrix(0, p, p)
    z = numeric(p)
    beta = matrix(NA, nrow=niter, ncol=p)
    beta[1, ] = lm.fit(X,y)$coefficients #Start with OLS.
    
    uppers = ifelse(y==1, Inf, 0) #If y=1, then z>0, so trunc points were (0, Inf)
    lowers = ifelse(y==1, 0, -Inf) #If y=0, z<=0, so trunc points were (-Inf, 0)
  
    #If Flat Prior, posterior variance is (X'X)^-1 .
    beta_var = solve(Sigma_0_inv + t(X) %*% X)

    for (i in 1:(niter-1)) {
      #Sample (z|y,beta)
      z_mean = X %*% beta[i,]
      z = rtnorm(n, mean=z_mean, sd=rep(1, n), lower=lowers, upper=uppers)
  
      #Sample (beta | z, y)
      if (all(Sigma_0_inv==0)) {beta_mean = lm.fit(X, z)$coefficients} else{
        beta_mean = beta_var %*% (Sigma_0_inv %*% beta_0 + t(X) %*% z)}
      beta[i+1, ] = rmvnorm(1, mean=beta_mean, sigma=beta_var)
    }
    return(mcmc(beta[(burnin+1):niter, ]))
  }
  probit_mcmc_cpu = cmpfun(probit_mcmc_cpu)
}

source("sim_probit.R") #Generate the data
GPU_times = CPU_times = numeric(5)
filenames = paste0("data_0", 1:5, ".txt")

if(AWS){
  for (i in 1:5){ #Times saved seperately so I could write the paper asyncronously. 
    GPU_times[i] = system.time(probit_mcmc_gpu(Datapath=filenames[i]))["elapsed"]
    GPU = GPU_times[1:i]    
    save(GPU, file=paste0("GPU_probit_times_0",i,".RData"))
  }
} else{
  for (i in 1:5){ #Times saved seperately so I could write the paper asyncronously. 
    CPU_times[i] = system.time(probit_mcmc_cpu(Datapath=filenames[i]))["elapsed"]
    CPU = CPU_times[1:i]
    save(CPU, file=paste0("CPU_probit_times_0",i,".RData"))
  }  
}
