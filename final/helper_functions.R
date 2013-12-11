#Compute posterior of MVN prior and Binom likelihood with probit link. Return log posterior.
posterior.prob = function(Beta, y, beta.0, sigma.0, X){
  #Computes sum of Pr(Beta)~N(beta.0, sigma) )
  logprior = dmvnorm(Beta, mean=beta.0, sigma=sigma.0, log=TRUE)
  loglikelihood = sum(dbinom(y, size=1, prob=as.numeric(pnorm( Beta %*% t(X))), log=TRUE))
  logprior+loglikelihood
}
posterior.prob = cmpfun(posterior.prob)

MH = function(current, candidate, uniform, y, sigma.0, beta.0, X){ 
  #Computes the Metropolis-Hastings criterion. If the criterion exceeds a uniform (everything logged, btw),
  #return the new value. Otherwise, return the old one.
  r.top = posterior.prob(candidate, y, beta.0, sigma.0, X)
  r.bottom = posterior.prob(current, y, beta.0, sigma.0, X)
  r = r.top - r.bottom
  if (uniform < r) return(list(sample=candidate, accept=TRUE))
  else return( list(sample=current, accept=FALSE) )
}
MH = cmpfun(MH)

mh.probit <- function(y, X, beta.0=NA, sigma.0=NA, niter=1E5, burnin=1000, print.every=1000, retune=100, dataset=NA, verbose=FALSE)
{	
  if( !is.na(dataset) ){
      Data = read.table(dataset, header=TRUE)
      y=as.numeric(Data[,1])
      X=as.matrix(Data[,2:ncol(Data)])
    }
  p = ncol(X)
  n = nrow(X)
  #X = scale(X, scale=FALSE) #Rescale the covariates, for stability.
  if (all(is.na(beta.0))) beta.0 = rep(0, p)
  if (all(is.na(sigma.0))) sigma.0 = 1E8 * diag(p)
  
  beta.samples = matrix(NA, nrow=p, ncol=niter)
  acceptances = rep(NA, niter)
  acceptances[1] = TRUE
  
  uniform = log(runif(niter)) #Generate all uniforms at once, to utilize R's vectorization.
  probit_reg = glm(y~X-1, family=binomial(link="probit")) #Run a probit regression on the data.
  beta.samples[,1] = probit_reg$coefficients #Start at the Probit Regression MLE
  covar = vcov(probit_reg) #Covariance of Frequentist probit regression. To be scaled by a constant later.
  
  #Used to adjust the variance in the burn-in period. Jump less if space is large.
  var.scale = ifelse(p > 3, .01, 1)
  
  #Generate a lot of proposal jumps at once. If Z ~ N(0, S), then Z + b ~ N(b, S) for const b!
  #Vectorization makes things fast, yo.
  perturbations = matrix(nrow=p, ncol=niter)
  perturbations[,1:retune] = t(rmvnorm(retune, mean=rep(0,p), sigma=var.scale * covar))
  
  #If parameter space is big (p>3), I would expect to be rejected more.
  low   = ifelse(p>3, .10, .20)
  high  = ifelse(p>3, .30, .50)
  ideal = ifelse(p>3, .20, .30)
  
  for(l in 1:(niter-1)) {
    #Retune loop:
    if (l %% retune == 0 & l <= burnin) {
      acceptance = mean(acceptances[(l-retune):(l-1)])
      
      if (acceptance <= low | acceptance >= high) {
        scaling = exp(3 * (acceptance-ideal))
        var.scale = var.scale * scaling #If acceptance is greater than ideal, scale it down.
        if (verbose) {
          print(paste("Acceptance rate in iterations", l-retune+1, "through", l, "was", round(acceptance,3), sep=" "))
          print(paste("Scaling variance by:", round(scaling, 3), sep=" "))
        }
      }
      perturbations[,(l+1):(burnin+l)] = t(rmvnorm( burnin  , mean=rep(0,p), sigma=var.scale * covar))
    }
    
    #First legit obs loop:
    if ( l == (burnin+1) ) {
      #Generate all proposal perturbations by the final variance, then proceed as usual.
      if (verbose) print(paste("Starting the first non-burned observation at l=", l, sep=""))
      perturbations[ , l:niter] = t(rmvnorm( niter-l+1 , mean=rep(0,p), sigma=var.scale * covar))
    }
    
    current = beta.samples[,l]
    candidate = current+perturbations[,l]
    foo = MH(current, candidate, uniform[l], y, sigma.0, beta.0, X)
    beta.samples[,l+1] = foo$sample
    acceptances[l+1] = foo$accept
    
    if (l %% print.every == 0) print(paste("Current Metropolis iteration:", l, sep=" "))
  }
  beta.samples.burned = t(beta.samples[,(burnin+1):niter])
  return ( mcmc(beta.samples.burned) )
}
mh.probit = cmpfun(mh.probit)

#Does Gibbs Sampling for Bayesian Probit Regression.
gibbs_probit = function(y, X, beta_0 = NA, Sigma_0_inv=NA, niter=1E5, burnin=1E3, print.every=100, Datapath=NULL)
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
    if((i %% print.every) == 0) print(paste("Current Gibbs iteration:", i, sep=""))
    #Sample (z|y,beta)
    z_mean = X %*% beta[i,]
    z = rtnorm(n, mean=z_mean, sd=rep(1, n), lower=lowers, upper=uppers)
    
    #Sample (beta | z, y)
    if (all(Sigma_0_inv==0)) { beta_mean = lm.fit(X, z)$coefficients } else{
      beta_mean = beta_var %*% (Sigma_0_inv %*% beta_0 + t(X) %*% z)}
    beta[i+1, ] = rmvnorm(1, mean=beta_mean, sigma=beta_var)
  }
  return(mcmc(beta[(burnin+1):niter, ]))
}
gibbs_probit = cmpfun(gibbs_probit)