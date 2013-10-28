expit = function(X, Beta){
	exp(X %*% Beta) / {1+exp(X %*% Beta)}
}
expit = cmpfun(expit)

#Compute posterior of MVN prior and Binom likelihood. Return log posterior.
posterior.prob = function(Beta, y, beta.0, sigma.0, m, X){
	#Computes sum of Pr(Beta)~N(beta.0, sigma) )
	logprior = dmvnorm(Beta, mean=beta.0, sigma=sigma.0, log=TRUE)
	loglikelihood = sum(dbinom(y, size=m, prob=expit(X,Beta), log=TRUE))
	logprior+loglikelihood
}
posterior.prob = cmpfun(posterior.prob)

MH = function(current, candidate, uniform, m, y, sigma.0, beta.0, X){ 
	#Computes the Metropolis-Hastings criterion. If the criterion exceeds a uniform (everything logged, btw),
	#return the new value. Otherwise, return the old one.
	r.top = posterior.prob(candidate, y, beta.0, sigma.0, m, X)
	r.bottom = posterior.prob(current, y, beta.0, sigma.0, m, X)
	r = r.top - r.bottom
	if (uniform < r) return(list(sample=candidate, accept=TRUE))
	else return( list(sample=current, accept=FALSE) )
}
MH = cmpfun(MH)

"bayes.logreg.samples" <- function(m, y, X, beta.0=NA, sigma.0=NA, niter=10000, burnin=1000, print.every=1000, retune=100, verbose=TRUE)
{	
	p = ncol(X)
	n = nrow(X)
	if (all(is.na(beta.0))) beta.0 = rep(0, p)
	if (all(is.na(sigma.0))) sigma.0 = diag(p)
	Sigma.0.inv = solve(sigma.0) #Compute the inverse of precision--covariance. Speeds up inner loop
	
	beta.samples = matrix(NA, nrow=p, ncol=niter)
	acceptances = rep(NA, niter)
	acceptances[1] = TRUE

  uniform = log(runif(niter)) #Generate all uniforms at once, to utilize R's vectorization.
	logist_reg = glm(cbind(y, m-y) ~ X-1, family="binomial") #Run a logistic regression on the data.
  beta.samples[,1] = logist_reg$coefficients #Start at the Logistic Regression MLE
  covar = vcov(logist_reg) #Covariance of Frequentist logistic regression. To be scaled by a constant later.
	
  
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
		foo = MH(current, candidate, uniform[l], m, y, sigma.0, beta.0, X)
		beta.samples[,l+1] = foo$sample
		acceptances[l+1] = foo$accept
		
		if (l %% print.every == 0) print(paste("Current iteration:", l, sep=" "))
	}
	beta.samples.burned = t(beta.samples[,(burnin+1):niter])
}
bayes.logreg.samples = cmpfun(bayes.logreg.samples)


"bayes.logreg" <- function(m, y, X, beta.0=NA, sigma.0.inv=NA, niter=10000, burnin=1000, print.every=1000, retune=100, verbose=TRUE, thin=1) {
	# m: Vector containing the number of trials for each observation (of length n)
	# y: Vector containing the number of successes for each observation (of length n)
	# X: Design matrix (of dimension n×p)
	# beta.0: Prior mean for beta (of length p)
	# Sigma.0.inv: Prior precision (inverse covariance) matrix for beta (of dimension p×p)
	# niter: Number of iterations to run the MCMC after the burnin period
	# burnin: Number of iterations for the burnin period (draws will not be saved)
	# print.every: Print an update to the user after every period of this many iterations
	# retune: Retune the proposal parameters every return iterations. No tuning should be done after the burnin period is completed
	# verbose: If TRUE then print lots of debugging output, else be silent
  # thin: integer-- Take every "thin"^th observation from the chain.
  if (all(!is.na(sigma.0.inv))) {sigma.0 = solve(sigma.0.inv)}
  samples = bayes.logreg.samples(m, y, X, beta.0, sigma.0, niter, burnin, print.every, retune, verbose)
  Posterior_Draws = samples[seq.int(1, (niter-burnin), by=thin), ]
  
  cbind(quantile(Posterior_Draws[,1], probs=seq(.01, .99, length=99)), 
				quantile(Posterior_Draws[,2], probs=seq(.01,.99, length=99)))
}

PPC.Graphs = function(Y, X, m=1E5, mi=rep(1,length(Y)), posterior=chain_thinned){
  #Y: Response variable
  #X: Design matrix (explanatory variables)
  #m: Number of PPC datasets to generate.
  #mi: Number of trials for each level of the design matrix.
  #posterior: MCMC object or matrix (n x p) of posterior draws.
  
  n = length(Y)
  niter = nrow(posterior) #Number of MCMC draws from posterior
  
  ppc.datasets <- matrix(nrow=m, ncol=n)
  rownames(ppc.datasets) <- paste("PPC_",1:m,sep="")
  
  index = sample.int(niter, m, replace=TRUE) #Sample the posteriors we want.
  
  #Calculate p_i using posterior draw.
  #Rows: Probability at each X_r; Columns: PP dataset m_i.
  p.i = sapply(index, function(i) expit(X, posterior[i,])) 
  
  PPC_Data = sapply(1:m, function(i) rbinom(n, mi, p.i[,i]))
  PPC_mean = apply(PPC_Data, 2, mean) #Apply the mean to each PPC Dataset
  PPC_var = apply(PPC_Data, 2, var) #And the variance
  
  true_mean = mean(Y)
  true_var = var(Y)
  
  par(mfrow=c(2,2))
  truehist(PPC_mean,main="Posterior Predictive Check on Mean", xlab="", ylab="")
  abline(v=true_mean, col="red")
  
  truehist(PPC_var,main="Posterior Predictive Check on Variance", xlab="", ylab="")
  abline(v=true_var, col="red")
  
  NULL
}
PPC.Graphs = cmpfun(PPC.Graphs)