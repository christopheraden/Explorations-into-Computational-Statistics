library(ggplot2); library(rbenchmark)

bisection = function(f, l, u, tol=1E-6, niter=100, debug=FALSE){
  x = numeric(niter)
  for(t in 1:niter){
    x[t] = {l + u} / 2 #Set the value of c.
    
    if( abs(f(x[t])) < tol ) { #Bisection method reached convergence. Return val.
      print(paste("Converged in ", t, " steps.", sep=''))
      return(list(final=x[t], iterations=t, xvalues=x[1:t], fvalues=f(x[1:t])))
    }
    
    if ( f(x[t]) * f(l) < 0 ) { #If f(c) * f(l) < 0, u=c. Else, l=c.
      u=x[t]
    } else { l=x[t] }
    
    if (debug){ #Print pieces of Bisection calculation
      print(paste("(x_t, f(x_t), f(u), f(l), u, l ): (", 
            paste(round(c(x[t], f(x[t]), f(u), f(l), u, l), 5), collapse=", ", sep=""), ")", sep=''))}
  }
  return(NULL) #Did not converge!
}

newt.raph = function(f, df, initial=0, tol=1E-6, niter=100, debug=FALSE){
  x = numeric(niter+1)
  x[1] = initial
  for(t in 1:niter){
    x[t+1] = x[t] - f(x[t]) / df(x[t]) #Heart of the NR Algorithm
    
    if (debug){ #Print pieces of NR calculation.
      print(paste("(x_t, f(x_t), f'(x_t), x_{t+1}): (", 
        paste(round(c(x[t], f(x[t]), df(x[t]), x[t+1]), 6), collapse=", ", sep=""), ")", sep=''))}  
    
    if( abs(f(x[t+1])) < tol ) { #NR reached convergence. Return val.
      print(paste("Converged in ", t, " steps.", sep=''))
      return(list(final=x[t+1], iterations=t, xvalues=x[1:(t+1)], fvalues = f(x[1:(t+1)])))
    }
  }
  return(NULL) #Did not converge!
}

#Define log-likelihood and derivatives.
log.l = function(lambda) {125 * log(2+lambda) + 38 * log(1-lambda) + 34 * log(lambda) }
dlog.l = function(lambda) { 125 / (2+lambda) - 38 / (1-lambda) + 34 / lambda}
ddlog.l = function(lambda) {-125 / (2+lambda)^2 -38 / (1-lambda)^2 - 34 / lambda^2}

MLE.bisection = bisection(f=dlog.l, l=.5, u=.8, tol=1E-5, niter=100, debug=FALSE)
system.time(replicate(n=1000, bisection(f=dlog.l, l=.5, u=.8, tol=1E-5, niter=100, debug=FALSE)) )

benchmark(bisection(f=dlog.l, l=.5, u=.8, tol=1E-5, niter=100, debug=FALSE), 
          newt.raph(f=dlog.l, df=ddlog.l, initial=.8, tol=1E-5, niter=100, debug=FALSE), replications=1E4)

x = seq(0, 1, by=.0001)
likelihoods = log.l(x) #By grid search, for the truth.
loglike.x = data.frame(likelihoods, x)
ggplot(loglike.x, aes(x=x, y=likelihoods)) + geom_line() + 
  geom_vline(xintercept = MLE.bisection$final, color="blue", linetype="longdash") + 
  geom_vline(xintercept = MLE.NR$final, color="red", linetype="dashed") +
  geom_vline(xintercept = x[which.max(likelihoods)], color="green", linetype="twodash") +
  ggtitle("Log-Likelihood versus Lambda, NR estimate in red, Bisection in blue, true in green") +
  xlab("Lambda (between 0 and 1)")  + ylab("Log-Likelihood")