##SENSITIVITY ANALYSIS, USING UNCORRELATED INFORMATIVE PRIOR
results.UI = blr(sum.yi=yij, mi=rep(1, length(yij)), X, nsamples=100000, beta.0=0, sigma0=diag(1000, nrow=p), jumpingvar=12) #Diffuse prior.
mcmc.samples.UI = mcmc.list(mcmc(t(results.UI$posterior.samples)))
ESS.UI = sapply(mcmc.samples.UI, effectiveSize)
AcceptanceRate.UI = length(unique(results.UI$posterior.samples[1,]))/length(results.UI$posterior.samples[1,])
plot(mcmc.samples.UI)

##SENSITIVITY ANALYSIS, USING CORRELATED INFORMATIVE PRIOR
Sigma0 = diag(p)
for(i in 1:(p-1)) Sigma0[i, i+1] = .5
for(j in 1:(p-1)) Sigma0[j+1, j] = .5
results.CI = blr(sum.yi=yij, mi=rep(1, length(yij)), X, nsamples=100000, beta.0=0, sigma0=Sigma0, jumpingvar=9) #Diffuse prior.
mcmc.samples.CI = mcmc.list(mcmc(t(results.CI$posterior.samples)))
ESS.CI = sapply(mcmc.samples.CI, effectiveSize)
AcceptanceRate.CI = length(unique(results.CI$posterior.samples[1,]))/length(results.CI$posterior.samples[1,])

#RUNNING THE MCMC WITH UNINFORMATIVE PRIOR
results.U = blr(sum.yi=yij, mi=rep(1, length(yij)), X, nsamples=100000, beta.0=0, sigma0=diag(p), jumpingvar=11) #Stepwise, informative prior.
mcmc.samples.U = mcmc.list(mcmc(t(results.U$posterior.samples)))
ESS.U = sapply(mcmc.samples.U, effectiveSize)
AcceptanceRate.U = length(unique(results.U$posterior.samples[1,]))/length(results.U$posterior.samples[1,])

PPC.Graphs(Y=yij, X, "chol", m=10000, mi, results.UI)
PPC.Graphs(Y=yij, X, "chol", m=10000, mi, results.CI)
PPC.Graphs(Y=yij, X, "chol", m=10000, mi, results.U)

PPC.Graphs(Y=yij, X, "stab.glu", m=10000, mi, results.U)
PPC.Graphs(Y=yij, X, "hdl", m=10000, mi, results.U)
PPC.Graphs(Y=yij, X, "age", m=10000, mi, results.U)
PPC.Graphs(Y=yij, X, "time.ppn", m=10000, mi, results.U)