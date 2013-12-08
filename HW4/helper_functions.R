library(RCUDA)
library(MASS)
library(msm)
library(coda)
library(compiler)
library(mvtnorm)

cuGetContext(TRUE)
system("make k_tnorm.ptx") #Run ptx makefile.
m = loadModule("k_tnorm.ptx")
k_tnorm = m$truncnormal_kernel #Get TNorm kernel

tnorm.gpu = function(N, mu=0.0, sigma=1.0, a=-Inf, b=Inf, maxtries=5000, threads_per_block=512L, verbose=FALSE) 
{
    a_len = length(a)
    b_len = length(b)
    mu_len = length(mu)
    sigma_len = length(sigma)

    # N = 10,000 < 32*512 => 32 blocks of 512 threads
    block_dims <- c(threads_per_block, 1L, 1L)
    grid_d1 <- max(floor(sqrt(N/threads_per_block)), 1)
    grid_d2 <- ceiling(N/(grid_d1*threads_per_block))
    grid_dims <- c(grid_d1, grid_d2, 1L)
    nthreads <- prod(grid_dims)*prod(block_dims) 

    if (verbose) {
    	print(paste("Grid size:", list(grid_dims), sep=" "))
    	print(paste("Block size:", list(block_dims), sep=" "))
    	print(paste("Total number of threads to launch = ", nthreads, sep=""))
	}

    d_mu <- copyToDevice(mu)
    d_sigma <- copyToDevice(sigma)
    d_a <- copyToDevice(a)
    d_b <- copyToDevice(b)
    d_results <- copyToDevice(rep(0.0, N))

    if (verbose) cat("Launching tnorm kernel.\n")
    results = .cuda(k_tnorm, "results"=d_results, as.integer(N), d_mu, d_sigma, d_a, d_b, as.integer(maxtries), 
	  as.integer(mu_len), as.integer(sigma_len), as.integer(a_len), as.integer(b_len),
	  gridDim=grid_dims, blockDim=block_dims, outputs="results")
    if (verbose) cat("Done with CUDA tnorm kernel.\n")
    return(results)
}
