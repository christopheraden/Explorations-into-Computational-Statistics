#include <stdio.h>
#include <stdlib.h>
#include <cuda.h>
#include <cuda_runtime.h>
#include <curand_kernel.h>
#include <math.h>

extern "C" 
{
	__device__ float sim_inv_cdf( float mu, float sigma, float phi_a, float phi_b, curandState localState)
	{	//Generate trunc StdNorm by inverse CDF if conditions are stable.
		float u = ((phi_b - phi_a) * curand_uniform(&localState)) + phi_a;
		float x = sigma * normcdfinvf(u) + mu;
		return x;
	} //Gets valid sample every time--so speed, very wow. 

	__device__ float simple_rejection( float mu, float sigma, float stdA, float stdB, int maxtries, curandState localState)
	{
		float res = 1.0f/0.0f; //set result to inf. For error checking.
		float z;

		for(int i=0; i < maxtries; i++)
		{
			z = curand_normal(&localState);
			if ( z <= stdB && z >= stdA) 
			{
				res = mu + (sigma*z);
				return res;
			}
		} //end maxtry for loop
		return res;
	} //end simple rejection-sampler.

	__device__ float one_sided_trunc( float mu, float sigma, float stdA, float stdB, curandState localState)
	{
		float logRho, z, logU, res, alpha, trunc;
		
		if (isinf(stdA)) trunc=-stdB; else trunc=stdA; //If a is -Inf, then b is truncated. Else, a is truncated. 

		alpha = (trunc + sqrtf(trunc*trunc + 4))/2; //Optimal alpha
		do { //Start rejection sample loop.
			z = (-logf( curand_uniform(&localState) )/ alpha) + trunc; //Truncated Exponential
			if (trunc < alpha) logRho = -((z-alpha) * (z-alpha)) / 2;
			else logRho = ((trunc-alpha)*(trunc-alpha) -(alpha-z)*(alpha-z)) / 2;
			logU = logf(curand_uniform(&localState));
		} while (logU > logRho);
		
		//If left trunc, do as usual. If right trunc, reflect the z value, then add mu.
		if (isinf(stdB)) res = mu + sigma * z;
		else res = mu - sigma*z;
		return res;
	} //end one-sided sampler.

	__device__ float robert( float mu, float sigma, float stdA, float stdB, curandState localState)
	{	//Condns unstable for inv-CDF. Do Robert (2009).
		float logrho, z, logu, res;
		do {
			z = (stdB-stdA) * curand_uniform(&localState) + stdA;
			logu = logf( curand_uniform(&localState) );
			if ( stdA<=0 && stdB>= 0 ) logrho = -(z*z)/2;
			else if (stdA > 0) logrho = -((stdA*stdA)-(z*z))/2;
			else logrho = -((stdB*stdB)-(z*z))/2;
		} while(logu > logrho); 

		res = sigma*z + mu;
		return res;
	} //end Robert sampler.
	
	__global__ void truncnormal_kernel(float *result, int n, float *mu, 
						float *sigma, float *a, float *b, int maxtries,
						int mu_len, int sigma_len, int a_len, int b_len)
	{
		int myblock = blockIdx.x + blockIdx.y * gridDim.x;
		int blocksize = blockDim.x * blockDim.y * blockDim.z;
		int subthread = threadIdx.z*(blockDim.x * blockDim.y) + threadIdx.y*blockDim.x + threadIdx.x;
		int idx = myblock * blocksize + subthread;

		if (idx >= n) return; //Index is larger than sample size--do no calculation here.
		curandState localState;
		curand_init(idx, idx, 0, &localState);

		//Declare vars in thread-local memory.
		float t_a = a[idx % a_len];
		float t_b = b[idx % b_len];
		float t_mu = mu[idx % mu_len];
		float t_sigma = sigma[idx % sigma_len];
		float res = 1.0f/0.0f;
		
		float stdA = (t_a - t_mu)/t_sigma; //Standardize truncation points. Done in-thread.
		float stdB = (t_b - t_mu)/t_sigma;
		
		float phi_a = normcdff(stdA); //Calculate CDF of trunc points from StdNormal. 
		float phi_b = normcdff(stdB);

		if ( phi_b - phi_a > 0.02f )
		{ //If stable conditions, use inverse-CDF.
			result[idx] = sim_inv_cdf(t_mu, t_sigma, phi_a, phi_b, localState); 
			return;
		}

		else if ( isinf(stdA) || isinf(stdB) ) 
		{ //One-sided truncation. 
			res = one_sided_trunc( t_mu, t_sigma, stdA, stdB, localState);
			result[idx] = res;
			return;
		}

		else if ( stdB-stdA >= sqrtf(6.2831853f) && phi_b-phi_a > 0.0001f)
		{ //Two-sided trunc with truncs far away--do the naive rejection sampler
			res = simple_rejection(t_mu, t_sigma, stdA, stdB, maxtries, localState);
			if (!isinf(res))
			{
				result[idx] = res;
				return;
			}
		}
		else{ //Do the Robert method.
			res = robert(t_mu, t_sigma, stdA, stdB, localState); 
			result[idx] = res;
			return;
		} //End truncation regions on same side of mean.
	} //end truncnorm kernel
} //end extern C.
