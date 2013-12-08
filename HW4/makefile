%.ptx: %.cu
	nvcc $< --ptx -arch=compute_30 -code=sm_30 -use_fast_math	

%.o: %.cu
	nvcc $< -arch=compute_30 -code=sm_30 -use_fast_math -o $@