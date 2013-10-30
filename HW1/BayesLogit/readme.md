Logical ordering of code:
=========================
For explanations on parameter choices and algorithm details, refer to the PDF or Tex file, HW1.{pdf, tex}.

Homework 2:
- Run BLR_Fit.R. You will need to change the setwd() command in the beginning to something that more closely reflects your own working directory.
- BLR_Fit.R calls helper_functions.R, which is a file with numerous smaller functions that do most of the Metropolis-Hastings work.
- The result of running BLR_Fit.R in array mode on the computer cluster is a series of resulting quantile CSV files in the results directory.

Homework 3:
- Run cancer.R. You will need to change your working directory like before.
- cancer.R calls the same helper_functions code as BLR_Fit.R did.
- Job can be run on the local machine or the compute cluster. There is no advantage to using the cluster with this code--it is not parallel.
