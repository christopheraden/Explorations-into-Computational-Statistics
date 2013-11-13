Logical ordering of code:
=========================
For explanations on algorithm details, refer to the PDF or Tex file, HW2.{pdf, tex}.

Problem 1 (BLB):
- Before running the full file on Gauss, you'll need to run BLB_lin_reg_pre.R. This file creates the subsets of the large matrix. This is done to reduce disk I/O, as well requiring that I only do the subset s times instead of r*s times. Much more scalable this way. I don't have a shell script to dispatch the job for you, but it's not CPU intensive enough that the sysmins will get angry if you run it against the controller node. Run "R --vanilla < BLB_lin_reg_pre.R". R will quit automatically upon completion.
- Run the BLB main code using the shell script created for the run with "sarray BLB_lin_reg_run.sh". This produces r*s datasets (250 by default) in the output folder, all logically named.
- Finally, the process file ("sbatch BLB_lin_reg_process.sh") will average over the models and produce standard error estimates for each covariate. If you have ggplot2, it makes a pretty plot in your working directory at the end. If you don't, it makes a hideous plot using the basic graphics functionality from R.

Problem 2 (Streaming):
- This one will require you to use Amazon Web Services or a working implementation of Hadoop to run, unless you use them on a really small dataset. Details for how to run the files on AWS can be found in the project spec or any AWS tutorial. 
- To test the MapReduce implementation on a small file locally, make sure that mapper.py and reducer.py are executable with "chmod 755 {mapp,reduc}er.py"
- You can simulate what it would be like running these Hadoop files on a Unix machine by running:
cat mini_out.txt | ./mapper.py | sort -k1,1 | ./reducer.py
- The result is a series of keys (x bins, y bins) and counts, separated by commas, one key per line, printed to STDOUT.
- If you can't do this because you're on a Windows machine, go get Cygwin, SSH to a unix machine, or get virtual machine software like VirtualBox or VMWare.

Problem 3 (Hive):
- Has to be run on AWS or a working implementation of Hadoop with Hive.
- On AWS or your machine with Hive, make a folder in your user folder called hive_data, and put the grouped-value data from the STA250 S3 bucket in there, with the name groups.txt (if you want to put it in other places with a different name, change the INPATH in the t_hive.sql file).
- Make the shell script executable with "chmod 755 get_group_means.sh", then run it with "./get_group_means.sh". Wait a minute or two, and you should have a file in the same directory as the shell script called aggs.csv. These are the group means and variances in CSV format. Can be easily read into R with "read.csv(file, header=FALSE)".
-To make a pretty picture, run "R < plot_results.R", making a PDF in the same directory called "mean_by_var.pdf". Requires ggplot2.