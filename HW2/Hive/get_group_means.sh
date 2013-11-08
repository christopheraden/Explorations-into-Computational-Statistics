#! /bin/bash
#Reads data file into Hive, then makes tables out
#of everything. Outputs aggregates to a file.

hive -f t_hive.sql
hive -e 'select * from group_aggs' > aggs.tsv

#Perl makes everything less readable.
#Pipe tab-delimited file into Perl regex, globally convert all tabs to commas.
#Export to CSV file.
cat aggs.tsv | perl -lpe 's/"/""/g; s/^|$/"/g; s/\t/","/g' > aggs.csv
rm aggs.tsv
