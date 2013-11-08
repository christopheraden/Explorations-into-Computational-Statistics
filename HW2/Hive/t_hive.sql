--Setting default input format and minimum file split size.
SET hive.base.inputformat=org.apache.hadoop.hive.ql.io.HiveInputFormat;
SET mapred.min.split.size=134217728;

--If tables already exist, the script will fail. Since I only run once
--it's totally legit to drop a bunch of tables.
DROP TABLE group_vals;
DROP TABLE group_aggs;

--Point Hive to Groups text file, load (group, value) pair.
CREATE TABLE group_vals ( group int, value double )
ROW FORMAT DELIMITED FIELDS TERMINATED BY '\t' STORED AS TEXTFILE;
LOAD DATA LOCAL INPATH '/home/hadoop/hive_data/groups.txt' INTO TABLE group_vals;

--Get aggregates of the data.
CREATE TABLE group_aggs (group int, mean double, variance double);
INSERT OVERWRITE TABLE group_aggs SELECT group, avg(value), variance(value) FROM group_vals GROUP BY group;
