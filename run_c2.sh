parm=1

d1=$((parm)) 
d2=$((parm+14)) 
d3=$((parm+l)) 
d4=$((parm+6)) 

today_dt=$(date --date="$d1 days ago" -I)
month_old_dt=$(date --date="$d2 days ago" -I)
early_dt=$(date --date="$d3 days ago" -I) 
wk_old_dt=${date --date="$d4 days ago" -I) 

echo $today_dt 
echo $month_old_dt 
echo $early_dt 
echo $wk_old_dt 
cd /data/asa/models/ASA-beacon3/ 

impala-shell --ssl -k -i lrdna2xmpappri -B --output_delimiter="," \
			--output_file='proxy_out.csv' \ 
			--var=wk_old_dt=111 $wk_old_dt 1" --var=today_dt="'$today_dt'" \ 
			-f c2_m2_etl_vl.sql
			
impala-shell --ssl -k -i lwtxaoaepappr -B --output_delimiter="," \
			--output_file='recent_traffic_new.csv' \ 
			--var=month_old_dt="'$month_old_dt'" --var=today_dt="'$today_dt'" \ 
			-f c2_m2_recent_traffic_new_vl.sql
			
			
/opt/anaconda3/bin/python3 DP_C2_Filters_vl.py 

R CMD BATCH --vanilla model_deploy_vl.R 

hadoop fs -moveFromLocal /data/asa/models/ASA-beacon3/beacon_model_3.csv 
/data/asa/models/ASA-beacon3/beacon_model_3.csv 

hive -f create_table_vl.sql 
hive -e "LOAD DATA INPATH '/data/asa/models/ASA-beacon3/beacon_model 3.csv' OVERWRITE INTO TABLE asa_testing.beacon_model3 PARTITION (dt='$today_dt') ;" 

impala-shell --ssl -k -i lwtxa0acpappr -q "INVALIDATE METADATA asa_testing.beacon_model3;"