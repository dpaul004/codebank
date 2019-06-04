select tl.*
		,t2.same_sc_bytes
		,t2.same_sc_bytes_cnt
		,t4.max_sc_bytes
		,t4.max_sc_bytes_cnt
		,t3.same_cs_bytes
		,t3.same_cs_bytes_cnt
		,tS.max_cs_bytes
		,tS.max_cs_bytes_cnt
		,t6.r_ip_unq_cnt
		,t6.host_user_cnt
		,t7.c_ip_cnt
		,t7c_ip_list
		,t8.cs_referer
		,t8.cs_referer_unq_cnt
		,t9.stb_sc_bytes_mn
		,t9.stb_sc_bytes_sd
		,tlO.stb_cs_bytes_mn
		,tlO.stb_cs_bytes_sd
from
	(select tbl2.cs_host
			,tbl2.cs_username
			,tbl2.day_nm
			,round(avg(tbl2.timediff)) as timediff_mean
			,round(stddev(tbl2.timediff)) as timediff sd
			,avg(tbl2.time_taken)
			,ndv(tbl2.cs_bytes) as cs_bytes_dist_cnt
			,ndv(tbl2.sc_bytes) as sc_bytes_dist_cnt
			,count(*)
		from
			(select tbl.cs_host
					,tbl.day_nm
					,tbl.cs username
					,(lead(unix_timestamp(tbl.datetm))
						over (partition by tbl.cs_host, tbl.cs username order by tbl.datetm)
						- unix_timestamp(tbl.datetm)) as timediff
					,tbl.time taken
					,tbl.cs_bytes
					,tbl.sc_bytes
				from
					(select cs host
					,day(datetm) as day_nm
					,cs_username
					,datetm
					,time_taken
					,cs_bytes
					,sc_bytes
				from proxy.proxy_logs_prod
				where r_ip > '' and r_ip I= '\N' and r_ip != '-'
					and dt = ${var:today_dt}
					and length(cs_user) > 6
					and sc_status <m 300) as tbl
				) as tbl2
		group by tbl2.cs_host
				,tbl2.cs_username
				,tbl2.day_nm
		having count(*) >= 30) as t1
		
join

	(Select distinct tbl2.*
	from
		(select tbl.cs_host as cs host
			,tbl.day_nm as day_nm
			,tbl.cs_username
			,first_value(tbl.sc_bytes)
				over (partition by tbl.cs_host, tbl.cs_username order by
						tbl.sc_bytes_cnt DESC) as same_sc_bytes
			,first_value(tbl.sc_bytes_cnt)
				over (partition by tbl.cs_host, tbl.cs_username order by
						tbl.sc_bytes_cnt DESC) as same_sc_bytes_cnt
		from
			(select cs host
				,day(datetm) as day_nm
				,cs_username
				,sc_bytes
				,count(*) as sc_bytes_cnt
			from proxy.proxy_logs_prod
			where r_ip > '' and r_ip != '\N' and r_ip !=
				and dt = ${var:today_dt}
				and length(cs_user) > 6
				and sc status <= 300
			group by cs_host, cs_username, sc_bytes) as tbl
	) as tbl2) as t2
	
join

	(Select distinct tbl2.*
	from
		(select tbl.cs_host as cs host
		,tbl.day_nm as day_nm
		,tbl.cs_username
		,first_value(tbl.cs_bytes)
			over (partition by tbl.cs_host, tbl.cs_username order by
				tbl.cs_bytes_cnt DESC) as same_cs_bytes
		,first_value(tbl.cs_bytes_cnt)
			over (partition by tbl.cs_host, tbl.cs_username order by
				tbl.cs_bytes_cnt DESC) as same_cs_bytes_cnt
	from
		(select cs host
				,day(datetm) as day_nm
				,cs_username
				,cs_bytes
				,count(*) as cs_bytes_cnt
		from proxy.proxy_logs_prod
		where r_ip > '' and r_ip != '\N' and r_ip != '-'
			and dt = ${var:today_dt}
			and length(cs_user) > 6
			and sc status <= 300
		group by cs_host, cs_username, cs_bytes) as tbl
		as tbl2) as t3

join

	(Select distinct tbl2.*
	from

		(select tbl.cs_host as cs host
				,tbl.day_nm as day_nm
				,tbl.cs_username
				,first_value(tbl.sc_bytes)
					over (partition by tbl.cs_host, tbl.cs username 
						order by tbl.sc_bytes DESC) as ma.x_sc_bytes
				,first_value(tbl,sc_bytes_cnt)
					over (partition by tbl.cs_host, tbl.cs username 
						order by tbl.sc_bytes DESC) as ma.x_sc_bytes_cnt
			from
				(select cs host
						,day(datetm) as day_nm
						,cs_username
						,sc_bytes
						,count(*) as sc_bytes_cnt
				from proxy.proxy_logsprod
				where r_ip > '' and r_ip != '\N' and r_ip != '-'
					and dt = ${var:today_dt}
					and length(cs_user) > 6
					and sc status <= 300
				group by cs_host, cs_username, sc_bytes) as tbl
		as tbl2) as t4
		
join

	(Select distinct tbl2.*
	from
		(select tbl.cs_host as cs_host
			,tbl.day_nm as day_nm
			,tbl.cs_username
			,first_value(tbl.cs_bytes)
				over (partition by tbl.cs_host, tbl.cs_username 
				order by tbl.cs_bytes DESC) as max_cs_bytes
			,first_value(tbl.cs_bytes_cnt)
				over (partition by tbl.cs_host, tbl.cs_username 
					order by tbl.cs_bytes DESC) as max_cs_bytes_cnt
		 from
			(select cs_host
					,day(datetm) as day_nm
					,cs_username
					,cs_bytes
					,count(*) as cs_bytes_cnt
		from proxy.proxy_logs_prod
		where r_ip > '' and r_ip != '\N' and r_ip != '-'
			and dt = ${var:today_dt}
			and length(cs_user) > 6
			and sc_status <= 300
			group by cs_host, cs_username, cs_bytes) as tbl
		as tbl2) as t5
join

	(select cs_host
		,day{datetm) as day_nm
		,NDV(r_ip) as r_ip_unq_cnt
		,NDV(cs_username) as host_user_cnt
	from proxy.proxy_logs_prod
	where r_ip > '' and r_ip != '\N' and r_ip != '-'
		and dt between ${var:wk_old_dt} and ${var:today_dt}
		and length(cs_user) > 6
		and sc status <= 300
	group by cs_host) as t6

join

	(select tbl.cs host
			, tbl.cs_username
			, tbl.day_nm
			, group_concat(tbl.c_ip) as c_ip_list
			, ndv(tbl.c_ip) as c_ip_cnt
	from
		(select cs_host
			,cs_username
			,c_ip
			,day(datetm) as day_nm
		from proxy.proxy_logs_prod
		where r_ip > ' ' and r_ip != '\N' and r_ip != '-'
			and dt = ${var:today_dt}
			and length(cs_user) > 6
			and sc status <= 300
			group by cs_host, cs_username, c_ip) as tbl
	group by tbl.cs_host, tbl.cs_username)
	as t7
join

	(select tbl.cs_host as cs host
			,tbl.day_nm as day_nm
			,tbl.cs_username as cs_username
			,group_concat(tbl.cs_referer) as cs_referer
			,NDV(tbl.cs_referer) as cs_referer_unq_cnt
	(select cs_host
			,day(datetm) as day_nm
			,cs_username
			,cs_referer
	from proxy.proxy_logs_prod
	where r_ip > '' and r_ip != '\N' and r_ip != '-'
		and dt = ${var:today_dt}
		and length(cs_user) > 6
		and sc status <= 300
		and cs host in
			(select cs_host
			from proxy.proxy_logs_prod
			where r_ip > '' and r_ip != '\N' and r_ip != '-'
				and dt = ${var:today_dt}
				and length(cs_user) > 6
				and sc_status <= 300
			group by cs_host
			having count(distinct(cs_referer)) <=3)
		group by cs_host, cs_username, cs_referer ) as tbl
	group by tbl.cs_host, tbl.cs_username) as t8

join

	(select tbl3.cs_host
			, tbl3.cs_username
			, avg(tbl3.sc_bytes) as stb_sc_bytes_mn
			, stddev(tbl3.sc_bytes) as stb_sc_bytes_sd
	from
		(select tbl.* 
		 from
			(select cs_host
				, cs_username
				, sc_bytes
				, row_number() 
					over (partition by cs_host, cs_username order by sc_bytes) as r_n
			from proxy.proxy_logs_prod
			where r_ip > '' and r_ip != '\N' and r_ip !=
				and dt = ${var:today_dt}
				and length(cs_user) > 6
				and sc status <= 300) as tbll
			join
			(select cs_host, cs_username, count(*) as cnt
			from proxy.proxy_logsprod
			where r_ip > '' and r_ip != '\N' and r_ip != '-'
				and dt a ${var:today_dt}
				and length(cs_user) > 6
				and sc status <= 300
			group by cs_host, cs_username
			having count(*) >= 30) as tbl2
			
			on tbll.cs_host = tbl2.cs_host
			and tbll.cs_username = tbl2.cs_username
			and tbll.r n < tbl2.cnt*.75) as tbl3
		group by tbl3.cs_host, tbl3.cs_username) as t9

join

	(select tbl3.cs_host, tbl3.cs_username, avg(tbl3.cs_bytes) as stb_cs_bytes_mn,
	stddev(tbl3.cs_bytes) as stb_cs_bytes_sd
	from
		(select tbll.* from
			(select cs_host, cs_username, cs_bytes, row_number() over (partition by cs_host,
			cs_username order by cs_bytes) as r_n
			from proxy.proxy_logsprod
			where r_ip > '' and r_ip != '\N' and r_ip !=
				and dt = ${va􀁳:today_dt}
				and length(cs_user) > 6
				and sc status <= 300) as tbl1
				
			join
			
			(select cs_host, cs_username, count(*) as cnt
			from proxy.proxy_logs_prod
			where r_ip > '' and r_ip != '\N' and r_ip !=
				and dt = ${var:today_dt}
				and length(cs_user) > 6
				and sc status <= 300
				group by cs_host, cs_username
				having count(*) >= 30) as tbl2
			on tbll.cs host = tbl2.cs host - -
			and tbll.cs_username = tbl2.cs_username
			and tbll.r_n < tbl2.cnt*.75) as tbl3
		group by tbl3.cs_host, tbl3.cs_username) as tl0
		
on tl.cs host 		= t2.cs host
and tl.day_nm 		= t2.day_nm
and tl.cs_username 	= t2.cs_username
and tl.cs host 		= t3.cs_host
and tl.day_nm 		= t3.day_nm
and tl.cs username 	= t3.cs username
and tl.cs host 		= t4.cs host
and tl.day_nm 		= t4.day_nm
and tl.cs username 	= t4.cs username
and tl.cs host 		= ts.cs host 
and tl.day_nm 		= tS.day_nm
and tl.cs username 	= ts.cs username
and tl.cs host 		= t6.cs _host
and tl.day_nm 		= t6.day_nm
and tl.cs host 		= t7.cs host
and tl. cs username = t7.cs username
and tl.day_nm 		= t7.day_nm
and tl.cs host 		= ts.cs host
and tl.day_nm 		= t8.day_nm
and tl.cs username 	= ts8.cs_username
and tl.cs host 		= t9.cs host
and tl.cs username  = t9.cs username
and tl.cs host 		= tlO.cs host
and tl.cs username 	= tlO.cs _username
