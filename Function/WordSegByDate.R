WordSegByDate <- function(plat = "sina",table_text = "tb_stock01",localdataget = TRUE,from_date = "2015-10-15", end_date = "2015-10-25",
	dirwd = "D:/ntusd/股吧数据分析", localdatafolder = "E:/新浪财经新闻/New/Txt",...){
# for(j in all_count_comm_cut){
	# localdataget  如果为TRUE 则表示爬取的文本放在本地，直接从本地读取进行处理
	# 为FALSE 则从数据库读取
	dates_choose <- as.Date(0:as.numeric(difftime(end_date,from_date)),origin=from_date)

	options(scipen=20)#取消科学计数法
	test_len <- 0
	setwd(dirwd)
	source("sourceDir.R")
	sourceDir("function", trace = FALSE)


	insertWords(readLines("userdict/userdict.txt", encoding = "UTF-8"))
	insertWords(readLines("userdict/company.txt", encoding = "UTF-8"))
	insertWords(readLines("userdict/profwords.txt", encoding = "UTF-8"))
	insertWords(readLines("KeyWords/KeyWords.txt", encoding = "UTF-8"))
#   插入自定义词库 用于分词 


	if(localdataget){
		SQLdate <- list.files(localdatafolder)
	}else{
		con <- dbConnect(MySQL(),user='XXXX', password='XXXX',host='XXXX',dbname='bigdata',port=3306)
		dbSendQuery(con,'set names gbk')
		SQLQuery <- paste("SELECT DISTINCT post_date FROM ", table_text, sep="")
		res  <- dbSendQuery(con, SQLQuery)
		##### 读取数据并解决乱码问题 
		SQLdate <- fetch(res,-1) 
		dbDisconnect(con)
	}
# 从本地读取数据
	read_dir <- function(localdatafolder,date_choose){
		all_file_name <- list.files(file.path(localdatafolder,date_choose),full.names=TRUE)
		all_file_content <- lapply(all_file_name, function(file_i){
								file_content <- paste(readLines(file_i, encoding = "UTF-8"), collapse="")
								file_code <- unlist(strsplit(tail(unlist(strsplit(file_i,"/")),1),"_"))[2]
								data.frame(stockCode = file_code, content = file_content )
							})
		rbindlist(all_file_content)
	}


# 从数据库读取数据
	for(date_choose in intersect(as.character(dates_choose), unlist(SQLdate))){
		print(date_choose)
		tryCatch({
		#### 全部数据
		if(localdataget){
			MySQLComment_ini <- data.table(read_dir(localdatafolder,date_choose))

		}else{
			con <- dbConnect(MySQL(),user='root', password='root',host='192.168.1.141',dbname='bigdata',port=3306)
			dbSendQuery(con,'set names gbk')
			SQLQuery <- paste("SELECT post_date, post_code stockCode, post_content content FROM ", table_text," where post_date = '",date_choose,"'", sep="")
			res  <- dbSendQuery(con, SQLQuery)
			##### 读取数据并解决乱码问题 
			MySQLComment_ini <- fetch(res,-1) 
			dbDisconnect(con)
		}


		MySQLComment <- gsub("\n|\t","",MySQLComment_ini$content)
		#删除换行符
# 对读取的数据进行分词处理
		MySQLComment1 <- lapply(MySQLComment,sentence.seg.multi)

		seged_table <- data.table(text_date = date_choose, stockCode = as.character(MySQLComment_ini[["stockCode"]]),text_seged = unlist(MySQLComment1))

		setkey(seged_table, stockCode)

	# 下面代码可以选择是否以星期为单位存储
		# before_days <- as.POSIXlt(as.Date(-6:0,origin=date_choose))
		# close_mon <- as.Date(before_days[before_days$wday==1])
		# next_days <- as.POSIXlt(as.Date(0:6,origin=date_choose))
		# close_sun <- as.Date(next_days[next_days$wday==0])

		# results_dir <- file.path("RESULTS/guba/seged",paste(close_mon,close_sun,sep="至"))
		# if(!file_test("-d", results_dir)){
		# 	dir.create(results_dir)
		# }

		# 不以星期为单位存储，按照每天来存储
		results_dir <- file.path("RESULTS",plat,"seged", date_choose)
			if(!file_test("-d", results_dir)){
				dir.create(results_dir)
			}
		for(code_j in unique(seged_table[["stockCode"]])){
			write.table(seged_table[stockCode == code_j,], file = paste(file.path(results_dir, date_choose), "_", code_j, ".txt", sep = ""), row.names = FALSE, col.names = TRUE, sep = "&", quote = F, fileEncoding = "UTF-8")
			# 写入到文档
		}
		test_len <- length(MySQLComment1)
		rm(MySQLComment_ini,MySQLComment1 ,seged_table)
		gc()
		test_len
		
		}, error=function(e){test_len = 0; test_len}, finally = {write.table(list(test_len,date_choose,Sys.time()), file = "log/log.txt", row.names = FALSE, col.names = FALSE, sep = "  ", append = TRUE,quote = F, fileEncoding = "UTF-8");
		test_len})
	}
	

}
