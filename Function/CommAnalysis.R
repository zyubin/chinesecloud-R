Comm_analysis <- function(comm_cut_j,n_once,dirwd,...){
# for(j in all_count_comm_cut){
	options(scipen=20)#取消科学计数法

	
	test_len <- 0
	
	setwd(dirwd)
	source("sourceDir.R")
	sourceDir("function")


	insertWords(readLines("userdict/userdict.txt", encoding = "UTF-8"))
	insertWords(readLines("userdict/company.txt", encoding = "UTF-8"))
#   插入自定义词库 用于分词 

	
	cat("-------", comm_cut_j, " commets Completed",paste(rep("-",12-nchar(comm_cut_j)), collapse = ""),"\n")
	tryCatch({
	#### 全部数据
	con <- dbConnect(MySQL(),user='root', password='tcl_bigdata@321',host='192.168.1.142',dbname='bigdata',port=3306)
	dbSendQuery(con,'set names gbk')
	SQLQuery <- paste('SELECT id, title FROM tb_dfcf01 LIMIT ', comm_cut_j ," , ", n_once, sep="")
	res  <- dbSendQuery(con, SQLQuery)
	
	##### 读取数据并解决乱码问题 
	MySQLComment_ini <- fetch(res,-1) 
	dbDisconnect(con)

	MySQLComment <- gsub("\n|\t","",MySQLComment_ini$title)
	#删除换行符


	# cat("ok0 \n")
	# MySQLComment <- as.list(MySQLComment)  

	MySQLComment1 <- lapply(MySQLComment,function(x) {
		sentence.seg.multi(x)
		})
	seged_table <- data.table(MySQLComment_ini$id, unlist(MySQLComment1))
	write.table(seged_table, file = paste("Seged/Seged", comm_cut_j, ".txt", sep = ""), row.names = FALSE, col.names = FALSE, sep = "&", quote = F, fileEncoding = "UTF8")
	# 写入到文档
	test_len <- length(MySQLComment1)
	rm(MySQLComment_ini,MySQLComment1 ,seged_table)
	gc()
	test_len
	}, error=function(e){test_len = 0; test_len}, finally = {write.table(list(test_len,comm_cut_j,Sys.time()), file = "log/log.txt", row.names = FALSE, col.names = FALSE, sep = "  ", append = TRUE,quote = F, fileEncoding = "UTF8");
	return(test_len)})
	
}	
