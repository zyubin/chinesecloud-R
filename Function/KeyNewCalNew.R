KeyWordsCalNew <- function(newword = "土耳其", plat = "sina",from_date = "2015-10-01", end_date = "2015-10-07",dirwd = "D:/ntusd/股吧数据分析",...){
# for(j in all_count_comm_cut){
	dates_choose <- as.Date(0:as.numeric(difftime(end_date,from_date)),origin=from_date)
	options(scipen=20)#取消科学计数法
	test_len <- 0
	setwd(dirwd)
	source("sourceDir.R")
	sourceDir("function", trace = FALSE)
	KeyWordsNew <- newword
	KeyWords <- readLines("KeyWords/KeyWords.txt",encoding = "UTF-8")
	if(!KeyWordsNew%chin%KeyWords){
		write.table(KeyWordsNew, file = "KeyWords/KeyWords.txt", row.names = FALSE, col.names = FALSE, quote = F, fileEncoding = "UTF-8",append=TRUE)
		for(date_choose in intersect(as.character(dates_choose),list.files(file.path("RESULTS",plat,"seged")))){
			tryCatch({
			#### 全部数据


			# 下面代码可以选择是否以星期为单位存储
			# before_days <- as.POSIXlt(as.Date(-6:0,origin=date_choose))
			# close_mon <- as.Date(before_days[before_days$wday==1])
			# next_days <- as.POSIXlt(as.Date(0:6,origin=date_choose))
			# close_sun <- as.Date(next_days[next_days$wday==0])

			# results_dir <- file.path("RESULTS/guba", "seged",paste(close_mon,close_sun,sep="至"))
			# keywords_dir <- file.path("RESULTS/guba", "keywordscal",paste(close_mon,close_sun,sep="至"))
			# if(!file_test("-d", keywords_dir)){
			# 	dir.create(keywords_dir)
			# }

			keywords_dir <- file.path("RESULTS",plat,"keywordscal")
			data_dir <- file.path("RESULTS",plat,"seged", date_choose)

			filenames <- list.files(path=data_dir, full.names=TRUE)
			datalist <-  lapply(filenames, fread, sep = "&", encoding = "UTF-8", header=TRUE, colClasses=c("character","character","character"))
			# Reduce(function(x,y) {merge(x,y)}, datalist)
			all_words <- rbindlist(datalist)
			# all_words <- fread(paste(file.path(data_dir,date_choose),".txt", sep=""), sep = "&", encoding = "UTF-8", header = TRUE)

			KeyNewLocal <-grep(KeyWordsNew,all_words[["text_seged"]])
			if(length(KeyNewLocal)!=0){
				term_fre <- sum(unlist(lapply(all_words[["text_seged"]], str_count, KeyWordsNew)))
				term_loca <- paste(unique(all_words[["stockCode"]][KeyNewLocal]),collapse= ",")
				keywordscal <- data.table(text_date = date_choose, KeyWords = KeyWordsNew, term_fre = term_fre, doc_n = length(KeyNewLocal), term_loca =term_loca)
				write.table(keywordscal, file = paste(file.path(keywords_dir,"keywordsfreq",date_choose),".txt", sep=""), row.names = FALSE, col.names = FALSE, sep = "  ", quote = F, fileEncoding = "UTF-8",append=TRUE)
			}
			# 求出关键词的频数
			test_len <- nrow(keywordscal)
			test_len
			}, error=function(e){test_len = 0; test_len}, finally = {write.table(list(test_len,date_choose,Sys.time()), file = "log/log.txt", row.names = FALSE, col.names = FALSE, sep = "  ", append = TRUE,quote = F, fileEncoding = "UTF-8");
			test_len})
		}
	}
}
