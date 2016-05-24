KeyWordsCal <- function(plat = "sina",from_date = "2015-10-01", end_date = "2015-10-07",dirwd = "D:/ntusd/股吧数据分析",...){
# for(j in all_count_comm_cut){
	dates_choose <- as.Date(0:as.numeric(difftime(end_date,from_date)),origin=from_date)
	options(scipen=20)#取消科学计数法
	test_len <- 0
	setwd(dirwd)
	source("sourceDir.R")
	sourceDir("function", trace = FALSE)

	KeyWords <- readLines("KeyWords/KeyWords.txt",encoding = "UTF-8")





	for(date_choose in intersect(as.character(dates_choose),list.files(file.path("RESULTS",plat,"seged")))){
		print(date_choose)
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

		all_words_n <- sum(unlist(lapply(all_words[["text_seged"]], str_count, " ")))
		all_sente_n <- sum(unlist(lapply(all_words[["text_seged"]], str_count, "。")))
		corpus = Corpus(VectorSource(all_words[["text_seged"]]))
		(sample.dtm <- DocumentTermMatrix(corpus, control = list(dictionary = intersect(unlist(strsplit(all_words[["text_seged"]]," "))
,KeyWords),wordLengths = c(2, Inf))))
		# # # # the freq weight

		# (sample.dtm <- DocumentTermMatrix(corpus, control = list(wordLengths = c(2, Inf), weighting = function(x) weightTfIdf(x, normalize = FALSE),stopwords = TRUE)))
		### the if_idf weight

		# sample.dtm$dimnames
		# train<- scv$text
		sample_matrix = as.matrix(sample.dtm)
		# 求出关键词所在的文档编号
		aa <- apply(sample_matrix,2,function(x, stockCode = all_words[["stockCode"]])paste(unique(stockCode[x>0]),collapse= ","))
		aa[aa==""]=0##对没有的用0表示， 以免后续data.table读入出错
		IDF <- apply(sample_matrix,2,function(x)sum(x>0))
		keywordscal <- data.table(text_date = date_choose, KeyWords = names(aa), term_fre = apply(sample_matrix,2,sum), doc_n = IDF, term_loca = aa)
		# 求出关键词的频数
		write.table(keywordscal, file = paste(file.path(keywords_dir,"keywordsfreq",date_choose),".txt", sep=""), row.names = FALSE, col.names = TRUE, sep = "  ", quote = F, fileEncoding = "UTF-8")
		write.table(data.table(text_date = date_choose, all_words_n=all_words_n, all_sente_n=all_sente_n), file = paste(file.path(keywords_dir,"keywordsallfreq",date_choose),".txt", sep=""), row.names = FALSE, col.names = TRUE, sep = "  ", quote = F, fileEncoding = "UTF-8")

		test_len <- nrow(keywordscal)
		test_len
		}, error=function(e){test_len = 0; test_len}, finally = {write.table(list(test_len,date_choose,Sys.time()), file = "log/log.txt", row.names = FALSE, col.names = FALSE, sep = "  ", append = TRUE,quote = F, fileEncoding = "UTF-8");
		test_len})
	}



}
