KeyWordsNet <- function(search_word = "二胎",plat = "sina",from_date = "2015-11-01", end_date = "2015-11-12",
	dirwd = "D:/ntusd/股吧数据分析",KeyLevel = 1,...){

	options(scipen=20)#取消科学计数法
	test_len <- 0
	setwd(dirwd)
	source("sourceDir.R")
	sourceDir("function", trace = FALSE)
	KeyWords <- readLines("KeyWords/KeyWords.txt",encoding = "UTF-8")
	date_choose <- as.character(as.Date(0:as.numeric(difftime(end_date,from_date)),origin=from_date))

	# as.Date(seq(as.POSIXlt(from_date), as.POSIXlt(end_date), "1 day"))
	# 生成间隔时间


	tryCatch({

		test_len <- KeyWordsSearch(search_word = search_word,plat = plat ,from_date = from_date , end_date = end_date,dirwd =dirwd )
		searched_code <- strsplit(test_len[["term_loca"]], ",")

		date_dir <- rep(test_len[["text_date"]],lapply(searched_code, length))
		searched_files <- paste(date_dir, unlist(searched_code),sep="_")
		# "2015-11-12_601318"
		# 计算每只股票资讯对应的新闻编号
		all_data_dir <- file.path("RESULTS/sina/Info", unique(date_dir))
		all_file_names <- lapply(all_data_dir, function(dir_i){
			data.table(list.files(dir_i))
		})
		all_file_names <- rbindlist(all_file_names)
		all_file_names[,c("searched_files", "new_code"):=list(substring(V1,1,17),gsub(".txt","",substring(V1,19,100)))]

		searched_files <- data.table(searched_files)
		searched_files <- unique(merge(searched_files,all_file_names,by = "searched_files", all.x=TRUE), by ="new_code")[["searched_files"]]

		searched_files <- paste(file.path(getwd(),"RESULTS",plat,"seged",substring(searched_files,1,10),searched_files),".txt", sep="")
		# searched_files <- paste(file.path(getwd(),"RESULTS",plat,"seged",date_dir,searched_files),".txt", sep="")

		datalist <-  lapply(searched_files, fread, sep = "&", encoding = "UTF-8", header=TRUE, 
			colClasses=c("character","character","character"))
		# Reduce(function(x,y) {merge(x,y)}, datalist)
		all_words <- rbindlist(datalist)
		all_words <- all_words[grep(search_word,all_words[["text_seged"]])]

		corpus = Corpus(VectorSource(unique(all_words[["text_seged"]])))

		# corpus = Corpus(VectorSource(strsplit(unique(all_words[["text_seged"]])," ")))

		(sample.dtm <- DocumentTermMatrix(corpus, control = list(dictionary = intersect(unlist(strsplit(all_words[["text_seged"]]," "))
,KeyWords), weighting = function(x) weightTfIdf(x, normalize = FALSE),wordLengths = c(2, Inf))))
		# # # # the freq weight


		# (sample.dtm <- DocumentTermMatrix(corpus, control = list(wordLengths = c(2, Inf), weighting = function(x) weightTfIdf(x, normalize = FALSE),stopwords = TRUE)))
		### the if_idf weight

		# sample.dtm$dimnames
		# train<- scv$text
		sample_matrix = as.matrix(sample.dtm)
		# 求出关键词所在的文档编号
		aa <- sort(apply(sample_matrix,2,sum), decreasing=TRUE)
		aa <- aa[aa>0]
		Key_n <- ifelse(KeyLevel == 1, 10, 3)
		bb <- data.table(level = KeyLevel, search_word =search_word, KeyWords = names(aa[1:min(length(aa),Key_n)]), KeyWeight = aa[1:min(length(aa),Key_n)])
		rm(sample.dtm,aa,all_words,corpus,datalist,all_file_names);gc()
		if(KeyLevel == 1){
			for(Key_i in bb[["KeyWords"]]){
				bb3 <- KeyWordsNet(search_word = Key_i, plat = plat,from_date = from_date, end_date = end_date,dirwd = dirwd,KeyLevel = 2)
				bb <- rbind(bb,bb3)
			}
			# bb2 <- lapply(bb[["KeyWords"]],KeyWordsNet, plat = plat,from_date = from_date, end_date = end_date,dirwd = dirwd,KeyLevel = 2)
			# bb3 <- rbindlist(bb2)
			# bb <- rbind(bb,bb3)
			
		}
		cat(search_word,"\n")
		bb

# KeyWordsNet(search_word = bb[["KeyWords"]][2], plat = plat,from_date = from_date, end_date = end_date,dirwd = dirwd,KeyLevel = 2)
		}, error=function(e){test_len = 0; test_len}, finally = {
			write.table(c(class(test_len),date_choose,Sys.time()), file = "log/log.txt", row.names = FALSE, col.names = FALSE, sep = "  ", append = TRUE,quote = F, fileEncoding = "UTF-8");
			return(bb)})

}
