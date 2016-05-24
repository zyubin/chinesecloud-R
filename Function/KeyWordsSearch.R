KeyWordsSearch <- function(plat = "sina",search_word = "奥马电器",from_date = "2015-10-01", end_date = "2015-10-20",
	dirwd = "D:/ntusd/股吧数据分析",...){
	date_choose <- as.character(as.Date(0:as.numeric(difftime(end_date,from_date)),origin=from_date))

	# as.Date(seq(as.POSIXlt(from_date), as.POSIXlt(end_date), "1 day"))
	# 生成间隔时间

	options(scipen=20)#取消科学计数法
	test_len <- 0
	setwd(dirwd)
	source("sourceDir.R")
	sourceDir("function", trace = FALSE)
	
	tryCatch({
		seach_files <- paste(file.path(getwd(),"RESULTS",plat,"keywordscal/keywordsfreq",date_choose),".txt", sep="")
		all_files <- list.files(file.path(getwd(),"RESULTS",plat,"keywordscal/keywordsfreq"), full.names= TRUE)
		seach_files <- intersect(seach_files, all_files)
		datalist <-  lapply(seach_files, fread,  encoding = "UTF-8", header=TRUE, select = c(1,2,5), 
			colClasses=list(character=1:5))
		# Reduce(function(x,y) {merge(x,y)}, datalist)
		all_words <- rbindlist(datalist)

		setkey(all_words, KeyWords)
		subset(all_words, KeyWords == search_word &term_loca!="0")
		}, error=function(e){test_len = 0; test_len}, finally = {
			write.table(c(class(test_len),date_choose,Sys.time()), file = "log/log.txt", row.names = FALSE, col.names = FALSE, sep = "  ", append = TRUE,quote = F, fileEncoding = "UTF-8");
			all_words
	})

}
