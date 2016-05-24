# system.time(source(file='股吧数据分析.R',encoding='utf8'))
setwd("D:/ntusd/股吧数据分析")
source("sourceDir.R")
sourceDir("function")
#  加载所需的包  以及  函数
# 插入新的自定义词库到词库文件
# write.table(c("疯涨","dsd"),"userdict/userdict.txt",append = TRUE,row.names = FALSE, col.names = FALSE, quote = F, fileEncoding = "UTF8")
options(scipen=20)#取消科学计数法
library(Rwordseg)
finance_words <- readLines("userdict/userdict.txt", encoding = "UTF-8")
insertWords(finance_words, numfreq = rep(1000, length(finance_words)))

company_words <- readLines("userdict/company.txt", encoding = "UTF-8")
insertWords(company_words, numfreq = rep(1000, length(company_words)))

prof_words <- readLines("userdict/profwords.txt", encoding = "UTF-8")
insertWords(prof_words, numfreq = rep(1000, length(prof_words)))
insertWords(readLines("KeyWords/KeyWords.txt", encoding = "UTF-8"))




WordSegByDate(plat = "sina",table_text = "tb_stock01",localdataget = TRUE, from_date = "2015-10-10", end_date = "2015-11-10",
	dirwd = "D:/ntusd/股吧数据分析",localdatafolder = "E:/新浪财经新闻/New/Txt")

# 对文本进行分词
KeyWordsCal(plat = "sina", from_date = "2015-10-10", end_date = "2015-11-10",dirwd = "D:/ntusd/股吧数据分析")
# ###用查找新的关键词到关键词词库
# KeyWordsCalNew(newword = "二胎", plat = "sina",from_date = "2006-12-01", end_date = "2015-11-12",dirwd = "D:/ntusd/股吧数据分析")

# 计算关键词词频等
KeyWordsSearch(plat = "sina",search_word = "二胎",from_date = "2015-10-10", end_date = "2015-11-10",
	dirwd = "D:/ntusd/股吧数据分析")
# 搜索关键词
KeyWordsNet(search_word = "二胎",plat = "sina",from_date = "2015-10-10", end_date = "2015-11-10",
	dirwd = "D:/ntusd/股吧数据分析",KeyLevel = 1)

KeyWordsNetGraph(search_word = "二胎",plat = "sina",from_date = "2015-10-10", end_date = "2015-11-10",
	dirwd = "D:/ntusd/股吧数据分析")


#################################################################################
# 11:00 - 17:00
system.time(
	WordSegByDate(plat = "guba",table_text = "tb_dfcf01",from_date = "2012-08-11", end_date = "2015-11-01",
	dirwd = "D:/ntusd/股吧数据分析")
	)
KeyWordsCal(plat = "guba",from_date = "2015-07-01", end_date = "2015-11-01",dirwd = "D:/ntusd/股吧数据分析")

sourceDir("function")
KeyWordsSearch(plat = "guba",search_word = "二胎",from_date = "2015-10-01", end_date = "2015-11-01",
	dirwd = "D:/ntusd/股吧数据分析")
KeyWordsNet(search_word = "二胎",plat = "guba",from_date = "2014-10-01", end_date = "2015-11-01",
	dirwd = "D:/ntusd/股吧数据分析",KeyLevel = 1)
# 关键词网计算

from_date <- "2015-01-05";
end_date <- "2015-11-01"
plat <- "guba"
text_week= paste(floor(0:as.numeric(difftime(end_date, from_date))/7),"周",sep="")
text_date=as.character(as.Date(0:as.numeric(difftime(end_date,from_date)),origin= from_date))
DT2 <- data.table(text_date,text_week, key='text_date')


data_dir <- file.path(getwd(),"RESULTS",plat,"keywordscal/keywordsfreq",text_date)
filenames <- paste(data_dir,".txt",sep="")
All_files <- list.files(file.path(getwd(),"RESULTS",plat,"keywordscal/keywordsfreq"), full.names=TRUE)
filenames <- intersect(filenames,All_files)
datalist <-  lapply(filenames, fread, encoding = "UTF-8", header=TRUE, select = 1:4,colClasses=list(character=1:5))
DT1 <- rbindlist(datalist)
setkey(DT1, text_date)

All_DT <- merge(DT1,DT2,by="text_date",all.x=TRUE)
Term_sum <- All_DT[ ,list(term_fre=sum(as.numeric(term_fre)), doc_n=sum(as.numeric(doc_n))),by= .(KeyWords,text_week)]
# 按周计算关键词的频数和所在的文档数
data_dir <- file.path(getwd(),"RESULTS",plat,"keywordscal/keywordsallfreq",text_date)
filenames <- paste(data_dir,".txt",sep="")
All_files <- list.files(file.path(getwd(),"RESULTS",plat,"keywordscal/keywordsallfreq"), full.names=TRUE)
filenames <- intersect(filenames,All_files)
datalist <-  lapply(filenames, fread,  encoding = "UTF-8", header=TRUE, colClasses=list(character=1:3))
DT1 <- rbindlist(datalist)
setkey(DT1, text_date)

# 按周计算总频数和所在的文档数
All_DT <- merge(DT1,DT2,by="text_date",all.x=TRUE)
Sent_sum <- All_DT[ ,list(all_words_n=sum(as.numeric(all_words_n)), all_sente_n=sum(as.numeric(all_sente_n))),by= .(text_week)]
setkey(Term_sum, text_week)
setkey(Sent_sum, text_week)
Goal_TB <- merge(Term_sum,Sent_sum,by="text_week",all.x=TRUE)
Goal_TB[,index:=term_fre/all_sente_n]

setkey(Goal_TB, KeyWords)


search_word <- "华联控股"
Restult_TB <- subset(Goal_TB, KeyWords== search_word)
# 计算了关注指数
Restult_TB[,text_week_num:=as.numeric(gsub("周","",text_week))]

plot(Restult_TB[["index"]][order(Restult_TB[["text_week_num"]])], 
	main = paste( search_word ,"热度指数时间序列"),
	type = "b",
	ylab = "热度指数")
library(ggplot2)


all.date <- as.Date(0:as.numeric(difftime(end_date, from_date)),origin=from_date)
time1 <- all.date[seq(4,301,7)]
value1 <- as.vector(Restult_TB[["index"]][order(Restult_TB[["text_week_num"]])])
data<- data.frame(time= time1,value=value1)
p <- ggplot(data,aes(time,value))
Keywordsline <- p + geom_point(size=2)+ geom_line(size=0.5,colour='turquoise4')+ 
labs(x = NULL, y = "热度指数", title =paste(search_word,"热度指数走势图"))
ggsave(filename="Keywordsline.png", plot=Keywordsline, width=10, height=5)




####################################################################
# 获取股吧数据的历史发帖数量    按月统计
library(RMySQL)
library(ggplot2)
library(dplyr)
con <- dbConnect(MySQL(),user='XXXX', password='XXXX',host='XXXX',dbname='bigdata',port=3306)
dbSendQuery(con,'set names gbk')
SQLQuery <- "SELECT SUBSTR(post_date,1,7) AS post_month, SUM(post_num) AS post_num FROM (SELECT * FROM tb_dfcf_spark_byday WHERE post_date >= '2008-06-01' AND post_date < '2016-01-01'
	AND NOT post_date = 'unknown') b GROUP BY post_month ORDER BY post_month;"
res  <- dbSendQuery(con, SQLQuery)
##### 读取数据并解决乱码问题 
post_data <- fetch(res,-1) 
dbDisconnect(con)
# post_data <- post_data0[-c(1:20),]
#  获取上证指数
library(quantmod)
getSymbols('^SSEC',src='yahoo',from = '2008-06-01',to = '2015-11-12')
close <- (Cl(SSEC))
time1 <- index(close)
value1 <- as.vector(close)
yrng <- range(value1)
xrng <- range(time1)
data <- rbind(data.frame(group = "上证指数",time= time1,value= value1),
	data.frame(group = "股吧热度",time= as.Date(paste(post_data$post_month,"-01",sep="")),value=post_data$post_num))


timepoint <- as.Date(c('1999-07-02','2001-07-26','2005-04-29','2008-09-16','2010-03-31','2015-06-15'))
events <- c('证券法实施','国有股减持','股权分置改革','次贷危机全面爆发','融资融券试点','股市大跌')
data2 <- data.frame(group = "上证指数",timepoint,events,stock=value1[time1 %in% timepoint])

p <- ggplot(data,aes(time,value,colour=group,group = group))
# stock.guba.line <-
 p + geom_line(size=1)+ 
labs(x = NULL, y = "指数", title ="上证指数 VS 股吧热度指数走势图")+
geom_text(aes(timepoint, stock, label = paste(timepoint,events,sep='\n')),data = data2,colour = 'blue',vjust = c(-1,-1,-1,1.5,-0.5,-1),size = 3)+
geom_point(aes(timepoint, stock),data = data2,size = 5,colour = 'green',alpha=0.5)
# ggsave(filename="stock.guba.line.png", plot=stock.guba.line, width=10, height=6)
# 画两幅图方法2

mt <- ggplot(data, aes(time,value, colour=group)) + geom_line(size=2)
mt + facet_grid(group~., scales = "free")+theme(strip.text.y = element_text(size = 20))+ 
labs(x = NULL, title ="说明：工作日收视每周内平均每天收视人数")

# ggsave(filename="shoushi.png", plot=shoushi, width=10, height=6)


##############################################################################################


####################################################################
# 获取股吧数据的历史发帖数量    按天统计
library(RMySQL)
library(ggplot2)
con <- dbConnect(MySQL(),user='XXXX', password='XXXX',host='XXXX',dbname='bigdata',port=3306)
dbSendQuery(con,'set names gbk')
SQLQuery <- "SELECT post_date, COUNT(*) as post_num FROM tb_dfcf02
 GROUP BY post_date  ORDER BY post_date;"
res  <- dbSendQuery(con, SQLQuery)
##### 读取数据并解决乱码问题 
post_data0<- fetch(res,-1) 
dbDisconnect(con)
post_data <- post_data0[-c(1:450),]
post_data <- tbl_df(post_data)

post_data <- filter(post_data,post_num < 30000)
#  获取上证指数
library(quantmod)
getSymbols('^SSEC',src='yahoo',from = '1999-06-07',to = '2015-11-08')
close <- (Cl(SSEC))
time1 <- index(close)
value1 <- as.vector(close)
yrng <- range(value1)
xrng <- range(time1)
data<- rbind(data.frame(group = "上证指数",time= time1,value= value1),
	data.frame(group = "股吧热度",time= as.Date(post_data$post_date[post_data$post_date%chin%as.character(time1)]),
		value=post_data$post_num[post_data$post_date%chin%as.character(time1)]))

timepoint <- as.Date(c('1999-07-02','2001-07-26','2005-04-29','2008-09-16','2010-03-31','2015-06-15'))
events <- c('证券法实施','国有股减持','股权分置改革','次贷危机全面爆发','融资融券试点','股市大跌')
data2 <- data.frame(group = "上证指数",timepoint,events,stock=value1[time1 %in% timepoint])

p <- ggplot(data,aes(time,value,colour=group,group = group))
# stock.guba.line <- 
p + geom_line(size=1)+ 
labs(x = NULL, y = "指数", title ="上证指数 VS 股吧热度指数走势图")+
geom_text(aes(timepoint, stock, label = paste(timepoint,events,sep='\n')),data = data2,colour = 'blue',vjust = c(-1,-1,-1,1.5,-0.5,-1),size = 3)+
geom_point(aes(timepoint, stock),data = data2,size = 5,colour = 'green',alpha=0.5)
# ggsave(filename="stock.guba.line.png", plot=stock.guba.line, width=10, height=6)

#######################################
# 按周统计
library(RMySQL)
library(ggplot2)
con <- dbConnect(MySQL(),user='XXXX', password='XXXX',host='XXXX',dbname='bigdata',port=3306)
dbSendQuery(con,'set names gbk')
SQLQuery <- "SELECT post_date,  post_num FROM tb_dfcf_spark_byday WHERE post_date >= '2008-06-01' AND post_date < '2016-01-01' AND NOT post_date = 'unknown'"
res  <- dbSendQuery(con, SQLQuery)
##### 读取数据并解决乱码问题 
post_data0 <- fetch(res,-1) 
dbDisconnect(con)
# post_data <- post_data0[-c(1:450),]
post_data <- tbl_df(post_data0)
# post_data <- filter(post_data)
#  获取上证指数
library(quantmod)
library(data.table)
getSymbols('^SSEC',src='yahoo',from = '2008-06-01',to = '2015-12-31')
close <- (Cl(SSEC))
time1 <- index(close)
value1 <- as.vector(close)
yrng <- range(value1)
xrng <- range(time1)
#######################################
data <- rbind(data.frame(group = "上证指数",time= time1,value= value1),
	data.frame(group = "股吧热度",time= as.Date(post_data$post_date[post_data$post_date%chin%as.character(time1)]),
		value=post_data$post_num[post_data$post_date%chin%as.character(time1)]))

text_week= paste(floor(0:as.numeric(difftime('2015-11-08', '1999-06-07'))/7)+1,"周",sep="")
text_date=as.character(as.Date(0:as.numeric(difftime('2015-11-08','1999-06-07')),origin= '1999-06-07'))
date_week <- data.frame(time=as.Date(text_date),text_week=text_week)
date_week <- tbl_df(date_week)
Data <- data.table(left_join(data, date_week, by="time"))
Data_week <- Data[,mean(value),by=.(group ,text_week)]
Data_time <- Data[,tail(time,1),by=.(group ,text_week)]
Data_time <- select(Data_time,text_week,V1)
data <- left_join(Data_week, unique(Data_time), by="text_week")
names(data)=c("group","text_week","value","time" )


timepoint <- as.Date(c('1999-07-02','2001-07-26','2005-04-29','2008-09-16','2010-03-31','2015-06-15'))
events <- c('证券法实施','国有股减持','股权分置改革','次贷危机全面爆发','融资融券试点','股市大跌')
data2 <- data.frame(group = "上证指数",timepoint,events,stock=value1[time1 %in% timepoint])


p <- ggplot(data,aes(time,value,colour=group,group = group))
# stock.guba.line <- 
p + geom_line(size=1)+ 
labs(x = NULL, y = "指数", title ="上证指数 VS 股吧热度指数走势图")+
geom_text(aes(timepoint, stock, label = paste(timepoint,events,sep='\n')),data = data2,colour = 'blue',vjust = c(-1,-1,-1,1.5,-0.5,-1),size = 3)+
geom_point(aes(timepoint, stock),data = data2,size = 5,colour = 'green',alpha=0.5)
# ggsave(filename="stock.guba.line.png", plot=stock.guba.line, width=10, height=6)

# 画两幅图方法2
mt <- ggplot(data, aes(time,value, colour=group)) + geom_line(size=2)
mt + facet_grid(group~., scales = "free")+theme(strip.text.y = element_text(size = 20))+ 
labs(x = NULL, title ="说明：数据来源  东方财富网股吧 1.1 一条帖子")


##############################################################################################


####################################################################
# 获取股新浪财经新闻数量    按月统计
all_data_dir <- list.files("RESULTS/sina/seged", full.names = TRUE)
all_file_names <- lapply(all_data_dir, function(dir_i){
	data.table(list.files(dir_i))
})
all_file_names <- rbindlist(all_file_names)
all_file_names[ ,post_month:=substr(V1,1,7)]
all_file_names[ ,post_code:=gsub(".txt","",substr(V1,12,1000))]
all_file_names[ , `:=`( post_num = .N) , by = post_month]
library(ggplot2)
post_data <- subset(unique(subset(all_file_names,select=c(2,4))),post_num>10000)
#  获取上证指数
library(quantmod)
getSymbols('^SSEC',src='yahoo',from = '1999-06-01',to = '2015-11-12')
close <- (Cl(SSEC))
time1 <- index(close)
value1 <- as.vector(close)
yrng <- range(value1)
xrng <- range(time1)
data<- rbind(data.frame(group = "上证指数",time= time1,value= value1),
	data.frame(group = "股吧热度",time= as.Date(paste(post_data$post_month,"-01",sep="")),value=post_data$post_num/10))

timepoint <- as.Date(c('1999-07-02','2001-07-26','2005-04-29','2008-09-16','2010-03-31','2015-06-15'))
events <- c('证券法实施','国有股减持','股权分置改革','次贷危机全面爆发','融资融券试点','股市大跌')
data2 <- data.frame(group = "上证指数",timepoint,events,stock=value1[time1 %in% timepoint])

p <- ggplot(data,aes(time,value,colour=group,group = group))
stock.sina.line <- p + geom_line(size=1)+ 
labs(x = NULL, y = "指数", title ="上证指数 VS 股吧热度指数走势图")+
geom_text(aes(timepoint, stock, label = paste(timepoint,events,sep='\n')),data = data2,colour = 'blue',vjust = c(-1,-1,-1,1.5,-0.5,-1),size = 3)+
geom_point(aes(timepoint, stock),data = data2,size = 5,colour = 'green',alpha=0.5)
ggsave(filename="stock.sina.line.png", plot=stock.sina.line, width=10, height=6)
##############################################################################################



##############时间序列分析
p_index <- Restult_TB[["index"]][order(Restult_TB[["text_week_num"]])]
p_index <- ts(p_index, frequency=52, start=c(2015,2))
plot.ts(p_index)
p_index <- log(1+p_index)
plot.ts(p_index)
library("TTR")
p_indexSMA3 <- SMA(p_index,n=15)
plot.ts(p_indexSMA3)

p_indextimeseriescomponents <- decompose(p_indexSMA3 )
# 估计出的季节性、趋势的和不规则部分现在被存储在变量 
p_indextimeseriescomponents$seasonal;
p_indextimeseriescomponents$trend ;
p_indextimeseriescomponents$random

p_indextimeseriesseasonallyadjusted <- p_indextimeseries - p_indextimeseriescomponents$seasonal
# 我们可以使用“plot()”画出季节性修正时间序列，代码如下：
plot(p_indextimeseriesseasonallyadjusted)

library(quantmod)
#万科 000002.sz
setSymbolLookup(GZMT=list(name='000957.sz',src='yahoo', from = "2015-05-04", to = "2015-10-30"))
getSymbols("GZMT")
chartSeries(GZMT)



# ########################
# # 按天计算
# #####################
# from_date <- "2015-05-04";
# end_date <- "2015-10-30"
# plat <- "guba"
# search_word <- "新能源"

# data_dir <- file.path(getwd(),"RESULTS",plat,"keywordscal/keywordsfreq",text_date)
# filenames <- paste(data_dir,".txt",sep="")
# All_files <- list.files(file.path(getwd(),"RESULTS",plat,"keywordscal/keywordsfreq"), full.names=TRUE)
# filenames <- intersect(filenames,All_files)
# datalist <-  lapply(filenames, fread, encoding = "UTF-8", header=TRUE, select = 1:4,colClasses=list(character=1:5))
# DT1 <- rbindlist(datalist)
# setkey(DT1, text_date)
# ###DT 1 每个词的词频

# data_dir <- file.path(getwd(),"RESULTS",plat,"keywordscal/keywordsallfreq",text_date)
# filenames <- paste(data_dir,".txt",sep="")
# All_files <- list.files(file.path(getwd(),"RESULTS",plat,"keywordscal/keywordsallfreq"), full.names=TRUE)
# filenames <- intersect(filenames,All_files)
# datalist <-  lapply(filenames, fread,  encoding = "UTF-8", header=TRUE, colClasses=list(character=1:3))
# DT2 <- rbindlist(datalist)
# setkey(DT2, text_date)
# # DT2 关键词每天总词频
# Goal_TB <- merge(DT1,DT2,by="text_date",all.x=TRUE)
# Goal_TB[,index:=as.numeric(term_fre)/as.numeric(all_sente_n)]

# setkey(Goal_TB, KeyWords)
# Restult_TB <- subset(Goal_TB, KeyWords== search_word)
# # 计算了关注指数
# dayindex <- as.numeric(difftime(Restult_TB[["text_date"]],from_date, units="days"))


# plot(dayindex,Restult_TB[["index"]], 
# 	main = paste( search_word ,"热度指数时间序列"),
# 	type = "b",
# 	ylab = "热度指数")



