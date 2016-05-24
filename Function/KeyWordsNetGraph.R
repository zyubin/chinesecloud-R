# source("KeyWordsNetGraph.R",encoding="utf-8")
KeyWordsNetGraph <- function(search_word = "安科瑞",plat = "sina",from_date = "2015-07-01", end_date = "2015-08-01",
	dirwd = "D:/ntusd/股吧数据分析"){
# a1= data.table("V0",paste("V",1:20, sep=""))
# a2=data.table(rep(paste("V",1:20, sep=""),5), sample(c(paste("T",1:120, sep="")),100,replace=FALSE))
# g <- rbind(a1,a2)
setwd("D:/ntusd/股吧数据分析")
source("sourceDir.R")
sourceDir("function", trace = FALSE)
library(igraph) #加载igraph包
x<-par(bg="white") #设置背景颜色为黑色
search.time <- system.time(g <- KeyWordsNet(search_word = search_word,plat = plat,from_date =from_date,
 end_date = end_date, dirwd = dirwd))

print(g)
g2 <- graph.data.frame(d = g[,2:3,with=FALSE][search_word!=KeyWords],directed = F); #数据格式转换
V(g2) #查看顶点
E(g2) #查看边


# plot(g2,layout=layout.fruchterman.reingold,vertex.label=NA) #显示网络图
# plot(g2,layout=layout.fruchterman.reingold,vertex.size=2,vertex.color="red",edge.arrow.size=0.05,vertex.label=NA) #设置vertex大小和颜色后显示网络图

# plot(g2,
#     vertex.size=5,     #节点大小
#     layout=layout.fruchterman.reingold,  #布局方式
#     vertex.shape='none',    #不带边框
#     vertex.label.cex=1.5,    #节点字体大小
#     vertex.label.color='red',  #节点字体颜色
#     edge.arrow.size=0.7)    #连线的箭头的大小
# #关闭图形设备，将缓冲区中的数据写入文件

# plot(g2,
#     layout=layout.fruchterman.reingold,  #布局方式
#     vertex.size=20,
#     # vertex.shape='none',    #不带边框
#     vertex.label.cex=1,    #节点字体大小
#     vertex.label.color='red',  #节点字体颜色
#     edge.arrow.size=0.7)    #连线的箭头的大小
# #关闭图形设备，将缓冲区中的数据写入文件


# plot(g2,layout=layout.fruchterman.reingold,vertex.size=2,vertex.color="red",edge.arrow.size=0.05,vertex.label=TRUE) #设置vertex大小和颜色后显示网络图


com = walktrap.community(g2, steps = 5)
V(g2)$sg=com$membership
V(g2)$color = rainbow(max(V(g2)$sg),alpha=0.8)[V(g2)$sg]

V(g2)$size = rep(10,length(com$membership))
V(g2)$size[com$name==unique(g[level==1][["search_word"]])]=20
V(g2)$size[com$name%chin%unique(g[level==2][["search_word"]])]=15

# 删除 com  去掉圈子
plot(com,g2,
	layout=layout.fruchterman.reingold,
	vertex.size=V(g2)$size*1.5,
	vertex.color=V(g2)$color,
	edge.width=2,
	edge.arrow.size=5,
	vertex.label.cex=0.8,    #节点字体大小
	# edge.color = rgb(1,1,1,0.4),
	vertex.frame.color=NA,
	margin= rep(0, 4)
	# vertex.label=NA
	)



# All_index <- merge(g,data.table(paste("V",0:20, sep=""), ceiling(0:20/4)+1),by="V1",all.x=TRUE)
# Color_index <- unique(All_index[,2:3,with=FALSE],  by="V2.x")
# setnames(Color_index,c("V1","V2"))
# Color_index <-  unique(rbind(data.table(paste("V",0:20, sep=""), ceiling(0:20/4)+1), Color_index), by="V1")

# merge(data.table(V1=com$name),Color_index,by="V1",all.x=TRUE)


# # 社区图圈起来
# # karate <- graph.famous(g2)
# wc <- walktrap.community(g2)
# modularity(wc)
# membership(wc)
# plot(wc, g2,vertex.size=V(g2)$size )
}