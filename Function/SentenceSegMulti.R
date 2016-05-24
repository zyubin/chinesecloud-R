sentence.seg.multi <- function(sentence,...){

	# sentence <- MySQLComment[[1062]]
	sentence_seged <- " "
	
	tryCatch({
	
	if(sentence!=""){
			
			delete_symbols <- "[^一-龥a-zA-Z0-9]"

			sentence_split <- as.list(unlist(strsplit(sentence,"。")))

			sentence_seged <- lapply(sentence_split,function(x){sentence.seg.one(x,delete_symbols)})
			# sentence_symbols <- sentence_segment[sentence_segment%in%c("，","。","！","（","）",",","!",".","“","”","？",";","?","…","、","～","；","……",":","*","：","=","’","/","(","^",")","×","＾")]

			sentence_seged <- paste(paste(sentence_seged, collapse=" 。 ",sep=" "),"。", sep=" ")


	}
	
	}, error=function(e){sentence_seged <- " "}, finally = return(sentence_seged))


}

