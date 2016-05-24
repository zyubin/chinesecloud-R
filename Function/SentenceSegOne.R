sentence.seg.one <- function(sentence,delete_symbols,...){

	sentence_seged <- " "
	tryCatch({
		sentence <- gsub(delete_symbols,"",sentence)
		sentence <- gsub(" ","",sentence)

		sentence_seged <- segmentCN(sentence)
		sentence_seged <- paste(sentence_seged, collapse = " ")
		}, error=function(e){sentence_seged <- " "}, finally = return(sentence_seged))
		
	
}
