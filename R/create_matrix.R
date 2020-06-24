#'@title
#'@description
#'@param
#'@name
#'@rdname
#'@return
#'@examples
#'@export

create_matrix <- function(text =NULL, stopwords = NULL, control = list(), normalize = TRUE){

  if(is.null(text)) return(NULL)
  if(!is.null(stopwords))  text <-  tm::removeWords(text, words = stopwords)

  crp <- tm::Corpus(tm::VectorSource(text))

  tdm <- tm::TermDocumentMatrix(crp, control = control)
  dtm <- tm::DocumentTermMatrix(crp, control = control)

  wtdm <-  suppressWarnings(tm::weightTfIdf(tdm, normalize = normalize))

  # ordino le parole in ordine decrescente
  m <- as.matrix(tdm)
  word_freqs = sort(rowSums(m), decreasing=TRUE)

  dm <- dplyr::tibble(word = names(word_freqs), freq = word_freqs)
  dm <- dplyr::mutate(dm, prop = word_freqs / sum(dm$freq))

  structure(
    list(
      corpus = crp,
      tdm = tdm,
      dtm = dtm,
      wtdm = wtdm,
      dm = dm,
      stopwords = stopwords
    )
  )
}



