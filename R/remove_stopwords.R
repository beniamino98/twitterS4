#'@title
#'@description
#'@param
#'@name
#'@rdname
#'@return
#'@examples
#'@export

remove_stopwords <- function(x = NULL, stopwords = TRUE, lang = "it"){


  if(is.logical(stopwords) && stopwords == TRUE ){

    x <- tm::removeWords(x, tm::stopwords(kind = lang))

  } else if(is.character(stopwords)){

    x <- tm::removeWords(x, stopwords)

  }

  x <- tm::stripWhitespace(x)
  x <- stringr::str_trim(x)

  return(x)
}
