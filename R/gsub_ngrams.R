#'@title
#'@description
#'@param
#'@name
#'@rdname
#'@return
#'@examples
#'@export

gsub_ngrams <- function(x = NULL, lexicoin = NULL){

  stopifnot(!is.null(x) & is.character(x))

  if( is.null(lexicoin) || sum(colnames(lexicoin) %in% c("complex_form", "simple_form")) != 2 ){

    warning("lexicoin non valido deve contere almeno due colonne chiamate: complex_form, simple_form. ")

    return(x)

  }

  ngrams.detected <- lexicoin[stringr::str_detect(x, lexicoin$complex_form),]

  if(nrow(ngrams.detected) > 0){

    for(i in 1:nrow(ngrams.detected)){

      x <- gsub(ngrams.detected[i,]$complex_form, ngrams.detected[i,]$simple_form, x)

    }

  }

  return(x)
}
