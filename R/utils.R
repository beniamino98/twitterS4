#'@title
#'@description
#'@param
#'@name
#'@rdname
#'@return
#'@examples
#'@export

get_tokens <- function(x){

  x <- tm::stripWhitespace(x)
  x <- stringr::str_split(x, " ")[[1]]

  return(x)
}


#'@title
#'@description
#'@param
#'@name
#'@rdname
#'@return
#'@examples
#'@export

full_join_list <- function(.l = NULL, by = NULL){

  binded_df <- NULL

  i <- 2
  binded_df <- .l[[1]]
  while(i <= (length(.l))){

    binded_df <- dplyr::full_join(binded_df, .l[[i]],by = by)
    i <- i + 1

  }

  return(binded_df)

}

#'@title
#'@description
#'@param
#'@name
#'@rdname
#'@return
#'@examples
#'@export

if_na <- function(x = NULL , y = 0){

  index <- NULL
  index <- which(is.na(x))

  if(!purrr::is_empty(index)){

    x[index] <- y

  }
  return(x)
}

#'@title
#'@description
#'@param
#'@name
#'@rdname
#'@return
#'@examples
#'@export

unique_words <- function(x = NULL){

  words <- NA_character_

  if(is.null(x)) return(NA_integer_)

  if(is.character(x)){

    words <- paste(x, collapse = " ")
    words <- get_tokens(words)
    words <- unique(words)

  }

  return(words)

}





#'@title
#'@description
#'@param
#'@name
#'@rdname
#'@return
#'@examples
#'@export

normalize <- function(x, digits = 4) {

  if(sum(purrr::map_lgl(x, ~.x == 0)) == length(x)){
    return(x)
  } else {
    round(x/sum(x), digits)
  }

}



#'@title
#'@description
#'@param
#'@name
#'@rdname
#'@return
#'@examples
#'@export
#'

word_count <- function(x){

  x <- tm::removePunctuation(x)
  x <- tm::stripWhitespace(x)
  x <- stringr::str_split(x, " ")[[1]]

  length(x)

}


#'@title
#'@description
#'@param
#'@name
#'@rdname
#'@return
#'@examples
#'@export

word_length <- function(x){

  x <- tm::stripWhitespace(x)
  x <- stringr::str_split(x, "")[[1]]

  length(x)

}

