#'@title
#'@description
#'@param
#'@name
#'@rdname
#'@return
#'@examples

search_sentiment <- function(x = NULL, lexicoin = NULL ){


  stopifnot(sum(c("word", "value") %in% colnames(lexicoin)) == 2)

  sentiment_df <- dplyr::filter(lexicoin, word == x)[1,]

  if(is.na(sentiment_df$word)){

    sentiment_df <- dplyr::tibble(lang = "unknown", word = x, sentiment = "non_classificato", value = 0 )

  }

  return(sentiment_df)

}


#'@title
#'@description
#'@param
#'@name
#'@rdname
#'@return
#'@examples
#'@export

get_sentiments <- function(x = NULL, lexicoin = NULL){


  condition <- is.null(x) || is.na(x) || x %in% ""
  exist_sentiment_df <- is.null(lexicoin) && exists("dizionari")


  if(exist_sentiment_df && !is.null(dizionari$sentiment_data)){
    lexicoin <- dizionari$sentiment_data
  }


  if( condition ){

    response <- rep(0, length(x))

  } else {

    x <- get_tokens(x)

    lexicoin <-  dplyr::filter(lexicoin, word %in% x)

    response <- purrr::map_df(x, ~search_sentiment(x = .x, lexicoin = lexicoin ))

    response <- response$value

    names(response) <- x
  }

  return(response)
}

#'@title
#'@description
#'@param
#'@name
#'@rdname
#'@return
#'@examples
#'@export

classify_count <- function(text = NULL, lexicoin = NULL){

  class_count <- function(text = NULL, lexicoin = NULL){

    sentiment_count <- get_sentiments(text, lexicoin = lexicoin)

    count_pos <- sum(sentiment_count == 1 )
    count_neg <- sum(sentiment_count == -1 )


    label <- dplyr::case_when(
      count_pos > count_neg ~ "positivo",
      count_pos <  count_neg ~ "negativo",
      count_pos == count_neg ~ "neutro",
      sum(sentiment_count) == 0 ~ "neutro"
    )


    dplyr::tibble(text = text,
                  positivo = count_pos,
                  negativo = count_neg,
                  polarity = sum(sentiment_count),
                  label = label)

  }

  purrr::map_df(text, ~class_count(.x, lexicoin = lexicoin))

}
