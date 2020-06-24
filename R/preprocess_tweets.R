#'@title
#'@description
#'@param
#'@name
#'@rdname
#'@return
#'@examples
#'@export

preprocess_tweets <- function(.data = NULL, lemmatizza = TRUE, dizionari = NULL, categoria = NULL, verbose = TRUE, ... ){

  stopifnot(is_twitter_user(.data))

  text <- .data@tweets$text

  text_clean <- PreProcess(text, dizionari = dizionari,verbose = verbose, ...)

  .data@tweets_clean <- dplyr::select(.data@tweets, created_at, user_id, status_id, screen_name, screen_name, text, is_retweet, lang)
  .data@tweets_clean$n.words <- purrr::map_dbl(text_clean, word_count)
  .data@tweets_clean$text <- text_clean
  .data@tweets_clean <- na.omit(.data@tweets_clean)

  if(lemmatizza){

    tweets <- .data@tweets
    text_lemmatizzato <- lemmatize(text_clean, lexicoin = dizionari$lemmario, categoria = categoria, verbose = verbose)

    .data@tweets_lemmatizzati <- dplyr::select(tweets, created_at, user_id, status_id, screen_name, screen_name, text, is_retweet, lang)
    .data@tweets_lemmatizzati$n.words <- purrr::map_dbl(text_lemmatizzato, word_count)
    .data@tweets_lemmatizzati$text <- text_lemmatizzato
    .data@tweets_lemmatizzati <- na.omit(.data@tweets_lemmatizzati)
  }

  return(.data)

}
