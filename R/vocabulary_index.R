#'@title
#'@description
#'@param
#'@name
#'@rdname
#'@return
#'@examples
#'@export

vocabulary_index <- function( .data = NULL ){

  stopifnot(is_twitter_user(.data))

  screen_name <- .data@screen_name

  text_clean  <- .data@tweets_clean$text
  text_lemmatizzato <- .data@tweets_lemmatizzati$text

  if( !is.null(text_clean) ) {


    N.documents <- length(text_clean)

    dm <- suppressWarnings(create_matrix(text_clean)$dm)

    hapax <- nrow(dm[dm$freq==1,])
    V <- nrow(dm)
    N <- sum(dm$freq)

    index_clean <-  dplyr::tibble(screen_name = screen_name,
                                  type = "text_clean",  V = V, N = N,
                                  estensione_lessicale = V/N,
                                  hapax = hapax,
                                  ricercatezza_linguaggio = hapax/V,
                                  N.documents = N.documents)

  } else {

    index_clean <-  dplyr::tibble(screen_name = character(),
                                  type = character(),
                                  estensione_lessicale = integer(),
                                  hapax = integer(),
                                  ricercatezza_linguaggio = integer(),
                                  N.documents = integer())


  }


  if( !is.null(text_lemmatizzato) ) {


    N.documents <- length(text_lemmatizzato)

    dm <- suppressWarnings(create_matrix(text_lemmatizzato)$dm)

    hapax <- nrow(dm[dm$freq==1,])
    V <- nrow(dm)
    N <- sum(dm$freq)

    index_lemmatizzato <-  dplyr::tibble(screen_name = screen_name,
                                         type = "text_lemmatizzato", V = V, N = N,
                                         estensione_lessicale = V/N,
                                         hapax = hapax,
                                         ricercatezza_linguaggio = hapax/V,
                                         N.documents = N.documents)

  } else {

    index_lemmatizzato <-  dplyr::tibble(screen_name = character(),
                                         type = character(),
                                         estensione_lessicale = integer(),
                                         hapax = integer(),
                                         ricercatezza_linguaggio =integer(),
                                         N.documents = integer())

  }


  .data@vocabulary_index <-  dplyr::bind_rows(index_clean, index_lemmatizzato)

  return(.data)

}
