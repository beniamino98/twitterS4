#'@title
#'@description
#'@param
#'@examples

add_tweets <- function(.data = NULL, N = 600, verbose = FALSE){

  # controllo
  if(!is_twitter_user(.data)){

    stop("L'oggetto .data deve essere di classe twitter_user")

  }

  is_first_importation <- purrr::is_empty(.data@tweets)

  N <- ifelse(N > 3200, 3200, N)


  # se è la prima importazione: lo slot tweets è vuoto
  if(is_first_importation){

    .data@tweets <- rtweet::get_timeline(.data@screen_name, n = N)
    .data@tweets <- dplyr::filter(.data@tweets, !is.na(status_id))
    .data@tweets <- dplyr::arrange(.data@tweets, dplyr::desc(created_at))

    if(verbose) message(nrow(.data@tweets), " new tweets imported!")


  } else {

    # new data
    new_tweets <- rtweet::get_timeline(.data@screen_name, n = N )

    old_status_id <- .data@tweets$status_id
    new_tweets <- dplyr::filter(new_tweets, !is.na(status_id) & !( status_id %in% old_status_id ) )

    # unione di tutti i dati ( nuovi piu vecchi )
    .data@tweets <-  dplyr::bind_rows(new_tweets, .data@tweets )
    .data@tweets <-  dplyr::arrange(.data@tweets, dplyr::desc(created_at))

    if(verbose) message(nrow(new_tweets), " new tweets imported!")

  }

  return(.data)

}
