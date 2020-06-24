

add_followers_id <- function(.data = NULL,  N = 5000, verbose = FALSE  ){

  if( !is_twitter_user(.data) ){

    stop("L'oggetto .data deve essere di classe twitter_user")

  }

  # argomento retryonratelimit
  ratelimit <- ifelse(N > 5000, TRUE, FALSE)

  is_first_importation <- purrr::is_empty(.data@followers_id)

  screen_name <- .data@screen_name


  # caso uno prima importazione
  if(is_first_importation){

    .data@followers_id <- rtweet::get_followers(screen_name, n = N, retryonratelimit = ratelimit)

    # aggiungiamo il cursore
    next.cursor <- rtweet::next_cursor(.data@followers_id )
    .data@cursor_followers <-  ifelse(next.cursor == 0, NA_character_, next.cursor)

    # aggiungiamo lo screen_name
    .data@followers_id <- tibble::add_column(.data@followers_id, screen_name = screen_name, .before = "user_id")

    if(verbose) message("New ", nrow(.data@followers_id)," followers imported!")

  }

  # caso 2 non prima importazione con cursore non NA
  if(!is_first_importation && !is.na(.data@cursor_followers)){

    new_followers <- rtweet::get_followers(screen_name, n = N , retryonratelimit = ratelimit, page = .data@cursor_followers)

    # cursor
    .data@cursor_followers <- rtweet::next_cursor(new_followers)
    .data@cursor_followers <- ifelse(.data@cursor_followers == 0, NA_character_, .data@cursor_followers )

    # elimino dati gia presenti e unisco le due tabelle
    new_followers <- tibble::add_column(new_followers, screen_name = screen_name, .before = "user_id")

    old_followers <- .data@followers_id$user_id

    new_followers <- dplyr::filter(new_followers, !(user_id %in% old_followers))


    .data@followers_id <- dplyr::bind_rows(.data@followers_id, new_followers)

    if(verbose) message("New ", nrow(new_followers)," followers imported!")

  }

  # caso 3 non prima importazione con cursore NA
  if(!is_first_importation && is.na(.data@cursor_followers)){

    new_followers <- rtweet::get_followers(screen_name, n = N , retryonratelimit = ratelimit)

    # cursor
    .data@cursor_followers <- rtweet::next_cursor(new_followers)
    .data@cursor_followers <- ifelse(.data@cursor_followers == 0, NA_character_, .data@cursor_followers )

    # elimino dati gia presenti e unisco le due tabelle
    new_followers <- tibble::add_column(new_followers, screen_name = screen_name, .before = "user_id")

    old_followers <- .data@followers_id$user_id

    new_followers <- dplyr::filter(new_followers, !(user_id %in% old_followers))

    .data@followers_id <- dplyr::bind_rows(.data@followers_id, new_followers)

    if(verbose) message("New ", nrow(new_followers)," followers imported!")

  }



  return(.data)

}
