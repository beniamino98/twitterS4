#'@title
#'@description
#'@param
#'@examples

add_friends_id <- function(.data = NULL, N = 5000, update = FALSE, verbose = FALSE ){

  if(!is_twitter_user(.data)){

    stop("L'oggetto .data deve essere di classe twitter_user")

  }

  # screen_name
  screen_name <- .data@screen_name

  # friends_count ( spesso non si superano mai i 5000 amici )
  friends_count <- .data@info$friends_count

  # retryonratelimit argument
  ratelimit <- ifelse(N > 5000 && friends_count > 5000 , TRUE, FALSE)

  if(update){

    .data@friends_id <- data.frame()

    if(verbose) message("Old friend removed!")

  }

  is_first_importation <- purrr::is_empty(.data@friends_id)

  # importazione prima volta
  if( is_first_importation ){

    .data@friends_id <- rtweet::get_friends(screen_name, n = N, retryonratelimit = ratelimit)

    # next_cursor
    next.cursor <-  rtweet::next_cursor(.data@friends_id )
    .data@cursor_friends <- ifelse(next.cursor == 0, NA_character_, next.cursor)

    if(verbose) message("New ", nrow(.data@friends_id)," friends imported!")

  }


  if(!is_first_importation && !is.na(.data@cursor_friends)){

    new_friends <- rtweet::get_friends(screen_name, n = N , retryonratelimit = ratelimit, page = .data@cursor_friends)

    # next_cursor
    next.cursor <-  rtweet::next_cursor( new_friends )
    .data@cursor_friends <- ifelse(next.cursor == 0, NA_character_, next.cursor)


    # mi assicuro che non ci siano duplicati
    old_friends_id <-  .data@friends_id$user_id
    new_friends <-  dplyr::filter(new_friends, !(user_id %in% old_friends_id))
    .data@friends_id <- dplyr::bind_rows(.data@friends_id, new_friends)

    if(verbose) message("New ", nrow(new_friends)," friends imported!")

  }

  return(.data)

}



