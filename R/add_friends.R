#'@title
#'@description
#'@param
#'@name
#'@rdname
#'@return
#'@examples
#'@export

add_friends <- function(.data = NULL, n.friends = 1000, verbose = FALSE  ){

  if( !is_twitter_user(.data) ){

    stop("L'oggetto .data deve essere di classe twitter_user")

  }


  is_first_importation <- purrr::is_empty(.data@friends)
  is_empty_id <-  purrr::is_empty(.data@friends_id)

  n.friends <- ifelse(n.friends > 90000, 90000, n.friends)




  # caso uno prima importazione
  if(is_first_importation && !is_empty_id ){

    N.friends_id <- nrow(.data@friends_id)

    n.friends <- ifelse(n.friends > N.friends_id, N.friends_id, n.friends)

    .data@friends <- rtweet::lookup_users(.data@friends_id$user_id[1:n.friends])


    if(verbose) message("New ", nrow(.data@friends)," followers imported!")

  }


  if(!is_first_importation && !is_empty_id ){

    imported.friends <- .data@friends$user_id

    new_friends_id <- dplyr::filter(.data@friends_id, !(user_id %in% imported.friends))

    N.friends_id <- nrow(new_friends_id)

    if(new_friends_id == 0 ) {
      warning("no more followers id to import!")
      return(.data)
    }

    n.friends <- ifelse(n.friends > N.friends_id, N.friends_id, n.friends)

    .data@friends <- rtweet::lookup_users(.data@friends_id$user_id[1:n.friends])

    .data@friends <-dplyr::bind_rows(new_friends, .data@friends )

    if(verbose) message("New ", nrow(new_friends)," followers imported!")

  }

  return(.data)

}
