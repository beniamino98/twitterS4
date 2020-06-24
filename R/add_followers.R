#'@title
#'@description
#'@param
#'@name
#'@rdname
#'@return
#'@examples
#'@export

add_followers <- function(.data = NULL, n.followers = 10000, verbose = FALSE  ){

  if( !is_twitter_user(.data) ){

    stop("L'oggetto .data deve essere di classe twitter_user")

  }


  is_first_importation <- purrr::is_empty(.data@followers)
  is_empty_id <-  purrr::is_empty(.data@followers_id)

  n.followers <- ifelse(n.followers > 90000, 90000, n.followers)




  # caso uno prima importazione
  if(is_first_importation && !is_empty_id ){

    N.followers_id <- nrow(.data@followers_id)

    n.followers <- ifelse(n.followers > N.followers_id, N.followers_id, n.followers)

    .data@followers <- rtweet::lookup_users(.data@followers_id$user_id[1:n.followers])


    if(verbose) message("New ", nrow(.data@followers)," followers imported!")

  }


  if(!is_first_importation && !is_empty_id ){

    imported.followers <- .data@followers$user_id

    new_followers_id <- dplyr::filter(.data@followers_id, !(user_id %in% imported.followers))

    N.followers_id <- nrow(new_followers_id)

    if(N.followers_id == 0 ) {
      warning("no more followers id to import!")
      return(.data)
    }

    n.followers <- ifelse(n.followers > N.followers_id, N.followers_id, n.followers)

    new_followers <- rtweet::lookup_users(new_followers_id$user_id[1:n.followers])

    .data@followers <-dplyr::bind_rows(new_followers, .data@followers )

    if(verbose) message("New ", nrow(new_followers)," followers imported!")

  }

  return(.data)

}
