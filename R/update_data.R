#'@title
#'@description
#'@param
#'@name
#'@rdname
#'@return
#'@examples
#'@export
#'

update_data <- function(.data = NULL, info = TRUE, n.follower = 5000, n.friends = 5000, n.tweets = 600, verbose = TRUE){


  if(!is_twitter_user(.data)){

    stop("L'oggetto .data deve essere di classe twitter_user")

  }

  today_date <- Sys.Date()

  if(info &&(.data@info$importation_date[1] < today_date ) ){

    new_info <- rtweet::lookup_users(.data@screen_name)
    new_info <- dplyr::select(new_info, user_id, screen_name, account_created_at, protected, verified, description, followers_count:favourites_count)
    new_info <- tibble::add_column(new_info, importation_date = Sys.Date(), .before = "user_id")

    .data@info <-  dplyr::bind_rows(new_info, .data@info)
    .data@info <-  dplyr::arrange(.data@info, dplyr::desc(importation_date))

  }


  if(is.numeric(n.follower)){

    .data <- add_followers_id(.data, N = n.follower, verbose = verbose)

  }

  if(is.numeric(n.friends)){

    .data <- add_friends_id(.data, N = n.friends,  update = TRUE, verbose = verbose)

  }

  if(is.numeric(n.tweets)){

    .data <- add_tweets(.data, N = n.tweets, verbose = verbose)

  }

  return(.data)

}
