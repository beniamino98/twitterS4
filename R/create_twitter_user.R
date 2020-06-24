#'@title
#'@description
#'@param
#'@name
#'@rdname
#'@return
#'@examples
#'@export


create_twitter_user <- function(screen_name = NULL, tweets = TRUE, followers = TRUE, friends = TRUE,
                                n.tweets = 3200, n.followers = 5000, n.friends = 5000, verbose = TRUE){

  .data  <- twitter_user(screen_name = screen_name)

  # ricerchiamo lo screen_name
  user_info <- rtweet::lookup_users(.data@screen_name)

  # slot: user_id
  .data@user_id <- user_info$user_id

  # slot: profile_url
  .data@profile_url <- dplyr::select(user_info, user_id, screen_name, dplyr::contains("profile"))

  # slot: info
  .data@info <-  dplyr::select(user_info, user_id, screen_name, account_created_at,protected, verified, description, followers_count:favourites_count)
  .data@info <- tibble::add_column(.data@info, importation_date = Sys.Date(), .before = "user_id")

  # aggiungiamo nuovi tweet
  if( is.logical(tweets) && tweets) {

    .data <- add_tweets(.data, N = n.tweets)

    if(verbose) message( n.tweets, " tweets imported!")

  }

  # aggiungiamo nuovi followers
  if(is.logical(followers) && followers){

    .data <- add_followers_id(.data, N = n.followers)

    if(verbose) message( n.followers, " followers imported!")

  }

  # aggiungiamo nuovi friends
  if(is.logical(friends) && friends){

    .data <- add_friends_id(.data, N = n.friends)

    if(verbose) message( n.friends, " friends imported!")

  }


  if(verbose) message("Importation: ", screen_name, " complete!")

  return(.data)

}
