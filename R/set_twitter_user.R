#'@title
#'@description
#'@param user_id
#'@param screen_name
#'@param profile_url
#'@param info
#'@param followers
#'@param followers_id
#'@name
#'@rdname
#'@return
#'@examples
#'@export

# creazione della classe twitter user aggiungendo manualmente i varie elementi nei vari slot
set_twitter_user <- function(user_id = NULL,
                             screen_name = NULL,
                             profile_url = NULL,
                             info = NULL,
                             followers = NULL,
                             followers_id = NULL,
                             cursor_followers = NULL,
                             friends = NULL,
                             friends_id = NULL,
                             cursor_friends = NULL,
                             tweets = NULL,
                             retweets = NULL,
                             imported_retweets = NULL,
                             vocabulary_index = NULL,
                             tweets_clean = NULL,
                             tweets_lemmatizzati = NULL){


  stopifnot(!is.null(screen_name))

  new_user <- twitter_user()


  # aggiungiamo user_id
  if(!is.null(user_id) && is.character(user_id)){

    new_user@user_id <- user_id

  }

  if(!is.null(screen_name) && is.character(screen_name)){

    new_user@screen_name <- screen_name
  }

  if(!is.null(profile_url) && is.data.frame(profile_url)){

    new_user@profile_url <- profile_url
  }

  if(!is.null(info) && is.data.frame(info)){

    new_user@info <- info

  }

  if(!is.null(followers) && is.data.frame(followers)){

    new_user@followers <- followers
  }

  if(!is.null(followers_id) && is.data.frame(followers_id)){

    new_user@followers_id <- followers_id
  }


  if(!is.null(cursor_followers) && is.character(cursor_followers)){

    new_user@cursor_followers <- cursor_followers

  }

  if(!is.null(friends) && is.data.frame(friends)){

    new_user@friends <- friends

  }

  if(!is.null(friends_id) && is.data.frame(friends_id)){

    new_user@friends_id <- friends_id

  }

  if(!is.null(cursor_friends) && is.character(cursor_friends)){

    new_user@cursor_friends <- cursor_friends
  }

  if(!is.null(tweets) && is.data.frame(tweets)){

    new_user@tweets <- tweets

  }

  if(!is.null(retweets) && is.data.frame(retweets)){

    new_user@retweets <- retweets
  }

  if(!is.null(imported_retweets) && is.data.frame(imported_retweets)){

    new_user@imported_retweets <- imported_retweets
  }

  if(!is.null(vocabulary_index) && is.data.frame(vocabulary_index)){

    new_user@vocabulary_index <- vocabulary_index

  }

  if(!is.null(tweets_clean) && is.data.frame(tweets_clean)){

    new_user@tweets_clean <- tweets_clean

  }

  if(!is.null(tweets_lemmatizzati) && is.data.frame(tweets_lemmatizzati)){

    new_user@tweets_lemmatizzati <- tweets_lemmatizzati

  }


  return(new_user)


}



