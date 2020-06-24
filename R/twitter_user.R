#'@title
#'@description
#'@param
#'@name
#'@rdname
#'@return
#'@examples
#'@export
#'
#'


twitter_user <- setClass("twitter_user",
                         representation = list(
                           user_id = "character",
                           screen_name = "character",
                           profile_url = "data.frame",
                           info = "data.frame",

                           followers = "data.frame",
                           followers_id = "data.frame",
                           cursor_followers = "character",

                           friends = "data.frame",
                           friends_id = "data.frame",
                           cursor_friends = "character",

                           tweets = "data.frame",
                           retweets = "data.frame",
                           imported_retweets = "data.frame",
                           vocabulary_index    = "data.frame",
                           tweets_clean = "data.frame",
                           tweets_lemmatizzati = "data.frame"

                         ))
