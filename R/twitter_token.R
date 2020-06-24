#'@title
#'@description
#'@param
#'@name
#'@rdname
#'@return
#'@examples
#'@export


twitter_token <- function(appname = NA_character_,
                              consumer_key = NA_character_,
                              consumer_secret = NA_character_,
                              access_token = NA_character_,
                              access_secret= NA_character_,
                              verbose = TRUE){


  #connessione all'API di twitter
  token_twitter <<- rtweet::create_token (
    app = appname,
    consumer_key = consumer_key,
    consumer_secret = consumer_secret,
    access_token = access_token,
    access_secret = access_secret)

  if(verbose) message("Twitter Token created and added in Global Envirnoment")

}
