#'@title
#'@description
#'@param
#'@name
#'@rdname
#'@return
#'@examples
#'@export

setClass("twitter_user",
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

#'@title
#'@description
#'@param
#'@name
#'@rdname
#'@return
#'@examples
#'@export
#'
setMethod("show", signature = "twitter_user",
          definition = function(object){

            creation_data <- as.character(object@info[nrow(object@info),]$importation_date)

            imported.tweets <- ifelse(purrr::is_empty(object@tweets), 0, nrow(object@tweets))
            tot.tweets <- object@info$statuses_count[1]
            prop.imported.tweets <- round(imported.tweets/tot.tweets, 4) *100


            imported.followers_id <- ifelse(purrr::is_empty(object@followers_id), 0, nrow(object@followers_id))
            imported.followers <- ifelse(purrr::is_empty(object@followers), 0, nrow(object@followers))
            tot.followers <- object@info$followers_count[1]
            prop.imported.followers <- round(imported.followers_id/tot.followers, 4) *100

            imported.friends_id <- ifelse(purrr::is_empty(object@friends_id), 0, nrow(object@friends_id))
            imported.friends <- ifelse(purrr::is_empty(object@friends), 0, nrow(object@friends))
            tot.friends <- object@info$friends_count[1]
            prop.imported.friends <- round(imported.friends_id/tot.friends, 4) *100

            imported.retweets <- ifelse(purrr::is_empty(object@retweets), 0, nrow(object@retweets))


            cat("Twitter User \n",
                "creation date", creation_data, "\n",
                "screen_name:", object@screen_name, "\n",
                "\n",
                "imported tweets:", imported.tweets, "\n",
                "total tweets:", tot.tweets, "\n",
                "proportion imported tweets:",paste0(prop.imported.tweets,"%"), "\n",
                "\n",
                "imported followers id:", imported.followers_id, "\n",
                "imported followers profile:", imported.followers, "\n",
                "total followers:", tot.followers, "\n",
                "proportion imported followers:",paste0(prop.imported.followers,"%"), "\n",
                "\n",
                "imported friends id:", imported.friends_id, "\n",
                "imported friends profile:", imported.friends, "\n",
                "total friends:", tot.friends, "\n",
                "proportion imported friends:",paste0(prop.imported.friends,"%"), "\n",
                "\n",
                "imported retweets:", imported.retweets, "\n"
                )
          })



#'@title
#'@description
#'@param
#'@name
#'@rdname
#'@return
#'@examples
#'@export

is_twitter_user <- function(.data){
  class(.data) == "twitter_user"
}


#'@title
#'@description
#'@param
#'@name
#'@rdname
#'@return
#'@examples
#'@export

Hashtag <- function(..., n.max = NULL){

  extract_hashtags <- function(.data, n.max = NULL){

    if(!is_twitter_user(.data)){

      stop("L'oggetto .data deve essere di classe twitter_user")

    }

    hashtags <- unlist(.data@tweets$hashtags)
    screen_name <- .data@screen_name

    hashtags_df <- dplyr::tibble(screen_name = screen_name, hashtags = hashtags)
    hashtags_df <- na.omit(hashtags_df)
    hashtags_df <- dplyr::mutate(hashtags_df, hashtags = tolower(hashtags))
    hashtags_df <- dplyr::group_by(hashtags_df,screen_name, hashtags)
    hashtags_df <- dplyr::summarise(hashtags_df, freq = dplyr::n())
    hashtags_df <- dplyr::arrange(hashtags_df, dplyr::desc(freq))
    hashtags_df <- dplyr::mutate(hashtags_df, prop = freq / sum(hashtags_df$freq), N = sum(hashtags_df$freq))

    if(!is.null(n.max)){

      hashtags_df <- hashtags_df[1:n.max,]

    }

    return(hashtags_df)

  }

  .l = list(...)

  purrr::map_df(.l, ~extract_hashtags(.x, n.max ))

}

#'@title
#'@description
#'@param
#'@name
#'@rdname
#'@return
#'@examples
#'@export

Info <- function(...){

  .l <- list(...)
  .l <- purrr::map(.l, ~.x@info[1,])

  dplyr::bind_rows(.l)

}

#'@title
#'@description
#'@param
#'@name
#'@rdname
#'@return
#'@examples
#'@export

Tweets <- function(..., lemmatizzati = FALSE){

  .l <- list(...)
  if(lemmatizzati){
    .l <- purrr::map(.l, ~.x@tweets_lemmatizzati)
  } else {
  .l <- purrr::map(.l, ~.x@tweets)
  }
  dplyr::bind_rows(.l)
}



