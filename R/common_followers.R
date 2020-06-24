#'@title
#'@description
#'@param
#'@name
#'@rdname
#'@return
#'@examples
#'@export

common_followers <- function(... ){

  .l <- c(...)

  .f <- function(.l = NULL, .data = NULL ){



      followers <- purrr::map(.l, ~slot(.x, "followers_id")$user_id)

      x <- purrr::map_dbl(followers,  ~mean(.x %in% slot(.data, "followers_id")$user_id))
      x <- matrix(x,
                  nrow = length(x),
                  ncol = 1 ,
                  dimnames = list(c(), .data@screen_name))


      x <- as.data.frame(x)

      return(x)

  }


  y <- purrr::map(.l, ~.f(.l, .x))
  y <- dplyr::bind_cols(y)

  rownames(y) <-  purrr::map_chr(.l, ~.x@screen_name)

  y <- as.matrix(y)

  return(y)
}



#'@title
#'@description
#'@param
#'@name
#'@rdname
#'@return
#'@examples
#'@export
#'

common_friends <- function(...){

  .l <- c(...)

  .f <- function(.l = NULL, .data = NULL){


      friends <- purrr::map(.l, ~slot(.x, "friends_id")$user_id)

      x <- purrr::map_dbl(friends,  ~mean(.x %in% slot(.data, "friends_id")$user_id))
      x <- matrix(x,
                  nrow = length(x),
                  ncol = 1 ,
                  dimnames = list(c(), .data@screen_name))


      x <- as.data.frame(x)

      return(x)

  }


  y <- purrr::map(.l, ~.f(.l, .x))
  y <- dplyr::bind_cols(y)

  rownames(y) <-  purrr::map_chr(.l, ~.x@screen_name)

  y <- as.matrix(y)

  return(y)
}
