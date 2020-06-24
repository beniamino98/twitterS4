
#'@title
#'@description
#'@param
#'@name
#'@rdname
#'@return
#'@examples
#'@export

is_naive_bayes_text <- function(x = NULL){

  inherits(x, "naive_bayes_text")

}

#'@title
#'@description
#'@param
#'@name
#'@rdname
#'@return
#'@examples
#'@export

prior_probs <- function(x = NULL, levels = NULL) {


  if(!is.null(levels)) {
    x <- factor(x, levels = levels)
  }

  prior <- as.matrix(table(x))

  x <- prior[,1] / sum(prior)

  prior <-  matrix(x,
                   nrow = 1,
                   ncol = length(x),
                   dimnames = list(c(NULL), names(x)))

  prior <- dplyr::as_tibble(prior)

  return(prior)
}


#'@title
#'@description
#'@param
#'@name
#'@rdname
#'@return
#'@examples
#'@export


naive_names <- function(x = NULL){

  if(is_naive_bayes_text(x)){

    attr(x, "classname")

  } else {

    NULL

  }
}

#'@title
#'@description
#'@param
#'@name
#'@rdname
#'@return
#'@examples
#'@export


frequency_matrix <- function(text = NULL, .classname = NULL, freq.method =  c("tdm", "wtdm"), laplace = 1, tot.words = NULL ){


  null_df <- dplyr::tibble(word = tot.words, freq = 1, N = length(tot.words), probs = freq/N)

  col_names <- c("word", paste0(c("freq.","N.", "probs."), .classname))
  colnames(null_df) <- col_names
  freq.method <- freq.method[1]
  tot.words <- null_df[,1]


  if(!purrr::is_empty(text)){

    frequency_df <- create_matrix(text)[[freq.method]]

    m <- as.matrix(frequency_df)
    word_freqs = sort(rowSums(m), decreasing=TRUE)

    frequency_df <- dplyr::tibble(word = names(word_freqs), freq = word_freqs)

    frequency_df <- dplyr::full_join(frequency_df, tot.words , "word")
    frequency_df$freq <- if_na(frequency_df$freq, y = 0)
    frequency_df$freq <-  frequency_df$freq + laplace

    frequency_df$N <- sum(frequency_df$freq)
    frequency_df$probs <- (frequency_df$freq) / (frequency_df$N[1])


    colnames(frequency_df) <- col_names
    return(frequency_df)
  } else {
    return(null_df)
  }

}



#'@title
#'@description
#'@param
#'@name
#'@rdname
#'@return
#'@examples
#'@export


naive_bayes_text <- function(x = NULL, y = NULL, prior = NULL, laplace = 1, freq.method = c("tdm", "wtdm")){


  freq.method <- freq.method[1]
  .data <- dplyr::tibble(text = x, label = y)

  if(is.null(prior)){
    prior <- prior_probs(.data$label)
    .classname <- colnames(prior)
  } else {
    .classname = names(prior)
    prior <- matrix(prior, ncol = length(prior), dimnames = list(c(), .classname))
    prior <- dplyr::as_tibble(prior)
  }

  .l <- purrr::map(.classname, ~dplyr::filter(.data, label == .x)$text)
  names(.l) <- .classname

  unique.words <- unique_words(unlist(.l))

  frequency_list <- purrr::map2(.l,.classname, ~frequency_matrix(text = .x,.classname = .y, laplace = laplace, freq.method = freq.method,  tot.words = unique.words))

  frequency <- full_join_list(frequency_list, by = "word")

  frequency <- cbind(frequency, prior)
  frequency <- dplyr::as_tibble(frequency)


  attr(frequency, "classname") <- .classname
  attr(frequency, "class") <- c("naive_bayes_text", class(frequency))
  attr(frequency, "freq.method") <- freq.method
  attr(frequency, "data")  <- .data

  return(frequency)

}


