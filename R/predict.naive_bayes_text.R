#'@title
#'@description
#'@param
#'@name
#'@rdname
#'@return
#'@examples
#'@export
predict_naive <- function(model = NULL, text = NULL){

  token <- get_tokens(text)
  .classname <- naive_names(model)

  calculate_probs <- function(dm = NULL, .classname = NULL){

    probs.name <- paste0("probs.", .classname)
    prior.name <-  .classname

    prior.probs <- ifelse(dm[[prior.name]][1] == 0, 1, dm[[prior.name]][1])

    x <- sum(log(dm$freq * dm[[probs.name]])) + log(prior.probs )
    names(x) <- .classname

    return(x)

  }


  filter_model <- dplyr::filter(model, word %in% token)

  dm <- create_matrix(text)$dm[,1:2]

  dm <- na.omit(full_join_list(list(dm, filter_model), "word"))


  probs <- purrr::map_dbl(.classname, ~calculate_probs(dm, .x))
  probs <- if_na(probs)

  if(sum(purrr::map_lgl(probs, ~.x != 0)) == 0){
    probs <- log(model[,c(.classname)][1,1:length(.classname)])
  } else {
    probs <- matrix(probs, ncol = length(probs), dimnames = list(c(), .classname))
    probs <- dplyr::as_tibble(probs)
  }

  label <- names(which.max(probs))

  polarity <- abs(max(probs) - min(probs))

  probs <- dplyr::bind_cols(dplyr::tibble(text = text), probs,
                            label = label, polarity = polarity,
                            confidence = 1-exp(-polarity))

  return(probs)

}


#'@title
#'@description
#'@param
#'@name
#'@rdname
#'@return
#'@examples
#'@export

predict.naive_bayes_text <- function(model = NULL, documents = NULL, label = NULL, verbose = TRUE){

  if(is.null(documents)){
    documents <- attr(model, "data")$text
    label <- attr(model, "data")$label
  }

  if(verbose){

    {pb <- txtProgressBar(min = 0,max = length(documents),style=3)
    prediction <- list()
    for(i in 1:length(documents)){
      setTxtProgressBar(pb,i)
      prediction[[i]] <- predict_naive(model, documents[i])

    }
    close(pb)}

  } else {
    prediction <- list()
    for(i in 1:length(documents)){
      prediction[[i]] <- predict_naive(model, documents[i])

    }
  }

  prediction <- dplyr::bind_rows(prediction)

  if(!is.null(label)){

    accuracy <- round(mean(label == prediction$label), 5 )
    attr(prediction, "accuracy") <- accuracy

    if(verbose) message("\n", "Accuracy: ", accuracy)

  }
  return(prediction)
}


