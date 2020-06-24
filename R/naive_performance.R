#'@title
#'@description
#'@param
#'@name
#'@rdname
#'@return
#'@examples
#'@export

evaluate_naive_bayes <- function(.data = NULL, seeds = NULL, prop.train = 0.8){

  i.max <- length(seeds)

  result <- tibble(sample = integer(i.max),
                   method = rep("naive_bayes", i.max),
                   accuracy = integer(i.max),
                   sensitivity = integer(i.max),
                   specificity = integer(i.max),
                   precision = integer(i.max))

  for(i in 1:i.max){

    message("iter: ", i)
    sample(seeds[i])
    index.train <- sample(nrow(.data), nrow(.data)*prop.train)

    train <- .data[index.train,]
    test  <- .data[-index.train,]


    model <- twitterS4::naive_bayes_text(train$text, train$label, laplace = 1, freq = "tdm")

    pred_test <- predict(model, test$text, test$label)

    result$accuracy[i] <- attr(pred_test, "accuracy")

    pred_test <-mutate(pred_test, test = test$label)

    result$sample[i] <- seeds[i]
    result$sensitivity[i] <- sum(pred_test$label == "positivo" & pred_test$test == "positivo") / sum(pred_test$test == "positivo")
    result$specificity[i] <- sum(pred_test$label == "negativo" & pred_test$test == "negativo") / sum(pred_test$test == "negativo")
    result$precision[i]   <- sum(pred_test$label == "positivo" & pred_test$test == "positivo") / sum(pred_test$label == "positivo")

  }

  return(result)

}


#'@title
#'@description
#'@param
#'@name
#'@rdname
#'@return
#'@examples
#'@export

naive_performance <- function(train_data, seq.by = NULL, prop.train = 0.8, laplace = 1, freq = "tdm", seed = 1, plot = TRUE, verbose = TRUE){


  .seq <-c(seq.int(seq.by, nrow(train_data), seq.by), nrow(train_data))
  .seq <- .seq[!duplicated(.seq)]

  res <- list()

  for(i in 1:length(.seq)){

    if(verbose) message("Interation ", i, "/", length(.seq) )

    set.seed(seed)

    index.train <- sample(nrow(train_data[1:.seq[i],]), nrow(train_data[1:.seq[i],])*prop.train)

    train <- train_data[1:.seq[i],][ index.train,]
    test  <- train_data[1:.seq[i],][-index.train,]

    model <- naive_bayes_text(train$text, train$label, laplace = laplace, freq = freq)

    pred_train <- predict(model, verbose = verbose)
    pred_test  <- predict(model, test$text,test$label, verbose = verbose)

    test_accuracy  <- mean(test$label == pred_test$label)
    train_accuracy <- mean(train$label == pred_train$label)

    n.words <- nrow(model)

    res[[i]] <- dplyr::tibble(n.words = n.words, train_accuracy = train_accuracy, test_accuracy = test_accuracy)

  }

  res <-  dplyr::bind_rows(res)
  res <- dplyr::arrange(res, n.words)


  attr(model, "performance") <- res

  return(model)

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


plot_naive_performance <- function(model = NULL){


  df_performance <- attr(model, "performance")

  df<- dplyr::bind_rows(dplyr::tibble(n.words = df_performance$n.words,
                        accuracy = df_performance$train_accuracy,
                        tipologia = "training_data"),
                        dplyr::tibble(n.words = df_performance$n.words,
                        accuracy = df_performance$test_accuracy,
                        tipologia = "test_data"))


  ggplot2::ggplot(df)+
    ggplot2::geom_line(ggplot2::aes(n.words, accuracy, col = tipologia),size = 1.1)+
    ggplot2::geom_point(ggplot2::aes(n.words, accuracy,  col = tipologia), colour = "black", size = 1.5)+
    ggthemes::theme_solarized_2()+
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle=0, hjust=1, size = 35, colour = "black"),
      axis.text.y = ggplot2::element_text(angle=0, hjust=1, size = 35, colour = "black"),
      legend.text = ggplot2::element_text(angle=0, hjust=1, size = 35, colour = "black"),
      title = ggplot2::element_text(size = 35, colour = "black"))+
    ggplot2::scale_fill_brewer(palette = "Set2")+
    ggplot2::xlab("Numero di parole")+
    ggplot2::ylab("Accuratezza")+
    ggplot2::ggtitle("Accuratezza al variare del numero di parole")

}
