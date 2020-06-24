#'@title
#'@description
#'@param
#'@name
#'@rdname
#'@return
#'@examples
#'@export

polarity_cloud <- function(text = NULL, stopwords = NULL, lexicoin = NULL,
                           max.words = 150, title = "comparison cloud polarity", title.size = 2,
                           scale = c(3, 0.4), colors = c("gray50","darkgreen", "darkred"),control = list(), ... ){

  # estrazione del dataframe con le frequenze delle parole
  dm <- suppressWarnings(create_matrix(text, stopwords = stopwords, control = control)$dm[,1:2])

  sent <- purrr::map_dbl(dm$word, ~get_sentiments(.x, lexicoin = lexicoin))
  dm <- dplyr::mutate(dm, sentiment = sent)

  message("preparo il grafico...")

  # numero di parole da considerare per ciascun gruppo
  n.words = trunc(max.words / 3)


  polarity_matrix <- function(dm = NULL, sentiment = "positivo", n.words = NULL){

    labels <-  c("neutro", "positivo", "negativo")
    if(is.null(dm)){
      m <- matrix(nrow = 1, ncol = 3, dimnames = list(dm$word, labels))
      return(m)
    }

    sentiment <- match.arg(sentiment, labels )

    label <- dplyr::case_when(sentiment == "positivo" ~ 1,
                       sentiment == "negativo" ~ -1 ,
                       sentiment == "neutro" ~ 0)


    dm <- dplyr::filter(dm, sentiment == label)[1:n.words,]

    m <- matrix(nrow = nrow(dm), ncol = 3, dimnames = list(dm$word, labels))

    m[, sentiment] <- dm$freq
    m <- if_na(m)
    m
  }


  # term document matrix con le frequenze delle parole
  tdm <- rbind(polarity_matrix(dm , "neutro", n.words),
               polarity_matrix(dm, "positivo", n.words),
               polarity_matrix(dm, "negativo", n.words))

  tdm <- na.omit(tdm)
  tdm <- as.matrix(tdm)


  # creazione del grafico
  par(mar=c(0,0,0,0))
  wordcloud::comparison.cloud(tdm,
                              colors = colors,
                              max.words=max.words,
                              scale = scale,
                              random.order = FALSE,
                              title.size = title.size,
                              match.colors = T, ...)


  text(0.5,1, title,  font=1.5, cex = 2.9, adj = 2, pos = 1)


}
