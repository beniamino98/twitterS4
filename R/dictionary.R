#'@title
#'@description
#'@param
#'@name
#'@rdname
#'@return
#'@examples
#'@export

dictionary <- function(positive_sentiment = NULL,
                           negative_sentiment = NULL,
                           complex_words = NULL,
                           stopwords = NULL,
                           lemmario = NULL,
                           nuovi_lemmi = NULL){



  # aggiungiamo il dataframe di sentimenti positivi (lang, word, sentiment, value )
  if(!is.null(positive_sentiment)){

    stopifnot(is.character(positive_sentiment))

    positive_sentiment <- suppressMessages(readr::read_delim(positive_sentiment, delim = ";|,"))

  } else {

    positive_sentiment <- NA

  }


  # aggiungiamo il dataframe di sentimenti negativi (lang, word, sentiment, value )
  if(!is.null(negative_sentiment)){

    stopifnot(is.character(negative_sentiment))

    negative_sentiment <- suppressMessages(readr::read_delim(negative_sentiment, delim = ";|,"))

  } else {

    negative_sentiment <- NA

  }

  # aggiungiamo il dataframe di parole complesse (complex_form, simple_form,  sentiment, value )

  if(!is.null(complex_words)){

    stopifnot(is.character(complex_words))

    complex_words <- suppressMessages(readr::read_delim(complex_words, delim = ";|,"))

  } else {

    complex_words <- NA

  }


  # aggiungiamo il dataframe di stopwords ( word )
  if(!is.null(stopwords)){

    stopifnot(is.character(stopwords))

    stopwords <- suppressMessages(readr::read_delim(stopwords, delim = ";|,"))

  } else {

    stopwords <- NA

  }

  # aggiungiamo il dataframe per la lemmatizzazione (forma, lemma, CatGrF, CatGrL, FreqF, RangoF, FreqL, RangoL)
  # se presente un ulteriore dataframe con nuovi lemmi viene aggiunto al lemmario di base

  if(!is.null(lemmario)){

    stopifnot(is.character(lemmario))

    lemmario <- suppressMessages(readr::read_delim(lemmario, delim = ";|,"))

    if(!is.null(nuovi_lemmi)){

      nuove_formelemmi <- suppressMessages( readr::read_delim(nuovi_lemmi, ";|,"))

      lemmario <- dplyr::bind_rows(lemmario, nuove_formelemmi)

    }

    lemmario <- dplyr::mutate(lemmario,
                              forma = stringr::str_trim(forma),
                              lemma = stringr::str_trim(lemma))



  } else {

    lemmario <- NA

  }

  # se vengono importati i dataframe con sentimenti positivi e negativi vengono uniti in un nuovo dataframe completo
  # se viene importato il dataframe con le forme complesse aggiungiamo anche le forme complesse al dizionario completo
  # aggiungiamo solo le forme complesse con etichetta positivo / negativo no neutro
  if( (length(negative_sentiment) != 1 && !is.na(negative_sentiment)) & (length(positive_sentiment) != 1 && !is.na(positive_sentiment)) ){

    sentiment_data <- dplyr::bind_rows(negative_sentiment, positive_sentiment)

    if( (length(complex_words) != 1 && !is.na(complex_words))){

      complex_sent <- dplyr::filter(complex_words, sentiment %in% c("positivo", "negativo"))
      complex_sent <- dplyr::mutate(complex_sent, lang = "italian")
      complex_sent <- dplyr::select(complex_sent, lang, word = "simple_form", sentiment, value)

      sentiment_data <- dplyr::bind_rows(sentiment_data, complex_sent)

    }

    sentiment_data <- dplyr::mutate(sentiment_data, word = stringr::str_trim(word))
    sentiment_data <- dplyr::arrange(sentiment_data, word)



  } else{

    sentiment_data <- NA
  }

  dizionari <- structure(
    list(
      sentiment_data = sentiment_data,
      positive_sentiment = positive_sentiment,
      negative_sentiment = negative_sentiment,
      complex_words = complex_words,
      stopwords = stopwords,
      lemmario = lemmario
    ),

    class = c("list", "dictionary")

  )

  return(dizionari)

}

