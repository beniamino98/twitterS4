#'@title
#'@description
#'@param
#'@name
#'@rdname
#'@return
#'@examples
#'@export
# preprocessamento su un vettore di testi

PreProcess <- function(txt,
                       link = TRUE,
                       hashtag = TRUE,
                       mention = TRUE,
                       conversion = TRUE,
                       number = TRUE,
                       tolower = TRUE,
                       stopwords = TRUE,
                       dizionari = NULL,
                       lemmatizza = FALSE,
                       categoria = NULL,
                       lang = "it",
                       verbose = TRUE){


  if( (is.logical(stopwords) && stopwords) && !is.null(dizionari$stopwords)){

    stopwords <- dizionari$stopwords$word

  } else if(is.character(stopwords)) {
    stopwords <-  stopwords
  } else {
    stopwords <- NULL

  }
  if((is.logical(lemmatizza) & lemmatizza) && !is.null(dizionari$lemmario)){

    lemmario <- dizionari$lemmario

  } else {

    lemmario <- NULL

  }

  if(!is.null(dizionari$complex_words)){

    complex_words <- dizionari$complex_words

  } else {

    complex_words <- NULL

  }

  safe_preprocess <- purrr::safely(preprocess_text)

  text_clean <- purrr::map_chr(txt, ~safe_preprocess(.x,
                                                     link = link,
                                                     hashtag = hashtag ,
                                                     mention = mention,
                                                     conversion = conversion,
                                                     number = number,
                                                     tolower = tolower,
                                                     stopwords = stopwords,
                                                     complex_words = complex_words,
                                                     lang = lang )$result)

  if(verbose) message("pulizia dei testi completata!")

  text_clean <- ifelse(text_clean == "", NA_character_, text_clean)


  if(length(text_clean) == 1 && is.na(text_clean )) return(NA_character_)


  if((is.logical(lemmatizza) & lemmatizza) && !is.null(lemmario)){

    if(verbose) message("inizio lemmatizzazione dei testi")

    text_clean <- lemmatize(text_clean, lexicoin = lemmario, categoria = categoria, verbose = F)


  }

  return(text_clean)

}

#'@title
#'@description
#'@param
#'@name
#'@rdname
#'@return
#'@examples
#'@export

# preprocessamento su una testo
preprocess_text <- function(x,
                            link = TRUE,
                            hashtag = TRUE,
                            mention = TRUE,
                            conversion = TRUE,
                            number = TRUE,
                            tolower = TRUE,
                            stopwords = TRUE,
                            complex_words = NULL,
                            lang = "it"){

  remove_comma <- function(x) stringr::str_remove_all(x, "[,]")

  # condizioni di controllo
  if(is.null(x) || !is.character(x) ){

    message("vettore testi non valido")

    return(NA)

  }

  # rimozione delle virgole
  txt <- remove_comma(x)

  # rimozione dei link

  if( link ){

    txt <- gsub("(f|ht)(tp)(s?)(://)(.\\S+)[.|/](.\\S+)", " ", txt)

  }

  # rimuovo gli hashtag se TRUE
  # non li rimuovo se FALSE

  if( hashtag ) {

    txt <- gsub("#\\S+", " ", txt)

  }

  # rimuovo le mention se TRUE
  # non li rimuovo se FALSE

  if( mention ) {

    txt <- gsub("@\\S+", " ", txt)

  }

  # punteggiatura
  txt <- gsub("([#@])|[[:punct:]]", " \\1", txt)

  # caratteri di controllo
  txt <- gsub('[[:cntrl:]]', ' ', txt)

  # quelli che non sono caratteri grafici (quello che non è [[:alnum:][:punct:]])
  txt <- gsub('[^[:graph:]]', ' ', txt)


  # rimozione dei numeri se TRUE

  if(number) {

    txt <- gsub("[[:digit:]]", "", txt)

  }


  # conversione del testo  in latin1 se TRUE

  if( conversion ){

    txt <- iconv(txt, to = "latin1", sub = "")

  }


  # trasformazione di tutto il testo in minuscolo se TRUE
  if( tolower ) {

    txt <- base::tolower(txt)

  }

  # rimozione stopwords
  txt <- remove_stopwords(x = txt, stopwords = stopwords, lang = lang)

  # elimino spazi bianchi aggiuntivi
  txt <- tm::stripWhitespace(txt)

  # unisco le forme complesse
  if(!is.null(complex_words)){

    txt <- gsub_ngrams(txt, lexicoin = complex_words )
  }

  # rimozione stopword (parole unite in forme complesse )
  txt <-  remove_stopwords(x = txt, stopwords = stopwords, lang = lang )

  # elimino spazi bianchi aggiuntivi
  txt <- tm::stripWhitespace(txt)
  txt <- stringr::str_trim(txt)

  if( purrr::is_empty(txt) || is.na(txt)){ return(NA_character_) }

  return(txt)

}



