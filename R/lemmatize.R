#'@title
#'@description
#'@param
#'@name
#'@rdname
#'@return
#'@examples

# lemmatizzazione della singola parola
search_lemma <- function(x = NULL, lexicoin = NULL, categoria = NULL, verbose = TRUE){

  if(is.null(x) || length(x) > 1 || !is.character(x) ){ return(NA_character_) }

  if(is.null(lexicoin) || sum(colnames(lexicoin) %in% c("forma", "lemma", "CatGrF", "CatGrL")) != 4){

    if(verbose) message("non è stato inserito alcun lemmario!")

    lemma_df <- dplyr::tibble(forma = x, lemma = x, CatGrF = "unknown", CatGrL = "unknown")

    return(lemma_df)

  }

  if( verbose ) message("Eseguo la lemmatizzazione di: ", x)


  lemma_df <- dplyr::filter(lexicoin, forma == x)

  if(nrow(lemma_df) == 0){

    lemma_df <- dplyr::tibble(forma = x, lemma = x, CatGrF = "unknown", CatGrL = "unknown" )

  }


  if(is.null(categoria)){

    lemma_df <- dplyr::select(lemma_df[1,], forma, lemma, CatGrF, CatGrL)

  } else{

    categoria <- match.arg(categoria, c("avverbio","aggettivo", "nome_proprio", "sostantivo", "verbo"))

    categoria <- dplyr::case_when(
      categoria == "avverbio" ~ "B",
      categoria == "aggettivo" ~ "G",
      categoria == "nome_proprio" ~ "E",
      categoria == "sostantivo" ~ "S",
      categoria == "verbo" ~ "V")

    lemma_df <- dplyr::filter(lemma_df, CatGrL == categoria)
    lemma_df <- dplyr::select(lemma_df, forma, lemma, CatGrF, CatGrL)
    lemma_df <- na.omit(lemma_df)[1,]

  }

  return(lemma_df)

}

#'@title
#'@description
#'@param
#'@name
#'@rdname
#'@return
#'@examples
#'@export

# lemmatizzazione di un testo
search_lemmi <- function(txt = NULL, lexicoin = NULL, categoria = NULL, collapse = TRUE, verbose = TRUE ){


  if(is.null(txt) || is.na(txt) ){

    return(NA_character_)

  }

  txt.tokens <- get_tokens(txt)

  if(verbose) message("lemmatizzazione testo in corso... " )

  txt.lemma <- purrr::map_df(txt.tokens, ~search_lemma(.x, lexicoin = lexicoin, categoria = categoria, verbose = FALSE ))

  if(!is.null(categoria)){

    categoria <- match.arg(categoria, c("avverbio","aggettivo", "nome_proprio", "sostantivo", "verbo"))
    categoria <- dplyr::case_when(
      categoria == "avverbio" ~ "B",
      categoria == "aggettivo" ~ "G",
      categoria == "nome_proprio" ~ "E",
      categoria == "sostantivo" ~ "S",
      categoria == "verbo" ~ "V")

    txt.lemma <- dplyr::filter(txt.lemma, CatGrF %in% categoria | CatGrL %in% categoria | CatGrF == "unknown")
  }

  if( collapse ){

    txt.lemma <- paste(txt.lemma$lemma, collapse=  " ")

    if( is.na(txt.lemma) || txt.lemma == "" ) { return(NA_character_) }

  }


  return(txt.lemma)

}

#'@title
#'@description
#'@param
#'@name
#'@rdname
#'@return
#'@examples
#'@export

# lemmatizzazione di un vettore di testi
# lemmatizzazione di un vettore di testi
lemmatize <- function(txt = NULL, lexicoin = NULL, categoria = NULL, verbose = FALSE){

  if(verbose) message("Inizio Lemmatizzazione del vettore di  testi in corso...")

  unique_words <- unique(unlist(purrr::map(txt, get_tokens)))

  lemmario_ridotto <- dplyr::filter(lexicoin, forma %in% unique_words)

  purrr::map_chr(txt, ~search_lemmi(.x, lexicoin = lemmario_ridotto, categoria = categoria, collapse = T, verbose = verbose ))

}


