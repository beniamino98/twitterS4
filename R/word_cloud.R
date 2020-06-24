#'@title
#'@description
#'@param
#'@name
#'@rdname
#'@return
#'@examples
#'@export

word_cloud <- function(text = NULL, stopwords = NULL, max.words = 100,
                      min.freq = 20, scale = c(3, 0.4), titolo = "wordcloud", colors = NULL, control = list(), ...){

  dm <-  create_matrix(text, stopwords = stopwords, control = control)$dm[,1:2]

  if(is.null(colors)){
    colors <- RColorBrewer::brewer.pal(5, "Set2")
  }



  par(mar=c(0,0,0,0))
  wordcloud::wordcloud(words = dm$word, freq = dm$freq,
                       scale = scale,
                       min.freq = min.freq,
                       max.words = max.words,
                       random.order = FALSE,
                       colors = colors,...)

  text(0.5,1,titolo,  font=1.5, cex = 2, family = "mono", adj = 0, pos = 3)

}
