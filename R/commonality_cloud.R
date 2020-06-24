#'@title
#'@description
#'@param
#'@name
#'@rdname
#'@return
#'@examples
#'@export
commonality_cloud <- function(.l = NULL, stopwords = NULL, max.words = 150, scale = c(3,.5),
                              colors = NULL, title = NULL, control = list(), ... ){


  if(is.null(colors)){

    colors <- RColorBrewer::brewer.pal(8, "Set2")

  }


  text <- purrr::map_chr(.l, ~paste(.x, collapse = " "))

  tdm <- create_matrix(text, stopwords = stopwords, control = control)$tdm
  tdm <- as.matrix(tdm)

  if(is.null(title))  title <-  "Commonality Cloud"

  index.common <- c()
  for(i in 1:nrow(tdm)){
    index.common[i] <- !any(tdm[i,] == 0)
  }

  wordcloud::commonality.cloud(term.matrix = tdm[index.common,],
                               max.words = max.words,
                               scale = scale,
                               random.order=FALSE,
                               col = colors, ...)

  text(0.5,1,title, cex= 2.9, font = 1.5, adj = 2, pos = 1)


}
