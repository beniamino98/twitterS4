#'@title
#'@description
#'@param
#'@name
#'@rdname
#'@return
#'@examples
#'@export


comparison_cloud <- function(.l = NULL, stopwords = NULL, labels = NULL, max.words = 150, scale = c(3,.5),
                             colors = NULL, title = NULL, control = list(), ...){


  if(is.null(colors)){

    colors <- RColorBrewer::brewer.pal(8, "Set2")

  }


  text <- purrr::map_chr(.l, ~paste(.x, collapse = " "))

  tdm <- create_matrix(text, stopwords = stopwords, control = control)$tdm
  tdm <- as.matrix(tdm)

  if(is.null(labels)) labels <- paste0("class.",1:ncol(tdm))

  colnames(tdm) <- labels

  if(is.null(title))  title <-  "Comparison Cloud"

  # comparison cloud
  wordcloud::comparison.cloud( term.matrix = tdm,
                               scale = scale,
                               max.words = max.words,
                               random.order = F,
                               rot.per = 0.2,
                               colors = colors,
                               title.size = 1.5,
                               title.colors = colors,
                               title.bg.colors = "#efefef", ...)

  text(0.5,1,title, cex= 2.9, font = 1.5, adj = 2, pos = 1)


}

