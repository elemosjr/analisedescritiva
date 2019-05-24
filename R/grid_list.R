#'@title Gera um grid com varios graficos
#'
#'@description A partir de uma lista de graficos este pacote nos gera apenas um grafico com todos os contidos na lista.
#'
#'@details This code was taken from an answer from Josh O'Brien in stackoverflow <https://stackoverflow.com/a/10706828>
#'
#'@param lista
#'
#'@return plot
#'
#'@examples grid_list(list(qplot(mpg, wt, mtcars),qplot(factor(cyl), wt, mtcars)), grid = TRUE)
#'
#'@import gridExtra
#'
#'@export

grid_list <- function(lista)
{
  lista <- lista[!sapply(lista, is.null)]
  n <- length(lista)
  nCol <- floor(sqrt(n))
  do.call("grid.arrange", c(lista, ncol = nCol))
}
