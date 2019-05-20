#'@title Gera um grid com varios graficos
#'
#'@description A partir de uma lista de graficos este pacote nos gera apenas um grafico com todos os contidos na lista.
#'
#'@details This function was suposedly made by Josh O'Brien. The code was shared in an answer at stackoverflow <https://stackoverflow.com/a/10706828>
#'
#'@param lista
#'
#'@return plot
#'
#'@examples grid_list(list(qplot(mpg, wt, mtcars), qplot(factor(cyl), wt, mtcars)), grid = TRUE)
#'
#'@export

grid_list <- function(lista)
{
  lista <- lista[!sapply(lista, is.null)]
  n <- length(lista)
  nCol <- floor(sqrt(n))
  do.call("grid.arrange", c(lista, ncol = nCol))
}
