% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/grid_list.R
\name{grid_list}
\alias{grid_list}
\title{Gera um grid com varios graficos}
\usage{
grid_list(lista)
}
\arguments{
\item{lista}{}
}
\value{
plot
}
\description{
A partir de uma lista de graficos este pacote nos gera apenas um grafico com todos os contidos na lista.
}
\details{
This code was taken from an answer from Josh O'Brien in stackoverflow <https://stackoverflow.com/a/10706828>
}
\examples{
grid_list(list(qplot(mpg, wt, mtcars),qplot(factor(cyl), wt, mtcars)), grid = TRUE)

}
