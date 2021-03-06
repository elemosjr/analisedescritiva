% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_all.R
\name{plot_all}
\alias{plot_all}
\title{Plot de todas as variaveis}
\usage{
plot_all(df, grid = TRUE, num_plot = "density", coord_flip = FALSE,
  axis = FALSE, aes_fill = FALSE, order_data = TRUE, col = "black",
  fill = "gray40", alpha = 1, bar_position = "stack",
  density_position = "identity", violin_position = "dodge",
  box_position = "dodge2", hist_position = "stack",
  density_stat = "density", violin_stat = "ydensity",
  box_stat = "boxplot", hist_stat = "bin", binwidth = NULL,
  bins = 10, outlier.color = NULL, outlier.fill = NULL,
  outlier.shape = 19, outlier.size = 1.5, notch = FALSE,
  notchwidth = 0.5, add = NULL, na.rm = FALSE)
}
\arguments{
\item{df}{}

\item{grid}{= TRUE}

\item{num_plot}{= "boxplot"}

\item{coord_flip}{= FALSE}

\item{axis}{= FALSE}

\item{aes_fill}{= FALSE}

\item{col}{= "black"}

\item{fill}{= "gray40"}

\item{alpha}{= 1}

\item{binwidth}{= NULL}

\item{bins}{= NULL}

\item{outlier.color}{= NULL}

\item{outlier.fill}{= NULL}

\item{outlier.shape}{= 19}

\item{outlier.size}{= 1.5}

\item{notch}{= FALSE}

\item{notchwidth}{= 0.5}

\item{order_by}{= "data"}
}
\value{
plot
}
\description{
Retorna uma lista de graficos
}
\examples{
plot_all(iris)

}
