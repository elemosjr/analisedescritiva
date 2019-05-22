#'@title Gera varios graficos
#'
#'@description Retorna uma lista de graficos
#'
#'@param df
#'
#'@param axis = FALSE
#'
#'@param grid = TRUE
#'
#'@param col = "black"
#'
#'@param fill = "gray40"
#'
#'@param stat = "bin"
#'
#'@param position = "stack"
#'
#'@param binwidth = NULL
#'
#'@param bins = 10
#'
#'@return plot
#'
#'@examples conjunta_bar_cd(iris)
#'
#'@import ggplot2
#'
#'@export

hist_continuas <- function(df,
                           axis = FALSE,
                           grid = TRUE,
                           col = "black",
                           fill = "gray40",
                           stat = "bin",
                           position = "stack",
                           binwidth = NULL,
                           bins = 10)
{
  cols <- names(df)
  lista <- list()
  vars_id <- c()
  for(i in 1:length(cols))
  {  
    if(is.numeric(df[[i]][[1]]) | is.double(df[[i]][[1]]))
    {
      vars_id <- c(vars_id, i)
    }
  }
  
  if(is.null(vars_id))
  {
    stop("O dataframe nao tem variaveis continuas")
  }
  
  for(i in vars_id)
  {
    p <- ggplot(df, aes_string(x = names(df)[i]))+
      geom_histogram(col = col,
                     fill = fill,
                     stat = stat,
                     position = position,
                     binwidth = binwidth,
                     bins = bins)
    
    if(axis == FALSE)
    {
      p <- p+theme(axis.ticks.x = element_blank(),
                   axis.text.x = element_blank(),
                   axis.ticks.y = element_blank(),
                   axis.text.y = element_blank())
    }
  
    lista[[names(df)[i]]] <- p
    
  }

  
  if(grid == TRUE)
  {
    grid_list(lista)
  } else
  {
    return(lista) 
  }
}
