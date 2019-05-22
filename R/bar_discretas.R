#'@title Gera varios graficos
#'
#'@description Retorna uma lista de graficos
#'
#'@param df
#'
#'@param grid = TRUE
#'
#'@param axis = FALSE
#'
#'@param fill = FALSE
#'
#'@param coord_flip = FALSE
#'
#'@param bar_fill = "gray40"
#'
#'@param bar_alpha = 1
#'
#'@param bar_position = "dodge"
#'
#'@return plot
#'
#'@examples conjunta_bar_cd(iris)
#'
#'@import ggplot2
#'
#'@export

bar_discretas <- function(df,
                          grid = TRUE,
                          axis = FALSE,
                          fill = FALSE,
                          coord_flip = FALSE,
                          bar_fill = "gray40",
                          bar_alpha = 1,
                          bar_position = "dodge")
{
  cols <- names(df)
  lista <- list()
  vars_id <- c()
  for(i in 1:length(cols))
  {
    if(is.character(df[[i]][[1]]) | is.factor(df[[i]][[1]]))
    {
      vars_id <- c(vars_id, i)
    }
  }
  
  if(is.null(vars_id))
  {
    stop("O dataframe nao tem variaveis discretas.", call. = FALSE)
  } else
  {
    for(i in vars_id)
    {
      if(fill == TRUE)
      {
        p <- ggplot(df, aes_string(x = names(df)[i],
                                   fill = names(df)[i]))+
          geom_bar(alpha = bar_alpha,
                   position = bar_position)
      } else
      {
        p <- ggplot(df, aes_string(x = names(df)[i]))+
          geom_bar(fill = bar_fill,
                   alpha = bar_alpha,
                   position = bar_position)
      }
      
      if(coord_flip == TRUE)
      {
        p <- p+coord_flip()
      }
      
      if(axis == TRUE)
      {
        lista[[names(df)[i]]] <- p
      } else
      {
        lista[[names(df)[i]]] <- p+theme(axis.ticks.x = element_blank(),
              axis.text.x = element_blank(),
              axis.ticks.y = element_blank(),
              axis.text.y = element_blank(),
              legend.position = "none")
      }
    }
  } 
  
  if(grid == TRUE)
  {
    grid_list(lista)
  } else
  {
    return(lista) 
  }
}
