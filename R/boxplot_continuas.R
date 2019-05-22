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
#'@param coord_flip = FALSE
#'
#'@param col = "black"
#'
#'@param fill = "gray40"
#'
#'@param position = "dodge2"
#'
#'@param outlier.color = NULL
#'
#'@param outlier.fill = NULL
#'
#'@param outlier.shape = 19
#'
#'@param outlier.size = 1.5
#'
#'@param notch = FALSE
#'
#'@param notchwidth = 0.5
#'
#'@return plot
#'
#'@examples conjunta_bar_cd(iris)
#'
#'@import ggplot2
#'
#'@export

boxplot_continuas <- function(df,
                              grid = TRUE,
                              axis = FALSE,
                              coord_flip = FALSE,
                              col = "black",
                              fill = "gray40",
                              position = "dodge2",
                              outlier.color = NULL,
                              outlier.fill = NULL,
                              outlier.shape = 19,
                              outlier.size = 1.5,
                              notch = FALSE,
                              notchwidth = 0.5)
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
  } else
  {
    for(i in vars_id)
    {
      p <- ggplot(df, aes_string(y = names(df)[i]))+
        geom_boxplot(col = col,
                     fill = fill,
                     position = position,
                     outlier.color = outlier.color,
                     outlier.fill = outlier.fill,
                     outlier.shape = outlier.shape,
                     outlier.size = outlier.size,
                     notch = notch)
      
      if(axis == FALSE)
      {
        p <- p+theme(axis.ticks.x = element_blank(),
                     axis.text.x = element_blank(),
                     axis.ticks.y = element_blank(),
                     axis.text.y = element_blank(),
                     legend.position = "none")
      }
      
      if(coord_flip == TRUE)
      {
        p <- p+coord_flip()
      }
    
      lista[[names(df)[i]]] <- p
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
