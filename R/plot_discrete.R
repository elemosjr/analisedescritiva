#'@title Plot Discrete
#'
#'@description Retorna uma lista de graficos
#'
#'@param df
#'
#'@param grid = TRUE
#'
#'@param axis = FALSE
#'
#'@param factor = NULL
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
#'@param theme = NULL
#'
#'@return plot
#'
#'@examples conjunta_bar_cd(iris)
#'
#'@import ggplot2
#'@import dplyr
#'
#'@export

plot_discrete <- function(data,
                          grid = TRUE,
                          coord_flip = FALSE,
                          axis = FALSE,
                          factors = NULL,
                          aes_fill = FALSE,
                          col = "black",
                          fill = "gray40",
                          alpha = 1,
                          position = "dodge",
                          add = NULL)
{
  
  if(is.null(factors))
  {} else if(!is.character(factors))
  {
    stop("factors must be a character or character vector.",
         call. = FALSE)
  } else if(!(factors %in% names(df)))
  {
    stop("variable in factors is not in the dataset",
         call. = FALSE)
  } else
  {
    df <- mutate_at(df, factors, factor)
  }
  
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
  }
  
  for(i in vars_id)
  {
    if(aes_fill == TRUE)
    {
      p <- ggplot(df, aes_string(x = names(df)[i],
                                 fill = names(df)[i]))+
        geom_bar(col = col,
                 alpha = alpha,
                 position = position)+
        add
    } else
    {
      p <- ggplot(df, aes_string(x = names(df)[i]))+
        geom_bar(col = col,
                 fill = fill,
                 alpha = alpha,
                 position = position)+
        add
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
  
  if(grid == TRUE)
  {
    grid_list(lista)
  } else
  {
    return(lista) 
  }
}
