#'@title Gera varios scatterplots com variaveis de um dataframe
#'
#'@description Retorna uma lista de scatterplots, ou um grid de scatterplots com as variaveis continuas de um dado dataframe
#'
#'@param df
#'
#'@param grid = TRUE
#'
#'@param axis = FALSE
#'
#'@param point_size = 0.5
#'
#'@param point_shape = 1
#'
#'@param point_color = "black"
#'
#'@param point_alpha = 0.8
#'
#'@param point_position = "identity"
#'
#'@return plot
#'
#'@examples conjunta_scat_cc(iris)
#'
#'@import ggplot2
#'
#'@export

conjunta_scat_cc <- function(df,
                        grid = TRUE,
                        axis = FALSE,
                        point_size = 0.5,
                        point_shape = 1,
                        point_color = "black",
                        point_alpha = 0.8,
                        point_position = "identity")
{
  cols <- names(df)
  lista <- list()
  vars <- c()
  
  for(i in 1:length(cols))
  {  
    if(is.numeric(df[[i]][[1]]) | is.double(df[[i]][[1]]))
    {
      vars <- c(vars, names(df)[i])
    }
  }
  
  if(is.null(vars))
  {
    stop("O dataframe nao tem variaveis continuas",
         call.= FALSE)
  }
  
  if(length(vars) < 2)
  {
    stop("O dataframe nao tem variaveis continuas o suficiente.",
         call. = FALSE)
  }
  
  comb <- combn(vars, 2)
  
  for(i in 1:length(comb[1,]))
  {
    name <- paste0(comb[,i][1], "_", comb[,i][2])
    
    p <- ggplot(df, aes_string(x = comb[,i][1], 
                               y = comb[,i][2]))+
      geom_point(size = point_size, 
                 shape = point_shape, 
                 color = point_color, 
                 alpha = point_alpha,
                 position = point_position)+
      theme_minimal()
    
  
      if(axis == FALSE)
      {
        p <- p+theme(axis.ticks.x = element_blank(),
              axis.text.x = element_blank(),
              axis.ticks.y = element_blank(),
              axis.text.y = element_blank())
  
        lista[[name]] <- p
  
      } else if(axis == TRUE)
      {
        lista[[name]] <- p
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
