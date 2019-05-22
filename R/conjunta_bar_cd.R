#'@title Gera varios graficos de barra com variaveis de um dataframe
#'
#'@description Retorna uma lista de graficos de barra, ou um grid de graficos de barra com as conjuntas de variaveis continuas e discretas de um dado dataframe
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
#'@param bar_fill = "gray30"
#'
#'@param bar_alpha = 1
#'
#'@param bar_position = "stack"
#'
#'@return plot
#'
#'@examples conjunta_bar_cd(iris)
#'
#'@import ggplot2
#'
#'@export

conjunta_bar_cd <- function(df,
                        grid = TRUE,
                        axis = FALSE,
                        fill = FALSE,
                        coord_flip = FALSE,
                        bar_fill = "gray30",
                        bar_alpha = 1,
                        bar_position = "stack")
{
  cols <- names(df)
  lista <- list()
  varsc <- c()
  varsd <- c()

  for(i in 1:length(cols))
  {  
    if(is.numeric(df[[i]][[1]]) | is.double(df[[i]][[1]]))
    {
      varsc <- c(varsc, names(df)[i])
    }
  }
 
  if(is.null(varsc))
  {
    stop("O dataframe nao tem variaveis continuas",
         call. = FALSE)
  }
  
  for(i in 1:length(cols))
  {
    if(is.character(df[[i]][[1]]) | is.factor(df[[i]][[1]]))
    {
      varsd <- c(varsd, names(df)[i])
    }
  }
  
  if(is.null(varsd))
  {
    stop("O dataframe nao tem variaveis discretas",
         call. = FALSE)
  }
  
  comb <- expand.grid(varsd, varsc)
  
  for(i in 1:length(comb[,1]))
  {
    name <- paste0(comb[i,1], "_", comb[i,2])
    
    if(fill == TRUE)
    {
      p <- ggplot(df, aes_string(x = as.character(comb[i,1]),
                                 y = as.character(comb[i,2]),
                                 fill = as.character(comb[i,1])))+
        geom_col(alpha = bar_alpha,
                 position = bar_position)+
        theme_minimal()
    } else
    {
      p <- ggplot(df, aes_string(x = as.character(comb[i,1]),
                                 y = as.character(comb[i,2])))+
        geom_col(alpha = bar_alpha,
                 fill = bar_fill,
                 position = bar_position)+
        theme_minimal()
    }
    
    if(coord_flip == TRUE)
    {
      p <- p+coord_flip()
    }
    
    if(axis == TRUE)
    {
      lista[[name]] <- p
    } else
    {
      lista[[name]] <- p+theme(axis.ticks.x = element_blank(),
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
