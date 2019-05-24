#'@title Barras Bivariada
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
#'@param fill = "gray30"
#'
#'@param alpha = 1
#'
#'@param position = "stack"
#'
#'@param theme = NULL
#'
#'@return plot
#'
#'@examples conjunta_cd(iris)
#'
#'@import ggplot2
#'
#'@export

conjunta_cd <- function(df,
                        grid = TRUE,
                        axis = FALSE,
                        plot_type = "bar",
                        aes_fill = FALSE,
                        coord_flip = FALSE,
                        col = "black",
                        fill = "gray30",
                        alpha = 1,
                        bar_position = "stack",
                        box_position = "dodge2",
                        violin_position = "dodge",
                        box_stat = "boxplot",
                        violin_stat = "ydensity",
                        theme = NULL)
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
    
    if(plot_type == "bar" | plot_type == "col")
    {
      
      if(fill == TRUE)
      {
        p <- ggplot(df, aes_string(x = as.character(comb[i,1]),
                                   y = as.character(comb[i,2]),
                                   fill = as.character(comb[i,1])))+
          geom_col(alpha = alpha,
                   position = bar_position)+
          theme
      } else
      {
        p <- ggplot(df, aes_string(x = as.character(comb[i,1]),
                                   y = as.character(comb[i,2])))+
          geom_col(alpha = alpha,
                   fill = fill,
                   position = bar_position)+
          theme
      }
    } else if(plot_type == "boxplot" | plot_type == "box")
    {
      p <- ggplot(df, aes_string(x = as.character(comb[i,1]),
                                 y = as.character(comb[i,2])))+
        geom_boxplot(col = col,
                     fill = fill,
                     alpha = alpha,
                     position = box_position,
                     outlier.color = outlier.color,
                     outlier.fill = outlier.fill,
                     outlier.shape = outlier.shape,
                     outlier.size = outlier.size,
                     notch = notch)+
        theme
    } else if(plot_type == "violin" | plot_type == "viol")
    {
      p <- ggplot(df, aes_string(x = as.character(comb[i,1]),
                                 y = as.character(comb[i,2])))+
        geom_violin(col = col,
                    fill = fill,
                    alpha = alpha,
                    position = violin_position,
                    stat = violin_stat)+
        theme
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
