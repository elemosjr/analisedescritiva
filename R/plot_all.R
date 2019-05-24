#'@title Plot de todas as variaveis
#'
#'@description Retorna uma lista de graficos
#'
#'@param df
#'
#'@param grid = TRUE
#'
#'@param axis = FALSE
#'
#'@param aes_fill = FALSE
#'
#'@param order_by = "data"
#'
#'@param num_plot = "boxplot"
#'
#'@param coord_flip = FALSE
#'
#'@param col = "black"
#'
#'@param fill = "gray40"
#'
#'@param alpha = 1
#'
#'@param binwidth = NULL
#'
#'@param bins = NULL
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
#'@examples plot_all(iris)
#'
#'@import ggplot2
#'
#'@export

plot_all <- function(df,
                     grid = TRUE,
                     axis = FALSE,
                     aes_fill = FALSE,
                     order_data = TRUE,
                     num_plot = "density",
                     coord_flip = FALSE,
                     col = "black",
                     fill = "gray40",
                     alpha = 1,
                     bar_position = "stack",
                     density_position = "identity",
                     violin_position = "dodge",
                     box_position = "dodge2",
                     hist_position = "stack",
                     density_stat = "density",
                     violin_stat = "ydensity",
                     box_stat = "boxplot",
                     hist_stat = "bin",
                     binwidth = NULL,
                     bins = 10,
                     outlier.color = NULL,
                     outlier.fill = NULL,
                     outlier.shape = 19,
                     outlier.size = 1.5,
                     notch = FALSE,
                     notchwidth = 0.5,
                     na.rm = FALSE)
{
  cols <- names(df)
  lista <- list()
  varsd <- c()
  varsc <- c()
  
  for(i in 1:length(cols))
  {  
    if(is.numeric(df[[i]][[1]]) | is.double(df[[i]][[1]]))
    {
      varsc <- c(varsc, i)
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
      varsd <- c(varsd, i)
    }
  }
  
  if(is.null(varsd))
  {
    stop("O dataframe nao tem variaveis discretas",
         call. = FALSE)
  }
  
  for(i in varsd)
  {
    if(aes_fill == TRUE)
    {
      p <- ggplot(df, aes_string(x = names(df)[i],
                                 fill = names(df)[i]))+
        geom_bar(col = col,
                 position = bar_position,
                 alpha = alpha,
                 na.rm = na.rm)
    } else
    {
      p <- ggplot(df, aes_string(x = names(df)[i]))+
        geom_bar(fill = fill,
                 position = bar_position,
                 col = col,
                 alpha = alpha,
                 na.rm = na.rm)
    }
    
    if(coord_flip == TRUE)
    {
      p <- p+coord_flip()
    }
    
    if(axis == FALSE)
    {
      p <- p+theme(axis.ticks.x = element_blank(),
              axis.text.x = element_blank(),
              axis.ticks.y = element_blank(),
              axis.text.y = element_blank(),
              legend.position = "none")
    }
    
    lista[[names(df)[i]]] <- p
    
  }
  
  if(num_plot == "boxplot" | num_plot == "box")
  {
    for(i in varsc)
    {
      p <- ggplot(df, aes_string(y = names(df)[i]))+
        geom_boxplot(col = col,
                     fill = fill,
                     alpha = alpha,
                     position = box_position,
                     stat = box_stat,
                     outlier.color = outlier.color,
                     outlier.fill = outlier.fill,
                     outlier.shape = outlier.shape,
                     outlier.size = outlier.size,
                     notch = notch,
                     na.rm = na.rm)
      
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
  } else if(num_plot == "histogram" | num_plot == "hist")
  {
    for(i in varsc)
    {
      p <- ggplot(df, aes_string(x = names(df)[i]))+
        geom_histogram(col = col,
                       fill = fill,
                       alpha = alpha,
                       position = hist_position,
                       stat = hist_stat,
                       binwidth = binwidth,
                       bins = bins,
                       na.rm = na.rm)
      
      if(axis == FALSE)
      {
        p <- p+theme(axis.ticks.x = element_blank(),
                     axis.text.x = element_blank(),
                     axis.ticks.y = element_blank(),
                     axis.text.y = element_blank())
      }
      
      if(coord_flip == TRUE)
      {
        p <- p+coord_flip()
      }
      
      lista[[names(df)[i]]] <- p
      
    }
  } else if(num_plot == "density")
  {
    for(i in varsc)
    {
      p <- ggplot(df, aes_string(x = names(df)[i]))+
        geom_density(fill = fill,
                     col = col,
                     position = density_position,
                     stat = density_stat,
                     alpha = alpha,
                     na.rm = na.rm)
      
      if(axis == FALSE)
      {
        p <- p+theme(axis.ticks.x = element_blank(),
                     axis.text.x = element_blank(),
                     axis.ticks.y = element_blank(),
                     axis.text.y = element_blank())
      }
      
      if(coord_flip == TRUE)
      {
        p <- p+coord_flip()
      }
      
      lista[[names(df)[i]]] <- p
    }
  } else if(num_plot == "violin" | num_plot == "viol")
  {
    for(i in varsc)
    {
      p <- ggplot(df, aes_string(x = names(df)[i]))+
        geom_violin(fill = fill,
                    col = col,
                    position = violin_position,
                    stat = violin_stat,
                    alpha = alpha,
                    na.rm = na.rm)
      
      if(axis == FALSE)
      {
        p <- p+theme(axis.ticks.x = element_blank(),
                     axis.text.x = element_blank(),
                     axis.ticks.y = element_blank(),
                     axis.text.y = element_blank())
      }
      
      if(coord_flip == TRUE)
      {
        p <- p+coord_flip()
      }
      
      lista[[names(df)[i]]] <- p
    }
  }
  
  if(order_data == TRUE)
  {
    lista <- lista[names(df)]
  } 
  
  if(grid == TRUE)
  {
    grid_list(lista)
  } else
  {
    return(lista)
  }
}
