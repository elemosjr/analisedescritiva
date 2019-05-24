#'@title Histograma
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
#'@param theme = NULL
#'
#'@return plot
#'
#'@examples conjunta_bar_cd(iris)
#'
#'@import ggplot2
#'
#'@export

plot_continuous <- function(df,
                           axis = FALSE,
                           grid = TRUE,
                           col = "black",
                           fill = "gray40",
                           alpha = 1,
                           plot_type = "density",
                           hist_position = "stack",
                           box_position = "dodge2",
                           violin_position = "dodge",
                           density_position = "identity",
                           hist_stat = "bin",
                           box_stat = "boxplot",
                           violin_stat = "ydensity",
                           density_stat = "density",
                           binwidth = NULL,
                           bins = 10,
                           outlier.color = NULL,
                           outlier.fill = NULL,
                           outlier.shape = 19,
                           outlier.size = 1.5,
                           notch = FALSE,
                           notchwidth = 0.5,
                           theme = NULL,
                           na.rm = FALSE)
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
    if(plot_type == "histogram" | plot_type == "hist")
    {
      p <- ggplot(df, aes_string(x = names(df)[i]))+
        geom_histogram(col = col,
                       fill = fill,
                       stat = hist_stat,
                       position = hist_position,
                       binwidth = binwidth,
                       bins = bins,
                       na.rm = na.rm)+
        theme
    } else if(plot_type == "boxplot" | plot_type == "box")
    {
      p <- ggplot(df, aes_string(y = names(df)[i]))+
        geom_boxplot(col = col,
                     fill = fill,
                     position = box_position,
                     stat = box_stat,
                     outlier.color = outlier.color,
                     outlier.fill = outlier.fill,
                     outlier.shape = outlier.shape,
                     outlier.size = outlier.size,
                     notch = notch,
                     na.rm = na.rm)+
        theme
    } else if(plot_type == "violin" | plot_type == "viol")
    {
      p <- ggplot(df, aes_string(y = names(df)[i]))+
        geom_violin(aes(x = "x"),
                    col = col,
                    fill = fill,
                    alpha = alpha,
                    position = violin_position,
                    stat = violin_stat,
                    na.rm = na.rm)+
        theme
    } else if(plot_type == "density" | plot_type == "dens")
    {
      p <- ggplot(df, aes_string(x = names(df)[i]))+
        geom_density(col = col,
                    fill = fill,
                    alpha = alpha,
                    position = density_position,
                    stat = density_stat,
                    na.rm = na.rm)+
        theme
    } else
    {
      p <- NULL
    }
    
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
