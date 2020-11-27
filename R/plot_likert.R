str_insert_every_n <- function(x, n, string)
{
  iter <- nchar(x) / n
  m <- 1
  ret_ <- sapply(1:(iter+1),function(i)
    {
      m <- 1 + (i-1) * n
      substr(x, m, m + n - 1)
  })
  paste(ret_, collapse = string)
}

plot_likert <- function(dados,
                        descricao = NULL,
                        n_skip_desc = 20,
                        tidy = FALSE,
                        plot_grid = TRUE)
{
  if(!tidy)
  {
    vars <- names(dados)
    
    if(is.null(descricao))
    {
      descricao <- sapply(seq_along(vars),
                          function(x) paste(rep("‎", x), collapse = ""))
      names(descricao) <- vars
    }
    else
    {
      descricao <- sapply(descricao, str_insert_every_n,
                          n = n_skip_desc,
                          string = "-\n")
    }
      
    p <- dados %>% gather() %>%
      mutate(value = factor(value, levels = 1:5),
             desc = descricao[key]) %>%
      ggplot(aes(x = desc, fill = value)) +
        geom_bar(aes(y = stat(count / max(count))), position = "stack") +
        coord_flip()
    
    return(p)
  }
  
  if(is.null(descricao)) descricao <- NA
  
  vars <- names(dados)
  
  if(length(vars) == 1)
  {
    dados %>%
      ggplot(aes_string(x = descricao[vars], fill = vars)) +
        geom_bar(aes(stat(count / max(count))),
                 position = "stack", stat = "count") +
        coord_flip() %>% return()
  }
  
  lista <- list()
  
  for(i in seq_along(vars))
  {
    p <- dados %>%
      ggplot(aes(x = descricao[vars[i]])) +
        geom_bar(aes_string(fill = vars[i]),
                 position = "stack", stat = "count") +
        coord_flip()
    
    lista[[i]] <- p
  }
  
  if(plot_grid) grid_list(lista, 1)
  else return(lista)
}
