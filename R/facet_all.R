lista <- list()
for(i in names(df)){lista[[i]] <- ggplot(df, aes_string(i,i))+geom_point()+facet_wrap(~ am)}