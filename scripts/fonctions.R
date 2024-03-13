# Fonctions

# format données ----------------------------------------------------------
row_format <- function(data, 
                       row_any_digits = NULL, 
                       row_one_digits = NULL, 
                       row_percent = NULL){
  df <- data
  
  if (!is.null(row_any_digits)){
    for (i in row_any_digits){
      df[i, 2:length(df)] <- number(as.numeric(df[i, 2:length(df)]), big.mark = ' ')
    }
  }
  if (!is.null(row_one_digits)) {
    for (i in row_one_digits){
      df[i, 2:length(df)] <- number(as.numeric(df[i, 2:length(df)]), big.mark = ' ', accuracy = 0.1)
    }
  }
  if(!is.null(row_percent)) {
    for (i in row_percent){
      df[i, 2:length(df)] <- number(as.numeric(df[i, 2:length(df)]), big.mark = ' ', accuracy = 0.01, suffix = '%')
    }
  }
  return(df)
}



# format données ----------------------------------------------------------
formating <- function(data, 
                      row_any_digits = NULL, 
                      row_one_digits = NULL, 
                      row_percent = NULL){
  df <- data
  
  if (!is.null(row_any_digits)){
    for (i in row_any_digits){
      df[i, 2:length(df)] <- number(as.numeric(df[i, 2:length(df)]), big.mark = ' ')
    }
  }
  if (!is.null(row_one_digits)) {
    for (i in row_one_digits){
      df[i, 2:length(df)] <- number(as.numeric(df[i, 2:length(df)]), big.mark = ' ', accuracy = 0.1)
    }
  }
  if(!is.null(row_percent)) {
    for (i in row_percent){
      df[i, 2:length(df)] <- number(as.numeric(df[i, 2:length(df)]), big.mark = ' ', accuracy = 0.01, suffix = '%')
    }
  }
  return(df)
}

# Format données PS -------------------------------------------------------

col_format <- function(data, 
                       cols_any_digits = NULL, 
                       cols_one_digits = NULL, 
                       cols_percent = NULL){
  df <- data
  
  if (!is.null(cols_any_digits)){
    for (j in cols_any_digits){
      df[1:(nrow(df)), j] <- number(as.numeric(df[1:(nrow(df)), j]), big.mark = ' ') 
    }
  }
  if (!is.null(cols_one_digits)) {
    for (j in cols_one_digits){
      df[1:(nrow(df)), j] <- number(as.numeric(df[1:(nrow(df)), j]), big.mark = ' ', accuracy = 0.1) 
    }
  }
  if(!is.null(cols_percent)) {
    for (j in cols_percent){
      df[1:(nrow(df)), j] <- number(as.numeric(df[1:(nrow(df)), j]), big.mark = ' ', accuracy = 0.01, suffix = '%') 
    }
  }
  
  return(df)
}




# Format données PS -------------------------------------------------------

ps_format <- function(data, 
                      cols_any_digits = NULL, 
                      cols_one_digits = NULL, 
                      cols_percent = NULL){
  df <- data
  
  if (!is.null(cols_any_digits)){
    for (j in cols_any_digits){
      df[1:(nrow(df)-1), j] <- number(as.numeric(df[1:(nrow(df)-1), j]), big.mark = ' ') 
    }
  }
  if (!is.null(cols_one_digits)) {
    for (j in cols_one_digits){
      df[1:(nrow(df)-1), j] <- number(as.numeric(df[1:(nrow(df)-1), j]), big.mark = ' ', accuracy = 0.1) 
    }
  }
  if(!is.null(cols_percent)) {
    for (j in cols_percent){
      df[1:(nrow(df)-1), j] <- number(as.numeric(df[1:(nrow(df)-1), j])*100, big.mark = ' ', accuracy = 0.01, suffix = '%') 
    }
  }
  
  return(df)
}

# Graphique PS ------------------------------------------------------------


ps_graph <- function(df){
  for (i in 1:nrow(df)){
    for (j in c(3,4)){
      if (!is.na(df[i,j]) == TRUE){
        df[i,j] <- round(df[i,j]*100,2)
      } 
    }
    df[,1] <- as.character(df[,1])
  }
  
  fig <- df %>% 
    filter(`Année` == max(df$Année)) %>% 
    plot_ly(x=~PS, y=~`Libéral et mixte`, name = 'Libérale et mixte', type = 'bar',
            hovertemplate = "Libérale et mixte <br> <i><b>%{x}</b></i> : <b>%{y}%</b>") %>% 
    add_trace(y =~`Salarié exclusif`, name = 'Salarié exclusif',
              hovertemplate = 'Salarié exclusif <br> <i><b>%{x}</b></i> : <b>%{y}%</b>') %>% 
    layout(yaxis = list(title = 'Type de PS'), barmode = 'stack') %>% 
    layout(title = paste("Répartition des PS en", max(df$Année)))
  
  fig
}


# Graphique ES ------------------------------------------------------------

es_graph <- function(titre = NULL, col = NULL){
  # Création du df pour le graphique
  df <- t(es)
  colnames(df) <- df[1,]
  df <- as.data.frame(df[-c(1,10),])
  df <- df %>% 
    mutate(dpt = rownames(df)) %>% 
    select(dpt, `Répartition séjours MCO`, `Répartition séjours SSR`, `Répartition journées de PSY`) %>% 
    rename("dpt"=dpt, "MCO"= `Répartition séjours MCO`, "SSR" = `Répartition séjours SSR`, "PSY" = `Répartition journées de PSY`)
  # on enlève le '%'
  for (i in 2:4){
    df[,i] <- as.numeric(substr(df[,i],1, nchar(df[,i])-1))
  }
  df <- as.data.frame(df)
  
  # Création du graph
  fig1 <- df %>% 
    plot_ly(x = ~dpt, y = ~df[,col], 
            color = ~dpt, 
            type = "bar",
            hovertemplate = paste("Département %{x} : <b> %{y} % </b>")) %>%
    layout(legend = list(title=list(text='<b> Département </b>'))) %>% 
    layout(title = titre,
           xaxis = list(title = ""), 
           yaxis = list(title = "Répartition (en %)"),
           legend = list(title = "Département")) 
  return(fig1)
}
