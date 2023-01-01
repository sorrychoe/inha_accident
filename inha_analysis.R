library(bitTA)
library(tidytext)
library(tidyverse)
library(igraph)

inha %>% 
  filter(`주요 토픽` == 0) -> inha_topic

inha_topic <- inha_topic[, c(2:4)]

#network analysis######

topic_words <- inha_topic %>%
  rowid_to_column() %>%
  unnest_tokens(
    input = "키워드",
    output = "단어",
    token = morpho_mecab)


press_words <- table(topic_words$언론사, topic_words$단어)
press_words <- crossprod(t(press_words))
diag(press_words) <- 0 
press_words <- round(press_words/10000)
graph <- as.matrix(press_words)

graph_from_adjacency_matrix(graph, mode="undirected", weighted=TRUE)%>% 
  plot(edge.width = 8 * E(.)$weight / max(E(.)$weight), edge.label = ifelse(E(.)$weight >= 35, E(.)$weight, ""), 
       layout =  layout.kamada.kawai)

