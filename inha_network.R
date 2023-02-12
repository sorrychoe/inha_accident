library(tidyverse)
library(plotly)
library(tidytext)
library(tidylo)
library(gt) 
library(janitor)
library(igraph)
library(quanteda)


dim(inha) #362

inha %>% 
  select(키워드) %>% 
  rowid_to_column() %>%
  unnest_tokens(
    input = "키워드",
    output = "단어") %>% 
  filter(nchar(단어) >= 2) %>% 
  count(`단어`, sort = TRUE) -> words

dim(words) #4190

words %>% head(15) #counter

inha.words %>%
  bind_tf_idf(document = 언론사, term = 단어, n = n) %>%
  group_by(단어) %>% 
  arrange(-tf_idf) %>% 
  head(20) %>% 
  select(단어, tf_idf) #tf-idf

tfidf.df %>%
  filter(grepl("[가-힣]", 단어)) %>% 
  group_by(언론사) %>%
  slice_max(score, n = 20) -> dtm.df


######
dtm <- t(table(dtm.df[1:2]))
press <- crossprod(dtm)
diag(press) <- 0
dtm <- as.matrix(press)

press.nt <- graph_from_adjacency_matrix(dtm, mode="undirected", weighted=TRUE)
clust <- cluster_walktrap(press.nt)

plot(clust, press.nt)

######
com <- table(dtm.df[1:2])
com <- crossprod(com)
diag(com) <- 0
com<- as.data.frame(com)

dfm <- as.dfm(com)
fcm<- fcm(dfm)

quanteda.textplots::textplot_network(fcm, 
                 min_freq = 1, 
                 edge_alpha = 0.5, 
                 edge_color = "blue",
                 vertex_labelsize = rowSums(fcm),
                 edge_size = 2)
