library(tidyverse)
library(plotly)
library(tidytext)
library(tidylo)
library(gt) 
library(janitor)

unique(inha$군집)

inha %>% 
  group_by(언론사) %>% 
  tally() %>% 
  arrange(desc(n)) %>% gt() %>% tab_header('언론사 별 보도 빈도') %>% 
  cols_label(언론사 = "언론사", n = '빈도')

inha %>% 
  group_by(군집) %>% 
  tally() %>% 
  arrange(desc(n)) %>% gt() %>% tab_header('군집 별 기사 빈도') %>% 
  cols_label(군집 = "군집", n = '빈도')

inha %>% 
  filter(군집 == '인하대 사건 조사보도') %>% 
  group_by(언론사) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  ggplot(aes(x = "", y = n, fill = 언론사))+
  geom_bar(width = 1, stat = 'identity', color = "white")+
  theme(axis.text.y = element_blank(),axis.ticks = element_blank(), legend.text = element_text(size = 9))+
  geom_text(aes(label = paste(round(n/sum(n)*100, 2),"%")),
            position = position_stack(vjust = 0.6),
            check_overlap = TRUE,
            color = 'white')+
  coord_polar('y', start = 0)+
  xlab("")+
  ylab("")


inha %>% 
  filter(군집 == '인하대 사건 그 이후') %>% 
  group_by(언론사) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  ggplot(aes(x = "", y = n, fill = 언론사))+
  geom_bar(width = 1, stat = 'identity', color = "white")+
  theme(axis.text.y = element_blank(),axis.ticks = element_blank(), legend.text = element_text(size = 9))+
  geom_text(aes(label = paste(round(n/sum(n)*100, 2),"%")),
            position = position_stack(vjust = 0.5),
            check_overlap = TRUE,
            color = 'white')+
  coord_polar('y', start = 0)+
  xlab("")+
  ylab("")

inha %>% 
  filter(군집 == '젠더 이슈') %>% 
  group_by(언론사) %>% 
  tally() %>% 
  arrange(desc(n)) %>%
  ggplot(aes(x = "", y = n, fill = 언론사))+
  geom_bar(width = 1, stat = 'identity', color = "white")+
  theme(axis.text.y = element_blank(),axis.ticks = element_blank(), legend.text = element_text(size = 9))+
  geom_text(aes(label = paste(round(n/sum(n)*100, 2),"%")),
            position = position_stack(vjust = 0.5),
            check_overlap = TRUE,
            color = 'white')+
  coord_polar('y', start = 0)+
  xlab("")+
  ylab("")


##############################################################################
inha %>% 
  select(언론사, 키워드) %>% 
  rowid_to_column() %>%
  unnest_tokens(
    input = "키워드",
    output = "단어") %>% 
  group_by(언론사) %>% 
  filter(nchar(단어) >= 2) %>% 
  count(`단어`, sort = TRUE)-> inha.words

reorder_within <- function(x, by, within, fun = sum, sep = "___", ...) {
  new_x <- paste(x, within, sep = sep)
  stats::reorder(new_x, by, FUN = fun)
}

inha.words %>% 
  slice_max(n, n = 10) %>%
  ggplot(aes(x = n, y = reorder_within(단어, n, 언론사), fill = 언론사)) +
  geom_col(show.legend = F) +
  facet_wrap(~ 언론사, scales = 'free') + 
  scale_y_reordered()+
  xlab("")+
  ylab("")

inha.words %>%
  bind_tf_idf(document = 언론사, term = 단어, n = n) %>% 
  filter(grepl("[가-힣]", 단어)) %>% 
  mutate(score = round(tf_idf * 1000, 3)) %>% 
  arrange(-score) -> tfidf.df

tfidf.df %>% 
  slice_max(score, n = 5) %>%
  ggplot(aes(x = score, y = reorder_within(단어, score, 언론사), fill = 언론사)) +
  geom_col(show.legend = F) +
  facet_wrap(~ 언론사, scales = 'free') + 
  scale_y_reordered()+
  xlab("")+
  ylab("")

texts <- split(inha.words, inha.words$언론사)

top15 <-function(x){
  x <- x %>% head(20)
  return(x)
}

num = 1
for (k in texts){
  k <- top15(k)
  if (num == 1){
    words = k
    num = 2
  }else{
  words <- cbind(words, k)
  }
}

varnames <- c('언론사', '단어', 'n')

names(words)[1:ncol(words)]<- paste0(varnames,"_",1:30)

words %>% 
  select(!starts_with('언론사')) %>% 
  gt() %>% 
  tab_header('언론사 별 단어 빈도') %>% 
  tab_spanner(label = "경향신문",
              columns = 1:2) %>% 
  tab_spanner(label = "국민일보", 
              columns = 3:4)%>% 
  tab_spanner(label = "동아일보", 
              columns = 5:6)%>%
  tab_spanner(label = "문화일보", 
              columns = 7:8)%>% 
  tab_spanner(label = "서울신문", 
              columns = 9:10)%>%
  tab_spanner(label = "세계일보", 
              columns = 11:12)%>% 
  tab_spanner(label = "조선일보", 
              columns = 13:14)%>%
  tab_spanner(label = "중앙일보", 
              columns = 15:16)%>%
  tab_spanner(label = "한겨례", 
              columns = 17:18)%>%
  tab_spanner(label = "한국일보", 
              columns = 19:20)%>% 
  cols_label(
    단어_2 = "단어",
    n_3 = "빈도",
    단어_5 = "단어",
    n_6 = "빈도",
    단어_8 = "단어",
    n_9 = "빈도",
    단어_11 = "단어",
    n_12 = "빈도",
    단어_14 = "단어",
    n_15 = "빈도",
    단어_17 = "단어",
    n_18 = "빈도",
    단어_20 = "단어",
    n_21 = "빈도",
    단어_23 = "단어",
    n_24 = "빈도",
    단어_26 = "단어",
    n_27 = "빈도",
    단어_29 = "단어",
    n_30 = "빈도")


texts.tfidf <- split(tfidf.df, tfidf.df$언론사)

num = 1
for (k in texts.tfidf){
  k <- k %>% select(언론사, 단어, n, score)
  k <- top15(k)
  if (num == 1){
    words.tfidf = k
    num = 2
  }else{
    words.tfidf <- cbind(words.tfidf, k)
  }
}

varnames.tfidf<- c('언론사', '단어', '빈도', 'tfidf')

names(words.tfidf)[1:ncol(words.tfidf)]<- paste0(varnames.tfidf,"_",1:40)

words.tfidf %>% 
  select(!starts_with('언론사') & !starts_with('빈도')) %>% 
  gt() %>% 
  tab_header('언론사 별 단어 상대 빈도') %>% 
  tab_spanner(label = "경향신문",
              columns = 1:2) %>% 
  tab_spanner(label = "국민일보", 
              columns = 3:4)%>% 
  tab_spanner(label = "동아일보", 
              columns = 5:6)%>%
  tab_spanner(label = "문화일보", 
              columns = 7:8)%>% 
  tab_spanner(label = "서울신문", 
              columns = 9:10)%>%
  tab_spanner(label = "세계일보", 
              columns = 11:12)%>% 
  tab_spanner(label = "조선일보", 
              columns = 13:14)%>%
  tab_spanner(label = "중앙일보", 
              columns = 15:16)%>%
  tab_spanner(label = "한겨례", 
              columns = 17:18)%>%
  tab_spanner(label = "한국일보", 
              columns = 19:20)%>% 
  cols_label(
    단어_2 = "단어",
    tfidf_4 = "tfidf",
    단어_6 = "단어",
    tfidf_8 = "tfidf",
    단어_10 = "단어",
    tfidf_12 = "tfidf",
    단어_14 = "단어",
    tfidf_16 = "tfidf",
    단어_18 = "단어",
    tfidf_20 = "tfidf",
    단어_22 = "단어",
    tfidf_24 = "tfidf",
    단어_26 = "단어",
    tfidf_28 = "tfidf",
    단어_30 = "단어",
    tfidf_32 = "tfidf",
    단어_34 = "단어",
    tfidf_36 = "tfidf",
    단어_38 = "단어",
    tfidf_40 = "tfidf")


