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
  arrange(desc(n)) %>% 
  ggplot(aes(x = reorder(군집,n), y = n, fill = 군집))+
  geom_col()+
  coord_flip()+
  xlab("")+
  ylab("")

inha %>% 
  filter(군집 == '인하대 사건 조사보도') %>% 
  group_by(언론사) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  ggplot(aes(x = "", y = n, fill = 언론사))+
  geom_bar(width = 1, stat = 'identity', color = "white")+
  theme(axis.text.y = element_blank(),axis.ticks = element_blank(), legend.text = element_text(size = 9))+
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5),
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
  geom_text(aes(label = n),
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
  ggplot(aes(x = reorder(언론사, desc(n)), y = n, fill = 언론사))+
  geom_col()+
  xlab("")+
  ylab("")


##############################################################################
inha %>% 
  select(언론사, 키워드) %>% 
  rowid_to_column() %>%
  unnest_tokens(
    input = "키워드",
    output = "단어"
  ) -> inha.words

inha.words %>% 
  group_by(언론사) %>% 
  filter(nchar(단어) >= 2) %>% 
  count(`단어`, sort = TRUE) %>% 
  slice_max(n, n = 10) %>%
  ggplot(aes(x = n, y = reorder(단어,n), fill = 언론사)) +
  geom_col(show.legend = F) +
  facet_wrap(~ 언론사, scales = 'free')

inha.words %>% 
  group_by(언론사) %>% 
  filter(nchar(단어) >= 2) %>% 
  count(`단어`, sort = TRUE) %>% 
  bind_log_odds(set = 언론사, feature = 단어, n = n) %>% 
  arrange(-log_odds_weighted) -> log.odd.df

log.odd.df %>% 
  slice_max(log_odds_weighted, n = 10) %>%
  ggplot(aes(x = log_odds_weighted, y = reorder(단어,log_odds_weighted), fill = 언론사)) +
  geom_col(show.legend = F) +
  facet_wrap(~ 언론사, scales = 'free') + 
  xlab("")+
  ylab("")
