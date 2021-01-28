setwd("D:/IRIAF/L3")

library(dplyr)
library(rtweet)
library(sentimentr)
library(openssl)
library(lubridate)
library(tm)
library(tidytext)
library(wordcloud)
library(plotly)

# collect tweets----
tweets.raw <- search_tweets2(
  c("lupin"),
  include_rts = FALSE,
  n = 5000,
  type="recent",
  lang="en"
)

#variables selection ----
tweets.prep<-tweets.raw %>%
  select(created_at,screen_name,text,source,is_retweet,favorite_count,retweet_count,reply_count) %>%
  mutate(tweet.uuid=(sha2(paste0(created_at,screen_name))))

#get sentiment ----
sentiment <- tweets.prep %>%
  get_sentences(tweets.prep$text) %>%
  sentiment()

sentiment.unique<-sentiment %>% select(tweet.uuid,sentiment) %>%
  group_by(tweet.uuid) %>%
  summarise(mean_sentiment=mean(sentiment)) %>%
  mutate(polarity_level = ifelse(mean_sentiment < -0.2, "Negative",
                                 ifelse(mean_sentiment > 0.2, "Positive","Neutral")))

tweets.with.sentiment<-tweets.prep %>%
  inner_join(sentiment.unique,by="tweet.uuid")
tweets.with.sentiment %>% glimpse

saveRDS(tweets.with.sentiment, file = "my_tweets.rds")

#some stats ----
(tweets<-tweets.with.sentiment %>% count)
(twittos<-tweets.with.sentiment %>% summarize(n=n_distinct(screen_name)))
(likes<-tweets.with.sentiment %>% summarize(s=sum(favorite_count)))


tweets.with.sentiment %>% group_by(screen_name) %>% count %>% arrange(-n)
tweets.with.sentiment %>% group_by(polarity_level) %>% count

tweets.with.sentiment %>% summarise(m_is_retweet=mean(retweet_count))
tweets.with.sentiment %>% summarise(m_is_retweet=max(retweet_count))

#timeserie
(fig <- tweets.with.sentiment %>%
    mutate(date.hour=round_date(created_at,"hour")) %>%
    group_by(date.hour) %>% summarise(n.tweets=n()) %>%
  plot_ly(x = ~date.hour, y = ~n.tweets, mode = 'lines'))

(fig <- tweets.with.sentiment %>%
    mutate(date.hour=round_date(created_at,"hour")) %>%
    group_by(date.hour,polarity_level) %>% summarise(n.polarity=n()) %>%
    plot_ly(x = ~date.hour, y = ~n.polarity,color=~polarity_level, type = 'bar')) %>%
  layout(barmode = 'stack')



#wordcloud ----
new_df <- tweets.with.sentiment %>%
  unnest_tokens(word,text)%>%
  anti_join(stop_words) %>%
  filter(!(word %in% c("https","t.co"))) %>%
  select(word)

new_df %>% count(word,sort=TRUE)
wordcloud(new_df$word, max.words = 50, colors = brewer.pal(8, "Dark2"),  rot.per=0)
