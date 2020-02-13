library(dplyr)
library(ggplot2)
library(tm)
library(stringi)
library(stringr)
library(rebus)
library(tidytext)
library(tidyr)
library(wordcloud)
library(viridisLite)
#Data set
candi_tweets <- read.csv("tweets.csv", stringsAsFactors = F)

#Donald Trump Tweets
dt_tweets <- candi_tweets %>% select(id:original_author, time, is_quote_status:favorite_count) %>% 
  filter(handle == "realDonaldTrump", is_retweet == "False")
#Hillary Clinton Tweets
hc_tweets <- candi_tweets %>% select(id:original_author, time, is_quote_status:favorite_count) %>% 
  filter(handle == "HillaryClinton", is_retweet == "False")
#Donald Trump Retweets
dt_retweets <- candi_tweets %>% select(id:original_author, time, is_quote_status:favorite_count) %>% 
  filter(handle == "realDonaldTrump", is_retweet == "True")
#Hillary Clinton Retweets
hc_retweets <- candi_tweets %>% select(id:original_author, time, is_quote_status:favorite_count) %>% 
  filter(handle == "HillaryClinton", is_retweet == "True")




#analysis of HC retweets
#Top accounts retweeted by Hillary Clinton
hc_rt <- hc_retweets  %>% count(original_author) %>% top_n(10)  %>% mutate(original_author = reorder(original_author, n))


 ggplot(hc_rt, aes(x = original_author, y = n)) + 
   geom_col(fill = "royalblue", aes(colour = factor(original_author),)) + 
   coord_flip() + 
    theme(panel.background = element_rect(fill = "white"), legend.position = "none") + 
   labs(x = "Top 10 Retweeted People by Hillary Clinton",y = "Numbers of Retweet")
 
 
 #Hillary clinton retweets with Donald trump name
 dt_detect <- str_detect(hc_retweets$text, 
                        pattern = or("D", "d") %R% "onald" %R% " " %R% or("T", "t") %R% "rump")

 hc_rt_about_donald <- hc_retweets[dt_detect, ] %>% select(text, lang) %>% filter(lang == "en") %>% 
   unnest_tokens(word, text) %>% inner_join(get_sentiments("bing")) %>% 
   filter(word != "trump") %>% count(word ,sentiment)
 
 # top_hc_rt_words <- hc_rt_about_donald %>%  group_by(sentiment) %>% top_n(5) %>% ungroup() %>%
 #    mutate(word = reorder(word, n))
 # 
 # ggplot(top_hc_rt_words, aes(reorder(word, n), n, fill = sentiment)) + geom_col(show.legend = F) + 
 #    facet_wrap(~sentiment) + coord_flip()
 #    
 
 #wordcloud(hc_rt_sent$word, hc_rt_sent$n, max.words = 50, colors = c("grey80", "darkgoldenrod1", "tomato"))
 
 #hc_rt_sent %>% count(word) %>% arrange(desc(n))
 
 
 
 #analysis of DT retweets
 
 dt_rt <- dt_retweets %>% count(original_author) %>% top_n(10) %>% 
   mutate(original_author = reorder(original_author, n)) 
 
 ggplot(dt_rt, aes(x = original_author, y = n)) +
   geom_col(fill = "red", aes(colour = factor(original_author))) + 
   theme(panel.background = element_rect(fill = "white"),legend.position = "none") + 
   labs(x = "Top 10 Retweeted People by Donald Trump",y = "Numbers of Retweet") +
   coord_flip()
 
 hc_detect <- str_detect(dt_retweets$text, pattern = or("H", "h") %R% "illary" %R% " " %R% or("C", "c") %R% "linton")
 
 dt_rt_about_hillary <- dt_retweets[hc_detect, ] %>% select(text, lang) %>% filter(lang == "en") %>%
   unnest_tokens(word, text) %>% inner_join(get_sentiments("bing")) %>% count(word, sentiment)
 
 
 
 
 # funtions for cleaning corpus and transforming in to a matrix
 
 to_corpus <- function(x) {
   
   x = VectorSource(x)
   x = VCorpus(x)
   return(x)
 }
 
 clean_corpus <- function(corpus) {
   
   corpus <- tm_map(corpus, removePunctuation)
   corpus <- tm_map(corpus, content_transformer(tolower))
   corpus <- tm_map(corpus, removeNumbers)
   corpus <- tm_map(corpus, removeWords, words = c(stop_words$word, 
                            "donald", "trump", "hillary", "clinton", "realdonaldtrump", "hillaryclinton", 
                            "ted", "cruz", "tedcruz", "trumps"))
   corpus <- tm_map(corpus, stripWhitespace)
   return(corpus)
 }
 
 
 to_matrix <- function(x){
   x <- TermDocumentMatrix(x)
   x <- as.matrix(x)
   x <- rowSums(x)
   x <- sort(x, decreasing = T)
 }
 
 
 
 
 #analysis of DT tweets
 
 dt_corpus <- to_corpus(dt_tweets$text)
 dt_corpus_clean <- clean_corpus(dt_corpus)
 dt_term_freq <- to_matrix(dt_corpus_clean)
 
 #plot & wordcloud of Trump's most used words with names and english stopwords removed
barplot(dt_term_freq[1:20], width = 1   ,col = "red", las = 2, legend.text = "Trump Most Used Words")
 wordcloud(names(dt_term_freq), dt_term_freq, max.words = 100, colors = cividis(5))
 
 #using wordcloud2
 df2 <- data.frame(x = names(dt_term_freq), y = dt_term_freq)
 wordcloud2::wordcloud2(df2[1:100, ])
 
 
 #hillary tweets
 
 
 ht_corpus <- to_corpus(hc_tweets$text)
 ht_corpus_clean <- clean_corpus(ht_corpus)
 ht_term_freq <- to_matrix(ht_corpus_clean)
 
 
 #plot & wordcloud of Hillary's most used words with names and english stopwords removed
 barplot(ht_term_freq[1:20], col = "royalblue", las = 2, legend.text = "Hillary Most Used Words")
 wordcloud(names(ht_term_freq), ht_term_freq, max.words = 100, colors = cividis(4))
 
 #using wordcloud2
 df <- data.frame(x= names(ht_term_freq), y = ht_term_freq)
 wordcloud2::wordcloud2(df[1:100, ])
 
 
 
 #common words used by Trump & hillary using commonality.cloud
 
 hillary <- paste(hc_tweets$text, collapse = " ")
 trump <-  paste(dt_tweets$text, collapse = " ")
 hil_tru <- c(hillary, trump)
 
 hil_tru_corpus <- to_corpus(hillary_trump)
 hil_tru_corpus_clean <- clean_corpus(hil_tru_corpus)
 hil_tru_matrix <- as.matrix(TermDocumentMatrix(hil_tru_corpus_clean))
 
 commonality.cloud(hil_tru_matrix, max.words = 100, colors = cividis(2))
 
 #compare the words used by Trump & Hillary using comparison cloud
 
 colnames(hil_tru_matrix) <- c("Hillary", "Trump")
 comparison.cloud(hil_tru_matrix, max.words = 50, colors = c("royalblue", "red"))
 
 
 
 #sentiment analysis for Trump's tweets bing lexicon
 
 dt_bing <- dt_tweets %>% filter(lang == "en") %>% select(text) %>% unnest_tokens(word,text) %>% 
    inner_join(get_sentiments("bing")) %>% count(word, sentiment)
 
 #top 10 negative and positive words used by Trump
 dt_bing %>% group_by(sentiment) %>% top_n(10) %>% ungroup() %>% 
    ggplot(aes(reorder(word, n), n, fill = sentiment)) + geom_col(show.legend = F) +
    facet_wrap(~sentiment, scales = "free") + coord_flip()
 
 #percentage of positive words contribution
 dt_bing_positive <- dt_bing %>% filter(sentiment == "positive") %>%
    mutate(total_words = sum(n), positive_percentage = (n/total_words)*100) %>% arrange(desc(positive_percentage))
 
 #percentage of negative words contribution 
 dt_bing_negative <- dt_bing %>% filter(sentiment == "negative") %>% 
    mutate(total_words = sum(n), negative_percentage = (n/total_words)*100) %>% arrange(desc(negative_percentage))
 
 
 
 
 # positive to negative ratio
 
 
 
 
 
 
 
 