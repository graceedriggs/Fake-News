library(tidyverse)
library(DataExplorer)
library(caret)
library(naniar)
library(textcat)
library(tidytext)
library(stopwords)


#### Fake News

## load in data 
fake.train <- read_csv("/Users/graceedriggs/Documents/Stat 495/Fake News/fake-news/train.csv")
fake.test <- read_csv("/Users/graceedriggs/Documents/Stat 495/Fake News/fake-news/test.csv")

fake <- bind_rows(fake.test, fake.train)
summary(fake)

## see how much data is missing
plot_missing(fake.train)

######## separate the different languages
## Determine which language each article is in
fake <- fake %>%
  mutate(language=textcat::textcat(text))
fake %>% count(language) %>%
  arrange(desc(n)) %>%
  print(n=Inf)

## Combine some languages into same category
fake <- fake %>%
  mutate(language=fct_collapse(language, 
                               english=c("english", "middle_frisian", "scots",
                                         "scots_gaelic", "breton", "frisian",
                                         "manx", "catalan"),
                               russian=c("russian-koi8_r", "russian-iso8859_5",
                                         "russian-windows1251")))

## Lump together other languages
fake <- fake %>% 
  mutate(language=fct_explicit_na(language, na_level="Missing")) %>%
  mutate(language=fct_lump(language, n=6))
fake %>% count(language) %>%
  arrange(desc(n)) %>%
  print(n=Inf)




## replace the "nan" with NAs
fake <- fake %>% replace_with_na(replace = list(author = 'nan'))

fakeTrainComplete<- na.omit(fake.train) 
plot_missing(fakeTrainComplete)


############################################
## Calculate df-idf for most common words ##
## not including stop words               ##
############################################

## Create a set of stop words
sw <- bind_rows(get_stopwords(language="en"), #English
                get_stopwords(language="ru"), #Russian
                get_stopwords(language="es"), #Spanish
                get_stopwords(language="de"), #German
                get_stopwords(language="fr")) #French
sw <- sw %>%
  bind_rows(., data.frame(word="это", lexicon="snowball"))

## tidytext format
tidyNews <- fake %>%
  unnest_tokens(tbl=., output=word, input=text)

## Count of words in each article
news.wc <-  tidyNews %>%
  anti_join(sw) %>% 
  count(id, word, sort=TRUE)

## Number of non-stop words per article
all.wc <- news.wc %>% 
  group_by(id) %>% 
  summarize(total = sum(n))

## Join back to original df and calculate term frequency
news.wc <- left_join(news.wc, all.wc) %>%
  left_join(x=., y=fake %>% select(id, title))
news.wc <- news.wc %>% mutate(tf=n/total)
a.doc <- sample(news.wc$title,1)
ggplot(data=(news.wc %>% filter(title==a.doc)), aes(tf)) +
  geom_histogram() + ggtitle(label=a.doc)

## Find the tf-idf for the most common p% of words
word.count <- news.wc %>%
  count(word, sort=TRUE) %>%
  mutate(cumpct=cumsum(n)/sum(n))
ggplot(data=word.count, aes(x=1:nrow(word.count), y=cumpct)) + 
  geom_line()
top.words <- word.count %>%
  filter(cumpct<0.75)

news.wc.top <- news.wc %>% filter(word%in%top.words$word) %>%
  bind_tf_idf(word, id, n)
a.doc <- sample(news.wc$title,1)
news.wc.top %>% filter(title==a.doc) %>%
  slice_max(order_by=tf_idf, n=20) %>%
  ggplot(data=., aes(x=reorder(word, tf_idf), y=tf_idf)) + 
  geom_bar(stat="identity") +
  coord_flip() + ggtitle(label=a.doc)

## Convert from "long" data format to "wide" data format
## so that word tfidf become explanatory variables
names(news.wc.top)[1] <- "Id"
news.tfidf <- news.wc.top %>%
  pivot_wider(id_cols=Id,
              names_from=word,
              values_from=tf_idf)

## Fix NA's to zero
news.tfidf <- news.tfidf %>%
  replace(is.na(.), 0)

## Merge back with fake data
names(fake)[c(2,6)] <- c("Id", "isFake")
fake.tfidf <- left_join(fake, news.tfidf, by="Id")

## Remaining articles with NAs all have missing text so should get 0 tfidf
fake.clean <- fake.tfidf %>%
  select(-isFake, -title.x, -author.x, -text.x) %>% 
  replace(is.na(.), 0) %>% 
  left_join(fake.tfidf %>% select(Id, isFake, title.x, author.x, text.x),., by="Id")

## Write out clean dataset
write_csv(x=fake.clean %>% select(-author.x, -title.x, -text.x),
          path="./Cleanfake.csv")
