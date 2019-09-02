#'=========================================================================
#'AI for Transportation
#'=========================================================================

# Libraries -------------
rm(list=ls())
library(tidyverse)
library(tidytext)
library(readr)
library(qdap)
library(qdapTools)
library(dplyr)
library(readxl)
library(lubridate)
library(tm)
library(ggplot2)
library(scales)
library(reshape2)
library(wordcloud)
library(tidyr)
dev.off()

setwd("E:\\University of Sussex\\Courses\\Dissertation\\Data\\AI_Transport\\data")

#------INTRODUCTION----------

#1. Plot number of journal, news and patent from 2010 to 2018 (Data source: WIPO, SCOPUS, FACTIVA) --------
pubs_data <- read_excel("publication.xlsx")

pubsdf <- pubs_data %>%
  gather(key = "Publication", value = "value", -Year)
head(pubsdf)
pubsdf

ggplot(pubsdf, aes(x = Year, y = value)) + 
  geom_line(aes(color = Publication, linetype = Publication), size=1.3) + 
  geom_point() +
  labs(x = "Year", y = "Number of Publications") +
  scale_color_manual(values = c("darkred", "steelblue", "darkgreen")) + 
  scale_x_discrete(limits = pubsdf$Year)

#2. Plot Monthly Active Twitter Users from 2010 to 2019 (Data Source: Statista) -------
my_data <- read_excel("twitterusers.xlsx")
my_data$Time <- factor(my_data$Time, levels = my_data$Time[order(my_data$Number)])
ggplot(my_data, aes(Time, Number, group=1)) +
geom_line(color='steelblue', size=2, stat="identity") +
labs(x = "Time (Quarter/Year)", y = "Number of Monthly Active Twitter Users (Millions)")

#------DATA COLLECTION-------

#1. Prepare stop words -------------
load("stop_words.rda")
add_stop_words <- c("http", "https", "pic.twitter.com", "bit.ly", "0", "1", "2",
                    "3", "4", "5", "6", "7", "8", "9", "10", "11", "buff.ly", 
                    "ow.ly", "twitter.com")
add_lexicon <- c("custom", "custom", "custom", "custom", "custom", "custom",
                 "custom", "custom", "custom", "custom", "custom", "custom",
                 "custom", "custom", "custom", "custom", "custom", "custom", "custom")
custom_stop_words <- bind_rows(stop_words,
                               data_frame(word = add_stop_words, lexicon = add_lexicon))
custom_stop_words <- subset(custom_stop_words, word!="self")

percentage <- 0.001

#2. Parse Twitter data related to AI  -------------
filelist <- c("artificialintelligencetransportation", "autonomouscar", 
              "autonomouscars", "autonomousvehicle", 
              "autonomousvehicles", "intelligentautomotive", 
              "intelligenttransport", "intellilgenttransportation", 
              "driverless", "artificialintelligencetraffic")
i <- 1
j <- 1
singleword <- c(NULL)
twowords <- c(NULL)
threewords <- c(NULL)
df_twitter <- NULL
bigram <- NULL
trigram <- NULL
dfall <- NULL
dfkeyword <- NULL
dfkey <- NULL
dfuser <- NULL
listuser <- NULL

for (file in filelist)
{
  file1 <- paste(file, ".csv", sep = "")
  df <- read_delim(file1, delim = ";")
  
  #add attribute user
  j <- 1
  listuser <- NULL
  for (link in df$permalink) {
    user <- unlist(strsplit(link, "/", fixed = TRUE))
    listuser[[j]] <- user[4]
    j <- j+1
  }
  dfuser <- data.frame(user = listuser, df)
  
  #add attribute keyword
  listkeyword <- rep(file,nrow(dfuser))
  dfkey <- data.frame(keyword = listkeyword, dfuser)
  
  if (i==1) (
    dfall <- dfkey  
  ) else {
    dfall <- bind_rows(dfall, dfkey)
  }
  
  #the most frequent single words
  df_twitter <-  df %>%
    select(text, id) %>%
    unnest_tokens(word, text) %>%
    anti_join(custom_stop_words) %>%
    count(word, sort = TRUE)
  norow <- nrow(df_twitter)
  tinytop <- round(norow*percentage)
  if (i==1) {
    singleword <- df_twitter$word[1:tinytop]
  } else (
    singleword <- c(singleword, df_twitter$word[1:tinytop])
  )
  
  #the most frequent bigrams
  df_twitter <- df %>%
    select(text, id) %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% custom_stop_words$word,
           !word2 %in% custom_stop_words$word) %>%
    unite(word, word1, word2, sep=" ") %>%
    count(word, sort = TRUE)
  norow <- nrow(df_twitter)
  tinytop <- round(norow*percentage)
  if (i==1) {
    twowords <- df_twitter$word[1:tinytop]
  } else (
    twowords <- c(twowords, df_twitter$word[1:tinytop])
  )
  
  #the most frequent trigrams
  df_twitter <- df %>%
    select(text, id) %>%
    unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
    separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
    filter(!word1 %in% custom_stop_words$word,
           !word2 %in% custom_stop_words$word,
           !word3 %in% custom_stop_words$word) %>%
    unite(word, word1, word2, word3, sep=" ") %>%
    count(word, sort = TRUE)
  norow <- nrow(df_twitter)
  tinytop <- round(norow*percentage)
  if (i==1) {
    threewords <- df_twitter$word[1:tinytop]
  } else (
    threewords <- c(threewords, df_twitter$word[1:tinytop])
  )
  
  i <- i+1
}

dfall

#3. Get distinct most frequent ----------------

#unigrams
singleword
singleworddist <- unique(unlist(singleword))
singleworddist
write.csv(singleworddist, file = "singlewords.csv")

#bigrams
twowords
twowordsdist <- unique(unlist(twowords))
twowordsdist
write.csv(twowordsdist, file = "bigrams.csv")

#trigrams
threewords
threewordsdist  <- unique(unlist(threewords))
threewordsdist
write.csv(threewordsdist, file = "trigrams.csv")

#4. Find additional keywords ---------------

newfilelist <- c("aitransportation", "machinelearningtransportation", 
                  "selfdriving")

for (file in newfilelist)
{
  file1 <- paste(file, ".csv", sep = "")
  df <- read_delim(file1, delim = ";")
  #update attribute username
  j <- 1
  listuser <- NULL
  for (link in df$permalink) {
    user <- unlist(strsplit(link, "/", fixed = TRUE))
    listuser[[j]] <- user[4]
    j <- j+1
  }
  dfuser <- data.frame(user = listuser, df)
  listkeyword <- rep(file,nrow(dfuser))
  dfkey <- data.frame(keyword = listkeyword, dfuser)
  dfall <- bind_rows(dfall, dfkey)
}

#5. Save data in csv file

#all tweets (contains duplicate tweets with different keywords)
dfall <- dfall[order(dfall$date),]
write.csv(dfall, file = "MyData.csv")

#distinct tweets
dfalldis <- dfall %>% distinct(id, .keep_all = TRUE)
dfalldis <- dfalldis[order(dfalldis$date),] 
write.csv(dfalldis, file = "MyDataDistinct.csv")


#--------------DATA PROCESSING-----------------

#1. Preparation --------------

#convert the date format
dfdate <- dfall %>%
  mutate(date = as.Date(date))
dfdate$date

dfdisdate <- dfalldis %>%
  mutate(date = as.Date(date))
dfdisdate$date

#separate the data into 3 groups: <1000 obs, 1000-10000 obs, >10000 obs
dfdatea <- dfdate[(dfdate$keyword=="artificialintelligencetransportation" | 
                     dfdate$keyword=="intelligentautomotive" | 
                     dfdate$keyword=="intelligenttransport" | 
                     dfdate$keyword=="intelligenttransportation" | 
                     dfdate$keyword=="aitransportation" | 
                     dfdate$keyword=="machinelearningtransportation" |
                     dfdate$keyword=="artificialintelligencetraffic"),]

dfdateb <- dfdate[(dfdate$keyword=="autonomouscar" | 
                     dfdate$keyword=="autonomouscars" | 
                     dfdate$keyword=="autonomousvehicle" | 
                     dfdate$keyword=="autonomousvehicles"),]

dfdatec <- dfdate[(dfdate$keyword=="selfdriving" | 
                     dfdate$keyword=="driverless"),]

#2. Trends Analysis ------------------

#general trends
dfdisdate$date <- as.POSIXct(dfdisdate$date)
dev.off()
p <- ggplot(dfdisdate, aes(date, ..count..)) + 
  geom_histogram() +
  theme_bw() + xlab("Time (month-year)") + ylab("Number of Tweets") +
  scale_x_datetime(breaks = date_breaks("3 months"),
                   minor_breaks = date_breaks("3 months"),
                   labels = date_format("%b-%y", tz=Sys.timezone()),
                   limits = c(as.POSIXct("2014-07-01"), 
                              as.POSIXct("2019-07-01")) )
p

dfd <- dfdisdate %>%
  count(date)
ggplot(dfd, aes(date, n, group=1)) +
  geom_line(color='steelblue', size=1.3, stat="identity") +
  labs(x = "Time (month-year)", y = "Number of Tweets") + 
  scale_x_datetime(breaks = date_breaks("3 months"),
                   minor_breaks = date_breaks("3 months"),
                   labels = date_format("%b-%y", tz=Sys.timezone()),
                   limits = c(as.POSIXct("2014-07-01"), 
                              as.POSIXct("2019-07-01")) )

#the most frequent users for all tweets
userfreq <- count(dfdisdate, user, sort = TRUE)
userfreq
write.csv(userfreq, file = "userfreq.csv")

#pie chart of AI role for transportation
tweetstostring <- paste(dfdisdate$text, collapse = " ")
t_count <- str_count(tweetstostring, pattern = "traffic")
pt_count <- str_count(tweetstostring, pattern = "public transport")
f_count <- str_count(tweetstostring, pattern = "freight")
av_count <- str_count(tweetstostring, pattern = "autonomous vehicle")
slices <- c(t_count, pt_count, f_count, av_count)
Role <- c("Traffic Management", "Public Transportation", "Freight Transport System", "Autonomous Vehicles")
dfpie <- data.frame(Role, slices)

# Add variable position
dfpie <- dfpie %>%
  arrange(desc(Role)) %>%
  mutate(lab.ypos = cumsum(slices) - 0.5*slices)
dfpie

dev.off()
pie <- ggplot(dfpie, aes(x="", y=slices, fill=Role)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  geom_text(aes(y = lab.ypos, label = slices), color = "white") +
  theme_void()
pie

#specific keyword trends in one figure divided by grid using facet_wrap
dfdatefw <- dfdate %>%
  count(date, keyword)
dfdatefw$date <- as.POSIXct(dfdatefw$date)
fw <- ggplot(dfdatefw, aes(date, n, color = keyword), show.legend = FALSE) +
  geom_line(size = 1.3) +
  labs(x = "Time (month-year)", y = "Number of Tweets") + 
  scale_x_datetime(breaks = date_breaks("6 months"),
                   labels = date_format("%b-%y", tz=Sys.timezone()),
                   limits = c(as.POSIXct("2014-07-01"), 
                              as.POSIXct("2019-07-01")) )
fw + facet_wrap(~ keyword, ncol=2)

#trends for keywords in category 1 (<1000 obs)
keywordbytimea <- dfdatea %>%
  count(date, keyword)
keywordbytimea

keywordbytimea$date <- as.POSIXct(keywordbytimea$date)

ga <- ggplot(keywordbytimea, aes(date, n, color = keyword)) +
  geom_line(size = 1.3) +
  labs(x = "Time (month-year)", y = "Number of Tweets") + 
  scale_x_datetime(breaks = date_breaks("3 months"),
                   labels = date_format("%b-%y", tz=Sys.timezone()),
                   limits = c(as.POSIXct("2014-07-01"), 
                              as.POSIXct("2019-07-01")) )
ga
ga + facet_wrap(~ keyword, ncol=1)

#trends for keywords in category 2 (1000-10000 obs)

keywordbytimeb <- dfdateb %>%
  count(date, keyword)
keywordbytimeb

keywordbytimeb$date <- as.POSIXct(keywordbytimeb$date)

gb <- ggplot(keywordbytimeb, aes(date, n, color = keyword)) +
  geom_line(size = 1.3) +
  labs(x = "Time (month-year)", y = "Number of Tweets") + 
  scale_x_datetime(breaks = date_breaks("3 months"),
                   labels = date_format("%b-%y", tz=Sys.timezone()),
                   limits = c(as.POSIXct("2014-07-01"), 
                              as.POSIXct("2019-07-01")) )
gb
gb + facet_wrap(~ keyword, ncol=1)

#trends for keywords in category 3 (>10000 obs)

keywordbytimec <- dfdatec %>%
  count(date, keyword)
keywordbytimec

keywordbytimec$date <- as.POSIXct(keywordbytimec$date)

gc <- ggplot(keywordbytimec, aes(date, n, color = keyword)) +
  geom_line(size = 1.3) +
  labs(x = "Time (month-year)", y = "Number of Tweets") + 
  scale_x_datetime(breaks = date_breaks("3 months"),
                   labels = date_format("%b-%y", tz=Sys.timezone()),
                   limits = c(as.POSIXct("2014-07-01"), 
                              as.POSIXct("2019-07-01")) )
gc
gc + facet_wrap(~ keyword, ncol=1)

#3. Sentiment Analysis -------------

bing <- get_sentiments("bing")

#sentiment analysis for all tweets

dfallwords <- dfall %>%
  select(text, id) %>%
  unnest_tokens(word, text) %>%
  count(word, sort = TRUE)
dfallwords

dfallsa <- dfall %>%
  select(text, id) %>%
  unnest_tokens(word, text) %>%
  inner_join(bing) %>%
  count(word, sentiment, sort = TRUE)
dfallsa

dfallsa <- dfallwords %>%
  inner_join(bing) %>%
  count(word, sentiment, sort = TRUE)
dfallsa

write.csv(dfallsa, file = "SentimentAnalysis.csv")
#write.csv(dfallsa, file = "SentimentAnalysis3.csv")

aitransportsentiment <- dfdate %>%
  select(text, id, keyword, date) %>%
  unnest_tokens(word, text) %>%
  inner_join(bing) %>%
  count(keyword, date, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

aitransportsentiment$date <- as.POSIXct(aitransportsentiment$date)

sp <- ggplot(aitransportsentiment, aes(date, sentiment)) +
  geom_col(show.legend = FALSE) +
  labs(x = "Time (month-year)", y = "Sentiment") + 
  scale_x_datetime(breaks = date_breaks("3 months"),
                   labels = date_format("%b-%y", tz=Sys.timezone()),
                   limits = c(as.POSIXct("2014-07-01"), 
                              as.POSIXct("2019-07-01")) )
sp
#various keywords in one plot
sp + facet_wrap(~ keyword, ncol=2)

#the most frequent users during uber self-driving car accident in 18 March 2018

dfuseracc <- dfdisdate[(dfdisdate$date>="2018-03-18" & dfdisdate$date<="2018-03-20"),]

useracc <- count(dfuseracc, user, sort = TRUE)
useracc
write.csv(useracc, file = "useracc.csv")

#with text
useracc1 <- count(dfuseracc, user, text, sort = TRUE)
useracc1
write.csv(useracc1, file = "useracc1.csv")

#focused trends
# end of June 2016 = tesla
sp <- ggplot(aitransportsentiment, aes(date, sentiment)) +
  geom_col(show.legend = FALSE) +
  labs(x = "Time (date-month-year)", y = "Sentiment") + 
  scale_x_datetime(breaks = date_breaks("1 day"),
                   labels = date_format("%d-%b-%y"),
                   limits = c(as.POSIXct("2016-06-25"), 
                              as.POSIXct("2016-07-05")) )
sp

# March 2017 = uber
sp <- ggplot(aitransportsentiment, aes(date, sentiment)) +
  geom_col(show.legend = FALSE) +
  labs(x = "Time (date-month-year)", y = "Sentiment") + 
  scale_x_datetime(breaks = date_breaks("1 day"),
                   labels = date_format("%d-%b-%y"),
                   limits = c(as.POSIXct("2017-03-16"), 
                              as.POSIXct("2017-04-01")) )
sp

#sentiment analysis without selfdriving and driverless

dfwithoutdrive <- dfdate[(dfdate$keyword!="selfdriving" & 
                     dfdate$keyword!="driverless"),]

dfallsa2 <- dfwithoutdrive %>%
  select(text, id) %>%
  unnest_tokens(word, text) %>%
  inner_join(bing) %>%
  count(word, sentiment, sort = TRUE)
dfallsa2

write.csv(dfallsa2, file = "SentimentAnalysis2.csv")

aitransportsentiment2 <- dfwithoutdrive %>%
  select(text, id, keyword, date) %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("bing")) %>%
  count(keyword, date, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

aitransportsentiment2$date <- as.POSIXct(aitransportsentiment2$date)

ggplot(aitransportsentiment2, aes(date, sentiment)) +
  geom_col(show.legend = FALSE) +
  labs(x = "Time (month-year)", y = "Sentiment") + 
  scale_x_datetime(breaks = date_breaks("3 months"),
                   labels = date_format("%b-%y", tz=Sys.timezone()),
                   limits = c(as.POSIXct("2014-07-01"), 
                              as.POSIXct("2019-07-01")) )

#4. Word Cloud ---------------

#general word cloud for all tweets
dfallwc <- dfall %>%
  select(text, id) %>%
  unnest_tokens(word, text) %>%
  anti_join(custom_stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

#sentiment word cloud for all tweets
dev.off()
dfall %>%
  select(text, id) %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)

