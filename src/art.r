#Load Required Packages
if (!require ("tidyr")) {
  install.packages("tidyr", dependencies = TRUE )
  library(tidyr)
}
if (!require ("dplyr")) {
  install.packages("dplyr", dependencies = TRUE )
  library(dplyr)
}

#Load Required NLP Packages

if (!require ("tm")) {
  install.packages("tm", dependencies = TRUE )
  library(tm)
}
if (!require ("textdata")) {
  install.packages("textdata", dependencies = TRUE )
  library(textdata)
}
if (!require ("quanteda")) {
  install.packages("quanteda", dependencies = TRUE )
  library(quanteda)
}
if (!require ("SnowballC")) {
  install.packages("SnowballC", dependencies = TRUE )
  library(SnowballC)
}
if (!require ("colorspace")) {
  install.packages("colorspace", dependencies = TRUE )
  library(colorspace)
}


#set working directory
setwd("~/Desktop/werk")

#read file 
data <- read.csv("news.csv") #10k articles
str(data)

#try lubricate package
#add day cause
data$date <- as.Date(data$date)
data$day <- as.numeric(format(data$date, format = "%d"))
data$month <- as.numeric(format(data$date, format = "%m"))
data$day <- factor(data$day)
data$month <-factor(data$month)
data$year<-factor(data$year)

#convert to character vector
corp <- iconv(data$content, to="utf-8-mac")
str(corp)

#Create corpus
corpus <- Corpus(VectorSource(corp))
inspect(corpus[1:5])

#Define function to remove special characters
reorder.stoplist <- c(grep("[']", stopwords('english'), value = TRUE), 
                      stopwords('english')[!(1:length(stopwords('english')) %in% grep("[']", stopwords('english')))])

#cleaning
cleantext <- function(corpus){
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation) 
  corpus <- tm_map(corpus, removeWords, reorder.stoplist)
  corpus <- tm_map(corpus, removeWords, stopwords('english'))
  corpus <- tm_map(corpus, removeWords, c('also', '000','140','190','240','300','700','added', 'another',
                                          'still','ago','don','better','back','can','s','even','said', 'made',
                                          'get','may','say','called','see','going','much','just','going','think'
                                          ,'many','told','get','donald','states','way','including','one'))
  corpus <- tm_map(corpus, gsub, pattern = 'united', replacement = 'usa')
  corpus <- tm_map(corpus, gsub, pattern = '“', replacement = ' ')
  corpus <- tm_map(corpus, gsub, pattern = '”', replacement = ' ')
  corpus <- tm_map(corpus, gsub, pattern = 'ù', replacement = ' ')
  corpus <- tm_map(corpus, gsub, pattern = 'Ä̀', replacement = ' ')
  corpus <- tm_map(corpus, gsub, pattern = '‚̈̂', replacement = ' ')
  corpus <- tm_map(corpus, gsub, pattern ='‚̈', replacement = ' ')
  corpus <- tm_map(corpus, gsub, pattern = '‚̈̂', replacement = ' ')
  corpus <- tm_map(corpus, gsub, pattern ='ú', replacement = ' ')
  corpus <- tm_map(corpus, gsub, pattern ='ô', replacement = ' ')
  corpus <- tm_map(corpus, stripWhitespace) 
  #corpus <- tm_map(corpus, stemDocument) #stemming - optional as it has limitation
}
#remove s 
#won't can't 
clean <- cleantext(corpus)
inspect(clean[1:3])


#uses snowballC package
#sdata <- tm_map(corpus, stemDocument) 
inspect(corpus[1:5])
#inspect(sdata[1:5])

#Term Document Matrix - convert unstruc data to struc data
tdm <- TermDocumentMatrix(clean)
tdm2 <- DocumentTermMatrix(clean)
tdm
mat <- as.matrix(tdm)
#dat[19:40,1:25]

ttp <- t(tdm2) #transpose matrix

rowTotals <- apply(tdm2 , 1, sum) #Find the sum of words in each Document
dtm.new   <- tdm2[rowTotals> 0, ]           #remove all docs without words


#word frequency
rSum <- rowSums(mat)
rSum
(s1 <- subset(rSum, rSum>3000))
(s2<- subset(rSum, rSum<20))
(s3 <- subset(rSum, rSum>200))

length(rSum)
(ord <- order(rSum, decreasing = TRUE))
rSum[head(ord)]
#rSum[tail(ord)] #should be given importance, give description 

t <- t(dat) #transpose matrix

#word freqency 
idx <- which(dimnames(tdm)$Terms == "r")
inspect(tdm[idx + (0:5), 101:110])

#inspect frequent words
(freq.terms <- findFreqTerms(tdm, lowfreq=15))

#word associations
findAssocs(tdm, "china", 0.5)
findAssocs(tdm, "states", 0.5)
findAssocs(tdm, "trump", 0.5)

k <- rSum
k <- subset(k, k>4000)
barplot(k,
        las = 2,
        col = rainbow_hcl(27))

#------------------------Topic Modeling with tidy package -----------------------------
#Load Required Packages
if (!require ("tidytext")) {
  install.packages("tidytext", dependencies = TRUE )
  library(tidytext)
}
if (!require ("topicmodels")) {
  install.packages("topicmodels", dependencies = TRUE )
  library(topicmodels)
}
if (!require ("MASS")) {
  install.packages("MASS", dependencies = TRUE )
  library(MASS)
}
if (!require ("stringr")) {
  install.packages("stringr", dependencies = TRUE )
  library(stringr)
}
if (!require ("ggplot2")) {
  install.packages("ggplot2", dependencies = TRUE )
  library(ggplot2)
}


#build LDA model
ldatp <- LDA(dtm.new, k = 5, control = list(seed = 1234))
ldatp

tp_topics <- tidy(ldatp, matrix = "beta")
tp_topics

top_tpterms <- tp_topics %>%
  group_by(topic) %>%
  top_n(9, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

#Terms most common within each topic to examine per-topic-per-word probabilities.
top_tpterms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()

beta_spread <- tp_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

beta_spread

art <- tidy(ldatp, matrix = "gamma")
art

#top 5 topics
top_terms <- tp_topics %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms

top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()

#------------------------------Sentiment Analysis---------------------------#
#Load Required Packages
if (!require ("syuzhet")) {
  install.packages("syuzhet", dependencies = TRUE )
  library(syuzhet)
}
if (!require ("lubridate")) {
  install.packages("lubridate", dependencies = TRUE )
  library(lubridate)
}
if (!require ("scales")) {
  install.packages("scales", dependencies = TRUE )
  library(scales)
}
if (!require ("janeaustenr")) {
  install.packages("janeaustenr", dependencies = TRUE )
  library(janeaustenr)
}
if (!require ("reshape2")) {
  install.packages("reshape2", dependencies = TRUE )
  library(reshape2)
}
if (!require ("graph")) {
  install.packages("graph", dependencies = TRUE )
  library(graph)
}
if (!require ("Rgraphviz")) {
  install.packages("Rgraphviz", dependencies = TRUE )
  library(Rgraphviz)
}
if (!require ("SentimentAnalysis")) {
  install.packages("SentimentAnalysis", dependencies = TRUE )
  library(SentimentAnalysis)
}
if (!require ("tibble")) {
  install.packages("tibble", dependencies = TRUE )
  library(tibble)
}

install.packages("BiocManager")
BiocManager::install("Rgraphviz")

#sentiment lexicons
get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")

s <- get_nrc_sentiment(corp)
head(s)
get_nrc_sentiment('police')

sentiment <- analyzeSentiment(dtm.new, language = "english")

sentiment <- as.data.frame(sentiment)
head(sentiment)
summary(sentiment$SentimentGI)

#top 5 
sentiment %>% 
  summarize(sentiment = mean(SentimentGI)) %>%
  arrange(desc(sentiment)) %>%
  head(n= 5)

#bottom 5 
sentiment %>% 
  summarize(sentiment = mean(SentimentGI)) %>%
  arrange(sentiment) %>%
  head(n= 5)

nrcsent <- get_nrc_sentiment(corp)

sent <- as.data.frame(colSums(nrcsent))
sent <- rownames_to_column(sent) 
colnames(sent) <- c("emotion", "count")

ggplot(sent, aes(x = emotion, y = count, fill = emotion)) + geom_bar(stat = "identity") + theme_minimal() + theme(legend.position="none", panel.grid.major = element_blank()) + labs( x = "Emotion", y = "Total Count") + ggtitle("Sentiment Score") + theme(plot.title = element_text(hjust=0.5))

#--------------------------------Visualization-------------------------------#

if (!require ("wordcloud")) {
  install.packages("wordcloud", dependencies = TRUE )
  library(wordcloud)
}
if (!require ("wordcloud2")) {
  install.packages("wordcloud2", dependencies = TRUE )
  library(wordcloud2)
}
if (!require ("colorspace")) {
  install.packages("colorspace", dependencies = TRUE )
  library(colorspace)
}

#histogram 
wf = data.frame(term=names(rSum), occurences = rSum)
p <- ggplot(subset(wf, rSum>700),aes(term, occurences))
p <- p + geom_bar(stat = "identity")
#p

amat <- as.matrix(dtm.new)
tmat <- t(amat)
#word frequency
w <- rowSums(tmat)
w
w2 <- subset(w, w>4000 )
w2 
w5 <- subset(w, w<20)
w5

#Bar Plot
barplot(w2,
        las =2,
        col = rainbow(5))


#word clouds
w3 <- sort(rowSums(tmat), decreasing = TRUE)
wordcloud(words = names(w2),
          freq=w2)
wordcloud(words = names(w3),
          freq=w3, max.words=150,
          random.order = F,
          min.freq = 200,
          colors = brewer.pal(8, 'Dark2'),
          scale = c(5,0.3))

w4 <- data.frame(names(w2),w2)
w4

w5 <- sort(rowSums(tmat), decreasing = TRUE)
wordcloud(words = names(w5),
          freq=w5, max.words=20,
          random.order = F,
          min.freq = 700,
          colors = brewer.pal(8, 'Dark2'),
          scale = c(5,0.3))

#------------------------#

tdtp
freq <- rowSums(as.matrix(tdm2))
length(freq)
ord <- order(freq, decreasing = TRUE)
ord
freq[head(ord)]
freq[tail(ord)] #should be given importance, give description 



sums <- as.data.frame(rowSums(as.matrix(tdtp)))
wordcloud(words = names(sums), freq = w5, min.freq = 1000,
          max.words=20, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

dat1 <- as.matrix(tdtp)
rSum1 <- rowSums(dat1)
s11 <- subset(rSum1, rSum1>200 )

wordcloud(words = names(s11),
          freq=s11)

wordcloud(words = names(s11),
          freq=s11, max.words=150,
          random.order = F,
          min.freq = 200,
          colors = brewer.pal(8, 'Dark2'),
          scale = c(4,0.3))


