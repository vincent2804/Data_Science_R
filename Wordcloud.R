# twitter analysis Project 3

library(devtools)
library(twitteR)
library(ROAuth)
library(stringr)
library(plyr)
library(ggplot2)
library(RCurl)
library(streamR)
library(digest)
library(tm)
library(wordcloud)
library(RJSONIO)
download.file(url="http://curl.haxx.se/ca/cacert.pem",destfile="cacert.pem")
cKey = "7LwG08MeBsPdZdiaFg1Zbbze3"
cSecret = "VGsnb77RBtIQkCvgOCeZvhxMld7jWITkDc0SF2G37upHs1NELi"
reqURL = "https://api.twitter.com/oauth/request_token"
accURL = "https://api.twitter.com/oauth/access_token"
authURL = "https://api.twitter.com/oauth/authorize"

#FIRST PHASE DONE
#USE httr
accToken = "3022104764-FUJwW7HX4VPC2seMZQyERHCqbjMj51JSWxEzPqg"
accTokenSecret = "mKEQLgaejawEMosW8kFSnukHk8l3LsjJ7aDb2oQK5fgbU"
setup_twitter_oauth(cKey,cSecret,accToken,accTokenSecret)
my_oauth =setup_twitter_oauth(cKey,cSecret,accToken,accTokenSecret)
cyst_tweets=searchTwitter("cystic fibrosis",n=5000,lang="en")

text = twListToDF(cyst_tweets)

write.table(text, "c:/Users/acer/Desktop/Data-Science_R/Homework/mydata.txt", sep="\t")


df = do.call("rbind",lapply(text,as.data.frame))

df <- do.call(“rbind”, lapply(rdmTweets, as.data.frame))

df$text <- sapply(text$text,function(row) iconv(row,"latin9","ASCII",sub=""))

ctext = Corpus(VectorSource(text))

usableText=str_replace_all(text$text,"[^[:graph:]]", " ")

cyst = lapply(usableText, function(x) x$getText())

cyst1 = tm_map(ctext, content_transformer(function(x) iconv(enc2utf8(x), sub = "byte")))


cyst = sapply(cyst_tweets, function(x) x$getText())



tryTolower = function(x)
{
  # create missing value
  # this is where the returned value will be
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error = function(e) e)
  # if not an error
  if (!inherits(try_error, "error"))
    y = tolower(x)
  return(y)
}

cyst=sapply(cyst_tweets, function(x) tryTolower(x))







iconv(cyst$text, "ASCII", "UTF-8", sub="")

clean.text = function(x)
  
{
  # tolower
  x = tolower(x)
  # remove rt
  x = gsub("rt", "", x)
  # remove at
  x = gsub("@\\w+", "", x)
  # remove punctuation
  x = gsub("[[:punct:]]", "", x)
  # remove numbers
  x = gsub("[[:digit:]]", "", x)
  # remove links http
  x = gsub("http\\w+", "", x)
  # remove tabs
  x = gsub("[ |\t]{2,}", "", x)
  # remove blank spaces at the beginning
  x = gsub("^ ", "", x)
  # remove blank spaces at the end
  x = gsub(" $", "", x)
  return(x)
}


cyst_clean = clean.text(usableText)







# Corpus
or_corpus = Corpus(VectorSource(cyst_clean))
tdm = TermDocumentMatrix(or_corpus, control = list(removePunctuation = TRUE,stopwords = stopwords("english"), removeNumbers = TRUE, tolower = TRUE))
library(wordcloud)
m=as.matrix(tdm)
word_freqs=sort(rowSums(m),decreasing=TRUE)
dm=data.frame(word=names(word_freqs),freq=word_freqs)
wordcloud(dm$word,dm$freq,random.order=TRUE,max.words=100,colors=brewer.pal(8,"Dark2"))
inspect(c(tdm))
View(tdm)
print(tdm)



