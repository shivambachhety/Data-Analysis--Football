tweet_count<-function(a){
library(twitteR)
library(plyr)
library(ROAuth) 
library(stringr) 
library(ggplot2)
library(tm)
  api_key<- ""
  api_secret <- ""
  access_token <- ""
  access_token_secret <- ""
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)
posText <- read.delim("positive-words.txt", header=FALSE, stringsAsFactors=FALSE)
posText <- posText$V1
posText <- unlist(lapply(posText, function(x) { str_split(x, "\n") }))
negText <- read.delim("negative-words.txt", header=FALSE, stringsAsFactors=FALSE)
negText <- negText$V1
negText <- unlist(lapply(negText, function(x) { str_split(x, "\n") }))
pos.words = posText
neg.words = c(negText, 'wtf')
player_tweets = searchTwitter(paste("#",a), n=400, lang="en")
player_txt = sapply(player_tweets, function(t) t$getText() )
noof_tweets = length(player_txt)
player<- c(player_txt)
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{  scores = laply(sentences,
                  function(sentence, pos.words, neg.words)
                  {
                    sentence = gsub("[[:punct:]]", "", sentence)
                    
                    sentence = gsub("[[:cntrl:]]", "", sentence)
                    
                    sentence = gsub('\\d+', '', sentence)
                    
                    tryTolower = function(x)
                    {
                      y = NA
                      
                      try_error = tryCatch(tolower(x), error=function(e) e)
                      
                      if (!inherits(try_error, "error"))
                        y = tolower(x)
                      
                      return(y)
                    }
                    
                    sentence = sapply(sentence, tryTolower)
                    word.list = str_split(sentence, "\\s+")
                    words = unlist(word.list)
                    
                    pos.matches = match(words, pos.words)
                    neg.matches = match(words, neg.words)
                    
                    pos.matches = !is.na(pos.matches)
                    neg.matches = !is.na(neg.matches)
                    
                    score = sum(pos.matches) - sum(neg.matches)
                    return(score)
                  }, pos.words, neg.words, .progress=.progress )

scores.df = data.frame(text=sentences, score=scores)
return(scores.df)
}
scores = score.sentiment(player, pos.words,neg.words , .progress='text')
scores$player = factor(rep("Player", noof_tweets))
scores$positive <- as.numeric(scores$score >0)
scores$negative <- as.numeric(scores$score >0)
scores$neutral <- as.numeric(scores$score==0)
player_score <<- subset(scores, scores$player=="Player")
player_score$polarity <- ifelse(player_score$score >0,+1,ifelse(player_score$score < 0,-1,ifelse(player_score$score==0,0,0)))
qplot(factor(polarity), data=player_score, geom="bar", fill=factor(polarity))+xlab("Sentiment Categories") + ylab("No of tweets") + ggtitle("Twitter Sentimental Analysis")
temp_tweet=0
for(i in 1:400)
{
  temp_tweet=temp_tweet+player_score$polarity[i]
}
print(temp_tweet)
if(!is.na(temp_tweet)){
  compare_tweets<<-append(compare_tweets,temp_tweet)
  twitter_names<<-append(twitter_names,a)
}
}