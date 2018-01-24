twitter_graph<-function()
{
  compare_tweets<<-c()
  twitter_names<<-c()
  while(1)
  {
    cat("SEARCH PLAYER ON TWITTER(exit to finalise) : ")
    tweet_choice=scan(what = 'character')
    if(tweet_choice=="exit")
    {
      break;
    }
    tweet_count(tweet_choice)
  }
  barplot(compare_tweets,main = "TWITTER COMPARISON",ylim = c(-400,400),xlab="PLAYERS",ylab="SENTIMENT SCORE",names.arg = twitter_names,col = rainbow(7))
  
}