fifa_ranking<-function(){
library(rvest)
cat("Enter the number of Rankings")
n=scan()
url <- "http://www.rankfootball.com/"
my_html <- read_html(url)
table1 <- html_nodes(my_html,"table")[[2]]
team <- html_table(table1)
team <- team[1:n+1,1:3]
names(team) = c("Rank","Country","Points")
print(team)
table2 <- html_nodes(my_html,"table")[[2]]
move <- html_table(table2)
move <- move[1:n+1,3:4]
names(move) = c("Country","Move")
barplot(as.numeric(move$Move),main = "COUNTRY RANK MOVE COMPARISON(2016-17)",ylim=c(-10,20),xlab="TEAMS",ylab="RANK MOVE",names.arg = team$Country,col = rainbow(7))
}