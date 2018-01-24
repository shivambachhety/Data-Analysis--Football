compare_data<-function(basic,attri)
{
  cat(" CHOICES : ")
  cat("overall_rating, potential, crossing, finishing, heading_accuracy, short_passing, volleys, dribbling, curve, free_kick_accuracy, long_passing, ball_control, acceleration, spring_speed, agility, reactions, balance, shot_power, jumping, stamina, strength, long_shots, aggression, interceptions, positioning, vision, penalties, marking, standing_tackle, sliding_tackle, gk_diving, gk_handling, gk_kicking, gk_positioning, gk_reflexes
 \n")
  while(1)
  {
    cat("ENTER STATISTICAL NAME  TO PLOT ON GRAPH (exit to exit): ")
    graph_choice=scan(what = 'character')
    if(graph_choice=="exit")
    {
      break
    }
    stat<<-c()
    j=1
    while(j<=i)
    {
      temp_data=attri%>%filter(player_api_id==compare_api[j])
      temp_x=temp_data[1,graph_choice]
      temp_x=as.numeric(paste(temp_x))
      stat<<-append(stat,temp_x)
      j=j+1
    }
    barplot(stat,main = "PLAYER COMPARISON",xlab="PLAYERS",ylab=graph_choice,ylim=c(0,120),names.arg = compare_names,col = rainbow(7))
  }
}