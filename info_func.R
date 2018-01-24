info<-function(data_1,data_2,p_name)
{
  temp<<-data_1%>%filter(player_name==p_name)
  print(temp)
  temp2<<-data_2%>%filter(player_api_id==temp[1,3])
  print(temp2[1,])
  api<<-temp[1,3]
}