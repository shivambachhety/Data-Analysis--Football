search_list<-function(basic,attri)
{
  while(1){
    cat("             SEARCH\n\n")
    name=scan(what = 'character',sep = '?')
    if(name=="exit")
    {
      break
    }
    info(basic,attri,name)
    cat("\n    DO YOU WANT TO ADD HIM ?  y/n:  \n")
    choice=scan(what = 'character')
    if(choice=='y')
    {
      add_list(api,name)
    }
  }
}