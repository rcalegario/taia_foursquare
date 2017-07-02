checkins = read.csv("D:\\Taia\\R\\taia\\checkins3.csv", sep =";")
venues = read.csv("D:\\Taia\\R\\taia\\venues3.csv", sep =";")
users = read.csv("D:\\Taia\\R\\taia\\social_fdp.csv", sep=";")
dist = function(locals_lat_long){
  total = 0
  cond = TRUE
  for (v in locals_lat_long$venue_id){
    local  = locals_lat_long %>% filter(venue_id == v)
    if (cond){
      lat_aux = local[2] %>% sum()
      long_aux = local[3] %>% sum()
      cond = FALSE
    } else {
      latitude = local[2] %>% sum()
      longitude = local[3] %>% sum()
      total = total + sqrt(((latitude-lat_aux)^2)+(((longitude-long_aux)^2)))
      lat_aux = latitude
      long_aux = longitude
    }
  }
  total
}



writePL = function(checks, vens, uId) {
  require(dplyr)
  ven_user = checks %>% filter(user_id == uId) %>% select(venue_id)
  locals = inner_join(ven_user,vens)
  retorno = data_frame()
  
  if (nrow(locals)>2){
    total  = dist(locals)
    #print(total)
    processed = locals %>% filter(venue_id==0)
    #print(processed)
    
    for(l in locals$venue_id){
      
      no = locals %>% filter(venue_id==l)
      #print(no)
      processed = bind_rows(processed,no)
      #print(processed)
      resto = anti_join(locals,processed)
  
      for(l2 in resto$venue_id){
       
        no_aux = resto %>% filter(l2==venue_id)
        no_aux = bind_rows(no_aux,no)
        num = dist(no_aux)/total
        esc = data_frame(num)
        retorno = bind_rows(retorno,esc)
       
      }
    }
  } else {
    retorno = data_frame(num=1) 
  }
  retorno
}
users_solo = users %>% select(user_id) %>% distinct()

powerlaw = data_frame()
for (u in users_solo$user_id) {
  print(u)
  powerlaw = bind_rows(powerlaw, writePL(checkins, venues, u))
}
write.table(powerlaw,file="power_law.csv",quote = FALSE, row.names = FALSE,col.names = FALSE)

