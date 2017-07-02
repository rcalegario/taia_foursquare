grupoAmigos <- function(idUsuario, base){
  require(dplyr)
  amigos <- base %>% filter(user_id== idUsuario)
  amigos
}

getCIJ = function(user_i, venue_j, base_social, base_checks, sik){
  users_k = grupoAmigos(user_i, base_social) %>% select(friend_id)
  colnames(users_k) = c("user_id")
  
  users_venue_k = base_checks %>% 
    filter(venue_id == venue_j) %>% 
    semi_join(users_k, by="user_id") %>%
    select(user_id)
  colnames(users_venue_k) = c("friend_id")
  
  retorno = 0
  if(nrow(users_venue_k) > 0){
    numerador = sik %>% semi_join(users_venue_k, by="friend_id") %>% select(n) %>% sum()
    
    if(numerador > 0){
      colnames(users_k) = c("friend_id")
      denominador = sik %>% semi_join(users_k, by="friend_id")  %>% select(n) %>% sum()
      retorno = numerador/denominador 
    }
  }
  
  retorno
}

venues = checkins %>% select(venue_id) %>% distinct()
users = social %>% select(user_id) %>% distinct() %>% top_n(-1000)

getCI = function(user_id, base_social, base_checks, sik, all_venues){
  retorno = data_frame(user_id = 0, venue_id = 0, cij = 0)
  for(v in all_venues$venue_id){
    print(v)
    cij = getCIJ(user_id, v, base_social, base_checks, sik)
    retorno = retorno %>% bind_rows(data_frame(user_id = user_id, venue_id = v, cij = cij))
  }
  retorno
}

colnames(SI) = c("user_id","friend_id","n")
getCIJ(1, 2141, social, checkins, SI)
getCI(1, social, checkins, SI, venues)

