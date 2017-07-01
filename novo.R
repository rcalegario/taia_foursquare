# usuarios <- df_social
# lugares <- df_checkin
# df_social_spark <- copy_to(sc , df_social)
# df_lugares_spark <- copy_to(sc , df_checkin)

# df_checkin = read.csv('~/Desktop/CIn/TAIA/Projeto/csv/checkins.csv', sep = ";")
# df_checkin_uniq = df_checkin %>% select(user_id, venue_id) %>% distinct()
# write.csv(df_checkin_uniq, file = "checkin2.csv", row.names = FALSE)

# df_social = read.csv('~/Desktop/CIn/TAIA/Projeto/csv/socialgraph.csv', sep = ";")
# df_social_uniq = df_social %>% distinct()
# write.csv(df_scl_uniq, file = "social2.csv", row.names = FALSE)

library(dplyr)

freq_social = df_social_uniq %>% group_by(first_user_id) %>% summarise(feq = n())
freq_checkin = df_checkin_uniq %>% group_by(user_id) %>% summarise(frq = n())
colnames(freq_social) = c("user_id", "freq")
freq = freq_social %>% full_join(freq_checkin, by = "user_id")

df_social_uniq = read.csv("social2.csv")
df_checkin_uniq = read.csv("checkin2.csv")

user_venue = function(users, checkin){
  venues = data.frame()
  for(u in users){
    venues = checkin %>% filter(user_id == u) %>% bind_rows(venues)
  }
  venues
}

friends = data.frame(second_user_id = c(1:1000))
for(i in c(1:100)){
  friends = df_social_uniq %>% 
    filter(first_user_id == i) %>% 
    select(second_user_id) %>%
    union(friends)
}
colnames(friends) = c("user_id")

# df_social_sample = df_social_uniq %>% semi_join(friends, by = "first_user_id")

#######

social = read.csv("social_fdp.csv", sep = ";")
checkins = read.csv("checkins3.csv", sep = ";")

grupoAmigos <- function(idUsuario, base){
  require(dplyr)
  amigos <- base %>% filter(user_id== idUsuario)
  amigos
}

grupoLugares <- function(idUsuario, base){
  require(dplyr)
  lugares <- base%>%filter(user_id == idUsuario)
  lugares
}

calculos <- function(grupo1, grupo2, comparacao){
  require(dplyr)
  numerador <- grupo1 %>% semi_join(grupo2,by=comparacao) %>% count() 
  retorno = 0
  if(numerador > 0){
    denominador <- grupo1 %>% full_join(grupo2,by=comparacao) %>% count() 
    retorno = numerador/denominador
  }
  retorno
}

getSI <- function(idUsuario,base,base2,N){
  require(dplyr)
  amigos <- grupoAmigos(idUsuario,base)
  
  lugares <- grupoLugares(idUsuario,base2)
  lugar = FALSE
  if(nrow(lugares) > 0){
    lugar = TRUE
  }

  temp <- data.frame(x1=0,x2=0,n=0) 
  if(lugar){
    for( i in 1:length(amigos$friend_id)){
      idAmigo = amigos$friend_id[i]
      grupoAmigo <- grupoAmigos(idAmigo,base)
      n_amigos = calculos(amigos, grupoAmigo, "friend_id")
      
      grupoLugarAmigo <- grupoLugares(idAmigo,base2)
      n_lugares = calculos(lugares, grupoLugarAmigo, "venue_id")
      
      si = N * n_amigos + (1-N) * n_lugares
      
      if(si > 0){
        temp <- bind_rows(temp, data.frame(x1=idUsuario ,x2=idAmigo, x3=si))
      }
    }
  } else {
    for( i in 1:length(amigos$friend_id)){
      idAmigo = amigos$friend_id[i]
      
      grupoAmigo <- grupoAmigos(idAmigo,base)
      n_amigos = calculos(amigos, grupoAmigo, "friend_id")
      if(n_amigos > 0){
        si = N * n_amigos
        temp <- bind_rows(temp, data.frame(x1=idUsuario ,x2=idAmigo, x3=si))
      }
    } 
  }
  temp
}

users = social %>% select(user_id) %>% distinct()
users = users[with(users, order(user_id)),]

ini <- proc.time()
SIK = data.frame(x1 = 0, x2 = 0, n = 0)
for(j in c(1:1000)){
  print(j)
  novaSIK <- getSI(j, social ,checkins, .05) 
  SIK = union(SIK,novaSIK)
}
proc.time() - ini

