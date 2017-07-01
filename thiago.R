
getWIK = function(user_i, user_k, base_checks){
  require(dplyr)
  check_i = base_checks %>% filter(user_id == user_i) %>% select(venue_id)
  check_k = base_checks %>% filter(user_id == user_k) %>% select(venue_id)
  
  numerador = check_i %>% intersect(check_k) %>% nrow()
  denominador = sqrt((nrow(check_i) * nrow(check_k)))
  
  return((numerador/denominador))
}

getWI = function(user_i, base_checks){
  require(dplyr)
  base = data_frame(i = 0, k = 0, wik = 0)
  check_i = base_checks %>% filter(user_id == user_i) %>% select(venue_id)
  for(v in check_i$venue_id){
    users_k = base_checks %>% filter(venue_id == v) %>% select(user_id)
    for(u_k in users_k$user_id){
      x = base %>% filter(k == u_k) %>% count()
      if(x < 1){
        y = base %>% filter(i == u_k) %>% count()
        if(y < 1){
          wik = getWIK(user_i, u_k, base_checks)
          base = base %>% bind_rows(data_frame(i=user_i, k=u_k, wik=wik))
        }
      }
    }
  }
  base
}

library(dplyr)

users = social %>% select(user_id) %>% distinct() %>% top_n(-1000)
users = users[order(users$user_id),]

WIK = data_frame(i = 0, k = 0, wik = 0)
ini <- proc.time()
for(u in users){
  ini_u <- proc.time()
  print(u)
  novoWIK = getWI(u, checkins)
  WIK = bind_rows(WIK, novoWIK)
  proc.time() - ini_u
}
proc.time() - ini

write.table(WIK, file = "wik.csv", sep = ";", quote = FALSE, row.names = FALSE)
