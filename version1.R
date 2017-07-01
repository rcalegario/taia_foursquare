df_user = read.csv('~/Desktop/CIn/TAIA/Projeto/csv/users.csv', sep = ";")
df_social = read.csv('~/Desktop/CIn/TAIA/Projeto/csv/socialgraph.csv', sep = ";")
df_checkin = read.csv('~/Desktop/CIn/TAIA/Projeto/csv/checkins.csv', sep = ";")
# df_rating = read.csv('~/Desktop/CIn/TAIA/Projeto/csv/ratings.csv', sep = ";")
# df_venue = read.csv('~/Desktop/CIn/TAIA/Projeto/csv/venues.csv', sep = ";")

library(plyr)

a = users_checks[users_checks$freq >= 10,]
b = hist(a$freq)
barplot(b$counts, log = 'y', col = 'white', names.arg = b$breaks[-1])

x = hist(venue_checks[venue_checks$freq > 1,]$freq)
barplot((x$counts + 1), log = 'y', col = 'white', names.arg = x$breaks[-1])



k = 2
i = 4
users_social = count(df_social, 'first_user_id')
users_friend = subset(df_social, df_social$first_user_id %in% users_social$first_user_id)
users_checks = count(df_checkin, 'user_id')

users = union(users_checks$user_id, users_social$first_user_id)

df_SI = data.frame()
n = 0.05

for(u_i in users_social$first_user_id){
  print(u_i)
  F_i = df_social[df_social$first_user_id == u_i,]
  for(u_k in F_i$second_user_id){
    part_1 = 0
    F_k = df_social[df_social$first_user_id == u_k,]
    inter_user = length(intersect(F_k$second_user_id,F_i$second_user_id))
    if(inter_user > 0){
      union_user = length(union(F_k$second_user_id,F_i$second_user_id))
      part_1 = inter_user/union_user
    }
    
    part_2 = 0
    L_i = df_checkin[df_checkin$user_id == u_i,]
    if(length(L_i$user_id) > 0){
      L_k = df_checkin[df_checkin$user_id == user_k,]
      if(length(L_i$user_id) > 0){
        inter_local = length(intersect(L_k$venue_id,L_i$venue_id))
        if(inter_local > 0){
          union_local = length(union(L_k$venue_id,L_i$venue_id))
          part_2 = inter_local/union_local
        }
      }
    }
    SI_ki = (n*part_1) + ((1-n)*part_2)
    si = data.frame(u_i, u_k, SI_ki)
    df_SI = bind_rows(df_SI, si)
  }
}

fk = df_social[df_social$first_user_id == k,]
fi = df_social[df_social$first_user_id == i,]
inter_user = length(intersect(fk$second_user_id,fi$second_user_id))
part_1 = 0
if(inter_user > 0){
  union_user = length(union(fk,fi))
  part_1 = inter_user/union_user
}

Lk = df_checkin[df_checkin$user_id == k,]
Li = df_checkin[df_checkin$user_id == i,]
inter_local = length(intersect(Lk,Li))
part_2 = 0
if(inter_local > 0){
  union_local = length(union(Lk$venue_id,Li$venue_id))
  part_2 = inter_user/union_local
}

n = 0.5
SI_ki = (n*part_1) + ((1-n)*part_2)

