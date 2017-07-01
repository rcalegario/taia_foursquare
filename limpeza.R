library(dplyr)

# limpeza dos checkins
# checkins = read.csv('~/Desktop/CIn/TAIA/Projeto/csv/checkins.csv', sep = ";")
# new_checkins = checkins %>% select(user_id,venue_id) %>% group_by(user_id, venue_id) %>% summarise(freq_checks = n())
# write.table(new_checkins, file = "checkins_guv.csv", sep = ";", row.names = FALSE, quote = FALSE)



# social = read.csv("social2.csv")
# colnames(social) = c("user_id","friend_id")
# friends = data.frame(friend_id = c(1:1000))
# for(i in c(1:1000)){
#   friends = social %>% filter(user_id == i) %>% select(friend_id) %>% union(friends)
# }
# colnames(friends) = c("user_id")
# new_social = social %>% semi_join(friends, by = "user_id")
freq = social %>% group_by(user_id) %>% summarise(freq = n())
fdp = freq %>% filter(freq > 1500) %>% select(user_id)
new_social2 = social %>% anti_join(fdp, by="user_id") 
colnames(fdp) = c("friend_id")
new_social2 = new_social2 %>% anti_join(fdp, by="friend_id")
write.table(new_social2, file="social_fdp.csv", sep = ";", quote = FALSE, row.names = FALSE)

# checkins = read.csv("checkins_guv.csv", sep = ";")
# u = social %>% select(user_id) %>% distinct()
# chechins_calegario = checkins %>% semi_join(u, by="user_id") %>% select(-freq_checks) %>% distinct()
# write.table(chechins_calegario, file = "checkins3.csv", sep = ";", quote = FALSE, row.names = FALSE)

# venues = read.csv('~/Desktop/CIn/TAIA/Projeto/csv/venues.csv', sep = ";")
# v = checkins_calegario %>% select(venue_id) %>% distinct()
# colnames(venues) = c("venue_id","latitude","longitude")
# new_venue = venues %>% semi_join(v, by="venue_id")
# write.table(new_venue, file = "venues3.csv", sep = ";", quote = FALSE, row.names = FALSE)

social = read.csv("social_fdp.csv", sep = ";")
checkins = read.csv("checkins3.csv", sep = ";")
venues = read.csv("venues3.csv", sep = ";")
