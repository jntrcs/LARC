games2015<-sapply(1:nrow(all2015data[[17]][[1]]), FUN = function(n){
  sum(all2015data[[17]][[1]][n, 4])
})

hist(games2015)

games2016<-sapply(1:nrow(all2016data[[9]][[1]]), FUN = function(n){
  sum(all2016data[[9]][[1]][n, 4])
})

hist(games2016)
a<-all2015data[[17]][[1]]$Team
b<-all2016data[[9]][[1]]$Team

#teams in 2015 not in 2016
a[!a%in%b]

#Teams in 2016 notin 2015
b[!b%in%a]

#find teams that played less than three games
teams2015<-NULL
for (i in 1:nrow(all2015data[[17]][[1]]))
     {
     if (sum(all2015data[[17]][[1]][i, 4])<3)
      teams2015<-c(teams2015, all2015data[[17]][[1]][[i,1]])

}
teams2015

teams2016<-NULL
for (i in 1:nrow(all2016data[[9]][[1]]))
{
  if (sum(all2016data[[9]][[1]][i, 4])<3)
    teams2016<-c(teams2016, all2016data[[9]][[1]][[i,1]])
  
}
teams2016

teams2016[!teams2016%in%teams2015]
