library(tidyverse)
library(data.table)

list.files("../input")

cat("Loading Data.....")
train <- fread("../input/train_V2.csv")

cat("Functions")
 createTeamInformation <- function(x)
 {
 x <- summarise(group_by(x,matchId,groupId),
                
                team.partySize=n(),
                team.assists.mean=mean(assists),
                team.boosts.mean=mean(boosts),
                team.damageDealt.mean=mean(damageDealt),
                team.DBNOs.mean=mean(DBNOs),
                team.headshotKills.mean=mean(headshotKills),
                team.heals.mean=mean(heals),
                team.killPlace.mean=mean(killPlace),
                team.killStreaks.mean=mean(killStreaks),
                team.longestKill.mean=mean(longestKill),
                team.revives.mean=mean(revives),
                team.rideDistance.mean=mean(rideDistance),
                team.swimDistance.mean=mean(swimDistance),
                team.teamKills.mean=mean(teamKills),
                team.kills.mean=mean(kills),
                team.walkDistance.mean=mean(walkDistance),
                team.vehicleDestroys.mean=mean(vehicleDestroys),
                team.walkDistance.mean=mean(walkDistance),
                team.weaponsAcquired.mean=mean(weaponsAcquired),
                team.rankPoints.mean=mean(rankPoints)
                
                
 )
 return(x)
 }

#
createMatchInformation <- function(x)
{
x <- summarise(group_by(x,matchId),
               
               match.matchSize=n(),

               match.assists.mean=mean(assists),
               match.boosts.mean=mean(boosts),
               match.damageDealt.mean=mean(damageDealt),
               match.DBNOs.mean=mean(DBNOs),
               match.headshotKills.mean=mean(headshotKills),
               match.heals.mean=mean(heals),
               match.killPlace.mean=mean(killPlace),
               match.killStreaks.mean=mean(killStreaks),
               match.longestKill.mean=mean(longestKill),
               match.revives.mean=mean(revives),
               match.rideDistance.mean=mean(rideDistance),
               match.swimDistance.mean=mean(swimDistance),
               match.teamKills.mean=mean(teamKills),
               match.kills.mean=mean(kills),
               match.walkDistance.mean=mean(walkDistance),
               match.vehicleDestroys.mean=mean(vehicleDestroys),
               match.walkDistance.mean=mean(walkDistance),
               match.weaponsAcquired.mean=mean(weaponsAcquired),
               match.rankPoints.mean=mean(rankPoints)
)
return(x)
}

pre.engineer <- function(x)
{
  x<-x%>%mutate(prev_rank_system=ifelse(rankPoints==-1,1,0),
                rankPoints = ifelse(rankPoints==-1,((winPoints*0.8)+(killPoints*0.2)),rankPoints),
                rank=ifelse(rankPoints<1,1,
                     ifelse(rankPoints>=1 & rankPoints<1400,2,
                     ifelse(rankPoints>=1400 & rankPoints<1499,3,
                     ifelse(rankPoints>=1500 & rankPoints<1600,4,
                     ifelse(rankPoints>=1600 & rankPoints<1700,5,
                     ifelse(rankPoints>=1700 & rankPoints<1800,6,        
                     ifelse(rankPoints>=1800 & rankPoints<1900,7,
                     ifelse(rankPoints>=1900 & rankPoints<2000,8,
                     ifelse(rankPoints>=2000,9,0)))))))))
                )
  x$rank<-as.factor(x$rank)
  x<-x%>%select(-winPoints,-killPoints)
  return(x) 
}

post.engineer <- function(x)
{
x$matchType<-as.factor(x$matchType)
x <- x %>% mutate(diff.ind.walkDistance=walkDistance/match.walkDistance.mean,
                  diff.team.walkDistance=team.walkDistance.mean/match.walkDistance.mean,
                  diff.ind.kills=kills/match.walkDistance.mean,
                  diff.team.kills=team.kills.mean/match.kills.mean,
                  ind.accuracy=headshotKills/kills,
                  team.accuracy=team.headshotKills.mean/team.kills.mean,
                  unsual.team.modifier=ifelse(matchType=="squad-fpp",4-team.partySize,
                                                                 ifelse(matchType=="squad-tpp",4-team.partySize,
                                                                 ifelse(matchType=="duo-fpp",2-team.partySize,
                                                                 ifelse(matchType=="duo-tpp",2-team.partySize,
                                                                 ifelse(matchType=="solo",1-team.partySize,
                                                                 ifelse(matchType=="solo-fpp",1-team.partySize,0))))))
                                                                      )


return(x)
}
nrow(train)
train<-train%>%filter(!is.na(winPlacePerc))
nrow(train)
train <- pre.engineer(train)
train.teamed <- createTeamInformation(train)
train.match <- createMatchInformation(train)
train.relative <- train.teamed%>%inner_join(train.match)
train <- train %>% inner_join(train.relative)
train<-post.engineer(train)
train$matchType<-NULL
rm(train.teamed,train.relative)
gc()

normalize <- function(x) { 
    x <- sweep(x, 2, apply(x, 2, min)) 
    sweep(x, 2, apply(x, 2, max), "/") 
}

#train<-sample_n(train,2000000)


set.seed(10101)
train.ids<-train %>% select(Id)
train.ids<-sample_frac(train.ids,0.2)
holdout<-train%>%inner_join(train.ids)
train<-train%>%anti_join(train.ids)
rm(train.ids)
gc()
train.labels <- train$winPlacePerc
train<-train%>%select(-winPlacePerc)
train<-data.matrix(select(train,-Id,-groupId,-matchId))
train[is.nan(train)] = 0
input<-ncol(train)
train1<-normalize(train)
train<-train1
rm(train1)
library(keras)
print(input)
model <- keras_model_sequential() 
model %>% 
  layer_dense(units = 256, activation = "relu", input_shape = c(input)) %>% 
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 128, activation = "relu") %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 1, activation = "linear")

summary(model)
model %>% compile(
  loss = "mean_absolute_error",
  optimizer = optimizer_adam()
)

history <- model %>% fit(
  train, train.labels, 
  epochs = 54, batch_size = 128, 
  validation_split = 0.2, verbose=2
)

history

rm(train)
gc()

plot(history)

holdout.labels <- holdout$winPlacePerc
holdout<-holdout%>%select(-winPlacePerc)
holdout<-data.matrix(select(holdout,-Id,-groupId,-matchId))
holdout[is.nan(holdout)] = 0
holdout<-normalize(holdout)

preds<-model %>% evaluate(holdout, holdout.labels,verbose = 0)
preds1<-model %>% predict(holdout)

library(Metrics)
Metrics::mae(as.matrix(holdout.labels),preds1)

head(preds1)
head(as.matrix(holdout.labels))

rm(holdout,holdout.labels)

test <- fread("../input/test_V2.csv")

test <- pre.engineer(test)
test.teamed <- createTeamInformation(test)
test.match <- createMatchInformation(test)
test.relative <- test.teamed%>%inner_join(test.match)
test <- test %>% inner_join(test.relative)
test<-post.engineer(test)
test$matchType<-NULL
rm(test.match,test.relative)

test<-data.matrix(select(test,-Id,-groupId,-matchId))
test[is.nan(test)] = 0
test<-normalize(test)

preds<-model %>% predict(test)

sub <- fread("../input/sample_submission_V2.csv")
sub$winPlacePerc<-preds
write.csv(sub,"Sample_Keras_Dinner.csv",row.names=F)