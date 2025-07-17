library(dplyr)
library(cluster)
library(factoextra)
library(descr)
library(sjPlot)
library(ggcorrplot)



WC2022 = read.csv('Documents/Data Science Primer/QACDATAWC2022.csv', col.names = c('Player','Position','Squad','Age','Born','Goals','Shots','Shots on Target','Average Shot Distance','Free Kicks Made','Penalty Kicks Made','Penalty Kicks Attempted','Touches','Tackles','Tackles Won','Blocks','Interceptions','Passes Completed','Passes Attempted'))


WC2022$Team.Rank = with(WC2022,ifelse(Squad == 'Argentina', 1
                              ,ifelse(Squad == 'France', 2
                              ,ifelse(Squad == 'Croatia', 3
                              ,ifelse(Squad == 'Morocco', 4
                              ,ifelse(Squad == 'Netherlands', 5
                              ,ifelse(Squad == 'England', 6
                              ,ifelse(Squad == 'Brazil', 7
                              ,ifelse(Squad == 'Portugal', 8
                              ,ifelse(Squad == 'Japan', 9
                              ,ifelse(Squad == 'Senegal', 10
                              ,ifelse(Squad == 'Australia', 11
                              ,ifelse(Squad == 'Switzerland', 12
                              ,ifelse(Squad == 'Spain', 13
                              ,ifelse(Squad == 'United States', 14
                              ,ifelse(Squad == 'Poland', 15
                              ,ifelse(Squad == 'Korea Republic', 16
                              ,ifelse(Squad == 'Germany', 17
                              ,ifelse(Squad == 'Ecuador', 18
                              ,ifelse(Squad == 'Cameroon', 19
                              ,ifelse(Squad == 'Uruguay', 20
                              ,ifelse(Squad == 'Tunisia', 21
                              ,ifelse(Squad == 'Mexico', 22
                              ,ifelse(Squad == 'Belgium', 23
                              ,ifelse(Squad == 'Ghana', 24
                              ,ifelse(Squad == 'Saudi Arabia', 25
                              ,ifelse(Squad == 'Iran', 26
                              ,ifelse(Squad == 'Costa Rica', 27
                              ,ifelse(Squad == 'Denmark', 28
                              ,ifelse(Squad == 'Serbia', 29
                              ,ifelse(Squad == 'Wales', 30
                              ,ifelse(Squad == 'Canada', 31
                              ,ifelse(Squad == 'Qatar', 32, NA)))))))))))))))))))))))))))))))))
              

for(x in 1:nrow(WC2022)){
  WC2022$Position[x] <- substring(WC2022$Position[x],1,2)
}

Subset1 = WC2022[,c(2,6:ncol(WC2022))]
Subset1 = na.omit(Subset1)

Subset1_Scale = scale(select(Subset1, -c("Position")))
rownames(Subset1_Scale) <- Subset1$Position

fviz_nbclust(Subset1_Scale, kmeans, method = "wss")

output <- kmeans(Subset1_Scale, centers = 3, nstart = 20)
print(output)

kmcluster <- output$cluster

fviz_cluster(output, data = select(Subset1, -c("Position")))

test_df <- cbind(Subset1, cluster = output$cluster)

test_df %>% group_by(Position %>% summarize(freq = n(),percent = count))
prop.table(table(test_df$Position, test_df$cluster),margin = 1)

# Cluster does not show any two positions being very similar to each other and clustered together, therefore will run each position seperately for linear regression

# also say in cluster 3 there are clearly a couple of points that seem like outliers and with more time on this project would look into those and potentially try to get rid of them

lm1 = lm(formula = Team.Rank ~ Goals, Subset1)
lm2 = lm(formula = Team.Rank ~ Shots, Subset1)
lm3 = lm(formula = Team.Rank ~ Shots.on.Target, Subset1)
lm4 = lm(formula = Team.Rank ~ Average.Shot.Distance, Subset1)
lm5 = lm(formula = Team.Rank ~ Free.Kicks.Made, Subset1)
lm6 = lm(formula = Team.Rank ~ Penalty.Kicks.Made, Subset1)
lm7 = lm(formula = Team.Rank ~ Penalty.Kicks.Attempted, Subset1)
lm8 = lm(formula = Team.Rank ~ Touches, Subset1)
lm9 = lm(formula = Team.Rank ~ Tackles, Subset1)
lm10 = lm(formula = Team.Rank ~ Tackles.Won, Subset1)
lm11 = lm(formula = Team.Rank ~ Blocks, Subset1)
lm12 = lm(formula = Team.Rank ~ Interceptions, Subset1)
lm13 = lm(formula = Team.Rank ~ Passes.Completed, Subset1)
lm14 = lm(formula = Team.Rank ~ Passes.Attempted, Subset1)
lm15 = lm(formula = Team.Rank ~ Goals + Shots + Shots.on.Target + Average.Shot.Distance + Free.Kicks.Made + Penalty.Kicks.Made + Penalty.Kicks.Made + Penalty.Kicks.Attempted + Touches + Tackles + Tackles.Won + Blocks + Interceptions + Passes.Completed + Passes.Attempted, Subset1)

tab_model(lm1, lm2, lm3, lm4, lm5, lm6, lm7, lm8, lm9, lm10, lm11, lm12, lm13, lm14, lm15, dv.labels = c('model1','model2','model3','model4','model5','model6', 'model7', 'model8', 'model9','Model10', 'Model11', 'Model12', 'Model13', 'Model14', 'Model15'), show.aic = TRUE)

summary(lm4)

Subset1B = Subset1 %>% select(-Team.Rank, -Position)

ggcorrplot(Subset1B)
install.packages("psych")
library(psych)
corPlot(Subset1B)

# correlation plot shows that there is a some correlation between variables so likely there is multicollinearity when all variables are used in 
# linear regression causing correlation to go away

str(Subset1)

Subset1$Position = as.factor(Subset1$Position)

linear1 = lm(formula = Goals ~ relevel(Position, ref="FW"), Subset1)
# can justify using goals only for forward because clearly something special about being part of a forward as for the other its significant that they score less goals than forwards
linear2 = lm(formula = Shots ~ relevel(Position, ref='FW'), Subset1)
# can justify using shots only for forward because clearly something special about being part of a forward as for the other its significant that they shoot less than forwards
linear3 = lm(formula = Shots.on.Target ~ relevel(Position, ref='FW'), Subset1)
# can justify using shots on target only for forward because clearly something special about being part of a forward as for the other its significant that they shoot less than forwards

linear4 = lm(formula = Average.Shot.Distance ~ relevel(Position, ref='FW'), Subset1)
# can justify using average shot distance for forward

linear5 = lm(formula = Tackles ~ Position, Subset1)
# can justify using tackles won only for defense because clearly something special about being part of a defender as for the other its significant that they tackle less than defenders

linear6 = lm(formula = Tackles.Won ~ Position, Subset1)
# can justify using tackles won only for defense because clearly something special about being part of a defender as for the other its significant that they win less tackles than defenders

linear7 = lm(formula = Blocks ~ Position, Subset1)
# can justify using blocks only for defense because clearly something special about being part of a defender as for the other its significant that they win less blocks than defenders

linear8 = lm(formula = Interceptions ~ Position, Subset1)
# can justify using interceptions only for defense because clearly something special about being part of a defender as for the other its significant that they make less interceptions than defenders

linear9= lm(formula = Passes.Completed ~ relevel(Position, ref = 'MF'), Subset1)
# use for both MF and DF

linear10 = lm(formula = Passes.Attempted ~ relevel(Position, ref = 'MF'), Subset1)
# Use for both MF and DF

linear11 = lm(formula = Touches ~ relevel(Position, ref = 'MF'), Subset1)
# Use for both MF and DF

summary(linear1)
summary(linear2)
summary(linear3)
summary(linear4)
summary(linear5)
summary(linear6)
summary(linear7)
summary(linear8)
summary(linear9)
summary(linear10)
summary(linear11)

# SAY IN presentation there are soccer players that play many positions so in order to simplify 
# the data for analysis I took each players main position which is the first position listed in the dataset
# to make sure it was first in dataset I checked multiple random players and found their main position was the first listed

Subset1_Scale1 = as.data.frame(Subset1_Scale)

WC2022_2 = na.omit(WC2022)

Subset1_Scale1 = cbind(WC2022_2$Player,WC2022_2$Position,Subset1_Scale1)

names(Subset1_Scale1)[1] = "Players"
names(Subset1_Scale1)[2] = "Position"

Subset1_Scale1 %>% 
  dplyr::filter(Position == 'FW') -> ALL_FW

ALL_FW$BestFW = ALL_FW$Goals + ALL_FW$Shots + ALL_FW$Shots.on.Target - ALL_FW$Average.Shot.Distance

max(ALL_FW$BestFW)

ALL_FW %>% group_by(Players) %>% arrange(-BestFW) %>% head(5) %>% select(Players)

Subset1_Scale1 %>% 
  dplyr::filter(Position == 'MF') -> ALL_MF

ALL_MF$BestMF = ALL_MF$Passes.Completed + ALL_MF$Passes.Attempted + ALL_MF$Touches

ALL_MF %>% group_by(Players) %>% arrange(-BestMF) %>% head(5) %>% select(Players)

Subset1_Scale1 %>% 
  dplyr::filter(Position == 'DF') -> ALL_DF

ALL_DF$BestDF = ALL_DF$Tackles + ALL_DF$Tackles.Won + ALL_DF$Blocks + ALL_DF$Interceptions + ALL_DF$Passes.Completed + ALL_DF$Passes.Attempted + ALL_DF$Touches

ALL_DF %>% group_by(Players) %>% arrange(-BestDF) %>% head(5) %>% select(Players)



































# cluster the data make a subset of data including everything but player, position, squad, age, born for each world cup which will tell me which are similar to see which are similar for teams, positions etc (positions is the most important)
# add a rank for each team for both world cups
# do a linear regression comparing rank with subset of each position and compare the stats to see which are signficicant only use signficant stats to find out who the best players are for that position



# for 2022 wc 
# argentina 
# france
# croatia
# morocco
# Netherlands
# england
# brazil
# portugal 
# Japan
# Senegal
# Australia
# Switzerland
# Spain
# USA
# Poland
# South Korea
# Germany
# Ecuador
# cameroon
# uruguay
# tunisia
# mexico
# belgium
# ghana
# saudi arabia
# iran
# costa rica
# denmark
# serbia
# wales
# canada
# qatar

# for 2018 wc
# France
# belgium
# croatia
# brazil
# england
# uruguay
# russia
# mexico
# japan
# sweden
# portugal
# spain
# colombia
# switzerland
# iran
# south korea
# senegal
# argentina
# denmark
# peru
# nigeria
# serbia
# Morocco
# saudi arabia
# iceland
# poland
# tunisia 
# costa rica
# australia
# panama
# germany  
# egypt
    





