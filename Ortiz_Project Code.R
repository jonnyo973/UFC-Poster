library(readr)
library(ggplot2)
library(forcats)
library(dplyr)
library(tidyverse)
ufc <- read_csv("C:/Users/16023/Desktop/IST 719/data.csv")
u <- ufc
ufc <- ufc[,c(2,3,9,448,449,451,452,457,893,894)]
ufc <- na.omit(ufc)
par(mfrow = c(1,1))
# Challenger or Lower Rank Fighter
#hist(ufc$B_Age
#     , main = "Challenger's Age"
#     , xlab = "Age"
#     , col = "blue")
# Champiion or Higher Rank Fighter
#hist(ufc$R_Age
#     , main = "Champion's Age"
#     , xlab = "Age"
#    , col = "red")

d <- density(ufc$Last_round)
######################################
#### How and when fights are won  ####
######################################
ggplot(ufc, aes(x = Last_round, fill = winby)) +  geom_bar(data=ufc[which(ufc$winner=="blue"),]) + labs(title="How and When Fight Was Won", x ="Ending Round", y = "Number of Fights", fill = "Win By")


#plot(d, main = "Ending Round"
#     , xlab = "Round"
#     , col = "Red"
#     , border = "Blue")

#polygon(d, main = "Ending Round"
#        , xlab = "Round"
#        , col = "Red"
#        , border = "Blue")

#col.vec[ufc$winner == "red"] <- rgb(255,64,64, maxColorValue = 255)
#col.vec[ufc$winner == "blue"] <- rgb(64,64,255, maxColorValue = 255)
#  plot(ufc$R_Height
#     , ufc$R_Age
#     , pch = 16
#     , cex = 1
     
#     , main = "Champion Height and Age"
#     , xlab = "Height"
#     , ylab = "Age")



#######################################
#### violin plots : Red fights age/height
#######################################
library(ggplot2)
library(dplyr)
weight_classes <- c(56,61,65,70,77,84,91,120)
asd <- ufc[ufc$R_Weight == weight_classes,]
asd$R_Weight[asd$R_Weight == "56"] <- "Flyweight"
asd$R_Weight[asd$R_Weight == "61"] <- "Bantamweight"
asd$R_Weight[asd$R_Weight == "65"] <- "Featherweight"
asd$R_Weight[asd$R_Weight == "70"] <- "Lightweight"
asd$R_Weight[asd$R_Weight == "77"] <- "Welterweight"
asd$R_Weight[asd$R_Weight == "84"] <- "Middleweight"
asd$R_Weight[asd$R_Weight == "91"] <- "Lightheavyweight"
asd$R_Weight[asd$R_Weight == "120"] <- "Heavyweight"
asd$R_Weight <- as.factor(asd$R_Weight, c("Flyweight", "Bantamweight", "Featherweight", "Lightweight", "Welterweight", "Middleweight", "Lightheavyweight", "Heavyweight"))

#asd %>%
#         arrange(R_Weight) %>%
#         mutate(R_Weight = factor(R_Weight, levels=c("Flyweight", "Bantamweight", "Featherweight", "Lightweight", "Welterweight", "Middleweight", "Lightheavyweight", "Heavyweight"))) %>% 
#  ggplot(aes(x = R_Age, y = R_Weight, fill = R_Weight)) + geom_violin()+labs(title="Champions Age by Weight Class", x ="Age", y = "Weight Class", fill = "Weight Class")
#######################################
#### violin plots : Blue fights age/height
#######################################
dsa <- ufc[ufc$B_Weight == weight_classes,]
dsa$B_Weight[dsa$B_Weight == "56"] <- "Flyweight"
dsa$B_Weight[dsa$B_Weight == "61"] <- "Bantamweight"
dsa$B_Weight[dsa$B_Weight == "65"] <- "Featherweight"
dsa$B_Weight[dsa$B_Weight == "70"] <- "Lightweight"
dsa$B_Weight[dsa$B_Weight == "77"] <- "Welterweight"
dsa$B_Weight[dsa$B_Weight == "84"] <- "Middleweight"
dsa$B_Weight[dsa$B_Weight == "91"] <- "Lightheavyweight"
dsa$B_Weight[dsa$B_Weight == "120"] <- "Heavyweight"
dsa$B_Weight <- as.factor(dsa$B_Weight, levels = c("Flyweight", "Bantamweight", "Featherweight", "Lightweight", "Welterweight", "Middleweight", "Lightheavyweight", "Heavyweight"))

#dsa %>%
#  arrange(B_Weight) %>%
#  mutate(B_Weight = factor(B_Weight, levels=c("Flyweight", "Bantamweight", "Featherweight", "Lightweight", "Welterweight", "Middleweight", "Lightheavyweight", "Heavyweight"))) %>%
#ggplot( aes(x = B_Age, y = B_Weight, fill = B_Weight)) + geom_violin() +labs(title="Challengers Age by Weight Class",
#                                                                             x ="Age", y = "Weight Class", fill = "Weight Class")
#######################################
### Combined data for fighters ########
#######################################
ageweight <- data.frame(a= c(asd$R_Weight,dsa$B_Weight), b = c(asd$R_Age,dsa$B_Age))
ageweight$a <- as.factor(ageweight$a)
ageweight$b <- as.numeric(ageweight$b)
ageweight <- as.data.frame(ageweight)
colnames(ageweight) <- c("weight_class", "age")
aw <- tapply(ageweight$age, list(ageweight$weight_class), mean)
ageweight %>%
  arrange(weight_class) %>%
  mutate(weight_class = factor(weight_class, levels=c("Flyweight", "Bantamweight", "Featherweight", "Lightweight", "Welterweight", "Middleweight", "Lightheavyweight", "Heavyweight"))) %>%
ggplot( aes(x = age, y = weight_class, fill = weight_class)) + geom_violin() +labs(title="Ages in  Weight Classes",
                                                                 x ="Age", y = "Weight Class", fill = "Weight Class")

#######################################
#### Tree Map of how fights win
#######################################
library(treemap)
winby <- tapply(ufc$winner, list(ufc$winby), length)
winner <- tapply(ufc$winner, list(ufc$winner), length)
winby <- as.data.frame(winby)
colnames(winby) <- "How are people winning?"
df <- cbind(Winby = rownames(winby), winby)
df

treemap(df, index = "Winby", vSize = "How are people winning?")
sum(winner$winner)
2243/925
1300/2243

winner <- as.data.frame(winner)
df <- cbind(Winner = rownames(winner), winner, rownames(winby),winby)
df$color <- c("blue", "yellow", "red")
treemap(df, index = "Winner", vSize = "winner")
##############################################
#### The greatest fighters of all time    ####
##############################################
par(mar = c(9,5,5,5))
top10 <- tapply(ufc$winner, list(ufc$R_Name), length)
top10 <- as.data.frame(top10)
top <- top10[order(top10$top10),]
top <- as.data.frame(top)
top10 <- top[833:842]
a <- c(14,12,11,10,10,10,9,9,9,9)
length(a)
barplot(a
     , col = "red"
     , names.arg = c("Donald Cerrone", "Ovince Saint Preux", "Derrick Lewis", "Dustin Poirier", "Neil Magny", "Ross Pearson", "Demetrious Johnson", "Demian Maia", "Francisco Trinaldo", "Jacare Souza")
     , main = "Red Corner Winners"
     , ylab = "Wins"
     , las = 2)
top10$Var1
DJ <- u$R_Name[u$R_Name == "Demetrious Johnson"]
DJ <- subset(u, u$R_Name == "Demetrious Johnson")
rb1 <- data.frame(DJ$B__Round1_Grappling_Takedowns_Landed, DJ$`B__Round1_Strikes_Total Strikes_Landed`, DJ$`B__Round1_TIP_Control Time`, DJ$`B__Round1_TIP_Ground Control Time`)
rr1 <- data.frame(DJ$R__Round1_Grappling_Takedowns_Landed, DJ$`R__Round1_Strikes_Total Strikes_Landed`, DJ$`R__Round1_TIP_Control Time`, DJ$`R__Round1_TIP_Ground Control Time`)
rb2 <- data.frame(DJ$B__Round2_Grappling_Takedowns_Landed, DJ$`B__Round2_Strikes_Total Strikes_Landed`, DJ$`B__Round2_TIP_Control Time`, DJ$`B__Round2_TIP_Ground Control Time`)
rr2 <- data.frame(DJ$R__Round2_Grappling_Takedowns_Landed, DJ$`R__Round2_Strikes_Total Strikes_Landed`, DJ$`R__Round2_TIP_Control Time`, DJ$`R__Round2_TIP_Ground Control Time`)
rb3 <- data.frame(DJ$B__Round3_Grappling_Takedowns_Landed, DJ$`B__Round3_Strikes_Total Strikes_Landed`, DJ$`B__Round3_TIP_Control Time`, DJ$`B__Round3_TIP_Ground Control Time`)
rr3 <- data.frame(DJ$R__Round3_Grappling_Takedowns_Landed, DJ$`R__Round3_Strikes_Total Strikes_Landed`, DJ$`R__Round3_TIP_Control Time`, DJ$`R__Round3_TIP_Ground Control Time`)
a <- data.frame(colMeans(na.omit(rb1)),colMeans(na.omit(rr1)))
b <- data.frame(colMeans(na.omit(rb2)),colMeans(na.omit(rr2)))
c <- data.frame(colMeans(na.omit(rb3)),colMeans(na.omit(rr3)))
colnames(a) <- c("Blue Corner","Red Corner")
colnames(b) <- c("Blue Corner","Red Corner")
colnames(c) <- c("Blue Corner","Red Corner")
DF <- rbind(a,b,c)
rownames(DF) <- c("1TD","1ST","1CT","1GCT","2TD","2ST","2CT","2GCT","3TD","3ST","3CT","3GCT")
DF
rounds <- c("round 1","round 1","round 1","round 1","round 2","round 2","round 2","round 2","round 3","round 3","round 3","round 3")
rounds <- data.frame(rounds)
rn <- as.data.frame(rownames(DF))
rn
DF <- as.data.frame(c(DF,rn,rounds))
DF
colnames(DF) <- (c("Blue Corner","Red Corner","Skills Displayed", "Round"))
DF
(DF$`Red Corner`/DF$`Blue Corner`)
library(alluvial)
alluvial(dj[,c(1,3)], freq = dj$Freq
         , col = dj$corner
)

winby <- tapply(ufc$winner, list(ufc$winby), length)
winner <- tapply(ufc$winner, list(ufcJ$winner), length)
winby <- as.data.frame(winby)
colnames(winby) <- "How is the Average Fighter Winning?"
df <- cbind(Winby = rownames(winby), winby)
df
treemap(df, index = "Winby", vSize = "How is the Average Fighter Winning?")

winby <- tapply(DJ$winner, list(DJ$winby), length)
winner <- tapply(DJ$winner, list(DJ$winner), length)
winby <- as.data.frame(winby)
colnames(winby) <- "How is Mighty Mouse winning?"
df <- cbind(Winby = rownames(winby), winby)
df

treemap(df, index = "Winby", vSize = "How is Mighty Mouse winning?")

#######################################################################
# Build Dataset                                                       #
#### Rounds by strikes leading to winner for river map or alluvial    #
#######################################################################
rb1 <- data.frame(u$B__Round1_Grappling_Takedowns_Landed, u$`B__Round1_Strikes_Total Strikes_Landed`, u$`B__Round1_TIP_Control Time`, u$`B__Round1_TIP_Ground Control Time`)
rr1 <- data.frame(u$R__Round1_Grappling_Takedowns_Landed, u$`R__Round1_Strikes_Total Strikes_Landed`, u$`R__Round1_TIP_Control Time`, u$`R__Round1_TIP_Ground Control Time`)
rb2 <- data.frame(u$B__Round2_Grappling_Takedowns_Landed, u$`B__Round2_Strikes_Total Strikes_Landed`, u$`B__Round2_TIP_Control Time`, u$`B__Round2_TIP_Ground Control Time`)
rr2 <- data.frame(u$R__Round2_Grappling_Takedowns_Landed, u$`R__Round2_Strikes_Total Strikes_Landed`, u$`R__Round2_TIP_Control Time`, u$`R__Round2_TIP_Ground Control Time`)
rb3 <- data.frame(u$B__Round3_Grappling_Takedowns_Landed, u$`B__Round3_Strikes_Total Strikes_Landed`, u$`B__Round3_TIP_Control Time`, u$`B__Round3_TIP_Ground Control Time`)
rr3 <- data.frame(u$R__Round3_Grappling_Takedowns_Landed, u$`R__Round3_Strikes_Total Strikes_Landed`, u$`R__Round3_TIP_Control Time`, u$`R__Round3_TIP_Ground Control Time`)
rb4 <- data.frame(u$B__Round4_Grappling_Takedowns_Landed, u$`B__Round4_Strikes_Total Strikes_Landed`, u$`B__Round4_TIP_Control Time`, u$`B__Round4_TIP_Ground Control Time`)
rr4 <- data.frame(u$R__Round4_Grappling_Takedowns_Landed, u$`R__Round4_Strikes_Total Strikes_Landed`, u$`R__Round4_TIP_Control Time`, u$`R__Round4_TIP_Ground Control Time`)
rb5 <- data.frame(u$B__Round5_Grappling_Takedowns_Landed, u$`B__Round5_Strikes_Total Strikes_Landed`, u$`B__Round5_TIP_Control Time`, u$`B__Round5_TIP_Ground Control Time`)
rr5 <- data.frame(u$R__Round5_Grappling_Takedowns_Landed, u$`R__Round5_Strikes_Total Strikes_Landed`, u$`R__Round5_TIP_Control Time`, u$`R__Round5_TIP_Ground Control Time`)




a <- data.frame(colMeans(na.omit(rb1)),colMeans(na.omit(rr1)))
b <- data.frame(colMeans(na.omit(rb2)),colMeans(na.omit(rr2)))
c <- data.frame(colMeans(na.omit(rb3)),colMeans(na.omit(rr3)))
d <- data.frame(colMeans(na.omit(rb4)),colMeans(na.omit(rr4)))
e <- data.frame(colMeans(na.omit(rb5)),colMeans(na.omit(rr5)))
colnames(a) <- c("Blue Corner","Red Corner")
colnames(b) <- c("Blue Corner","Red Corner")
colnames(c) <- c("Blue Corner","Red Corner")
colnames(d) <- c("Blue Corner","Red Corner")
colnames(e) <- c("Blue Corner","Red Corner")
DF <- rbind(a,b,c,d,e)
rownames(DF) <- c("1TD","1ST","1CT","1GCT","2TD","2ST","2CT","2GCT","3TD","3ST","3CT","3GCT","4TD","4ST","4CT","4GCT","5TD","5ST","5CT","5GCT")
DF
rounds <- c("round 1","round 1","round 1","round 1","round 2","round 2","round 2","round 2","round 3","round 3","round 3","round 3","round 4","round 4","round 4","round 4","round 5","round 5","round 5","round 5")
rounds <- data.frame(rounds)
rn <- as.data.frame(rownames(DF))
rn
DF <- as.data.frame(c(DF,rn,rounds))
DF
colnames(DF) <- (c("Blue Corner","Red Corner","Skills Displayed", "Round"))
DF
(DF$`Red Corner`/DF$`Blue Corner`)

library(alluvial)
alluvial(FighterAluviall[,c(1,3)], freq = FighterAluviall$Freq
         , col = FighterAluviall$corner
         )
#########################################
# Stacked Bar Charts
#########################################
AVGF <- read_csv("C:/Users/16023/Desktop/IST 719/UFC.csv")
DJ <- read_csv("C:/Users/16023/Desktop/IST 719/dj.csv")
ggplot(DJ, aes(x= DJ[,c(2:4)], y= DJ$`Round 5`), geom_bar(position="fill", stat="identity"))

rownames(DJ) <- DJ$Skills
DJ <- DJ[,c(2:6)]
DJ <- as.matrix(DJ)
barplot(DJ
        , col = c("firebrick4","indianred4","red","indianred1")
        , border="white" 
        , space=0.04
        , xlab = "Round"
        , main = "Demetrious Johnson Skills Each Round")
abline(h=4, col="black")
legend("topleft", c("Ground Control Time", "Control Time", "Strikes", "Takedowns"),
       col=c("indianred1","red","indianred4", "firebrick4"), lty=1, cex=.7)

rownames(AVGF) <- AVGF$Skills
AVGF <- AVGF[,c(2:6)]
AVGF <- as.matrix(AVGF)
barplot(AVGF
        , col = c("navy","blue","lightskyblue", "lightskyblue1")
        , border="white" 
        , space=0.04
        , xlab = "Round"
        , main = "Average Winner Skills Each Round")
abline(h=4, col="black")
legend("topleft", c("Ground Control Time", "Control Time", "Strikes", "Takedowns"),
       col=c("lightskyblue1","lightskyblue","blue", "navy"), lty=1, cex=.7)

ggplot(DJ, aes(x = Last_round, fill = winby)) +  geom_bar(data=DJ[which(DJ$winner=="red"),]) + labs(title="How and When Mighty Mouse Fights Were Won", x ="Ending Round", y = "Number of Fights", fill = "Win By")

