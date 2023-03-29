rm(list=ls())
options(warn=-1)
options(scipen = 20)

library(haven)
library(dplyr)


census <- read.csv("C:\\Users\\obay\\OneDrive\\Documents\\R\\ECN 620 Term project\\pumf-98M0001-E-2016-individuals_F1.csv")

print(summary(census))
cond <- census$MTNEn==8 | census$KOL==8 | census$LWAEN>=8 | census$LWAFR>=8 | census$LWBEN>=8 | census$LWBFR>=8 | census$HDGREE>=88 | census$HDGREE<2 | census$NOCS>=88 | census$NAICS>=88 | census$Wages<=10000 | census$Wages>=88888888

census_b <- census[!cond,]

print(summary(census_b))


#cond_1 <- census_a$MTNEn==8 | census_a$KOL==8 | census_a$LWAEN>=8 | census_a$LWAFR>=8 | census_a$LWBEN>=8 | census_a$LWBFR >=8 | census_a$HDGREE >= 88 | census_a$NOCS >= 88 | census_a$NAICS >= 88 | census_a$Wages <= 10000 | census_a$Wages >= 88888888

#census_b <- census_a[!cond_1,]
#print(summary(census_b))
# Language Dummies
census_b$UNIL_ENGLISH <- ifelse((census_b$MTNEn == 1) & (census_b$KOL == 1) & (census_b$LWAEN == 1) & (census_b$LWBFR == 0),1,0)
census_b$BIL_MEFE <- ifelse((census_b$MTNEn == 1) & (census_b$KOL == 3) & (census_b$LWAEN == 1) & (census_b$LWBFR == 0),1,0)
census_b$BIL_MEFF <- ifelse((census_b$MTNEn == 1) & (census_b$KOL == 3) & (census_b$LWAEN == 1) & (census_b$LWBFR == 1),1,0)
census_b$BIL_FRENCH <- ifelse((census_b$MTNEn == 1) & (census_b$KOL == 3) & (census_b$LWAFR == 1) & (census_b$LWBEN == 1),1,0)




# Other Dummies

census_b$lWages <- log(census_b$Wages)
census_b$city <- ifelse((census_b$CMA == 505)|(census_b$CMA == 535),1,0)
census_b$married <- ifelse(census_b$MarStH == 2,1,0)
census_b$high_school <- ifelse(census_b$HDGREE == 2,1,0)
census_b$trade_college <- ifelse((census_b$HDGREE >= 3)&(census_b$HDGREE <= 7),1,0)
census_b$university <- ifelse((census_b$HDGREE == 8)|(census_b$HDGREE == 9),1,0)
census_b$post_graduate <- ifelse((census_b$HDGREE >= 10)&(census_b$HDGREE <= 13),1,0)
census_b$management <- ifelse((census_b$NOCS >= 1)&(census_b$NOCS <=2),1,0)
census_b$professional <- ifelse((census_b$NOCS >= 3)&(census_b$NOCS <=5),1,0)
census_b$semi_professional <- ifelse((census_b$NOCS == 6)|(census_b$NOCS == 7),1,0)
census_b$blue_collar <- ifelse((census_b$NOCS >= 8)&(census_b$NOCS <= 10),1,0)
census_b$public <- ifelse((census_b$NAICS == 3)|(census_b$NAICS == 14)|(census_b$NAICS == 15)|(census_b$NAICS == 19),1,0)
census_b$goods <- ifelse((census_b$NAICS == 1)|(census_b$NAICS == 2)|(census_b$NAICS == 4)|(census_b$NAICS == 5)|(census_b$NAICS == 6)|(census_b$NAICS == 7)|(census_b$NAICS == 8),1,0)
census_b$service <- ifelse((census_b$NAICS == 9)|(census_b$NAICS == 10)|(census_b$NAICS == 11)|(census_b$NAICS == 12)|(census_b$NAICS == 13)|(census_b$NAICS == 16)|(census_b$NAICS == 17)|(census_b$NAICS == 18),1,0)

# splitting the sample into a male and female

temp <- split(census_b,census_b$Sex)
male <- temp[[2]]
female <- temp[[1]]

stats <- rbind(
  unlist(lapply(male,function(x) mean(x))),
  unlist(lapply(male,function(x) sd(x))))
  unlist(lapply(female,function(x) mean(x))),
  unlist(lapply(female,function(x) sd(x)))
)
sum(with(census_b, census_b$BIL_FRENCH==1 & census_b$Sex==2))

tb1_on <- aggregate(Wages ~ Sex + UNIL_ENGLISH + BIL_MEFE + BIL_MEFF + BIL_FRENCH, data=census_b, FUN=mean)

M1 <- lm(lWages ~ city + married + BIL_MEFE + BIL_MEFF + BIL_FRENCH + trade_college + university + post_graduate, data = male)

M2 <- lm(lWages ~ city + married + BIL_MEFE + BIL_MEFF + BIL_FRENCH + trade_college + university + post_graduate + management + semi_professional + professional + goods + service, data = male)

F1 <- lm(lWages ~ city + married + BIL_MEFE + BIL_MEFF + BIL_FRENCH + trade_college + university + post_graduate, data = female)

F2 <- lm(lWages ~ city + married + BIL_MEFE + BIL_MEFF + BIL_FRENCH + trade_college + university + post_graduate + management + semi_professional + professional + goods + service, data = female)

print(summary(F1))
print(summary(F2))
lMale1 <- lm(lWages ~ married + BIL_MEFE + BIL_MEFF + BIL_FRENCH, data = male)
