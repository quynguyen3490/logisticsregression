library(tidyverse)
library(lubridate)

library(caTools)
library(ROCR) 

df1 <- as.data.frame(idle_analysis)



df1[is.na(df1)] <- 0
df1[is.null(df1)] <- 0

df1$RGS1_DATE <- mdy(df1$RGS1_DATE)
df1[,1:4] <- as.factor(df1[,1:4])
str(df1)

df2 <- df1 %>% 
  filter(RGS1_DATE >= mdy("7/1/2022") & RGS1_DATE < mdy("8/1/2022") & DRR_GROUP == "STS_UP") %>% 
  select(MDN,DRR_GROUP, TU_2ND_VALUE, DATA_USAGE,
         VOICE_OFFNET_USAGE, VOICE_INC_USAGE, SMS_INC_USAGE, SMS_OFFNET_USAGE,
         NET_ALL, IS_S1)
df2[df2=='NULL'] <- 0

#linear regression net rev
net_rev_model <- lm(NET_ALL ~ DATA_USAGE + VOICE_OFFNET_USAGE, data=df2)
summary(net_rev_model)
set.seed(1234)

ind <- sample(2, nrow(df2), replace = T, prob = c(0.8, 0.2))
train <- df2[ind==1,]
test <- df2[ind==2,]

test

#IS_S1 is label, remain column is dimension, family is binomial because IS_S1 only return 0 or 1
mymodel <- glm(IS_S1 ~ TU_2ND_VALUE+DATA_USAGE+VOICE_OFFNET_USAGE+
                 VOICE_INC_USAGE+SMS_INC_USAGE+SMS_INC_USAGE+SMS_OFFNET_USAGE+NET_ALL,
               data=train, family = 'binomial')
summary(mymodel)

p1 <- predict(mymodel, train, type="response")
pred1 <- ifelse(p1>0.5,1,0)
tab1 <- table(Predicted = pred1, Actual = train$IS_S1)
tab1
1 - sum(diag(tab1))/sum(tab1)


p2 <- predict(mymodel, df4, type = 'response')
pred2 <- ifelse(p2>0.5, 1, 0)
tab2 <- table(Predicted = pred2, Actual = test$IS_S1)
tab2
diag(tab2)
sum(diag(tab2))/sum(tab2)

df3 <- cbind(test, p2)

df4 <- cbind(df4,p2)

ggplot(df3, aes(x=VOICE_OFFNET_USAGE, y=p2, color=IS_S1)) +
  geom_point() +
  xlim(10,100) +
  stat_smooth(method="glm", color="green", se=FALSE,
              method.args = list(family=binomial))
