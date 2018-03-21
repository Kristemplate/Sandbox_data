model1=lmer(Bias~FB..1..vs.MC.2.+ ConRew + TrialNumber +
              Age+Gender+(1+FB..1..vs.MC.2.|Participant)
            +(1+FB..1..vs.MC.2.|ConRew),simondata,REML=FALSE)

nullmodel1=lmer(Bias~ ConRew +
                  Age+Gender+(FB..1..vs.MC.2.|Participant)
                +(FB..1..vs.MC.2.|ConRew),simondata,REML=FALSE)

coef(model1)

anova(nullmodel1,model1)
summary(model1)


mean(simondata$Bias[simondata$TrialNumber == 1])
mean(simondata$Bias[simondata$TrialNumber == 2])
mean(simondata$Bias[simondata$TrialNumber == 3])
mean(simondata$Bias[simondata$TrialNumber == 4])
mean(simondata$Bias[simondata$TrialNumber == 5])
mean(simondata$Bias[simondata$TrialNumber == 6])
mean(simondata$Bias[simondata$TrialNumber == 7])
mean(simondata$Bias[simondata$TrialNumber == 8])
mean(simondata$Bias[simondata$TrialNumber == 9])
mean(simondata$Bias[simondata$TrialNumber == 10])
mean(simondata$Bias[simondata$TrialNumber == 11])
mean(simondata$Bias[simondata$TrialNumber == 12])
mean(simondata$Bias[simondata$TrialNumber == 13])
mean(simondata$Bias[simondata$TrialNumber == 14])
mean(simondata$Bias[simondata$TrialNumber == 15])
mean(simondata$Bias[simondata$TrialNumber == 16])
library(plyr)

aaatable <- ddply(simondata, c("TrialNumber","FBMem","LongShort"),summarise,
                   bias = mean(Bias)
)

boxplot(Bias ~ FBMem,
        col=c("white","lightgray"),simondata)


library(lmerTest)

model1=lmer(logBiasPlus~FB..1..vs.MC.2.+ ConRew +
              Age+Gender+(1+FB..1..vs.MC.2.|Participant)
            +(1+FB..1..vs.MC.2.|ConRew),simondata,REML=FALSE)

nullmodel1=lmer(logBiasPlus~ ConRew +
                  Age+Gender+(FB..1..vs.MC.2.|Participant)
                +(FB..1..vs.MC.2.|ConRew),simondata,REML=FALSE)

coef(model1)

anova(nullmodel1,model1)
summary(model1)
