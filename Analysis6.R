setwd("/Users/Simon/Documents/Rdata")
simondata=read.csv("MergedRdata2.csv")

View(simondata)

which(!complete.cases(simondata))

boxplot(Bias ~ FBMem*Reward*LongShort,
        col=c("white","lightgray"),simondata)

model1=lmer(Bias~FB..1..vs.MC.2.+ ConRew +
                   Age+Gender+(1+FB..1..vs.MC.2.|Participant)
                 +(1+FB..1..vs.MC.2.|ConRew),simondata,REML=FALSE)

nullmodel1=lmer(Bias~ ConRew +
                  Age+Gender+(FB..1..vs.MC.2.|Participant)
                +(FB..1..vs.MC.2.|ConRew),simondata,REML=FALSE)

coef(model1)

anova(nullmodel1,model1)


model2=lmer(Bias~ConRew+FB..1..vs.MC.2.+Age+Gender+
                   (1+ConRew|Participant)+(1+ConRew|FB..1..vs.MC.2.),
                 simondata,REML=FALSE)

nullmodel2= lmer(Bias~FB..1..vs.MC.2.+Age+Gender+
                   (ConRew|Participant)+(ConRew|FB..1..vs.MC.2.),
                 simondata,REML=FALSE)

coef(model2)

anova(nullmodel2,model2)


model3=lmer(Bias~FB..1..vs.MC.2.*ConRew +FB..1..vs.MC.2. +ConRew
                         +Age+Gender+(1+FB..1..vs.MC.2.|ConRew)
                         +(1+FB..1..vs.MC.2.|Participant),simondata,REML=FALSE)

nullmodel3=lmer(Bias~FB..1..vs.MC.2. +ConRew
                +Age+Gender+(FB..1..vs.MC.2.|ConRew)
                +(FB..1..vs.MC.2.|Participant),simondata,REML=FALSE)

anova(nullmodel3,model3)

library(simr)



fixef(model1) ["x"]


model1 <- glmer(z~X + (1|g), family = "poisson", data=simondata)
