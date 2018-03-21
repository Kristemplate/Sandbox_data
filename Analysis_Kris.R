setwd("C:/Users/ct886924/Documents/STUDIES/Study1_MT/Simon_data")
simondata=read.csv("MergedRdata2.csv")

AQ_means = mean(simondata$AQ)


# Outlier removal (is this justified?)

m_bias = mean(simondata$Bias)
sd_bias = sd(simondata$Bias)

hi = m_bias + 3*sd_bias
lo = m_bias - 3*sd_bias

simondata = simondata[simondata$Bias < hi & simondata$Bias > lo,]



# Outlier removal for FB and MC separately

m_bias_FB = mean(simondata$Bias[simondata$FBMem == "FB"])
sd_bias_FB = sd(simondata$Bias[simondata$FBMem == "FB"])

hi_FB = m_bias_FB + 3 * sd_bias_FB
lo_FB = m_bias_FB - 3 * sd_bias_FB

m_bias_Mem = mean(simondata$Bias[simondata$FBMem == "Mem"])
sd_bias_Mem = sd(simondata$Bias[simondata$FBMem == "Mem"])

hi_Mem = m_bias_Mem + 3 * sd_bias_Mem
lo_Mem = m_bias_Mem - 3 * sd_bias_Mem

# simondata[simondata$FBMem == "FB",] = simondata[simondata$FBMem == "FB"][simondata$Bias > lo_FB]
simondata_FB = simondata[simondata$FBMem == "FB",]


# simondata[simondata$FBMem == "Mem",] = simondata[simondata$FBMem == "Mem",][simondata$Bias > lo_Mem,]
# simondata[simondata$FBMem == "Mem",] = simondata[simondata$FBMem == "Mem",][simondata$Bias < hi_Mem,]
simondata_Mem = simondata[simondata$FBMem == "Mem",]

simondata_FB = simondata_FB[simondata_FB$Bias < hi_FB,]
simondata_Mem = simondata_Mem[simondata_Mem$Bias < hi_Mem,]


simondata_new = rbind(simondata_FB,simondata_Mem)
simondata_backup = simondata
simondata = simondata_new

simondata$isoutlier = rep(NA,length(simondata$TrialNumber))
for (i in 1:length(simondata$isoutlier)){
  if (simondata$FBMem[i] == "FB" & simondata$Bias[i] > hi_FB)
  {
    simondata$isoutlier[i] = 1
  } else if (simondata$FBMem[i] == "Mem" & simondata$Bias[i] > hi_Mem)
  {
    simondata$isoutlier[i] = 1
  } else {simondata$isoutlier[i] = 0}
}


library(lme4)
library(lmerTest)


which(!complete.cases(simondata))

boxplot(Bias ~ FBMem*Reward*LongShort,
        col=c("white","lightgray"),simondata)

boxplot(Bias ~ FBMem,
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
summary(model3)



fixef(model1) ["x"]


model1 <- glmer(z~X + (1|g), family = "poisson", data=simondata)





# New analysis (March-) ####

means = ddply(simondata,c("Participant","FBMem"), summarise,
              bias = mean(Bias))

FB_vec = means$bias[means$FBMem == "FB"]
Mem_vec = means$bias[means$FBMem == "Mem"]
t.test(FB_vec,Mem_vec, paired = T)


# more temp models ####

model4=lmer(Bias~FB..1..vs.MC.2.*ConRew +FB..1..vs.MC.2. +ConRew
            +Age+Gender
            +(1+FB..1..vs.MC.2.|Participant),simondata,REML=FALSE)

summary(model4)

simondata$AQ.mc = rep(NA,length(simondata$AQ))


for (i in 1:length(simondata$AQ)){
  simondata$AQ.mc[i] = simondata$AQ[i] - AQ_means
}

model5=lmer(Bias~FB..1..vs.MC.2.*ConRew*AQ.mc +FB..1..vs.MC.2. +ConRew
            +Age+Gender
            +(1+FB..1..vs.MC.2.|Participant),simondata,REML=FALSE)

model6=lmer(Bias~FB..1..vs.MC.2.*ConRew*TrialNumber +FB..1..vs.MC.2. +ConRew
            +Age+Gender
            +(1+FB..1..vs.MC.2.|Participant),simondata,REML=FALSE)
