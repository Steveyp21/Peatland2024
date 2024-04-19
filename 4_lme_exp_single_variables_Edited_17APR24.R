

#install.packages("nlme")
library(nlme)
library(tidyverse)

#load("C:/LocalData/sp991/GEOM184/CodeForMSc/Final/Inputs/Points.RData")

#Points<-Points%>%
#  mutate(Peat_DepthL = log(Peat_Depth+1),
#         NDVI_Win = ((B5_S2_Win-B4_S2_Win)/(B5_S2_Win+B4_S2_Win)),
#         NDVI_Sum = ((B5_S2_Sum-B4_S2_Sum)/(B5_S2_Sum+B4_S2_Sum)))


#Linear mixed model with constant fixed effects -> smallest AIC
#Linear mixed model with constant fixed effects -> smallest AIC
DSM.model <- lme(fixed = Peat_DepthL ~ Bodmin_DSM_clipped, data = Points, random = ~ 1 | dummy, method = "ML") 
slope.model <- lme(fixed = Peat_DepthL ~ slope, data = Points, random = ~ 1 | dummy, method = "ML") 
aspect.model <- lme(fixed = Peat_DepthL ~ aspect, data = Points, random = ~ 1 | dummy, method = "ML") 
SinAspect.model <- lme(fixed = Peat_DepthL ~ SinAspect, data = Points, random = ~ 1 | dummy, method = "ML") 
CosAspect.model <- lme(fixed = Peat_DepthL ~ CosAspect, data = Points, random = ~ 1 | dummy, method = "ML") 
roughness.model <- lme(fixed = Peat_DepthL ~ roughness, data = Points, random = ~ 1 | dummy, method = "ML") 
TPI.model <- lme(fixed = Peat_DepthL ~ TPI, data = Points, random = ~ 1 | dummy, method = "ML") 
TRI.model <- lme(fixed = Peat_DepthL ~ TRI, data = Points, random = ~ 1 | dummy, method = "ML") 
TWI.model <- lme(fixed = Peat_DepthL ~ Bodmin_TWI_Final, data = Points, random = ~ 1 | dummy, method = "ML") 
#VV_p95.model <- lme(fixed = Peat_DepthL ~ VV_p95, data = Points, random = ~ 1 | dummy, method = "ML") 
#VV_p5.model  <- lme(fixed = Peat_DepthL ~ VV_p5, data = Points, random = ~ 1 | dummy, method = "ML") 
#VH_p95.model  <- lme(fixed = Peat_DepthL ~ VH_p95, data = Points, random = ~ 1 | dummy, method = "ML") 
#VH_p5.model  <- lme(fixed = Peat_DepthL ~ VH_p5, data = Points, random = ~ 1 | dummy, method = "ML") 
S1.model  <- lme(fixed = Peat_DepthL ~ Bodmin_S1_clipped, data = Points, random = ~ 1 | dummy, method = "ML") 
B1_S2_Sum.model  <- lme(fixed = Peat_DepthL ~ Bodmin_S2_Sum_clipped_1, data = Points, random = ~ 1 | dummy, method = "ML") 
B2_S2_Sum.model  <- lme(fixed = Peat_DepthL ~ Bodmin_S2_Sum_clipped_2, data = Points, random = ~ 1 | dummy, method = "ML") 
B3_S2_Sum.model  <- lme(fixed = Peat_DepthL ~ Bodmin_S2_Sum_clipped_3, data = Points, random = ~ 1 | dummy, method = "ML") 
B4_S2_Sum.model  <- lme(fixed = Peat_DepthL ~ Bodmin_S2_Sum_clipped_4, data = Points, random = ~ 1 | dummy, method = "ML") 
B5_S2_Sum.model  <- lme(fixed = Peat_DepthL ~ Bodmin_S2_Sum_clipped_5, data = Points, random = ~ 1 | dummy, method = "ML") 
B6_S2_Sum.model <- lme(fixed = Peat_DepthL ~ Bodmin_S2_Sum_clipped_6, data = Points, random = ~ 1 | dummy, method = "ML") 
B7_S2_Sum.model  <- lme(fixed = Peat_DepthL ~ Bodmin_S2_Sum_clipped_7, data = Points, random = ~ 1 | dummy, method = "ML") 
B8_S2_Sum.model  <- lme(fixed = Peat_DepthL ~ Bodmin_S2_Sum_clipped_8, data = Points, random = ~ 1 | dummy, method = "ML") 
B8A_S2_Sum.model  <- lme(fixed = Peat_DepthL ~ Bodmin_S2_Sum_clipped_9, data = Points, random = ~ 1 | dummy, method = "ML") 
B9_S2_Sum.model  <- lme(fixed = Peat_DepthL ~ Bodmin_S2_Sum_clipped_10, data = Points, random = ~ 1 | dummy, method = "ML") 
#B10_S2_Sum.model  <- lme(fixed = Peat_DepthL ~ B10_S2_Sum, data = Points, random = ~ 1 | dummy, method = "ML") 
B11_S2_Sum.model  <- lme(fixed = Peat_DepthL ~ Bodmin_S2_Sum_clipped_11, data = Points, random = ~ 1 | dummy, method = "ML") 
B12_S2_Sum.model  <- lme(fixed = Peat_DepthL ~ Bodmin_S2_Sum_clipped_12, data = Points, random = ~ 1 | dummy, method = "ML") 
B1_S2_Win.model  <- lme(fixed = Peat_DepthL ~ Bodmin_S2_Win_clipped_1, data = Points, random = ~ 1 | dummy, method = "ML") 
B2_S2_Win.model  <- lme(fixed = Peat_DepthL ~ Bodmin_S2_Win_clipped_2, data = Points, random = ~ 1 | dummy, method = "ML") 
B3_S2_Win.model <- lme(fixed = Peat_DepthL ~ Bodmin_S2_Win_clipped_3, data = Points, random = ~ 1 | dummy, method = "ML") 
B4_S2_Win.model  <- lme(fixed = Peat_DepthL ~ Bodmin_S2_Win_clipped_4, data = Points, random = ~ 1 | dummy, method = "ML") 
B5_S2_Win.model  <- lme(fixed = Peat_DepthL ~ Bodmin_S2_Win_clipped_5, data = Points, random = ~ 1 | dummy, method = "ML") 
B6_S2_Win.model  <- lme(fixed = Peat_DepthL ~ Bodmin_S2_Win_clipped_6, data = Points, random = ~ 1 | dummy, method = "ML") 
B7_S2_Win.model  <- lme(fixed = Peat_DepthL ~ Bodmin_S2_Win_clipped_7, data = Points, random = ~ 1 | dummy, method = "ML") 
B8_S2_Win.model  <- lme(fixed = Peat_DepthL ~ Bodmin_S2_Win_clipped_8, data = Points, random = ~ 1 | dummy, method = "ML") 
B8A_S2_Win.model  <- lme(fixed = Peat_DepthL ~ Bodmin_S2_Win_clipped_9, data = Points, random = ~ 1 | dummy, method = "ML") 
B9_S2_Win.model  <- lme(fixed = Peat_DepthL ~ Bodmin_S2_Win_clipped_10, data = Points, random = ~ 1 | dummy, method = "ML") 
#B10_S2_Win.model  <- lme(fixed = Peat_DepthL ~ B10_S2_Win, data = Points, random = ~ 1 | dummy, method = "ML") 
B11_S2_Win.model  <- lme(fixed = Peat_DepthL ~ Bodmin_S2_Win_clipped_11, data = Points, random = ~ 1 | dummy, method = "ML") 
B12_S2_Win.model  <- lme(fixed = Peat_DepthL ~ Bodmin_S2_Win_clipped_12, data = Points, random = ~ 1 | dummy, method = "ML") 
NDVI_Win.model <- lme(fixed = Peat_DepthL ~ NDVI_Win_df, data = Points, random = ~ 1 | dummy, method = "ML") 
NDVI_Sum.model <- lme(fixed = Peat_DepthL ~ NDVI_Sum_df, data = Points, random = ~ 1 | dummy, method = "ML") 
Dose.model <- lme(fixed = Peat_DepthL ~ Dose, data = Points, random = ~ 1 | dummy, method = "ML") 
TotCount.model <- lme(fixed = Peat_DepthL ~ TotCount, data = Points, random = ~ 1 | dummy, method = "ML") 
K.model <- lme(fixed = Peat_DepthL ~ K, data = Points, random = ~ 1 | dummy, method = "ML") 
Ur.model <- lme(fixed = Peat_DepthL ~ Ur, data = Points, random = ~ 1 | dummy, method = "ML") 
Th.model <- lme(fixed = Peat_DepthL ~ Th, data = Points, random = ~ 1 | dummy, method = "ML") 

AICTable <- data.frame(matrix(nrow = 0, ncol = 5))
colnames(AICTable) <- c("Fixed", "AIC", "RMSE", "r2", "p-value")

a<-summary(DSM.model)
AICTable[1,2] <- a$AIC
AICTable[1,1] <- as.character(a$terms[[3]])
AICTable[1,3] <- exp(sqrt(mean((a$data$Peat_DepthL - a$fitted[,1])^2)))-1
b<-MuMIn::r.squaredGLMM(DSM.model)
AICTable[1,4] <- b[1]
c<- anova(DSM.model)
AICTable[1,5] <-c$`p-value`[2]

a<-summary(slope.model)
AICTable[2,2] <- a$AIC
AICTable[2,1] <- as.character(a$terms[[3]])
AICTable[2,3] <- exp(sqrt(mean((a$data$Peat_DepthL - a$fitted[,1])^2)))-1
b<-MuMIn::r.squaredGLMM(slope.model)
AICTable[2,4] <- b[1]
c<- anova(slope.model)
AICTable[2,5] <-c$`p-value`[2]

a<-summary(aspect.model)
AICTable[3,2] <- a$AIC
AICTable[3,1] <- as.character(a$terms[[3]])
AICTable[3,3] <- exp(sqrt(mean((a$data$Peat_DepthL - a$fitted[,1])^2)))-1
b<-MuMIn::r.squaredGLMM(aspect.model)
AICTable[3,4] <- b[1]
c<- anova(aspect.model)
AICTable[3,5] <-c$`p-value`[2]

a<-summary(SinAspect.model)
AICTable[4,2] <- a$AIC
AICTable[4,1] <- as.character(a$terms[[3]])
AICTable[4,3] <- exp(sqrt(mean((a$data$Peat_DepthL - a$fitted[,1])^2)))-1
b<-MuMIn::r.squaredGLMM(SinAspect.model)
AICTable[4,4] <- b[1]
c<- anova(SinAspect.model)
AICTable[4,5] <-c$`p-value`[2]

a<-summary(CosAspect.model)
AICTable[5,2] <- a$AIC
AICTable[5,1] <- as.character(a$terms[[3]])
AICTable[5,3] <- exp(sqrt(mean((a$data$Peat_DepthL - a$fitted[,1])^2)))-1
b<-MuMIn::r.squaredGLMM(CosAspect.model)
AICTable[5,4] <- b[1]
c<- anova(CosAspect.model)
AICTable[5,5] <-c$`p-value`[2]

a<-summary(roughness.model)
AICTable[6,2] <- a$AIC
AICTable[6,1] <- as.character(a$terms[[3]])
AICTable[6,3] <- exp(sqrt(mean((a$data$Peat_DepthL - a$fitted[,1])^2)))-1
b<-MuMIn::r.squaredGLMM(roughness.model)
AICTable[6,4] <- b[1]
c<- anova(roughness.model)
AICTable[6,5] <-c$`p-value`[2]

a<-summary(TPI.model)
AICTable[7,2] <- a$AIC
AICTable[7,1] <- as.character(a$terms[[3]])
AICTable[7,3] <- exp(sqrt(mean((a$data$Peat_DepthL - a$fitted[,1])^2)))-1
b<-MuMIn::r.squaredGLMM(TPI.model)
AICTable[7,4] <- b[1]
c<- anova(TPI.model)
AICTable[7,5] <-c$`p-value`[2]

a<-summary(TRI.model)
AICTable[8,2] <- a$AIC
AICTable[8,1] <- as.character(a$terms[[3]])
AICTable[8,3] <- exp(sqrt(mean((a$data$Peat_DepthL - a$fitted[,1])^2)))-1
b<-MuMIn::r.squaredGLMM(TRI.model)
AICTable[8,4] <- b[1]
c<- anova(TRI.model)
AICTable[8,5] <-c$`p-value`[2]

a<-summary(TWI.model)
AICTable[9,2] <- a$AIC
AICTable[9,1] <- as.character(a$terms[[3]])
AICTable[9,3] <- exp(sqrt(mean((a$data$Peat_DepthL - a$fitted[,1])^2)))-1
b<-MuMIn::r.squaredGLMM(TWI.model)
AICTable[9,4] <- b[1]
c<- anova(TWI.model)
AICTable[9,5] <-c$`p-value`[2]

#a<-summary(VV_p95.model)
#AICTable[10,2] <- a$AIC
#AICTable[10,1] <- as.character(a$terms[[3]])
#AICTable[10,3] <- exp(sqrt(mean((a$data$Peat_DepthL - a$fitted[,1])^2)))-1
##b<-MuMIn::r.squaredGLMM(VV_p95.model)
#AICTable[10,4] <- b[1]
#c<- anova(VV_p95.model)
#AICTable[10,5] <-c$`p-value`[2]

#a<-summary(VV_p5.model)
#AICTable[11,2] <- a$AIC
#AICTable[11,1] <- as.character(a$terms[[3]])
#AICTable[11,3] <- exp(sqrt(mean((a$data$Peat_DepthL - a$fitted[,1])^2)))-1
#b<-MuMIn::r.squaredGLMM(VV_p5.model)
#AICTable[11,4] <- b[1]
#c<- anova(VV_p5.model)
#AICTable[11,5] <-c$`p-value`[2]

#a<-summary(VH_p95.model)
#AICTable[12,2] <- a$AIC
#AICTable[12,1] <- as.character(a$terms[[3]])
#AICTable[12,3] <- exp(sqrt(mean((a$data$Peat_DepthL - a$fitted[,1])^2)))-1
#b<-MuMIn::r.squaredGLMM(VH_p95.model)
#AICTable[12,4] <- b[1]
#c<- anova(VH_p95.model)
#AICTable[12,5] <-c$`p-value`[2]

#a<-summary(VH_p5.model)
#AICTable[13,2] <- a$AIC
#AICTable[13,1] <- as.character(a$terms[[3]])
#AICTable[13,3] <- exp(sqrt(mean((a$data$Peat_DepthL - a$fitted[,1])^2)))-1
#b<-MuMIn::r.squaredGLMM(VH_p5.model)
#AICTable[13,4] <- b[1]
#c<- anova(VH_p5.model)
#AICTable[13,5] <-c$`p-value`[2]

a<-summary(S1.model)
AICTable[13,2] <- a$AIC
AICTable[13,1] <- as.character(a$terms[[3]])
AICTable[13,3] <- exp(sqrt(mean((a$data$Peat_DepthL - a$fitted[,1])^2)))-1
b<-MuMIn::r.squaredGLMM(S1.model)
AICTable[13,4] <- b[1]
c<- anova(S1.model)
AICTable[13,5] <-c$`p-value`[2]

a<-summary(B1_S2_Sum.model)
AICTable[14,2] <- a$AIC
AICTable[14,1] <- as.character(a$terms[[3]])
AICTable[14,3] <- exp(sqrt(mean((a$data$Peat_DepthL - a$fitted[,1])^2)))-1
b<-MuMIn::r.squaredGLMM(B1_S2_Sum.model)
AICTable[14,4] <- b[1]
c<- anova(B1_S2_Sum.model)
AICTable[14,5] <-c$`p-value`[2]

a<-summary(B2_S2_Sum.model)
AICTable[15,2] <- a$AIC
AICTable[15,1] <- as.character(a$terms[[3]])
AICTable[15,3] <- exp(sqrt(mean((a$data$Peat_DepthL - a$fitted[,1])^2)))-1
b<-MuMIn::r.squaredGLMM(B2_S2_Sum.model)
AICTable[15,4] <- b[1]
c<- anova(B2_S2_Sum.model)
AICTable[15,5] <-c$`p-value`[2]

a<-summary(B3_S2_Sum.model )
AICTable[16,2] <- a$AIC
AICTable[16,1] <- as.character(a$terms[[3]])
AICTable[16,3] <- exp(sqrt(mean((a$data$Peat_DepthL - a$fitted[,1])^2)))-1
b<-MuMIn::r.squaredGLMM(B3_S2_Sum.model)
AICTable[16,4] <- b[1]
c<- anova(B3_S2_Sum.model)
AICTable[16,5] <-c$`p-value`[2]

a<-summary(B4_S2_Sum.model)
AICTable[17,2] <- a$AIC
AICTable[17,1] <- as.character(a$terms[[3]])
AICTable[17,3] <- exp(sqrt(mean((a$data$Peat_DepthL - a$fitted[,1])^2)))-1
b<-MuMIn::r.squaredGLMM(B4_S2_Sum.model)
AICTable[17,4] <- b[1]
c<- anova(B4_S2_Sum.model)
AICTable[17,5] <-c$`p-value`[2]

a<-summary(B5_S2_Sum.model )
AICTable[18,2] <- a$AIC
AICTable[18,1] <- as.character(a$terms[[3]])
AICTable[18,3] <- exp(sqrt(mean((a$data$Peat_DepthL - a$fitted[,1])^2)))-1
b<-MuMIn::r.squaredGLMM(B5_S2_Sum.model)
AICTable[18,4] <- b[1]
c<- anova(B5_S2_Sum.model)
AICTable[18,5] <-c$`p-value`[2]

a<-summary(B6_S2_Sum.model)
AICTable[19,2] <- a$AIC
AICTable[19,1] <- as.character(a$terms[[3]])
AICTable[19,3] <- exp(sqrt(mean((a$data$Peat_DepthL - a$fitted[,1])^2)))-1
b<-MuMIn::r.squaredGLMM(B6_S2_Sum.model)
AICTable[19,4] <- b[1]
c<- anova(B6_S2_Sum.model)
AICTable[19,5] <-c$`p-value`[2]

a<-summary(B7_S2_Sum.model)
AICTable[20,2] <- a$AIC
AICTable[20,1] <- as.character(a$terms[[3]])
AICTable[20,3] <- exp(sqrt(mean((a$data$Peat_DepthL - a$fitted[,1])^2)))-1
b<-MuMIn::r.squaredGLMM(B7_S2_Sum.model)
AICTable[20,4] <- b[1]
c<- anova(B7_S2_Sum.model)
AICTable[20,5] <-c$`p-value`[2]

a<-summary(B8_S2_Sum.model)
AICTable[21,2] <- a$AIC
AICTable[21,1] <- as.character(a$terms[[3]])
AICTable[21,3] <- exp(sqrt(mean((a$data$Peat_DepthL - a$fitted[,1])^2)))-1
b<-MuMIn::r.squaredGLMM(B8_S2_Sum.model)
AICTable[21,4] <- b[1]
c<- anova(B8_S2_Sum.model)
AICTable[21,5] <-c$`p-value`[2]

a<-summary(B8A_S2_Sum.model)
AICTable[22,2] <- a$AIC
AICTable[22,1] <- as.character(a$terms[[3]])
AICTable[22,3] <- exp(sqrt(mean((a$data$Peat_DepthL - a$fitted[,1])^2)))-1
b<-MuMIn::r.squaredGLMM(B8A_S2_Sum.model)
AICTable[22,4] <- b[1]
c<- anova(B8A_S2_Sum.model)
AICTable[22,5] <-c$`p-value`[2]

a<-summary(B9_S2_Sum.model)
AICTable[23,2] <- a$AIC
AICTable[23,1] <- as.character(a$terms[[3]])
AICTable[23,3] <- exp(sqrt(mean((a$data$Peat_DepthL - a$fitted[,1])^2)))-1
b<-MuMIn::r.squaredGLMM(B9_S2_Sum.model)
AICTable[23,4] <- b[1]
c<- anova(B9_S2_Sum.model)
AICTable[23,5] <-c$`p-value`[2]

#a<-summary(B10_S2_Sum.model)
#AICTable[24,2] <- a$AIC
#AICTable[24,1] <- as.character(a$terms[[3]])
#AICTable[24,3] <- exp(sqrt(mean((a$data$Peat_DepthL - a$fitted[,1])^2)))-1
#b<-MuMIn::r.squaredGLMM(B10_S2_Sum.model)
#AICTable[24,4] <- b[1]
#c<- anova(B10_S2_Sum.model)
#AICTable[24,5] <-c$`p-value`[2]

a<-summary(B11_S2_Sum.model)
AICTable[25,2] <- a$AIC
AICTable[25,1] <- as.character(a$terms[[3]])
AICTable[25,3] <- exp(sqrt(mean((a$data$Peat_DepthL - a$fitted[,1])^2)))-1
b<-MuMIn::r.squaredGLMM(B11_S2_Sum.model)
AICTable[25,4] <- b[1]
c<- anova(B11_S2_Sum.model)
AICTable[25,5] <-c$`p-value`[2]

a<-summary(B12_S2_Sum.model)
AICTable[26,2] <- a$AIC
AICTable[26,1] <- as.character(a$terms[[3]])
AICTable[26,3] <- exp(sqrt(mean((a$data$Peat_DepthL - a$fitted[,1])^2)))-1
b<-MuMIn::r.squaredGLMM(B12_S2_Sum.model)
AICTable[26,4] <- b[1]
c<- anova(B12_S2_Sum.model)
AICTable[26,5] <-c$`p-value`[2]

a<-summary(B1_S2_Win.model)
AICTable[27,2] <- a$AIC
AICTable[27,1] <- as.character(a$terms[[3]])
AICTable[27,3] <- exp(sqrt(mean((a$data$Peat_DepthL - a$fitted[,1])^2)))-1
b<-MuMIn::r.squaredGLMM(B1_S2_Win.model)
AICTable[27,4] <- b[1]
c<- anova(B1_S2_Win.model)
AICTable[27,5] <-c$`p-value`[2]

a<-summary(B2_S2_Win.model)
AICTable[28,2] <- a$AIC
AICTable[28,1] <- as.character(a$terms[[3]])
AICTable[28,3] <- exp(sqrt(mean((a$data$Peat_DepthL - a$fitted[,1])^2)))-1
b<-MuMIn::r.squaredGLMM(B2_S2_Win.model)
AICTable[28,4] <- b[1]
c<- anova(B2_S2_Win.model)
AICTable[28,5] <-c$`p-value`[2]

a<-summary(B3_S2_Win.model)
AICTable[29,2] <- a$AIC
AICTable[29,1] <- as.character(a$terms[[3]])
AICTable[29,3] <- exp(sqrt(mean((a$data$Peat_DepthL - a$fitted[,1])^2)))-1
b<-MuMIn::r.squaredGLMM(B3_S2_Win.model)
AICTable[29,4] <- b[1]
c<- anova(B3_S2_Win.model)
AICTable[29,5] <-c$`p-value`[2]

a<-summary(B4_S2_Win.model)
AICTable[30,2] <- a$AIC
AICTable[30,1] <- as.character(a$terms[[3]])
AICTable[30,3] <- exp(sqrt(mean((a$data$Peat_DepthL - a$fitted[,1])^2)))-1
b<-MuMIn::r.squaredGLMM(B4_S2_Win.model)
AICTable[30,4] <- b[1]
c<- anova(B4_S2_Win.model)
AICTable[30,5] <-c$`p-value`[2]

a<-summary(B5_S2_Win.model)
AICTable[31,2] <- a$AIC
AICTable[31,1] <- as.character(a$terms[[3]])
AICTable[31,3] <- exp(sqrt(mean((a$data$Peat_DepthL - a$fitted[,1])^2)))-1
b<-MuMIn::r.squaredGLMM(B5_S2_Win.model)
AICTable[31,4] <- b[1]
c<- anova(B5_S2_Win.model)
AICTable[31,5] <-c$`p-value`[2]

a<-summary(B6_S2_Win.model)
AICTable[32,2] <- a$AIC
AICTable[32,1] <- as.character(a$terms[[3]])
AICTable[32,3] <- exp(sqrt(mean((a$data$Peat_DepthL - a$fitted[,1])^2)))-1
b<-MuMIn::r.squaredGLMM(B6_S2_Win.model)
AICTable[32,4] <- b[1]
c<- anova(B6_S2_Win.model)
AICTable[32,5] <-c$`p-value`[2]

a<-summary(B7_S2_Win.model)
AICTable[33,2] <- a$AIC
AICTable[33,1] <- as.character(a$terms[[3]])
AICTable[33,3] <- exp(sqrt(mean((a$data$Peat_DepthL - a$fitted[,1])^2)))-1
b<-MuMIn::r.squaredGLMM(B7_S2_Win.model)
AICTable[33,4] <- b[1]
c<- anova(B7_S2_Win.model)
AICTable[33,5] <-c$`p-value`[2]

a<-summary(B8_S2_Win.model )
AICTable[34,2] <- a$AIC
AICTable[34,1] <- as.character(a$terms[[3]])
AICTable[34,3] <- exp(sqrt(mean((a$data$Peat_DepthL - a$fitted[,1])^2)))-1
b<-MuMIn::r.squaredGLMM(B8_S2_Win.model)
AICTable[34,4] <- b[1]
c<- anova(B8_S2_Win.model)
AICTable[34,5] <-c$`p-value`[2]

a<-summary(B8A_S2_Win.model)
AICTable[35,2] <- a$AIC
AICTable[35,1] <- as.character(a$terms[[3]])
AICTable[35,3] <- exp(sqrt(mean((a$data$Peat_DepthL - a$fitted[,1])^2)))-1
b<-MuMIn::r.squaredGLMM(B8A_S2_Win.model)
AICTable[35,4] <- b[1]
c<- anova(B8A_S2_Win.model)
AICTable[35,5] <-c$`p-value`[2]

a<-summary(B9_S2_Win.model)
AICTable[36,2] <- a$AIC
AICTable[36,1] <- as.character(a$terms[[3]])
AICTable[36,3] <- exp(sqrt(mean((a$data$Peat_DepthL - a$fitted[,1])^2)))-1
b<-MuMIn::r.squaredGLMM(B9_S2_Win.model)
AICTable[36,4] <- b[1]
c<- anova(B9_S2_Win.model)
AICTable[36,5] <-c$`p-value`[2]

#a<-summary(B10_S2_Win.model)
#AICTable[37,2] <- a$AIC
#AICTable[37,1] <- as.character(a$terms[[3]])
#AICTable[37,3] <- exp(sqrt(mean((a$data$Peat_DepthL - a$fitted[,1])^2)))-1
#b<-MuMIn::r.squaredGLMM(B10_S2_Win.model)
#AICTable[37,4] <- b[1]
#c<- anova(B10_S2_Win.model)
#AICTable[37,5] <-c$`p-value`[2]

a<-summary(B11_S2_Win.model)
AICTable[38,2] <- a$AIC
AICTable[38,1] <- as.character(a$terms[[3]])
AICTable[38,3] <- exp(sqrt(mean((a$data$Peat_DepthL - a$fitted[,1])^2)))-1
b<-MuMIn::r.squaredGLMM(B11_S2_Win.model)
AICTable[38,4] <- b[1]
c<- anova(B11_S2_Win.model)
AICTable[38,5] <-c$`p-value`[2]

a<-summary(B12_S2_Win.model )
AICTable[39,2] <- a$AIC
AICTable[39,1] <- as.character(a$terms[[3]])
AICTable[39,3] <- exp(sqrt(mean((a$data$Peat_DepthL - a$fitted[,1])^2)))-1
b<-MuMIn::r.squaredGLMM(B12_S2_Win.model)
AICTable[39,4] <- b[1]
c<- anova(B12_S2_Win.model)
AICTable[39,5] <-c$`p-value`[2]

a<-summary(NDVI_Sum.model )
AICTable[40,2] <- a$AIC
AICTable[40,1] <- as.character(a$terms[[3]])
AICTable[40,3] <- exp(sqrt(mean((a$data$Peat_DepthL - a$fitted[,1])^2)))-1
b<-MuMIn::r.squaredGLMM(NDVI_Sum.model)
AICTable[40,4] <- b[1]
c<- anova(NDVI_Sum.model)
AICTable[40,5] <-c$`p-value`[2]

a<-summary(NDVI_Win.model )
AICTable[41,2] <- a$AIC
AICTable[41,1] <- as.character(a$terms[[3]])
AICTable[41,3] <- exp(sqrt(mean((a$data$Peat_DepthL - a$fitted[,1])^2)))-1
b<-MuMIn::r.squaredGLMM(NDVI_Win.model)
AICTable[41,4] <- b[1]
c<- anova(NDVI_Win.model)
AICTable[41,5] <-c$`p-value`[2]

a<-summary(Dose.model )
AICTable[42,2] <- a$AIC
AICTable[42,1] <- as.character(a$terms[[3]])
AICTable[42,3] <- exp(sqrt(mean((a$data$Peat_DepthL - a$fitted[,1])^2)))-1
b<-MuMIn::r.squaredGLMM(Dose.model)
AICTable[42,4] <- b[1]
c<- anova(Dose.model)
AICTable[42,5] <-c$`p-value`[2]

a<-summary(TotCount.model )
AICTable[43,2] <- a$AIC
AICTable[43,1] <- as.character(a$terms[[3]])
AICTable[43,3] <- exp(sqrt(mean((a$data$Peat_DepthL - a$fitted[,1])^2)))-1
b<-MuMIn::r.squaredGLMM(TotCount.model)
AICTable[43,4] <- b[1]
c<- anova(TotCount.model)
AICTable[43,5] <-c$`p-value`[2]

a<-summary(K.model )
AICTable[44,2] <- a$AIC
AICTable[44,1] <- as.character(a$terms[[3]])
AICTable[44,3] <- exp(sqrt(mean((a$data$Peat_DepthL - a$fitted[,1])^2)))-1
b<-MuMIn::r.squaredGLMM(K.model)
AICTable[44,4] <- b[1]
c<- anova(K.model)
AICTable[44,5] <-c$`p-value`[2]

a<-summary(Ur.model )
AICTable[45,2] <- a$AIC
AICTable[45,1] <- as.character(a$terms[[3]])
AICTable[45,3] <- exp(sqrt(mean((a$data$Peat_DepthL - a$fitted[,1])^2)))-1
b<-MuMIn::r.squaredGLMM(Ur.model)
AICTable[45,4] <- b[1]
c<- anova(Ur.model)
AICTable[45,5] <-c$`p-value`[2]

a<-summary(Th.model )
AICTable[46,2] <- a$AIC
AICTable[46,1] <- as.character(a$terms[[3]])
AICTable[46,3] <- exp(sqrt(mean((a$data$Peat_DepthL - a$fitted[,1])^2)))-1
b<-MuMIn::r.squaredGLMM(Th.model)
AICTable[46,4] <- b[1]
c<- anova(Th.model)
AICTable[46,5] <-c$`p-value`[2]

save(AICTable, file = "C:/LocalData/sp991/GEOM184/CodeForMSc/Final/Inputs/AICTableExpSingle.RData")

