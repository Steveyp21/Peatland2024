library(nlme)
library(MuMIn)
library(tidyverse)

#Load("C:/LocalData/sp991/GEOM184/CodeForMSc/Final/Inputs/Points.RData")

#Points<-Points%>%
#  mutate(Peat_DepthL = log(Peat_Depth+1),
#         NDVI_Win = ((B5_S2_Win-B4_S2_Win)/(B5_S2_Win+B4_S2_Win)),
#         NDVI_Sum = ((B5_S2_Sum-B4_S2_Sum)/(B5_S2_Sum+B4_S2_Sum)))

null.model <- lme(fixed = Peat_DepthL ~ Bodmin_DSM_clipped + slope + aspect + SinAspect + CosAspect + roughness +
                 TPI + TRI + Bodmin_TWI_Final + Bodmin_S1_clipped + Bodmin_S2_Sum_clipped_1 + Bodmin_S2_Sum_clipped_2 +
                 Bodmin_S2_Sum_clipped_3 + Bodmin_S2_Sum_clipped_4 + Bodmin_S2_Sum_clipped_5 + Bodmin_S2_Sum_clipped_6 + Bodmin_S2_Sum_clipped_7 + Bodmin_S2_Sum_clipped_8 + 
                 Bodmin_S2_Sum_clipped_9 + Bodmin_S2_Sum_clipped_10 + Bodmin_S2_Sum_clipped_11 + Bodmin_S2_Sum_clipped_12 + Bodmin_S2_Win_clipped_1 +
                 Bodmin_S2_Win_clipped_2 + Bodmin_S2_Win_clipped_3 + Bodmin_S2_Win_clipped_4 + Bodmin_S2_Win_clipped_5 + Bodmin_S2_Win_clipped_6 + Bodmin_S2_Win_clipped_7 +
                 Bodmin_S2_Win_clipped_8 + Bodmin_S2_Win_clipped_9 + Bodmin_S2_Win_clipped_10 + Bodmin_S2_Win_clipped_11 + Bodmin_S2_Win_clipped_12 +
                 NDVI_Sum_df + NDVI_Win_df + Dose + TotCount +K + Ur + Th, 
                 data = Points, random = ~ 1 | dummy, 
                method = "ML") 

a<- summary(null.model)
a
sqrt(mean((a$data$Peat_Depth - a$fitted[,1])^2))
exp(sqrt(mean((a$data$Peat_DepthL - a$fitted[,1])^2)))-1
MuMIn::r.squaredGLMM(null.model)


#corExp for exponential spatial autocorelation 
exp.sp <- update(null.model, correlation = corExp(1, form = ~ X + Y), method = "ML")
a<-summary(exp.sp)
a
sqrt(mean((a$data$Peat_Depth - a$fitted[,1])^2))
exp(sqrt(mean((a$data$Peat_DepthL - a$fitted[,1])^2)))-1
MuMIn::r.squaredGLMM(exp.sp)

##singularity issue encountered during the estimation process - CODE SKIPPED NEEDS RESOLUTION
#corGaus for Gaussian spatial autocorrelation
#gau.sp <- update(null.model, correlation = corGaus(1, form = ~ X + Y), method = "ML")
#a<-summary(gau.sp)
#a
#sqrt(mean((a$data$Peat_Depth - a$fitted[,1])^2))
#exp(sqrt(mean((a$data$Peat_DepthL - a$fitted[,1])^2)))-1
#MuMIn::r.squaredGLMM(gau.sp)

#corSpher for spherical spatial autocorrelation
spher.sp <- update(null.model, correlation = corSpher(1, form = ~ X + Y), method = "ML")
a<-summary(spher.sp)
a
sqrt(mean((a$data$Peat_Depth - a$fitted[,1])^2))
exp(sqrt(mean((a$data$Peat_DepthL - a$fitted[,1])^2)))-1
MuMIn::r.squaredGLMM(spher.sp )

#corLin for linear
lin.sp <- update(null.model, correlation = corLin(1, form = ~ X + Y), method = "ML")
a<-summary(lin.sp)
a
sqrt(mean((a$data$Peat_Depth - a$fitted[,1])^2))
exp(sqrt(mean((a$data$Peat_DepthL - a$fitted[,1])^2)))-1
MuMIn::r.squaredGLMM(lin.sp)

#corRatio for rational quadratics
ratio.sp <- update(null.model, correlation = corRatio(1, form = ~ X + Y), method = "ML")
a<-summary(ratio.sp)
a
sqrt(mean((a$data$Peat_Depth - a$fitted[,1])^2))
exp(sqrt(mean((a$data$Peat_DepthL - a$fitted[,1])^2)))-1
MuMIn::r.squaredGLMM(ratio.sp)



library(caret)
library(leaps)
# Set seed for reproducibility
set.seed(123)
# Set up repeated k-fold cross-validation
train.control <- trainControl(method = "cv", number = 10)
# Train the model

step.model <- caret::train(Peat_DepthL ~ Bodmin_DSM_clipped + slope + aspect + SinAspect + CosAspect + roughness +
                             TPI + TRI + Bodmin_TWI_Final + Bodmin_S1_clipped + Bodmin_S2_Sum_clipped_1 + Bodmin_S2_Sum_clipped_2 +
                             Bodmin_S2_Sum_clipped_3 + Bodmin_S2_Sum_clipped_4 + Bodmin_S2_Sum_clipped_5 + Bodmin_S2_Sum_clipped_6 + Bodmin_S2_Sum_clipped_7 + Bodmin_S2_Sum_clipped_8 + 
                             Bodmin_S2_Sum_clipped_9 + Bodmin_S2_Sum_clipped_10 + Bodmin_S2_Sum_clipped_11 + Bodmin_S2_Sum_clipped_12 + Bodmin_S2_Win_clipped_1 +
                             Bodmin_S2_Win_clipped_2 + Bodmin_S2_Win_clipped_3 + Bodmin_S2_Win_clipped_4 + Bodmin_S2_Win_clipped_5 + Bodmin_S2_Win_clipped_6 + Bodmin_S2_Win_clipped_7 +
                             Bodmin_S2_Win_clipped_8 + Bodmin_S2_Win_clipped_9 + Bodmin_S2_Win_clipped_10 + Bodmin_S2_Win_clipped_11 + Bodmin_S2_Win_clipped_12 +
                             NDVI_Sum_df + NDVI_Win_df + Dose + TotCount +K + Ur + Th, 
                           data = Points,
                           #correlation = corExp(1, form = ~ X + Y),
                           correlation = corSpher(1, form = ~ X + Y), 
                           method = "leapSeq",  #other options "leapBackward", "leapForward" or "leapSeq" (stepwise)
                           tuneGrid = data.frame(nvmax = 1:29), #max number of predictors
                           trControl = train.control
)

#step.model$results
step.model$bestTune
summary(step.model$finalModel)


##CONVERGENCE ISSUE CODE SKIPPED NEEDS RESOLUTION
#Forward Peat Depth
#b.model <- lme(fixed = Peat_Depth ~ Bodmin_DSM_clipped + slope + aspect + CosAspect +
#                 Bodmin_TWI_Final + Bodmin_S1_clipped + Bodmin_S2_Sum_clipped_2 +
#                 Bodmin_S2_Sum_clipped_3 + Bodmin_S2_Sum_clipped_4 +  Bodmin_S2_Sum_clipped_6 + Bodmin_S2_Sum_clipped_7 + Bodmin_S2_Sum_clipped_8 + 
#                 Bodmin_S2_Sum_clipped_9 + Bodmin_S2_Sum_clipped_10 + Bodmin_S2_Sum_clipped_11 + Bodmin_S2_Sum_clipped_12 + Bodmin_S2_Win_clipped_1 +
#                 Bodmin_S2_Win_clipped_2 + Bodmin_S2_Win_clipped_3 + Bodmin_S2_Win_clipped_4 + Bodmin_S2_Win_clipped_5 + Bodmin_S2_Win_clipped_6 + Bodmin_S2_Win_clipped_7 +
#                 Bodmin_S2_Win_clipped_8 + Bodmin_S2_Win_clipped_9 + Bodmin_S2_Win_clipped_10 + Bodmin_S2_Win_clipped_12 +
#                 Dose + TotCount + K + Ur + Th, 
#               correlation = corGaus(1, form = ~ X + Y),
#               data = Points, random = ~ 1 | dummy, method = "ML") 

#a<- summary(b.model)
#a
#sqrt(mean((a$data$Peat_Depth - a$fitted[,1])^2))
#MuMIn::r.squaredGLMM(b.model)

#Forward Exponential Peat Depth
b.model <- lme(fixed = Peat_DepthL ~ aspect +  CosAspect + 
                 TRI + Bodmin_S1_clipped + Bodmin_S2_Sum_clipped_1  +
                 Bodmin_S2_Sum_clipped_3 +  Bodmin_S2_Sum_clipped_4 + Bodmin_S2_Sum_clipped_8 + 
                 Bodmin_S2_Sum_clipped_10 + Bodmin_S2_Sum_clipped_11 + 
                 Bodmin_S2_Win_clipped_5 + Bodmin_S2_Win_clipped_6 +
                 Bodmin_S2_Win_clipped_10 + Bodmin_S2_Win_clipped_11 +
                 NDVI_Win_df + K + Ur, 
               correlation = corSpher(1, form = ~ X + Y),
               data = Points, random = ~ 1 | dummy, method = "ML") 

summary(b.model)
a<- summary(b.model)
a
sqrt(mean((a$data$Peat_DepthL - a$fitted[,1])^2))
exp(sqrt(mean((a$data$Peat_DepthL - a$fitted[,1])^2)))-1
MuMIn::r.squaredGLMM(b.model)


#backward Peat Depth
b.model <- lme(fixed = Peat_Depth ~ aspect + CosAspect + roughness +
                                   Bodmin_TWI_Final +  Bodmin_S1_clipped + 
                 Bodmin_S2_Sum_clipped_3 +  Bodmin_S2_Sum_clipped_4 + Bodmin_S2_Sum_clipped_6 + Bodmin_S2_Sum_clipped_7 + Bodmin_S2_Sum_clipped_8 + 
                 Bodmin_S2_Sum_clipped_9 + Bodmin_S2_Sum_clipped_10 + Bodmin_S2_Sum_clipped_11 + Bodmin_S2_Win_clipped_1 +
                 Bodmin_S2_Win_clipped_3 + Bodmin_S2_Win_clipped_5 + Bodmin_S2_Win_clipped_6 + Bodmin_S2_Win_clipped_7 +
                 Bodmin_S2_Win_clipped_8 + Bodmin_S2_Win_clipped_10 + Bodmin_S2_Win_clipped_12+
                                   Dose + K + Ur + Th, 
                                   correlation = corGaus(1, form = ~ X + Y),
                                   data = Points, random = ~ 1 | dummy, method = "ML") 
 
a<- summary(b.model)
a
sqrt(mean((a$data$Peat_Depth - a$fitted[,1])^2))
MuMIn::r.squaredGLMM(b.model)


#backward Exponential Peat Depth
b.model <- lme(fixed = Peat_DepthL ~  aspect + CosAspect + 
                 TPI + TRI + Bodmin_S1_clipped + Bodmin_S2_Sum_clipped_1 + 
                 Bodmin_S2_Sum_clipped_3 +   Bodmin_S2_Sum_clipped_5 + Bodmin_S2_Sum_clipped_6 + Bodmin_S2_Sum_clipped_7 + Bodmin_S2_Sum_clipped_8 + 
                 Bodmin_S2_Sum_clipped_9 + Bodmin_S2_Sum_clipped_10 + Bodmin_S2_Sum_clipped_11 + Bodmin_S2_Win_clipped_1 +
                 Bodmin_S2_Win_clipped_5 + Bodmin_S2_Win_clipped_6 + 
                 Bodmin_S2_Win_clipped_9 + Bodmin_S2_Win_clipped_10 + Bodmin_S2_Win_clipped_11 + Bodmin_S2_Win_clipped_12 +
                 NDVI_Win_df + Dose+ K + Ur + Th, 
               correlation = corSpher(1, form = ~ X + Y),
               data = Points, random = ~ 1 | dummy, method = "ML") 

summary(b.model)
a<- summary(b.model)
a
sqrt(mean((a$data$Peat_DepthL - a$fitted[,1])^2))
exp(sqrt(mean((a$data$Peat_DepthL - a$fitted[,1])^2)))-1
MuMIn::r.squaredGLMM(b.model)


#sequential Peat Depth
b.model <- lme(fixed = Peat_Depth ~ aspect + CosAspect + roughness +
                 Bodmin_TWI_Final +  Bodmin_S1_clipped + Bodmin_S2_Sum_clipped_2 +
                 Bodmin_S2_Sum_clipped_3 +  Bodmin_S2_Sum_clipped_4 +  Bodmin_S2_Sum_clipped_6 + Bodmin_S2_Sum_clipped_7 + Bodmin_S2_Sum_clipped_8 + 
                 Bodmin_S2_Sum_clipped_9 + Bodmin_S2_Sum_clipped_10 + Bodmin_S2_Sum_clipped_11 + Bodmin_S2_Sum_clipped_12 + Bodmin_S2_Win_clipped_1 +
                 Bodmin_S2_Win_clipped_3 + Bodmin_S2_Win_clipped_4 + Bodmin_S2_Win_clipped_6 + Bodmin_S2_Win_clipped_7 +
                 Bodmin_S2_Win_clipped_8 + Bodmin_S2_Win_clipped_10 + Bodmin_S2_Win_clipped_12 +
                 NDVI_Win_df + Dose+ K + Ur + Th, 
               correlation = corExp(1, form = ~ X + Y),
               data = Points, random = ~ 1 | dummy, method = "ML") 

a<- summary(b.model)
a
sqrt(mean((a$data$Peat_Depth - a$fitted[,1])^2))
MuMIn::r.squaredGLMM(b.model)

#sequential Exponential Peat Depth
b.model <- lme(fixed = Peat_DepthL ~  aspect +  CosAspect + 
                 TPI + TRI +  Bodmin_S1_clipped +  Bodmin_S2_Sum_clipped_1 +  Bodmin_S2_Sum_clipped_2 +
                 Bodmin_S2_Sum_clipped_3 +   Bodmin_S2_Sum_clipped_5 +  Bodmin_S2_Sum_clipped_6 +  Bodmin_S2_Sum_clipped_7 +  Bodmin_S2_Sum_clipped_8 + 
                 Bodmin_S2_Sum_clipped_9 +  Bodmin_S2_Sum_clipped_10 +  Bodmin_S2_Sum_clipped_11 + 
                 Bodmin_S2_Win_clipped_2 +  Bodmin_S2_Win_clipped_5 + Bodmin_S2_Win_clipped_6 + 
                 Bodmin_S2_Win_clipped_9 + Bodmin_S2_Win_clipped_10 + Bodmin_S2_Win_clipped_11 +
                 NDVI_Win_df +  K + Ur , 
               correlation = corSpher(1, form = ~ X + Y),
               data = Points, random = ~ 1 | dummy, method = "ML") 

summary(b.model)
a<- summary(b.model)
a
sqrt(mean((a$data$Peat_DepthL - a$fitted[,1])^2))
exp(sqrt(mean((a$data$Peat_DepthL - a$fitted[,1])^2)))-1
MuMIn::r.squaredGLMM(b.model)



#### plot histogram####
hist(Points$Peat_Depth, breaks = 50, xlab = "Peat Depth (cm)", main = "")


#### plot graphs ####
exp.model <- lme(fixed = Peat_DepthL ~ Bodmin_DSM_clipped + slope + aspect + SinAspect + CosAspect + roughness +
                   TPI + TRI + Bodmin_TWI_Final + Bodmin_S1_clipped + Bodmin_S2_Sum_clipped_1 + Bodmin_S2_Sum_clipped_2 +
                   Bodmin_S2_Sum_clipped_3 + Bodmin_S2_Sum_clipped_4 + Bodmin_S2_Sum_clipped_5 + Bodmin_S2_Sum_clipped_6 + Bodmin_S2_Sum_clipped_7 + Bodmin_S2_Sum_clipped_8 + 
                   Bodmin_S2_Sum_clipped_9 + Bodmin_S2_Sum_clipped_10 + Bodmin_S2_Sum_clipped_11 + Bodmin_S2_Sum_clipped_12 + Bodmin_S2_Win_clipped_1 +
                   Bodmin_S2_Win_clipped_2 + Bodmin_S2_Win_clipped_3 + Bodmin_S2_Win_clipped_4 + Bodmin_S2_Win_clipped_5 + Bodmin_S2_Win_clipped_6 + Bodmin_S2_Win_clipped_7 +
                   Bodmin_S2_Win_clipped_8 + Bodmin_S2_Win_clipped_9 + Bodmin_S2_Win_clipped_10 + Bodmin_S2_Win_clipped_11 + Bodmin_S2_Win_clipped_12 +
                   NDVI_Sum_df + NDVI_Win_df + Dose + TotCount +K + Ur + Th, 
                 data = Points, random = ~ 1 | dummy, 
                 method = "ML") 


a<- summary(exp.model)
plot(a$fitted[,1], a$data$Peat_DepthL, xlab = "Modelled", ylab = "Measured", main = "Ln(Peat Depth +1)")
abline(0,1)
plot(a$data$Peat_DepthL- a$fitted[,1],a$fitted[,1], xlab = "Residual", ylab = "Measured")



lin.model <- lme(fixed = Peat_Depth ~ Bodmin_DSM_clipped + slope + aspect + SinAspect + CosAspect + roughness +
                   TPI + TRI + Bodmin_TWI_Final + Bodmin_S1_clipped + Bodmin_S2_Sum_clipped_1 + Bodmin_S2_Sum_clipped_2 +
                   Bodmin_S2_Sum_clipped_3 + Bodmin_S2_Sum_clipped_4 + Bodmin_S2_Sum_clipped_5 + Bodmin_S2_Sum_clipped_6 + Bodmin_S2_Sum_clipped_7 + Bodmin_S2_Sum_clipped_8 + 
                   Bodmin_S2_Sum_clipped_9 + Bodmin_S2_Sum_clipped_10 + Bodmin_S2_Sum_clipped_11 + Bodmin_S2_Sum_clipped_12 + Bodmin_S2_Win_clipped_1 +
                   Bodmin_S2_Win_clipped_2 + Bodmin_S2_Win_clipped_3 + Bodmin_S2_Win_clipped_4 + Bodmin_S2_Win_clipped_5 + Bodmin_S2_Win_clipped_6 + Bodmin_S2_Win_clipped_7 +
                   Bodmin_S2_Win_clipped_8 + Bodmin_S2_Win_clipped_9 + Bodmin_S2_Win_clipped_10 + Bodmin_S2_Win_clipped_11 + Bodmin_S2_Win_clipped_12 +
                   NDVI_Sum_df + NDVI_Win_df + Dose + TotCount +K + Ur + Th, 
                 data = Points, random = ~ 1 | dummy, 
                 method = "ML") 


b<- summary(lin.model)
plot(b$fitted[,1], b$data$Peat_Depth, xlab = "Modelled", ylab = "Measured", main = "Peat Depth")
abline(0,1)

plot(b$data$Peat_Depth- b$fitted[,1],b$fitted[,1], xlab = "Residual", ylab = "Measured")




