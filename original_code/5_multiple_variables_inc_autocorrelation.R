library(nlme)
library(MuMIn)
library(tidyverse)

load("C:/Users/ng292/OneDrive - University of Exeter/NfCPGS/PeatMapping/Points.RData")

Points<-Points%>%
  mutate(Peat_DepthL = log(Peat_Depth+1),
         NDVI_Win = ((B5_S2_Win-B4_S2_Win)/(B5_S2_Win+B4_S2_Win)),
         NDVI_Sum = ((B5_S2_Sum-B4_S2_Sum)/(B5_S2_Sum+B4_S2_Sum)))

null.model <- lme(fixed = Peat_DepthL ~ DSM + slope + aspect + SinAspect + CosAspect + roughness +
                 TPI + TRI + TWI +  VV_p95 + VV_p5 + VH_p95 + VH_p5 + B1_S2_Sum + B2_S2_Sum +
                 B3_S2_Sum +  B4_S2_Sum + B5_S2_Sum + B6_S2_Sum + B7_S2_Sum + B8_S2_Sum + 
                 B8A_S2_Sum + B9_S2_Sum + B10_S2_Sum + B11_S2_Sum + B12_S2_Sum + B1_S2_Win +
                 B2_S2_Win + B3_S2_Win + B4_S2_Win + B5_S2_Win + B6_S2_Win + B7_S2_Win +
                 B8_S2_Win + B8A_S2_Win + B9_S2_Win + B10_S2_Win + B11_S2_Win + B12_S2_Win +
                 NDVI_Sum+NDVI_Win+Dose+TotCount+K+Ur+Th, 
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


#corGaus for Gaussian spatial autocorrelation
gau.sp <- update(null.model, correlation = corGaus(1, form = ~ X + Y), method = "ML")
a<-summary(gau.sp)
a
sqrt(mean((a$data$Peat_Depth - a$fitted[,1])^2))
exp(sqrt(mean((a$data$Peat_DepthL - a$fitted[,1])^2)))-1
MuMIn::r.squaredGLMM(gau.sp)

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

step.model <- caret::train(Peat_DepthL ~ DSM + slope + aspect + SinAspect + CosAspect + roughness +
                             TPI + TRI + TWI +  VV_p95 + VV_p5 + VH_p95 + VH_p5 + B1_S2_Sum + B2_S2_Sum +
                             B3_S2_Sum +  B4_S2_Sum + B5_S2_Sum + B6_S2_Sum + B7_S2_Sum + B8_S2_Sum + 
                             B8A_S2_Sum + B9_S2_Sum + B10_S2_Sum + B11_S2_Sum + B12_S2_Sum + B1_S2_Win +
                             B2_S2_Win + B3_S2_Win + B4_S2_Win + B5_S2_Win + B6_S2_Win + B7_S2_Win +
                             B8_S2_Win + B8A_S2_Win + B9_S2_Win + B10_S2_Win + B11_S2_Win + B12_S2_Win+
                             NDVI_Sum  + NDVI_Win + Dose+ TotCount + K + Ur + Th, 
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



#Forward Peat Depth
b.model <- lme(fixed = Peat_Depth ~ DSM + slope + aspect + CosAspect +
                 TWI +  VV_p95 + VV_p5 + VH_p95 + VH_p5 + B2_S2_Sum +
                 B3_S2_Sum +  B4_S2_Sum +  B6_S2_Sum + B7_S2_Sum + B8_S2_Sum + 
                 B8A_S2_Sum + B9_S2_Sum + B10_S2_Sum + B11_S2_Sum + B12_S2_Sum + B1_S2_Win +
                 B2_S2_Win + B3_S2_Win + B4_S2_Win + B5_S2_Win + B6_S2_Win + B7_S2_Win +
                 B8_S2_Win + B8A_S2_Win + B9_S2_Win + B10_S2_Win +  B12_S2_Win+
                 Dose+ TotCount + K + Ur + Th, 
               correlation = corGaus(1, form = ~ X + Y),
               data = Points, random = ~ 1 | dummy, method = "ML") 

a<- summary(b.model)
a
sqrt(mean((a$data$Peat_Depth - a$fitted[,1])^2))
MuMIn::r.squaredGLMM(b.model)

#Forward Exponential Peat Depth
b.model <- lme(fixed = Peat_DepthL ~ aspect +  CosAspect + 
                 TRI + VV_p5 + VH_p5 + B1_S2_Sum  +
                 B3_S2_Sum +  B4_S2_Sum + B8_S2_Sum + 
                 B9_S2_Sum + B10_S2_Sum + B11_S2_Sum + 
                 B5_S2_Win + B6_S2_Win +
                 B9_S2_Win + B11_S2_Win +
                 NDVI_Win + K + Ur, 
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
                                   TWI +  VV_p95 + VV_p5 + VH_p95 + 
                                   B3_S2_Sum +  B4_S2_Sum + B6_S2_Sum + B7_S2_Sum + B8_S2_Sum + 
                                   B8A_S2_Sum + B9_S2_Sum + B10_S2_Sum + B11_S2_Sum + B1_S2_Win +
                                   B3_S2_Win + B5_S2_Win + B6_S2_Win + B7_S2_Win +
                                   B8_S2_Win + B9_S2_Win + B10_S2_Win + B12_S2_Win+
                                   Dose + K + Ur + Th, 
                                   correlation = corGaus(1, form = ~ X + Y),
                                   data = Points, random = ~ 1 | dummy, method = "ML") 
 
a<- summary(b.model)
a
sqrt(mean((a$data$Peat_Depth - a$fitted[,1])^2))
MuMIn::r.squaredGLMM(b.model)


#backward Exponential Peat Depth
b.model <- lme(fixed = Peat_DepthL ~  aspect + CosAspect + 
                 TPI + TRI + VV_p5 + VH_p95 + B1_S2_Sum + 
                 B3_S2_Sum +   B5_S2_Sum + B6_S2_Sum + B7_S2_Sum + B8_S2_Sum + 
                 B8A_S2_Sum + B9_S2_Sum + B10_S2_Sum + B11_S2_Sum + B1_S2_Win +
                 B5_S2_Win + B6_S2_Win + 
                 B8A_S2_Win + B9_S2_Win + B10_S2_Win + B11_S2_Win + B12_S2_Win+
                 NDVI_Win + Dose+ K + Ur + Th, 
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
                 TWI +  VV_p95 + VV_p5 + VH_p95 + B2_S2_Sum +
                 B3_S2_Sum +  B4_S2_Sum +  B6_S2_Sum + B7_S2_Sum + B8_S2_Sum + 
                 B8A_S2_Sum + B9_S2_Sum + B10_S2_Sum + B11_S2_Sum + B12_S2_Sum + B1_S2_Win +
                 B3_S2_Win + B4_S2_Win + B6_S2_Win + B7_S2_Win +
                 B8_S2_Win + B9_S2_Win + B10_S2_Win + B12_S2_Win+
                 NDVI_Win + Dose+ K + Ur + Th, 
               correlation = corExp(1, form = ~ X + Y),
               data = Points, random = ~ 1 | dummy, method = "ML") 

a<- summary(b.model)
a
sqrt(mean((a$data$Peat_Depth - a$fitted[,1])^2))
MuMIn::r.squaredGLMM(b.model)

#sequential Exponential Peat Depth
b.model <- lme(fixed = Peat_DepthL ~  aspect +  CosAspect + 
                 TPI + TRI +  VV_p5 + VH_p95 + VH_p5 + B1_S2_Sum + B2_S2_Sum +
                 B3_S2_Sum +  B5_S2_Sum + B6_S2_Sum + B7_S2_Sum + B8_S2_Sum + 
                 B8A_S2_Sum + B9_S2_Sum + B10_S2_Sum + B11_S2_Sum + 
                 B2_S2_Win +  B5_S2_Win + B6_S2_Win + 
                 B8A_S2_Win + B9_S2_Win + B10_S2_Win + B11_S2_Win +
                 NDVI_Win +  K + Ur , 
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
exp.model <- lme(fixed = Peat_DepthL ~ DSM + slope + aspect + SinAspect + CosAspect + roughness +
                   TPI + TRI + TWI +  VV_p95 + VV_p5 + VH_p95 + VH_p5 + B1_S2_Sum + B2_S2_Sum +
                   B3_S2_Sum +  B4_S2_Sum + B5_S2_Sum + B6_S2_Sum + B7_S2_Sum + B8_S2_Sum + 
                   B8A_S2_Sum + B9_S2_Sum + B10_S2_Sum + B11_S2_Sum + B12_S2_Sum + B1_S2_Win +
                   B2_S2_Win + B3_S2_Win + B4_S2_Win + B5_S2_Win + B6_S2_Win + B7_S2_Win +
                   B8_S2_Win + B8A_S2_Win + B9_S2_Win + B10_S2_Win + B11_S2_Win + B12_S2_Win +
                   NDVI_Sum+NDVI_Win+Dose+TotCount+K+Ur+Th, 
                 data = Points, random = ~ 1 | dummy, 
                 method = "ML") 


a<- summary(exp.model)
plot(a$fitted[,1], a$data$Peat_DepthL, xlab = "Modelled", ylab = "Measured", main = "Ln(Peat Depth +1)")
abline(0,1)
plot(a$data$Peat_DepthL- a$fitted[,1],a$fitted[,1], xlab = "Residual", ylab = "Measured")



lin.model <- lme(fixed = Peat_Depth ~ DSM + slope + aspect + SinAspect + CosAspect + roughness +
                   TPI + TRI + TWI +  VV_p95 + VV_p5 + VH_p95 + VH_p5 + B1_S2_Sum + B2_S2_Sum +
                   B3_S2_Sum +  B4_S2_Sum + B5_S2_Sum + B6_S2_Sum + B7_S2_Sum + B8_S2_Sum + 
                   B8A_S2_Sum + B9_S2_Sum + B10_S2_Sum + B11_S2_Sum + B12_S2_Sum + B1_S2_Win +
                   B2_S2_Win + B3_S2_Win + B4_S2_Win + B5_S2_Win + B6_S2_Win + B7_S2_Win +
                   B8_S2_Win + B8A_S2_Win + B9_S2_Win + B10_S2_Win + B11_S2_Win + B12_S2_Win +
                   NDVI_Sum+NDVI_Win+Dose+TotCount+K+Ur+Th, 
                 data = Points, random = ~ 1 | dummy, 
                 method = "ML") 


b<- summary(lin.model)
plot(b$fitted[,1], b$data$Peat_Depth, xlab = "Modelled", ylab = "Measured", main = "Peat Depth")
abline(0,1)

plot(b$data$Peat_Depth- b$fitted[,1],b$fitted[,1], xlab = "Residual", ylab = "Measured")




