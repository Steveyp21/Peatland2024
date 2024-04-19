
load("C:/Users/ng292/OneDrive - University of Exeter/NfCPGS/PeatMapping/Points.RData")

Points<-Points%>%
  mutate(Peat_DepthL = log(Peat_Depth+1),
         NDVI_Win = ((B5_S2_Win-B4_S2_Win)/(B5_S2_Win+B4_S2_Win)),
         NDVI_Sum = ((B5_S2_Sum-B4_S2_Sum)/(B5_S2_Sum+B4_S2_Sum)))

a.model <- lme(fixed = Peat_Depth ~ DSM + slope + aspect + SinAspect + CosAspect + roughness +
                   TPI + TRI + TWI +  VV_p95 + VV_p5 + VH_p95 + VH_p5 + B1_S2_Sum + B2_S2_Sum +
                   B3_S2_Sum + B4_S2_Sum + B5_S2_Sum + B6_S2_Sum + B7_S2_Sum + B8_S2_Sum + 
                   B8A_S2_Sum + B9_S2_Sum + B10_S2_Sum + B11_S2_Sum + B12_S2_Sum + B1_S2_Win +
                   B2_S2_Win + B3_S2_Win + B4_S2_Win + B5_S2_Win + B6_S2_Win + B7_S2_Win +
                   B8_S2_Win + B8A_S2_Win + B9_S2_Win + B10_S2_Win + B11_S2_Win + B12_S2_Win +
                   NDVI_Sum+ NDVI_Win+Dose+TotCount+K+Ur+Th, data = Points, 
               random = ~ 1 | dummy, method = "ML") 

summary(a.model)



b.model <- lme(fixed = Peat_DepthL ~ DSM + slope + aspect + SinAspect + CosAspect + roughness +
                 TPI + TRI + TWI +  VV_p95 + VV_p5 + VH_p95 + VH_p5 + B1_S2_Sum + B2_S2_Sum +
                 B3_S2_Sum + B5_S2_Sum + B6_S2_Sum + B7_S2_Sum + B8_S2_Sum + 
                 B8A_S2_Sum + B9_S2_Sum + B10_S2_Sum + B11_S2_Sum + B12_S2_Sum + B1_S2_Win +
                 B2_S2_Win + B3_S2_Win + B4_S2_Win + B5_S2_Win + B6_S2_Win + B7_S2_Win +
                 B8_S2_Win + B8A_S2_Win + B9_S2_Win + B10_S2_Win + B11_S2_Win + B12_S2_Win +
                 NDVI_Sum+NDVI_Win+Dose+TotCount+K+Ur+Th, data = Points, random = ~ 1 | dummy, method = "ML") 

summary(b.model)

#stepwise linear regression
library(MASS)
# Fit the full model 
full.model <- lm(Peat_DepthL ~ DSM + slope + aspect + SinAspect + CosAspect + roughness +
                   TPI + TRI + TWI +  VV_p95 + VV_p5 + VH_p95 + VH_p5 + B1_S2_Sum + B2_S2_Sum +
                   B3_S2_Sum + B5_S2_Sum + B6_S2_Sum + B7_S2_Sum + B8_S2_Sum + 
                   B8A_S2_Sum + B9_S2_Sum + B10_S2_Sum + B11_S2_Sum + B12_S2_Sum + B1_S2_Win +
                   B2_S2_Win + B3_S2_Win + B4_S2_Win + B5_S2_Win + B6_S2_Win + B7_S2_Win +
                   B8_S2_Win + B8A_S2_Win + B9_S2_Win + B10_S2_Win + B11_S2_Win + B12_S2_Win+
                   NDVI_Sum  +NDVI_Win+Dose+TotCount+K+Ur+Th, 
                  data = Points)

# Stepwise regression model
step.model <- stepAIC(full.model, direction = "both", 
                      trace = TRUE)
a<-summary(step.model)
a$AIC


library(caret)
library(leaps)
# Set seed for reproducibility
set.seed(123)
# Set up repeated k-fold cross-validation
train.control <- trainControl(method = "cv", number = 10)
# Train the model

step.model <- caret::train(Peat_Depth ~ DSM + slope + aspect + SinAspect + CosAspect + roughness +
                             TPI + TRI + TWI +  VV_p95 + VV_p5 + VH_p95 + VH_p5 + B1_S2_Sum + B2_S2_Sum +
                             B3_S2_Sum + B5_S2_Sum + B6_S2_Sum + B7_S2_Sum + B8_S2_Sum + 
                             B8A_S2_Sum + B9_S2_Sum + B10_S2_Sum + B11_S2_Sum + B12_S2_Sum + B1_S2_Win +
                             B2_S2_Win + B3_S2_Win + B4_S2_Win + B5_S2_Win + B6_S2_Win + B7_S2_Win +
                             B8_S2_Win + B8A_S2_Win + B9_S2_Win + B10_S2_Win + B11_S2_Win + B12_S2_Win+
                             NDVI_Sum  + NDVI_Win + Dose+ TotCount + K + Ur + Th, 
                           data = Points,
                           method = "leapForward",  #other options "leapBackward", "leapForward" or "leapSeq" (stepwise)
                           tuneGrid = data.frame(nvmax = 1:5), #max number of predictors
                           trControl = train.control
)

step.model$results
step.model$bestTune
summary(step.model$finalModel)

library(nlme)
#to get AIC
b.model <- lme(fixed = Peat_DepthL ~DSM + slope + aspect + SinAspect + CosAspect  ,
               data = Points, random = ~ 1 | dummy, method = "ML") 
summary(b.model)

