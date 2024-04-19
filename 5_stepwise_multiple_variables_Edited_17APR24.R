
#Load("C:/LocalData/sp991/GEOM184/CodeForMSc/Final/Inputs/Points.RData")

#Points<-Points%>%
#  mutate(Peat_DepthL = log(Peat_Depth+1),
#         NDVI_Win = ((B5_S2_Win-B4_S2_Win)/(B5_S2_Win+B4_S2_Win)),
#         NDVI_Sum = ((B5_S2_Sum-B4_S2_Sum)/(B5_S2_Sum+B4_S2_Sum)))

a.model <- lme(fixed = Peat_Depth ~ Bodmin_DSM_clipped + slope + aspect + SinAspect + CosAspect + roughness +
                 TPI + TRI + Bodmin_TWI_Final + Bodmin_S1_clipped + Bodmin_S2_Sum_clipped_1 + Bodmin_S2_Sum_clipped_2 +
                 Bodmin_S2_Sum_clipped_3 + Bodmin_S2_Sum_clipped_4 + Bodmin_S2_Sum_clipped_5 + Bodmin_S2_Sum_clipped_6 + Bodmin_S2_Sum_clipped_7 + Bodmin_S2_Sum_clipped_8 + 
                 Bodmin_S2_Sum_clipped_9 + Bodmin_S2_Sum_clipped_10 + Bodmin_S2_Sum_clipped_11 + Bodmin_S2_Sum_clipped_12 + Bodmin_S2_Win_clipped_1 +
                 Bodmin_S2_Win_clipped_2 + Bodmin_S2_Win_clipped_3 + Bodmin_S2_Win_clipped_4 + Bodmin_S2_Win_clipped_5 + Bodmin_S2_Win_clipped_6 + Bodmin_S2_Win_clipped_7 +
                 Bodmin_S2_Win_clipped_8 + Bodmin_S2_Win_clipped_9 + Bodmin_S2_Win_clipped_10 + Bodmin_S2_Win_clipped_11 + Bodmin_S2_Win_clipped_12 +
                 NDVI_Sum_df + NDVI_Win_df + Dose + TotCount +K + Ur + Th, data = Points, 
               random = ~ 1 | dummy, method = "ML") 

summary(a.model)



b.model <- lme(fixed = Peat_DepthL ~ Bodmin_DSM_clipped + slope + aspect + SinAspect + CosAspect + roughness +
                 TPI + TRI + Bodmin_TWI_Final + Bodmin_S1_clipped + Bodmin_S2_Sum_clipped_1 + Bodmin_S2_Sum_clipped_2 +
                 Bodmin_S2_Sum_clipped_3 + Bodmin_S2_Sum_clipped_4 + Bodmin_S2_Sum_clipped_5 + Bodmin_S2_Sum_clipped_6 + Bodmin_S2_Sum_clipped_7 + Bodmin_S2_Sum_clipped_8 + 
                 Bodmin_S2_Sum_clipped_9 + Bodmin_S2_Sum_clipped_10 + Bodmin_S2_Sum_clipped_11 + Bodmin_S2_Sum_clipped_12 + Bodmin_S2_Win_clipped_1 +
                 Bodmin_S2_Win_clipped_2 + Bodmin_S2_Win_clipped_3 + Bodmin_S2_Win_clipped_4 + Bodmin_S2_Win_clipped_5 + Bodmin_S2_Win_clipped_6 + Bodmin_S2_Win_clipped_7 +
                 Bodmin_S2_Win_clipped_8 + Bodmin_S2_Win_clipped_9 + Bodmin_S2_Win_clipped_10 + Bodmin_S2_Win_clipped_11 + Bodmin_S2_Win_clipped_12 +
                 NDVI_Sum_df + NDVI_Win_df + Dose + TotCount +K + Ur + Th, data = Points, random = ~ 1 | dummy, method = "ML") 

summary(b.model)

#stepwise linear regression
library(MASS)
# Fit the full model 
full.model <- lm(Peat_DepthL ~ Bodmin_DSM_clipped + slope + aspect + SinAspect + CosAspect + roughness +
                   TPI + TRI + Bodmin_TWI_Final + Bodmin_S1_clipped + Bodmin_S2_Sum_clipped_1 + Bodmin_S2_Sum_clipped_2 +
                   Bodmin_S2_Sum_clipped_3 + Bodmin_S2_Sum_clipped_4 + Bodmin_S2_Sum_clipped_5 + Bodmin_S2_Sum_clipped_6 + Bodmin_S2_Sum_clipped_7 + Bodmin_S2_Sum_clipped_8 + 
                   Bodmin_S2_Sum_clipped_9 + Bodmin_S2_Sum_clipped_10 + Bodmin_S2_Sum_clipped_11 + Bodmin_S2_Sum_clipped_12 + Bodmin_S2_Win_clipped_1 +
                   Bodmin_S2_Win_clipped_2 + Bodmin_S2_Win_clipped_3 + Bodmin_S2_Win_clipped_4 + Bodmin_S2_Win_clipped_5 + Bodmin_S2_Win_clipped_6 + Bodmin_S2_Win_clipped_7 +
                   Bodmin_S2_Win_clipped_8 + Bodmin_S2_Win_clipped_9 + Bodmin_S2_Win_clipped_10 + Bodmin_S2_Win_clipped_11 + Bodmin_S2_Win_clipped_12 +
                   NDVI_Sum_df + NDVI_Win_df + Dose + TotCount +K + Ur + Th, 
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

step.model <- caret::train(Peat_Depth ~ Bodmin_DSM_clipped + slope + aspect + SinAspect + CosAspect + roughness +
                             TPI + TRI + Bodmin_TWI_Final + Bodmin_S1_clipped + Bodmin_S2_Sum_clipped_1 + Bodmin_S2_Sum_clipped_2 +
                             Bodmin_S2_Sum_clipped_3 + Bodmin_S2_Sum_clipped_4 + Bodmin_S2_Sum_clipped_5 + Bodmin_S2_Sum_clipped_6 + Bodmin_S2_Sum_clipped_7 + Bodmin_S2_Sum_clipped_8 + 
                             Bodmin_S2_Sum_clipped_9 + Bodmin_S2_Sum_clipped_10 + Bodmin_S2_Sum_clipped_11 + Bodmin_S2_Sum_clipped_12 + Bodmin_S2_Win_clipped_1 +
                             Bodmin_S2_Win_clipped_2 + Bodmin_S2_Win_clipped_3 + Bodmin_S2_Win_clipped_4 + Bodmin_S2_Win_clipped_5 + Bodmin_S2_Win_clipped_6 + Bodmin_S2_Win_clipped_7 +
                             Bodmin_S2_Win_clipped_8 + Bodmin_S2_Win_clipped_9 + Bodmin_S2_Win_clipped_10 + Bodmin_S2_Win_clipped_11 + Bodmin_S2_Win_clipped_12 +
                             NDVI_Sum_df + NDVI_Win_df + Dose + TotCount +K + Ur + Th, 
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
b.model <- lme(fixed = Peat_DepthL ~Bodmin_DSM_clipped + slope + aspect + SinAspect + CosAspect  ,
               data = Points, random = ~ 1 | dummy, method = "ML") 
summary(b.model)

