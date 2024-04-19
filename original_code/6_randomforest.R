library(doParallel)   # Foreach Parallel Adapter for the parallel Package
library(caret)
library(tidyverse)
library(rfinterval)

load("C:/Users/ng292/OneDrive - University of Exeter/NfCPGS/PeatMapping/Points.RData")

Points<-Points%>%
  mutate(Peat_DepthL = log(Peat_Depth+1),
         NDVI_Win = ((B5_S2_Win-B4_S2_Win)/(B5_S2_Win+B4_S2_Win)),
         NDVI_Sum = ((B5_S2_Sum-B4_S2_Sum)/(B5_S2_Sum+B4_S2_Sum)))

set.seed(123)
# Set up repeated k-fold cross-validation
train.control <- trainControl(method = "cv", number = 10)
cores=detectCores()

#full model
cl <- makePSOCKcluster(cores[1]-1) #not to overload your computer
registerDoParallel(cl)

modFit_rf <- caret::train(Peat_DepthL  ~ DSM + slope + aspect + SinAspect + CosAspect + roughness +
                            TPI + TRI + TWI +  VV_p95 + VV_p5 + VH_p95 + VH_p5 + B1_S2_Sum + B2_S2_Sum +
                            B3_S2_Sum + + B4_S2_Sum + B5_S2_Sum + B6_S2_Sum + B7_S2_Sum + B8_S2_Sum + 
                            B8A_S2_Sum + B9_S2_Sum + B10_S2_Sum + B11_S2_Sum + B12_S2_Sum + B1_S2_Win +
                            B2_S2_Win + B3_S2_Win + B4_S2_Win + B5_S2_Win + B6_S2_Win + B7_S2_Win +
                            B8_S2_Win + B8A_S2_Win + B9_S2_Win + B10_S2_Win + B11_S2_Win + B12_S2_Win+
                            NDVI_Sum  + NDVI_Win + Dose+ TotCount + K + Ur + Th,
                   method = "rf",
                   data = Points,
                   metric = "RMSE",
                   tuneGrid = expand.grid(.mtry=c(1:15)),
                   trControl=train.control)
stopCluster(cl)

modFit_rf$results

#vector version of random forest
rf <-randomForest(Peat_Depth  ~ DSM + slope + aspect + SinAspect + CosAspect + roughness +
                    TPI + TRI + TWI +  VV_p95 + VV_p5 + VH_p95 + VH_p5 + B1_S2_Sum + B2_S2_Sum +
                    B3_S2_Sum + B5_S2_Sum + B6_S2_Sum + B7_S2_Sum + B8_S2_Sum + 
                    B8A_S2_Sum + B9_S2_Sum + B10_S2_Sum + B11_S2_Sum + B12_S2_Sum + B1_S2_Win +
                    B2_S2_Win + B3_S2_Win + B4_S2_Win + B5_S2_Win + B6_S2_Win + B7_S2_Win +
                    B8_S2_Win + B8A_S2_Win + B9_S2_Win + B10_S2_Win + B11_S2_Win + B12_S2_Win+
                    NDVI_Sum  + NDVI_Win + Dose+ TotCount + K + Ur + Th,
                    data = Points,
                    ntree=500,
                    mtry = 8) 

I<- as.data.frame(importance(rf))

varImpPlot(rf, main = "")



###Exp peat depth final model
cores=detectCores()
cl <- makePSOCKcluster(cores[1]-1) #not to overload your computer
registerDoParallel(cl)

modFit_rf <- caret::train(Peat_DepthL  ~ DSM + 
                            TRI +   VV_p5 + VH_p95 + VH_p5 + B1_S2_Sum + B2_S2_Sum +
                            B3_S2_Sum +  B5_S2_Sum + 
                            B8A_S2_Sum + B9_S2_Sum + B12_S2_Sum + B1_S2_Win +
                            B4_S2_Win + 
                            B8A_S2_Win + B9_S2_Win + B10_S2_Win + B12_S2_Win+
                            NDVI_Win +  TotCount + K + Ur + Th,
                          method = "rf",
                          data = Points,
                          metric = "RMSE",
                          tuneGrid = expand.grid(.mtry=c(1:15)),
                          trControl=train.control)
stopCluster(cl)

modFit_rf$results
saveRDS(modFit_rf, "C:/Users/ng292/OneDrive - University of Exeter/NfCPGS/PeatMapping/RF_model.rds")


#predicting peat depth
rasterdataB<- raster::brick("C:/Users/ng292/OneDrive - University of Exeter/NfCPGS/PeatMapping/PeatRasters.tif")
cl2 <- makePSOCKcluster(cores[1]-1) #not to overload your computer
registerDoParallel(cl2)
preds_rf <- predict(rasterdataB, model = modFit_rf$finalModel)
PeatDepth <- exp(preds_rf)-1
stopCluster(cl2)

writeRaster(preds_rf, "C:/Users/ng292/OneDrive - University of Exeter/NfCPGS/PeatMapping/preds_rf.tif")
writeRaster(PeatDepth, "C:/Users/ng292/OneDrive - University of Exeter/NfCPGS/PeatMapping/ModPeatDepth.tif")


#assessing uncertainty
Points1 <- Points%>%
  select(c(Peat_DepthL, DSM, TRI, VV_p5, VH_p95, VH_p5, B1_S2_Sum, B2_S2_Sum, B3_S2_Sum,
           B5_S2_Sum, B8A_S2_Sum, B9_S2_Sum, B12_S2_Sum, B1_S2_Win, B4_S2_Win, B8A_S2_Win,
           B9_S2_Win, B10_S2_Win, B12_S2_Win, NDVI_Win, TotCount, K, Ur, Th, Training))

train_data <- Points1%>%
      filter(Training == 1)%>%
      select(-c(Training))
train_data<-  st_drop_geometry(train_data)

test_data <- Points1%>%
  filter(Training == 2)%>%
  select(-c(Training))
test_data<-  st_drop_geometry(test_data)


output <-rfinterval(Peat_DepthL~.,
           train_data = train_data, 
           test_data = test_data,
           method = "oob", 
           alpha = 0.05,
           symmetry = FALSE)

y <- test_data$Peat_DepthL
mean(output$oob_interval$lo < y & output$oob_interval$up > y)
