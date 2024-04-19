library(doParallel)   # Foreach Parallel Adapter for the parallel Package
library(caret)
library(tidyverse)
library(rfinterval)

install.packages("randomForest")
library(randomForest)

#load("C:/LocalData/sp991/GEOM184/CodeForMSc/Final/Inputs/Points.RData")

#numerical calculation of NDVI from S2_SumPoints
#NDVI_Sum_df <-  ((S2_SumPoints_df$Bodmin_S2_Sum_clipped_5-S2_SumPoints_df$Bodmin_S2_Sum_clipped_4)/(S2_SumPoints_df$Bodmin_S2_Sum_clipped_5+S2_SumPoints_df$Bodmin_S2_Sum_clipped_4))
#NDVI_Win_df <- ((S2_WinPoints_df$Bodmin_S2_Win_clipped_5-S2_WinPoints_df$Bodmin_S2_Win_clipped_4)/(S2_WinPoints_df$Bodmin_S2_Win_clipped_5+S2_WinPoints_df$Bodmin_S2_Win_clipped_4))

# Create data frames for NDVI_Sum and NDVI_Win
#NDVI_Sum_df <- as.data.frame(NDVI_Sum_df)
#NDVI_Win_df <- as.data.frame(NDVI_Win_df)

#Add ID column to df
#NDVI_Sum_df<- tibble::rowid_to_column(NDVI_Sum_df, "ID")
#NDVI_Win_df<- tibble::rowid_to_column(NDVI_Win_df, "ID")

#add NDVI data to 'Points'
#Points <- full_join(Points, NDVI_Sum_df, by = "ID")
#Points <- full_join(Points, NDVI_Win_df, by = "ID")

#Peat_Depth <- Points$Peat_Depth

# Add the calculated NDVI values to the Points data frame
#Points <- mutate(Points,
#                 Peat_DepthL = log(Peat_Depth + 1))

#Points<-Points%>%
#  mutate(Peat_DepthL = log(Peat_Depth+1),
#         NDVI_Win = ((B5_S2_Win-B4_S2_Win)/(B5_S2_Win+B4_S2_Win)),
#         NDVI_Sum = ((B5_S2_Sum-B4_S2_Sum)/(B5_S2_Sum+B4_S2_Sum)))

set.seed(123)
# Set up repeated k-fold cross-validation
train.control <- trainControl(method = "cv", number = 10)
cores=detectCores()

#full model
cl <- makePSOCKcluster(cores[1]-1) #not to overload your computer
registerDoParallel(cl)


stopCluster(cl)

modFit_rf$results

#vector version of random forest
rf <-randomForest(Peat_Depth  ~ Bodmin_DSM_clipped + slope + aspect + SinAspect + CosAspect + roughness +
                    TPI + TRI + Bodmin_TWI_Final + Bodmin_S1_clipped + Bodmin_S2_Sum_clipped_1 + Bodmin_S2_Sum_clipped_2 +
                    Bodmin_S2_Sum_clipped_3 + Bodmin_S2_Sum_clipped_4 + Bodmin_S2_Sum_clipped_5 + Bodmin_S2_Sum_clipped_6 + Bodmin_S2_Sum_clipped_7 + Bodmin_S2_Sum_clipped_8 + 
                    Bodmin_S2_Sum_clipped_9 + Bodmin_S2_Sum_clipped_10 + Bodmin_S2_Sum_clipped_11 + Bodmin_S2_Sum_clipped_12 + Bodmin_S2_Win_clipped_1 +
                    Bodmin_S2_Win_clipped_2 + Bodmin_S2_Win_clipped_3 + Bodmin_S2_Win_clipped_4 + Bodmin_S2_Win_clipped_5 + Bodmin_S2_Win_clipped_6 + Bodmin_S2_Win_clipped_7 +
                    Bodmin_S2_Win_clipped_8 + Bodmin_S2_Win_clipped_9 + Bodmin_S2_Win_clipped_10 + Bodmin_S2_Win_clipped_11 + Bodmin_S2_Win_clipped_12 +
                    NDVI_Sum_df + NDVI_Win_df + Dose + TotCount +K + Ur + Th,
                    data = Points,
                    ntree=500,
                    mtry = 8) 

I<- as.data.frame(importance(rf))

varImpPlot(rf, main = "")



###Exp peat depth final model
cores=detectCores()
cl <- makePSOCKcluster(cores[1]-1) #not to overload your computer
registerDoParallel(cl)

modFit_rf <- caret::train(Peat_DepthL  ~ Bodmin_DSM_clipped + 
                            TRI +   Bodmin_S1_clipped + Bodmin_S2_Sum_clipped_1 + Bodmin_S2_Sum_clipped_2 +
                            Bodmin_S2_Sum_clipped_3 +  Bodmin_S2_Sum_clipped_5 + 
                            Bodmin_S2_Sum_clipped_9 + Bodmin_S2_Sum_clipped_10 + Bodmin_S2_Sum_clipped_12 + Bodmin_S2_Win_clipped_1 +
                            Bodmin_S2_Win_clipped_4 + 
                            Bodmin_S2_Win_clipped_9 + Bodmin_S2_Win_clipped_10 + Bodmin_S2_Win_clipped_12+
                            NDVI_Win_df + TotCount + K + Ur + Th,
                          method = "rf",
                          data = Points,
                          metric = "RMSE",
                          tuneGrid = expand.grid(.mtry=c(1:15)),
                          trControl=train.control)
stopCluster(cl)

modFit_rf$results
saveRDS(modFit_rf, "C:/LocalData/sp991/GEOM184/CodeForMSc/Final/Inputs/RF_model.rds")


#predicting peat depth
rasterdataB<- raster::brick("C:/LocalData/sp991/GEOM184/CodeForMSc/Final/Inputs/PeatRasters.tif")
cl2 <- makePSOCKcluster(cores[1]-1) #not to overload your computer
registerDoParallel(cl2)

names(rasterdataB)<- c("Bodmin_DSM_clipped", "aspect", "CosAspect", "SinAspect", "slope", "roughness", 
                      "TPI", "TRI", "Bodmin_TWI_Final", "Bodmin_S1_clipped", "Bodmin_S2_Sum_clipped_1", "Bodmin_S2_Sum_clipped_2", "Bodmin_S2_Sum_clipped_3", "Bodmin_S2_Sum_clipped_4", "Bodmin_S2_Sum_clipped_5", "Bodmin_S2_Sum_clipped_6", 
                      "Bodmin_S2_Sum_clipped_7", "Bodmin_S2_Sum_clipped_8", "Bodmin_S2_Sum_clipped_9", "Bodmin_S2_Sum_clipped_10", "Bodmin_S2_Sum_clipped_11", 
                      "Bodmin_S2_Sum_clipped_12", "Bodmin_S2_Win_clipped_1", "Bodmin_S2_Win_clipped_2", "Bodmin_S2_Win_clipped_3", "Bodmin_S2_Win_clipped_4", "Bodmin_S2_Win_clipped_5", 
                      "Bodmin_S2_Win_clipped_6", "Bodmin_S2_Win_clipped_7", "Bodmin_S2_Win_clipped_8", "Bodmin_S2_Win_clipped_9", "Bodmin_S2_Win_clipped_10", 
                      "Bodmin_S2_Win_clipped_11", "Bodmin_S2_Win_clipped_12", "Dose", "TotCount", "Th", "K","Ur","NDVI_Sum_df", "NDVI_Win_df")


preds_rf <- predict(rasterdataB, model = modFit_rf$finalModel)
PeatDepth <- exp(preds_rf)-1
stopCluster(cl2)

writeRaster(preds_rf, "C:/LocalData/sp991/GEOM184/CodeForMSc/Final/Inputs/preds_rf.tif")
writeRaster(PeatDepth, "C:/LocalData/sp991/GEOM184/CodeForMSc/Final/Inputs/ModPeatDepth.tif")


#assessing uncertainty
Points1 <- Points%>%
  select(c(Peat_DepthL, Bodmin_DSM_clipped, TRI, Bodmin_S1_clipped, Bodmin_S2_Sum_clipped_1, Bodmin_S2_Sum_clipped_2, Bodmin_S2_Sum_clipped_3,
           Bodmin_S2_Sum_clipped_5, Bodmin_S2_Sum_clipped_9, Bodmin_S2_Sum_clipped_10, Bodmin_S2_Sum_clipped_12, Bodmin_S2_Win_clipped_1, Bodmin_S2_Win_clipped_4, Bodmin_S2_Win_clipped_9,
           Bodmin_S2_Win_clipped_10, Bodmin_S2_Win_clipped_12, NDVI_Win_df, TotCount, K, Ur, Th, Training))

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
