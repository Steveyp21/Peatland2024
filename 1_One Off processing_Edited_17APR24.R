# One off processing to create datasets for peat mapping

library(terra)
library(sf)
library(fields) #for thin plate spline

#origional 2 m DSM's
DSM_B <- rast("C:/LocalData/sp991/GEOM184/CodeForMSc/Final/Inputs/Bodmin_DSM_clipped.tif")

S1_B <- rast("C:/LocalData/sp991/GEOM184/CodeForMSc/Final/Inputs/Bodmin_S1_clipped.tif")

S2_Sum_B <- rast("C:/LocalData/sp991/GEOM184/CodeForMSc/Final/Inputs/Bodmin_S2_Sum_clipped.tif")

S2_Win_B <- rast("C:/LocalData/sp991/GEOM184/CodeForMSc/Final/Inputs/Bodmin_S2_Win_clipped.tif")

#Resample from 2 m resolution to 10 m aligned with S1 and S2 data 
#resample(DSM_B, S1_B, method = "min", filename="C:/LocalData/sp991/GEOM184/CodeForMSc/Final/Inputs/Bodmin_DSM_Resampled10m.tif")
#resample(S2_Sum_B, S1_B, method = "min", filename="C:/LocalData/sp991/GEOM184/CodeForMSc/Final/Inputs/Bodmin_S2_Sum_Resampled10m.tif")
#resample(S2_Win_B, S1_B, method = "min", filename="C:/LocalData/sp991/GEOM184/CodeForMSc/Final/Inputs/Bodmin_S2_Win_Resampled10m.tif")

#### Thin Plate Spline for Radiometric data ####
#radPoint<- st_read("D:NfCPGS/BodminPeatDepth/Rad/RadPoint_B2.shp")
DSM <- rast("C:/LocalData/sp991/GEOM184/CodeForMSc/Final/Inputs/Bodmin_DSM_Resampled10m.tif")

#radPoints_tm <- st_transform(radPoint, crs = crs(DSM))
#xy<- data.frame(st_coordinates(radPoints_tm))
#xy<- data.frame(xy$X, xy$Y)
#x<- rast(DSM)
#remove(radPoint)

#Dose
#v<- data.frame(radPoints_tm$Dose)
#remove(radPoints_tm, DSM)
#tps_dose <- Tps(xy,v)
#remove(xy, v)
#Dose<- interpolate(x, tps_dose)
#writeRaster(Dose, filename = "D:NfCPGS/BodminPeatDepth/Rad/Dose_B.tif")

#Tot_Count
#v1 <- data.frame(radPoints_tm$Tot_Count)
#remove(radPoints_tm, DSM)
#tps_Tot_Count <- Tps(xy,v1)
#remove(xy, v1)
#Tot_Count<- interpolate(x, tps_Tot_Count)
#writeRaster(Tot_Count, filename = "D:NfCPGS/BodminPeatDepth/Rad/Tot_Count_B.tif")

#K
#v2 <- data.frame(radPoints_tm$K)
#remove(radPoints_tm, DSM)
#tps_K <- Tps(xy,v2)
#remove(xy, v2)
#K<- interpolate(x, tps_K)
#writeRaster(K, filename = "D:NfCPGS/BodminPeatDepth/Rad/K_B.tif")

#Ur
#v3 <- data.frame(radPoints_tm$Ur)
#remove(radPoints_tm, DSM)
#tps_Ur <- Tps(xy,v3)
#remove(xy, v3)
#Ur<- interpolate(x, tps_Ur)
#writeRaster(Ur, filename = "D:NfCPGS/BodminPeatDepth/Rad/Ur_B.tif")

#Th
#v4 <- data.frame(radPoints_tm$Th)
#remove(radPoints_tm, DSM)
#tps_Th <- Tps(xy,v4)
#remove(xy, v4)
#Th<- interpolate(x, tps_Th)
#writeRaster(Th, filename = "D:NfCPGS/BodminPeatDepth/Rad/Th_B.tif")










