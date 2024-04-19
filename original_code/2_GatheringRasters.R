library(terra)
library(sf)
library(tidyverse)

S1 <- rast("D:NfCPGS/BodminPeatDepth/S1/S1_StAustellBox.tif")

peatdepthpoints<- st_read("D:/NfCPGS/BodminPeatDepth/ProbedPeatDepths.shp")
peatdepthpoints <- st_transform(peatdepthpoints, crs = crs(S1))
xy<- data.frame(st_coordinates(peatdepthpoints))
xy<-xy[,1:2]

S1Points<- terra::extract(S1, xy, method = "simple", fun = mean, bind = TRUE)

S2_Sum <- rast("D:NfCPGS/BodminPeatDepth/S2/S2_Sum_Bodmin.tif")
S2_SumPoints<- terra::extract(S2_Sum, xy, method = "simple", fun = mean, bind = TRUE)

S2_Win <- rast("D:NfCPGS/BodminPeatDepth/S2/S2_Win_Bodmin.tif")
S2_WinPoints<- terra::extract(S2_Win, xy, method = "simple", fun = mean, bind = TRUE)

# 10 m DSM's
DSM <- rast("D:NfCPGS/BodminPeatDepth/LZ_LiDAR/BodminLZ_10m.tif")
DSMPoints<- terra::extract(DSM, xy, method = "simple", fun = mean, bind = TRUE)%>%
  rename(., DSM = StAustelLZ_2m)

Slope<- terrain(DSM_B, v= "slope", neighbors = 8, unit = "degrees")
SlopePoints<- terra::extract(Slope, xy, method = "simple", fun = mean, bind = TRUE)

Aspect<- terrain(DSM_B, v= "aspect", neighbors = 8, unit = "degrees")
AspectPoints<- terra::extract(Aspect, xy, method = "simple", fun = mean, bind = TRUE)
remove(Aspect_A, Aspect_B, Aspect_C, Aspect_L)

SinAspect <- sin((Aspect*pi/180))
CosAspect = cos((Aspect*pi/180))

AspectPoints<- AspectPoints%>%
  mutate(SinAspect = sin((aspect*pi/180)),
         CosAspect = cos((aspect*pi/180)))

# Terrain Ruggedness Index
TRI<- terrain(DSM_B, v= "TRI", neighbors = 8)
TRIPoints<- terra::extract(TRI, xy, method = "simple", fun = mean, bind = TRUE)

# Topographic Position Index
TPI<- terrain(DSM_B, v= "TPI", neighbors = 8)
TPIPoints<- terra::extract(TPI, xy, method = "simple", fun = mean, bind = TRUE)

#roughness
Roughness<- terrain(DSM_B, v= "roughness", neighbors = 8)
RoughnessPoints<- terra::extract(Roughness, xy, method = "simple", fun = mean, bind = TRUE)

# Topographic Wetness Index -calculated using GRASS r.topidx in QGIS
TWI_B <- rast("D:NfCPGS/BodminPeatDepth/TWI/TWI_Bodmin.tif")
TWI <- resample(TWI_B, S1, method = "near")
TWIPoints<- terra::extract(TWI, xy, method = "simple", fun = mean, bind = TRUE)%>%  
  rename(., TWI = TWI_StAustell)

#Radiometric Dose
Dose<- rast("D:NfCPGS/BodminPeatDepth/rad/Dose_B.tif")
DosePoints<- terra::extract(Dose, xy, method = "simple", fun = mean, bind = TRUE)
remove(Dose_A, Dose_B, Dose_C, Dose_L)

#Total Counts
Total_Counts<- rast("D:NfCPGS/BodminPeatDepth/rad/Tot_Count_B.tif")
Total_CountsPoints<- terra::extract(Total_Counts, xy, method = "simple", fun = mean, bind = TRUE)

#K
K<- rast("D:NfCPGS/BodminPeatDepth/rad/K_B.tif")
KPoints<- terra::extract(K, xy, method = "simple", fun = mean, bind = TRUE)

#Ur
Ur<- rast("D:NfCPGS/BodminPeatDepth/rad/Ur_B.tif")
UrPoints<- terra::extract(Ur, xy, method = "simple", fun = mean, bind = TRUE)

#Th
Th<- rast("D:NfCPGS/BodminPeatDepth/rad/Th_B.tif")
ThPoints<- terra::extract(Th, xy, method = "simple", fun = mean, bind = TRUE)

peatdepthpoints <- tibble::rowid_to_column(peatdepthpoints, "ID")
xyID<- tibble::rowid_to_column(xy, "ID")

Points <- full_join(peatdepthpoints, xyID, by = "ID")
Points <- full_join(Points,DSMPoints, by = "ID")
Points <- full_join(Points, SlopePoints, by = "ID")
Points <- full_join(Points, AspectPoints, by = "ID")
Points <- full_join(Points, RoughnessPoints, by = "ID")
Points <- full_join(Points, TPIPoints, by = "ID")
Points <- full_join(Points, TRIPoints, by = "ID")
Points <- full_join(Points, TWIPoints, by = "ID")
Points <- full_join(Points, S1Points, by = "ID")
Points <- full_join(Points, S2_SumPoints, by = "ID")
Points <- full_join(Points, S2_WinPoints, by = "ID", suffix = c("_S2_Sum", "_S2_Win"))
Points <- full_join(Points, DosePoints, by = "ID")
Points <- full_join(Points, Total_CountsPoints, by = "ID")
Points <- full_join(Points, KPoints, by = "ID")
Points <- full_join(Points, UrPoints, by = "ID")
Points <- full_join(Points, ThPoints, by = "ID")

Points <- Points%>%
  rename(., Dose = radPoints_tm_A.Dose)%>%
  rename(., K = radPoints_tm_A.K)%>%
  rename(., Ur = radPoints_tm_A.Ur)%>%
  rename(., Th = radPoints_tm_A.Th)%>%
  rename(., TotCount = radPoints_tm_A.Tot_Count)

dummy <- rep(1, 560) 
Points <- cbind(Points, dummy)  

#save(Points, file = "C:/Users/ng292/OneDrive - University of Exeter/NfCPGS/PeatMapping/Points.RData")
#load("C:/Users/ng292/OneDrive - University of Exeter/NfCPGS/PeatMapping/Points.RData")


NDVI_Sum <-  ((S2_Sum$B5-S2_Sum$B4)/(S2_Sum$B5+S2_Sum$B4))
NDVI_Win <- ((S2_Win$B5-S2_Win$B4)/(S2_Win$B5+S2_Win$B4))
  
rasterdata<- rast(list(DSM, Aspect, CosAspect, SinAspect, Slope, Roughness, TPI, TRI, TWI, S1, S2_Sum, S2_Win,
                         Dose, Total_Counts, Th, K, Ur, NDVI_Sum, NDVI_Win))

remove(DSM, Aspect, CosAspect, SinAspect, Slope, Roughness, TPI, TRI, TWI, S1, S2_Sum, S2_Win,
        Dose, Total_Counts, Th, K, Ur, NDVI_Sum, NDVI_Win)

names(rasterdata)<- c("DSM", "aspect", "CosAspect", "SinAspect", "slope", "roughness", 
                      "TPI", "TRI", "TWI", "VV_p95", "VV_p5",  "VH_p95", "VH_p5",
                      "B1_S2_Sum", "B2_S2_Sum", "B3_S2_Sum", "B4_S2_Sum", "B5_S2_Sum", "B6_S2_Sum", 
                      "B7_S2_Sum", "B8_S2_Sum", "B8A_S2_Sum", "B9_S2_Sum", "B10_S2_Sum", "B11_S2_Sum", 
                      "B12_S2_Sum", "B1_S2_Win", "B2_S2_Win", "B3_S2_Win", "B4_S2_Win", "B5_S2_Win", 
                      "B6_S2_Win", "B7_S2_Win", "B8_S2_Win", "B8A_S2_Win", "B9_S2_Win", "B10_S2_Win", 
                      "B11_S2_Win", "B12_S2_Win", "Dose", "TotCount", "Th", "K","Ur","NDVI_Sum", "NDVI_Win")

#writeRaster(rasterdata, "C:/Users/ng292/OneDrive - University of Exeter/NfCPGS/PeatMapping/PeatRasters.tif")
