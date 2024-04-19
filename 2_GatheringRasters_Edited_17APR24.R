library(terra)
library(sf)
library(tidyverse)

S1 <- rast("C:/LocalData/sp991/GEOM184/CodeForMSc/Final/Inputs/Bodmin_S1_clipped.tif")
DSM_B <- rast("C:/LocalData/sp991/GEOM184/CodeForMSc/Final/Inputs/Bodmin_DSM_clipped.tif")

peatdepthpoints<- st_read("C:/LocalData/sp991/GEOM184/CodeForMSc/Inputs/Project_B/Bodmin_Probed_Peat_Depths.shp")
peatdepthpoints <- st_transform(peatdepthpoints, crs = crs(S1))
xy<- data.frame(st_coordinates(peatdepthpoints))
xy<-xy[,1:2]

S1Points<- terra::extract(S1, xy, method = "simple", fun = mean, bind = TRUE)

S2_Sum <- rast("C:/LocalData/sp991/GEOM184/CodeForMSc/Final/Inputs/Bodmin_S2_Sum_Resampled10m.tif")
S2_SumPoints<- terra::extract(S2_Sum, xy, method = "simple", fun = mean, bind = TRUE)

S2_Win <- rast("C:/LocalData/sp991/GEOM184/CodeForMSc/Final/Inputs/Bodmin_S2_Win_Resampled10m.tif")
S2_WinPoints<- terra::extract(S2_Win, xy, method = "simple", fun = mean, bind = TRUE)

# 10 m DSM's
DSM <- rast("C:/LocalData/sp991/GEOM184/CodeForMSc/Final/Inputs/Bodmin_DSM_Resampled10m.tif")
DSMPoints<- terra::extract(DSM, xy, method = "simple", fun = mean, bind = TRUE)

Slope<- terrain(DSM_B, v= "slope", neighbors = 8, unit = "degrees")
SlopePoints<- terra::extract(Slope, xy, method = "simple", fun = mean, bind = TRUE)

Aspect<- terrain(DSM_B, v= "aspect", neighbors = 8, unit = "degrees")
AspectPoints<- terra::extract(Aspect, xy, method = "simple", fun = mean, bind = TRUE)
remove(Aspect_A, Aspect_B, Aspect_C, Aspect_L)

SinAspect <- sin((Aspect*pi/180))
CosAspect = cos((Aspect*pi/180))

#AspectPoints<- AspectPoints%>%
  #mutate(SinAspect = sin((aspect*pi/180)),
         #CosAspect = cos((aspect*pi/180)))

# Calculate sine and cosine of aspect angle (in degrees)
AspectPoints$SinAspect <- sin((AspectPoints$aspect * pi / 180))
AspectPoints$CosAspect <- cos((AspectPoints$aspect * pi / 180))


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
TWI_B <- rast("C:/LocalData/sp991/GEOM184/CodeForMSc/Final/Inputs/Bodmin_TWI_Final.tif")
TWI <- resample(TWI_B, S1, method = "near")
TWIPoints<- terra::extract(TWI, xy, method = "simple", fun = mean, bind = TRUE)  
#  rename(., TWI = TWI_StAustell)

#Radiometric Dose
Dose<- rast("C:/LocalData/sp991/GEOM184/CodeForMSc/Inputs/Project_B/Dose_B.tif")
DosePoints<- terra::extract(Dose, xy, method = "simple", fun = mean, bind = TRUE)
remove(Dose_A, Dose_B, Dose_C, Dose_L)

#Total Counts
Total_Counts<- rast("C:/LocalData/sp991/GEOM184/CodeForMSc/Inputs/Project_B/Tot_Count_B.tif")
Total_CountsPoints<- terra::extract(Total_Counts, xy, method = "simple", fun = mean, bind = TRUE)

#K
K<- rast("C:/LocalData/sp991/GEOM184/CodeForMSc/Inputs/Project_B/K_B.tif")
KPoints<- terra::extract(K, xy, method = "simple", fun = mean, bind = TRUE)

#Ur
Ur<- rast("C:/LocalData/sp991/GEOM184/CodeForMSc/Inputs/Project_B/Ur_B.tif")
UrPoints<- terra::extract(Ur, xy, method = "simple", fun = mean, bind = TRUE)

#Th
Th<- rast("C:/LocalData/sp991/GEOM184/CodeForMSc/Inputs/Project_B/Th_B.tif")
ThPoints<- terra::extract(Th, xy, method = "simple", fun = mean, bind = TRUE)

peatdepthpoints <- tibble::rowid_to_column(peatdepthpoints, "ID")
xyID<- tibble::rowid_to_column(xy, "ID")

library(tibble)

# Define the list of variables to convert to data frames
variables <- c("DSMPoints", "SlopePoints", "AspectPoints", "RoughnessPoints", 
               "TPIPoints", "TRIPoints", "TWIPoints", "S1Points", 
               "S2_SumPoints", "S2_WinPoints", "DosePoints", 
               "Total_CountsPoints", "KPoints", "UrPoints", "ThPoints")

# Iterate over the list of variables and convert them to data frames
for (var in variables) {
  assign(paste0(var, "_df"), as.data.frame(get(var)))
}

DSMPoints_df<- tibble::rowid_to_column(DSMPoints_df, "ID")
SlopePoints_df<- tibble::rowid_to_column(SlopePoints_df, "ID")
AspectPoints_df<- tibble::rowid_to_column(AspectPoints_df, "ID")
RoughnessPoints_df<- tibble::rowid_to_column(RoughnessPoints_df, "ID")
TPIPoints_df<- tibble::rowid_to_column(TPIPoints_df, "ID")
TRIPoints_df<- tibble::rowid_to_column(TRIPoints_df, "ID")
TWIPoints_df<- tibble::rowid_to_column(TWIPoints_df, "ID")
S1Points_df<- tibble::rowid_to_column(S1Points_df, "ID")
S2_SumPoints_df<- tibble::rowid_to_column(S2_SumPoints_df, "ID")
S2_WinPoints_df<- tibble::rowid_to_column(S2_WinPoints_df, "ID")
DosePoints_df<- tibble::rowid_to_column(DosePoints_df, "ID")
Total_CountsPoints_df<- tibble::rowid_to_column(Total_CountsPoints_df, "ID")
KPoints_df<- tibble::rowid_to_column(KPoints_df, "ID")
UrPoints_df<- tibble::rowid_to_column(UrPoints_df, "ID")
ThPoints_df<- tibble::rowid_to_column(ThPoints_df, "ID")

Points <- full_join(peatdepthpoints, xyID, by = "ID")
Points <- full_join(Points, DSMPoints_df, by = "ID")
Points <- full_join(Points, SlopePoints_df, by = "ID")
Points <- full_join(Points, AspectPoints_df, by = "ID")
Points <- full_join(Points, RoughnessPoints_df, by = "ID")
Points <- full_join(Points, TPIPoints_df, by = "ID")
Points <- full_join(Points, TRIPoints_df, by = "ID")
Points <- full_join(Points, TWIPoints_df, by = "ID")
Points <- full_join(Points, S1Points_df, by = "ID")
Points <- full_join(Points, S2_SumPoints_df, by = "ID")
Points <- full_join(Points, S2_WinPoints_df, by = "ID", suffix = c("_S2_Sum", "_S2_Win"))
Points <- full_join(Points, DosePoints_df, by = "ID")
Points <- full_join(Points, Total_CountsPoints_df, by = "ID")
Points <- full_join(Points, KPoints_df, by = "ID")
Points <- full_join(Points, UrPoints_df, by = "ID")
Points <- full_join(Points, ThPoints_df, by = "ID")


Points <- Points%>%
  rename(., Dose = radPoints_tm.Dose)%>%
  rename(., K = radPoints_tm.K)%>%
  rename(., Ur = radPoints_tm.Ur)%>%
  rename(., Th = radPoints_tm.Th)%>%
  rename(., TotCount = radPoints_tm.Tot_Count)

dummy <- rep(1, 370) 
Points <- cbind(Points, dummy)  

save(Points, file = "C:/LocalData/sp991/GEOM184/CodeForMSc/Inputs/Project_B/Points.RData")
#load("C:/LocalData/sp991/GEOM184/CodeForMSc/Inputs/Project_B/Points.RData")

# Convert S2_Sum to a dataframe
#S2_Sum_df <- terra::as.data.frame(S2_Sum)
#S2_Win_df <- terra::as.data.frame(S2_Win)

# Rename columns
names(S2_Sum)[names(S2_Sum) == "Bodmin_S2_Sum_clipped_1"] <- "B1"
names(S2_Sum)[names(S2_Sum) == "Bodmin_S2_Sum_clipped_2"] <- "B2"
names(S2_Sum)[names(S2_Sum) == "Bodmin_S2_Sum_clipped_3"] <- "B3"
names(S2_Sum)[names(S2_Sum) == "Bodmin_S2_Sum_clipped_4"] <- "B4"
names(S2_Sum)[names(S2_Sum) == "Bodmin_S2_Sum_clipped_5"] <- "B5"
names(S2_Sum)[names(S2_Sum) == "Bodmin_S2_Sum_clipped_6"] <- "B6"
names(S2_Sum)[names(S2_Sum) == "Bodmin_S2_Sum_clipped_7"] <- "B7"
names(S2_Sum)[names(S2_Sum) == "Bodmin_S2_Sum_clipped_8"] <- "B8"
names(S2_Sum)[names(S2_Sum) == "Bodmin_S2_Sum_clipped_9"] <- "B8A"
names(S2_Sum)[names(S2_Sum) == "Bodmin_S2_Sum_clipped_10"] <- "B9"
names(S2_Sum)[names(S2_Sum) == "Bodmin_S2_Sum_clipped_11"] <- "B11"
names(S2_Sum)[names(S2_Sum) == "Bodmin_S2_Sum_clipped_12"] <- "B12"

names(S2_Win)[names(S2_Win) == "Bodmin_S2_Win_clipped_1"] <- "B1"
names(S2_Win)[names(S2_Win) == "Bodmin_S2_Win_clipped_2"] <- "B2"
names(S2_Win)[names(S2_Win) == "Bodmin_S2_Win_clipped_3"] <- "B3"
names(S2_Win)[names(S2_Win) == "Bodmin_S2_Win_clipped_4"] <- "B4"
names(S2_Win)[names(S2_Win) == "Bodmin_S2_Win_clipped_5"] <- "B5"
names(S2_Win)[names(S2_Win) == "Bodmin_S2_Win_clipped_6"] <- "B6"
names(S2_Win)[names(S2_Win) == "Bodmin_S2_Win_clipped_7"] <- "B7"
names(S2_Win)[names(S2_Win) == "Bodmin_S2_Win_clipped_8"] <- "B8"
names(S2_Win)[names(S2_Win) == "Bodmin_S2_Win_clipped_9"] <- "B8A"
names(S2_Win)[names(S2_Win) == "Bodmin_S2_Win_clipped_10"] <- "B9"
names(S2_Win)[names(S2_Win) == "Bodmin_S2_Win_clipped_11"] <- "B11"
names(S2_Win)[names(S2_Win) == "Bodmin_S2_Win_clipped_12"] <- "B12"

NDVI_Sum <-  ((S2_Sum$B5-S2_Sum$B4)/(S2_Sum$B5+S2_Sum$B4))
NDVI_Win <- ((S2_Win$B5-S2_Win$B4)/(S2_Win$B5+S2_Win$B4))

#Resample all files to same resolution
DSM <- terra::resample(DSM, Aspect)
CosAspect <- terra::resample(CosAspect, Aspect)
SinAspect <- terra::resample(SinAspect, Aspect)
Slope <- terra::resample(Slope, Aspect)
Roughness <- terra::resample(Roughness, Aspect)
TPI <- terra::resample(TPI, Aspect)
TRI <- terra::resample(TRI, Aspect)
TWI <- terra::resample(TWI, Aspect)
S1 <- terra::resample(S1, Aspect)
S2_Sum <- terra::resample(S2_Sum, Aspect)
S2_Win <- terra::resample(S2_Win, Aspect)
Dose <- terra::resample(Dose, Aspect)   
Total_Counts <- terra::resample(Total_Counts, Aspect)
Th <- terra::resample(Th, Aspect)
K <- terra::resample(K, Aspect)
Ur <- terra::resample(Ur, Aspect)
NDVI_Sum <- terra::resample(NDVI_Sum, Aspect)
NDVI_Win <- terra::resample(NDVI_Win, Aspect)

rasterdata<- rast(list(DSM, Aspect, CosAspect, SinAspect, Slope, Roughness, TPI, TRI, TWI, S1, S2_Sum, S2_Win,
                         Dose, Total_Counts, Th, K, Ur, NDVI_Sum, NDVI_Win))

#rasterdataTEST<- rast(list(DSM, Aspect, CosAspect, SinAspect, Slope, Roughness, TPI, TRI, S1, S2_Sum, S2_Win,
                           #Dose, Total_Counts, Th, K, Ur, NDVI_Sum, NDVI_Win))


remove(DSM, Aspect, CosAspect, SinAspect, Slope, Roughness, TPI, TRI, TWI, S1, S2_Sum, S2_Win,
        Dose, Total_Counts, Th, K, Ur) #NDVI_Sum, NDVI_Win)

names(rasterdata)<- c("DSM", "aspect", "CosAspect", "SinAspect", "slope", "roughness", 
                      "TPI", "TRI", "TWI", "S1", "B1_S2_Sum", "B2_S2_Sum", "B3_S2_Sum", "B4_S2_Sum", "B5_S2_Sum", "B6_S2_Sum", 
                      "B7_S2_Sum", "B8_S2_Sum", "B8A_S2_Sum", "B9_S2_Sum", "B11_S2_Sum", 
                      "B12_S2_Sum", "B1_S2_Win", "B2_S2_Win", "B3_S2_Win", "B4_S2_Win", "B5_S2_Win", 
                      "B6_S2_Win", "B7_S2_Win", "B8_S2_Win", "B8A_S2_Win", "B9_S2_Win", 
                      "B11_S2_Win", "B12_S2_Win", "Dose", "TotCount", "Th", "K","Ur","NDVI_Sum", "NDVI_Win")

writeRaster(rasterdata, "C:/LocalData/sp991/GEOM184/CodeForMSc/Final/Inputs/PeatRasters.tif")
