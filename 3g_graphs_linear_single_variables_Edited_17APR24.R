library(tidyverse)
library(ggplot2)
library(MASS)
library(patchwork)

#load("C:/LocalData/sp991/GEOM184/CodeForMSc/Final/Inputs/Points.RData")

#Points<-Points%>%
  #mutate(Peat_DepthL = log(Peat_Depth+1),
       #  NDVI_Win = ((B5_S2_Win-B4_S2_Win)/(B5_S2_Win+B4_S2_Win)),
        # NDVI_Sum = ((B5_S2_Sum-B4_S2_Sum)/(B5_S2_Sum+B4_S2_Sum)))




K<- ggplot(data = Points, aes(x =K, y = Peat_Depth)) +
  geom_point(colour = "gray30") +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "K (%K)",
       y = "Peat Depth (cm)")+
  theme_classic() 


TotCount<- ggplot(data = Points, aes(x =TotCount, y = Peat_Depth)) +
  geom_point(colour = "gray30") +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Total Count (csp)",
       y = "Peat Depth (cm)")+
  theme_classic() 

Dose<- ggplot(data = Points, aes(x =Dose, y = Peat_Depth)) +
  geom_point(colour = "gray30") +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Dose (nGy/h)",
       y = "Peat Depth (cm)")+
  theme_classic() 

Ur<- ggplot(data = Points, aes(x =Ur, y = Peat_Depth)) +
  geom_point(colour = "gray30") +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Ur (eU)",
       y = "Peat Depth (cm)")+
  theme_classic() 

Th<- ggplot(data = Points, aes(x =Th, y = Peat_Depth)) +
  geom_point(colour = "gray30") +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Th (eTh)",
       y = "Peat Depth (cm)")+
  theme_classic() 

B4_S2_Win<- ggplot(data = Points, aes(x =Bodmin_S2_Win_clipped_4, y = Peat_Depth)) +
  geom_point(colour = "gray30") +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Winter Red (B4)",
       y = "Peat Depth (cm)")+
  theme_classic() 

NDVI_Win<- ggplot(data = Points, aes(x =NDVI_Win_df, y = Peat_Depth)) +
  geom_point(colour = "gray30") +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Winter NDVI",
       y = "Peat Depth (cm)")+
  theme_classic() 

B5_S2_Win<- ggplot(data = Points, aes(x =Bodmin_S2_Win_clipped_5, y = Peat_Depth)) +
  geom_point(colour = "gray30") +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Winter NIR (B5)",
       y = "Peat Depth (cm)")+
  theme_classic() 

DSM<- ggplot(data = Points, aes(x =Bodmin_DSM_clipped, y = Peat_Depth)) +
  geom_point(colour = "gray30") +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Elevation (m)",
       y = "Peat Depth (cm)")+
  theme_classic() 

TWI<- ggplot(data = Points, aes(x =Bodmin_TWI_Final, y = Peat_Depth)) +
  geom_point(colour = "gray30") +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "TWI",
       y = "Peat Depth (cm)")+
  theme_classic() 

S1<- ggplot(data = Points, aes(x =Bodmin_S1_clipped, y = Peat_Depth)) +
  geom_point(colour = "gray30") +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "S1 VH",
       y = "Peat Depth (cm)")+
  theme_classic() 



wrap_plots(K, TotCount, Dose, Ur, Th, B4_S2_Win, NDVI_Win, B5_S2_Win, DSM, TWI, S1,
           ncol=3)