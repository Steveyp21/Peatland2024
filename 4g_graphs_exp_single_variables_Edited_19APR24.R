library(tidyverse)
library(ggplot2)
library(MASS)
library(patchwork)

#Load("C:/LocalData/sp991/GEOM184/CodeForMSc/Final/Inputs/Points.RData")

#Points<-Points%>%
#  mutate(Peat_DepthL = log(Peat_Depth+1),
#         NDVI_Win = ((B5_S2_Win-B4_S2_Win)/(B5_S2_Win+B4_S2_Win)),
#         NDVI_Sum = ((B5_S2_Sum-B4_S2_Sum)/(B5_S2_Sum+B4_S2_Sum)))




K<- ggplot(data = Points, aes(x =K, y = Peat_DepthL)) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "R2")), hjust = -2.9) +
  geom_point(colour = "gray30") +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "K (%K)",
       y = "Ln(Peat Depth +1) (Ln(cm))")+
  theme_classic() 

K

TotCount<- ggplot(data = Points, aes(x =TotCount, y = Peat_DepthL)) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "R2")), hjust = -2.5) +
  geom_point(colour = "gray30") +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Total Count (csp)",
       y = "Ln(Peat Depth +1) (Ln(cm))")+
  theme_classic() 

TotCount

Dose<- ggplot(data = Points, aes(x =Dose, y = Peat_DepthL)) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "R2")), hjust = -2.5) +
  geom_point(colour = "gray30") +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Dose (nGy/h)",
       y = "Ln(Peat Depth +1) (Ln(cm))")+
  theme_classic() 

Dose

Ur<- ggplot(data = Points, aes(x =Ur, y = Peat_DepthL)) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "R2")), hjust = -2.9) +
  geom_point(colour = "gray30") +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Ur (eU)",
       y = "Ln(Peat Depth +1) (Ln(cm))")+
  theme_classic() 

Ur

Th<- ggplot(data = Points, aes(x =Th, y = Peat_DepthL)) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "R2")), hjust = -2.7) +
  geom_point(colour = "gray30") +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Th (eTh)",
       y = "Ln(Peat Depth +1) (Ln(cm))")+
  theme_classic() 

Th

B4_S2_Win<- ggplot(data = Points, aes(x =Bodmin_S2_Win_clipped_4, y = Peat_DepthL)) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "R2")), hjust = -2.3) +
  geom_point(colour = "gray30") +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Winter Red (B4)",
       y = "Ln(Peat Depth +1) (Ln(cm))")+
  theme_classic() 

B4_S2_Win

NDVI_Win<- ggplot(data = Points, aes(x =NDVI_Win_df, y = Peat_DepthL)) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "R2")), hjust = -2.9) +
  geom_point(colour = "gray30") +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Winter NDVI",
       y = "Ln(Peat Depth +1) (Ln(cm))")+
  theme_classic() 

NDVI_Win

B5_S2_Win<- ggplot(data = Points, aes(x =Bodmin_S2_Win_clipped_5, y = Peat_DepthL)) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "R2")), hjust = -2.3) +
  geom_point(colour = "gray30") +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Winter NIR (B5)",
       y = "Ln(Peat Depth +1) (Ln(cm))")+
  theme_classic() 

B5_S2_Win

DSM<- ggplot(data = Points, aes(x =Bodmin_DSM_clipped, y = Peat_DepthL)) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "R2")), hjust = -2.2) +
  geom_point(colour = "gray30") +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Elevation (m)",
       y = "Ln(Peat Depth +1) (Ln(cm))")+
  theme_classic() 

DSM

TWI<- ggplot(data = Points, aes(x =Bodmin_TWI_Final, y = Peat_DepthL)) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "R2")), hjust = -2.5) +
  geom_point(colour = "gray30") +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "TWI",
       y = "Ln(Peat Depth +1) (Ln(cm))")+
  theme_classic() 

TWI

S1<- ggplot(data = Points, aes(x =Bodmin_S1_clipped, y = Peat_DepthL)) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "R2")), hjust = -2.4) +
  geom_point(colour = "gray30") +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "S1 VH",
       y = "Ln(Peat Depth +1) (Ln(cm))")+
  theme_classic() 

S1


wrap_plots(K, TotCount, Dose, Ur, Th, B4_S2_Win, NDVI_Win, B5_S2_Win, DSM, TWI, S1,
           ncol=3)