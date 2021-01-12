library(tidyverse)
library(readxl)
library(magrittr)
library(sf)
library(tmap)



# Import IUC, IMD 20 , Mid Year Population Estimates 2019

# Mid Year Population Estimates (England and Wales)
Pop_19 <- read_excel("data/SAPE22DT2-mid-2019-lsoa-syoa-estimates-unformatted.xlsx",sheet="Mid-2019 Persons",skip = 4)

#Index Multiple Deprivation 2019 (England)
IMD_19 <-  read_excel("data/File_1_-_IMD2019_Index_of_Multiple_Deprivation.xlsx",sheet="IMD2019")


#Import IUC (UK)
IUC <- read_csv("data/iuc2018.csv")



# Clean up the data
IMD_19 %>%
  rename(LSOA =  "LSOA code (2011)", IMD = "Index of Multiple Deprivation (IMD) Rank") %>%
  select(LSOA,IMD) %>%
  mutate(IMD_Q = ntile(IMD, 5, result="factor"),IMD_D = ntile(IMD, 10))




Pop_19 %<>%
  rename(LSOA = "LSOA Code",TotPop = "All Ages",LAD_20 = "LA Code (2020 boundaries)") %>%
  select(LSOA,TotPop,LAD_20)

IUC %<>%
  rename(LSOA = LSOA11_CD) %>%
  select(LSOA, GRP_CD, GRP_LABEL) %>%
  mutate(IUC_Target = if_else(GRP_CD %in% 7:10,1,0))


# Combine Data
Combine <- IMD_19 %>%
            left_join(IUC) %>%
            left_join(Pop_19) %>%
            mutate(IUC_IMD =
              if_else(((IMD_Q == 1) & (IUC_Target == 1)),"Materially Deprived; e-Unengaged",
                      if_else(((IMD_Q > 1) & (IUC_Target == 1)),"Materially Not Deprived; e-Unngaged",
                              if_else(((IMD_Q == 1) & (IUC_Target == 0)),"Materially Deprived; e-Engaged",
                                      if_else(((IMD_Q > 1) & (IUC_Target == 0)),"Materially Not Deprived; e-Engaged","No")))))
          

#Fix Factor Order for IUC
Combine %<>%
  mutate(GRP_LABEL = factor(GRP_LABEL, levels = c("e-Cultural Creators","e-Professionals","e-Veterans","Youthful Urban Fringe","e-Rational Utilitarians","e-Mainstream","Passive and Uncommitted Users","Digital Seniors","Settled Offline Communities","e-Withdrawn")
))


# Graphs & Tables

# Population in England by Material Deprivation and e-Engagement
Combine %>% 
  group_by(IUC_IMD) %>% 
  summarise(Total_Population = sum(TotPop))
            


#IMD Decile by IUC Group
ggplot(Combine, aes(fill=factor(IMD_D), y=TotPop, x=GRP_LABEL))+ 
  geom_bar(position="dodge", stat="identity")

                            
#Map
#Import polygons & merge data
LSOA <- st_read("data/LSOA_2011.gpkg") %>%
  rename(LSOA = LSOA11CD) %>%
  left_join(Combine)



tmap_mode("view")
# Liverpool City Region Map

LSOA %>%
  filter(LAD_20 %in% c("E08000012","E08000011","E08000013","E08000014","E08000015","E06000006")) %>%
tm_shape() +
  tm_polygons("IUC_IMD")



# National Map
LSOA %>%
  tm_shape() +
  tm_polygons("IUC_IMD")
