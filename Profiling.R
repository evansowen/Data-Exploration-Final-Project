library(openxlsx)
library(tidyverse)
library(expss)
library(cluster)    # clustering algorithms
library(factoextra)
library(rpart)
library(rpart.plot)
library(tidyverse)

setwd("~/Desktop/Segmentation Project/Working Folder")
df <- read.xlsx(file.choose(), 1) # df2.w_clusters

df <- mutate(df, OwnsPet = ifelse(NumberPets>0, "Yes", "No"))

labels <- c("Mid Value", "Low Value", "High Value")
levels(df$cluster2)  <- labels           

df<- mutate(df, TenureCat = case_when(
  PhoneCoTenure %in%  1:12       ~ "1-12months",
  PhoneCoTenure %in%  12:24       ~ "12-24months",
  PhoneCoTenure %in%  24:36       ~ "24-36months",
  PhoneCoTenure > 36       ~ "36months"
)
) 

# Service Utilization

df <- mutate(df, DataUtil = DataLastMonth/(DataLastMonth+VoiceLastMonth+EquipmentLastMonth))
df <- mutate(df, EquipUtil = EquipmentLastMonth/(DataLastMonth+VoiceLastMonth+EquipmentLastMonth))

Utilization <- select(df, VoiceUtil, DataUtil, EquipUtil, cluster2)

Utilization1 <- pivot_longer(Utilization, !cluster2,  
                             names_to = "Service", values_to = "PercentUtilization")

ggplot(Utilization1, aes(x=Service, y=PercentUtilization, fill=factor(cluster2)))+
  geom_violin()+
  scale_y_log10()+
  coord_flip()

dff <- Utilization1 %>% group_by(cluster2, Service) %>% summarize(mean=mean(PercentUtilization))

df<- mutate(df, EquipCat= case_when(
  EquipUtil < 0.25       ~ "<25%",
  EquipUtil > 0.25 & EquipUtil < 0.5       ~ "25-50%",
  EquipUtil > 0.50 & EquipUtil < 0.75       ~ "50-75%",
  EquipUtil > 0.75     ~  ">75%"
)
) 

df<- mutate(df, VoiceCat= case_when(
  VoiceUtil < 25       ~ "<25%",
  VoiceUtil > 25 & VoiceUtil < 50       ~ "25-50%",
  VoiceUtil > 50 & VoiceUtil < 75      ~ "50-75%",
  VoiceUtil > 75     ~  ">75%"
)
) 

df<- mutate(df, DataCat= case_when(
  DataUtil < .25       ~ "<25%",
  DataUtil > .25 & DataUtil < .50       ~ "25-50%",
  DataUtil > .50 & DataUtil < .75      ~ "50-75%",
  DataUtil > .75     ~  ">75%"
)
) 


