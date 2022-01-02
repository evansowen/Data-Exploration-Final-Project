# Load libraries and data
library(data.table)
library(lubridate)
library(bit64)
library(dplyr)
library(stringr)
library(anytime)
library(tidyverse)
library(ggplot2)
library(broom)
library(janitor)
library(dlookr)
library(purrr)
library(psych)
library(openxlsx)
library(expss)
library(cluster)    # clustering algorithms
library(factoextra)
library(wesanderson)
library(klustR)
library(ggally)

setwd("~/Desktop/Segmentation Project/Working Folder")
df <- read.xlsx(file.choose(), 1)

# First Trial - Good Clusters
# Feature Selection,   would like to find high value customer segments based upon ARPU or Revenue

# Choose Select Variables
# Needs to be numeric,  no outliers.  Scale numerical data.
df$HouseholdSize<-as.numeric(df$HouseholdSize)
df$IncomeIndLog <- log(df$HHIncome/df$HouseholdSize)
df1 <- select(df, Age, EducationYears, IncomeIndLog, EmploymentLength, ARPU_Log)

# Scale/Normalize Data
df1.scaled <- scale(df1)

# Determine best number of clusters
fviz_nbclust(df1.scaled, kmeans, nstart = 25, method = "wss")+
  labs(subtitle = "WSS Method")
  
fviz_nbclust(df1.scaled, kmeans, nstart = 25, method='silhouette')+
  labs(subtitle = "Silhouette Method")

fviz_nbclust(df1.scaled, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method")

nc <- NbClust(df1.scaled, min.nc=3, max.nc=5, method="kmeans") # too long
table(nc$Best.nc[1,]) # 
barplot(table(nc$Best.nc[1,]))

# Determine Optimal # Clusters then run Kmeans Algorithm
# Four clusters
set.seed(1234)
km1 <- kmeans(df1.scaled, 4, nstart=25) # Kmeans results
kcluster=km1$cluster

kmeans.med1 <- aggregate(as.data.frame(df1.scaled), 
                         by=list(cluster=kcluster), median)  # aggregate data, median of each cluster

kmeans.med2 <- aggregate(as.data.frame(df1), 
                        by=list(cluster=kcluster), median)  # aggregate data, median of each cluster

df.w_clusters <- cbind(df, cluster = km1$cluster)  # add clusters to original DF

cluster.plot <- fviz_cluster(km1, data = df1, geom="point", alpha=0.5,  
              main = paste ("Kmeans Cluster Plot \n Customer Value Segmentation")) + 
  scale_fill_manual(values = wes_palette("Darjeeling1"))+
  scale_color_manual(values = wes_palette("Darjeeling1"))+
  theme_bw()+
  theme(legend.position = "none",
    plot.title = element_text(hjust=0.5, face="bold", size=20),
    axis.text.y = element_text(color="black", size=14),
    axis.text.x = element_text(color="black", size=14),
    axis.title = element_text(color="black", face="bold", size=16),
    panel.border = element_rect(fill=NA, size=1),
    panel.grid.major = element_line(color="grey75", size=0.5))
  
ggsave(filename="KMeansClusterPlot.png", 
       plot=cluster.plot, device="png", 
       height=5.5, width=9.0, units='in', dpi=600)

sil <- silhouette(km1$cluster, dist(df1.scaled))
fviz_silhouette(sil,ggtheme=theme_bw(), print.summary = TRUE)



ggplot(df.w_clusters, aes(x=cluster, y=ARPU_Log, group=cluster))+geom_boxplot()

km1$size  # identification of size of clusters

# parallell coordinates plot
pacoplot(data = df1.scaled, clusters = km1$cluster,
         colorScheme = c("red", "green", "orange", "blue", "yellow"),
         labelSizes = list(yaxis = 16, yticks = 12),
         measures = list(avg = mean))
# END for k=4


# Three Cluster Results
set.seed(1234)
km2 <- kmeans(df1.scaled, 3, nstart=25) # Kmeans results
kcluster2=km2$cluster

kmeans2.med1 <- aggregate(as.data.frame(df1.scaled), 
                         by=list(cluster=kcluster2), median)  # aggregate data, median of each cluster
write.xlsx(kmeans2.med1, "ClusterMedian2.xlsx")

kmeans2.med2 <- aggregate(as.data.frame(df1), 
                         by=list(cluster=kcluster2), median)  # aggregate data, median of each cluster
write.xlsx(kmeans2.med2, "ClusterMe1.xlsx")

df2.w_clusters <- cbind(df, cluster2 = km2$cluster)  # add clusters to original DF
write.xlsx(df2.w_clusters, "df2.w_clusters.xlsx")

cluster.plot2 <- fviz_cluster(km2, data = df1, geom="point", alpha=0.5,  
                             main = paste ("Kmeans Cluster Plot \n Customer Value Segmentation")) + 
  scale_fill_manual(values = wes_palette("Darjeeling1"))+
  scale_color_manual(values = wes_palette("Darjeeling1"))+
  theme_bw()+
  theme(legend.position = "none",
        plot.title = element_text(hjust=0.5, face="bold", size=20),
        axis.text.y = element_text(color="black", size=14),
        axis.text.x = element_text(color="black", size=14),
        axis.title = element_text(color="black", face="bold", size=16),
        panel.border = element_rect(fill=NA, size=1),
        panel.grid.major = element_line(color="grey75", size=0.5))

ggsave(filename="KMeansClusterPlot2.png", 
       plot=cluster.plot2, device="png", 
       height=5.5, width=9.0, units='in', dpi=600)

sil <- silhouette(km2$cluster, dist(df1.scaled))
fviz_silhouette(sil,ggtheme=theme_bw(), print.summary = TRUE)

ggplot(df2.w_clusters, aes(x=cluster2, y=ARPU_Log, group=cluster2))+geom_boxplot()

km2$size

#

features1 <- c("cluster2", "Age", "EmploymentLength", "EducationYears")
df.select1 <- df2.w_clusters[,features1] %>% 
  pivot_longer(-1, names_to = "FeatureVariable") %>% mutate(cluster2 = case_when(
    cluster2 == "1" ~ "Mid-Value",
    cluster2 == "2" ~ "Low-Value",
    cluster2 == "3" ~ "High-Value"
  ))

FeatureVar1 <- ggplot(df.select1, aes(x=value, y=FeatureVariable, fill=factor(cluster2)))+
  geom_boxplot() +
  coord_flip() +
  scale_fill_manual(values = wes_palette("Darjeeling1"))+
  scale_color_manual(values = wes_palette("Darjeeling1"))+
  theme_bw()+
  theme(plot.title = element_text(hjust=0, face="bold", size=22),
        axis.text.y = element_text(color="black", size=16),
        axis.text.x = element_text(color="black", size=16),
        axis.title = element_text(color="black", face="bold", size=18),
        plot.subtitle = element_text(hjust=0, size=16),
        panel.border = element_rect(fill=NA, size=1),
        legend.text = element_text(size=16),
        legend.position = "top",
        legend.title = element_blank(),
        panel.grid.major = element_line(color="grey75", size=0.5)) +
  labs(title="Data Distributions of Feature Variables per Cluster",
       subtitle="K-Means Clustering (k=3)\n", x="Value\n",
       y="\nFeature Variable")

ggsave(filename="FeatureVar1.png", 
       plot=FeatureVar1, device="png", 
       height=6.5, width=9.5, units='in', dpi=600)

# FeatureVar2

features2 <- c("cluster2", "ARPU_Log", "IncomeIndLog")
df.select2 <- df2.w_clusters[,features2] %>% 
  pivot_longer(-1, names_to = "FeatureVariable") %>% mutate(cluster2 = case_when(
    cluster2 == "1" ~ "Mid-Value",
    cluster2 == "2" ~ "Low-Value",
    cluster2 == "3" ~ "High-Value"
  ))

FeatureVar2 <- ggplot(df.select2, aes(x=value, y=FeatureVariable, fill=factor(cluster2)))+
  geom_boxplot() +
  coord_flip() +
  scale_fill_manual(values = wes_palette("Darjeeling1"))+
  scale_color_manual(values = wes_palette("Darjeeling1"))+
  scale_x_continuous(sec.axis = sec_axis(~ .), name = "Value")+
  theme_bw()+
  theme(plot.title = element_text(hjust=0, face="bold", size=22),
        axis.text.x = element_text(color="black", size=16),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.y.right = element_line(),
        axis.text.y.right = element_text(color="black", size=16),
        axis.title.y = element_blank(),
        axis.title.x = element_text(color="black", face="bold", size=18),
        axis.title.y.right = element_text(color="black", face="bold", size=18),
        plot.subtitle = element_text(hjust=0, size=16),
        panel.border = element_rect(fill=NA, size=1),
        legend.text = element_blank(),
        legend.position = NULL,
        legend.title = element_blank(),
        panel.grid.major = element_line(color="grey75", size=0.5)) +
  labs(title=NULL,
       subtitle=NULL,
       y="\nFeature Variable")

ggsave(filename="FeatureVar2.png", 
       plot=FeatureVar2, device="png", 
       height=4.5, width=3.5, units='in', dpi=600)

