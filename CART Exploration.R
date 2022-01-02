library(openxlsx)
library(tidyverse)
library(expss)
library(cluster)    # clustering algorithms
library(factoextra)
library(rpart)
library(rpart.plot)
library(tidyverse)


setwd("~/Desktop/Segmentation Project/Working Folder")
df <- read.xlsx(file.choose(), 1)
