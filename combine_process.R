library(signal)
library(tidyverse)
library(readxl)
options(scipen = 100)
source('function for analysis.R')
EMG <- read_excel("Walking14_combine.xlsx",sheet = "EMG")
Kin <- read_excel("Walking14_combine.xlsx",sheet = "Model Ouptut")
Kin$R_GRF[Kin$R_GRF <0] <- NA

EMG[is.na(EMG)] <- 0
EMG_env <- apply(EMG,2,standard_process) 


df <- cbind(Kin, EMG_env)

L_stride <- Segmentation(df, "L")
R_stride <- Segmentation(df, "R")








