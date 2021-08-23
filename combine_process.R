library(signal)
library(tidyverse)
library(readxl)
options(scipen = 100)
source('function for analysis.R')
EMG <- read_excel("Walking14_combine.xlsx",sheet = "EMG")
Kin <- read_excel("Walking14_combine.xlsx",sheet = "Model Ouptut")

standard_process <- function(x,
                             fs = 1000,
                             HP = 30,
                             LP = 450) {
  z <- x - mean(x)
  z <- filtfilt(butter(4, c(HP, LP) / (fs / 2), "pass"), x = z)
  z <- abs(z)
  z <- filtfilt(butter(4, 6 / (fs / 2), "low"), x = z) 
  z<-approx(z, n=6000)$y
  return(z)
}
EMG[is.na(EMG)] <- 0
EMG_env <- apply(EMG,2,standard_process) 


df <- cbind(Kin, EMG_env)

L_stride <- Segmentation(df, "L")
R_stride <- Segmentation(df, "R")


Plot_all_step(L_stride, n=12)
