setwd("C:/Users/whkwong/Dropbox/Split belt study")
library(tidyverse)
library(readxl)
Walking01 <- read_excel("Walking01.xlsx")
L_step <- !is.na(Walking01$L_GRF_Z)
R_step <- !is.na(Walking01$R_GRF_Z)
ts.plot(L_step)
L_TO <- vector()
L_HS <- vector()
R_TO <-vector()
R_HS <- vector()

### create timeframe for L toe off  
for(i in 1:(length(L_step)-1)) {
  
  if(L_step[i]==1 & L_step[i+1] == 0) {
    L_TO <- c(L_TO, i+1)
  }
}

### create timeframe for L heel strike 
for(i in 1:(length(L_step)-1)) {
  
  if(L_step[i]==0 & L_step[i+1] == 1) {
    L_HS <- c(L_HS, i+1)
  }
}

### remove the First TO if preceed of HS
if(L_TO[1] < L_HS[1]) {
  L_TO <-L_TO[-1]
} 

### create timeframe for R toe off  
for(i in 1:(length(R_step)-1)) {
  
  if(R_step[i]==1 & R_step[i+1] == 0) {
    R_TO <- c(R_TO, i+1)
  }
}

### create timeframe for R heel strike 
for(i in 1:(length(R_step)-1)) {
  
  if(R_step[i]==0 & R_step[i+1] == 1) {
    R_HS <- c(R_HS, i+1)
  }
}

### remove the First TO if proceed of HS
if(R_TO[1] < R_HS[1]) {
  R_TO <-R_TO[-1]
} 

###Segmentation for L
L_stide <- list()
for(i in 1:(length(L_HS)-1)) {
  L_stide[[i]] <- Walking01[L_HS[i]:L_HS[i+1],]
}

###Segmentation for R
R_stide <- list()
for(i in 1:(length(R_HS)-1)) {
  R_stide[[i]] <- Walking01[R_HS[i]:R_HS[i+1],]
}




max_R_KAM <- lapply(R_stide, function(x) {
  max(x$R_KAM_Y)
})

to101 <- function(x) {
  return(approx(x, n=101)$y)
}
tn_L_Stride <- list()
 for(i in 1:length(L_stide)) {
  tn_L_Stride[[i]] <- apply(L_stide[[i]],2,to101)
}


L_knee <- matrix(NA, nrow = 101, ncol = 100)
for(i in 1:length(L_stide)) {
  L_knee[,i] <- tn_L_Stride[[i]][,6]
  
}
L_knee <- as.data.frame(L_knee) 
L_knee$Time <-1:101
df <- gather(L_knee, key = series, value = value, -Time) %>% na.omit()

# plot on same grid, each series colored differently -- 
# good if the series have same scale
ggplot(df, aes(x = Time, y = value)) + geom_line(aes(colour = series))


