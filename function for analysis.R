Segmentation <- function(x, side = "R") {
  if(side =="R") {
  R_step <- !is.na(x$R_GRF)  
  } else if(side == "L") {
  R_step <- !is.na(x$L_GRF)
  }

  R_TO <-vector()
  R_HS <- vector()
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
  R_stride <- list()
  for(i in 1:(length(R_HS)-1)) {
    R_stride[[i]] <- x[R_HS[i]:R_HS[i+1],]
  }
  return(R_stride)
}

to101 <- function(x) {
  return(approx(x, n=101)$y)
}


Plot_all_step <- function(x, n = 6) {
  
  tn_L_Stride <- list()
  for(i in 1:length(x)) {
    tn_L_Stride[[i]] <- apply(x[[i]],2,to101)
  }
  
  
  L_knee <- matrix(NA, nrow = 101, ncol = 100)
  for(i in 1:length(x)) {
    L_knee[,i] <- tn_L_Stride[[i]][,n]
    
  }
  L_knee <- as.data.frame(L_knee) 
  L_knee$Time <-1:101
  df <- gather(L_knee, key = series, value = value, -Time) %>% na.omit()
  
  # plot on same grid, each series colored differently -- 
  # good if the series have same scale
  ggplot(df, aes(x = Time, y = value)) + geom_line(aes(colour = series))  
  
}

get_max <-function(x){
  list_max <- lapply(x, function(x) {
    apply(x,2, max)
  })
  df_max <- data.frame(t(list_max[[1]]))
  
  for(i in 2:length(list_max)){
  df_max <- rbind(df_max, data.frame(t(list_max[[i]])))
  }
  return(df_max)
}
 TO <- function(x, side = "R") {
     if(side =="R") {
       R_step <- !is.na(x$R_GRF)  
     } else if(side == "L") {
       R_step <- !is.na(x$L_GRF)
     }
     
     R_TO <-vector()
     R_HS <- vector()
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

     return(R_TO)
   }

 get_TO <- function(x, y = df, z = 'R') {
   to <- TO(y, z)
   df_TO <- as.data.frame(x[[1]][1,])
   for(i in 1:length(x)) {
   n  = which(x[[i]][["Frame"]]==to[i])
   df_TO <- rbind(df_TO, x[[i]][n,])
   }
   df_TO <- df_TO[-1,]
   return(df_TO)
 }

 removeby_cor <- function(x) {
   tn_L_Stride <- list()
   for(i in 1:length(x)) {
     tn_L_Stride[[i]] <- apply(x[[i]],2,to101)
   }
   
   
   L_knee <- matrix(NA, nrow = 101, ncol = length(R_stride))
   for(i in 1:length(x)) {
     L_knee[,i] <- tn_L_Stride[[i]][,9]
     
   }
   L_knee <- as.data.frame(L_knee)
  cor_df <- vector()
  for(i in 1:length(x)) {
    n <- mean(apply(L_knee, 2, function(z){
    cor(z,L_knee[,i])
      
   }))
    cor_df <-c(cor_df,n)
  }
  rem <- which(cor_df <0.9)
  for(i in rev(rem)) {
    x[[i]] <- NULL
  }
  return(x)
 }


