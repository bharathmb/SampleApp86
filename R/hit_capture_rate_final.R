#rm(list=ls())
#install.packages("flux")
#options(java.home="C:\\Program Files\\Java\\jre1.8.0_181\\")


library(plotly)
library(dplyr)

################## Function for HitRate v Capture Rate ##################

hitvcap <- function(data){
  
dataset_test_1 <- data
colnames(dataset_test_1) <- c("Prob","DV")

Func <- function(cutoff, datas){
  
  
  ################# Predictions based on cutoff
  for ( i in 1:nrow(dataset_test_1)){
    
    if(dataset_test_1$Prob[i]>=cutoff){
      dataset_test_1$Pred[i] = 1
    } else{
      dataset_test_1$Pred[i]=0
    }
  }
  
  ################### Correct one's predicted  
  for ( i in 1:nrow(dataset_test_1)){
    
    if(dataset_test_1$Pred[i]+dataset_test_1$DV[i]==2){
      dataset_test_1$Pred1_crct[i] = 1
    } else{
      dataset_test_1$Pred1_crct[i]=0
    }
  }
  
  ################## Zero's Falsely Predicted
  temp <- subset(dataset_test_1,dataset_test_1$DV==1)
  for ( i in 1:nrow(temp)){
    
    if(temp$DV+temp$Pred[i]==1){
      temp$Pred1_false[i] = 1
    } else{
      temp$Pred1_false[i]=0
    }
  }
  df <- dataset_test_1
  tot_ones <- sum(df$DV == 1)
  tot_zeros <- length(df$DV==0)
  tot_ones_pred <- length(df$Pred == 1)
  hits <- sum(df$Pred1_crct == 1)/tot_ones
  miss <- sum(temp$Pred1_false == 1)/tot_ones
  ret <- matrix(c(hits, miss), nrow = 1)
  colnames(ret) <- c("Hits", "Miss")
  
  return(ret)
}


vec <- seq(0,1,0.1)
hit.df <- data.frame()

for(i in vec){
  hit.df <- rbind(hit.df, Func(i, dataset_test))
}



labels <- data.frame(x = hit.df$Miss, 
                     y = hit.df$Hits,
                     Threshold = vec)

hit.df$Hits <- round(hit.df$Hits,2)
hit.df$Miss <- round(hit.df$Miss,2)

hit.df$Threshold=labels$Threshold


plot_ly(hit.df, y = ~Hits, x = ~Threshold,name='Hit Rate',type='scatter',mode='lines') %>%
  
  add_trace(y = ~Miss,name='Capture Rate',mode='lines'
  ) %>%
  layout(xaxis = list(range = c(0,1), zeroline = F, showgrid = F,
                      title = "Threshold"),
         yaxis = list(range = c(0,1), zeroline = F, showgrid = F,
                      domain = c(0, 0.9),
                      title = "Hit Rate/Miss Rate"),
         plot_bgcolor = "aliceblue",title="Hit Rate vs Capture Rate"
         )


}

################# Calling the function

########### Input to the function('a') should be a dataframe with the first column as the Probabilities and second column as 'DV'. 


hitvcap(a)
