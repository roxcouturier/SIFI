##### under H0 #####

getwd()

directory_path <- "./Tables/Table 2/H0"
file_names <- list.files(directory_path, pattern = "\\.csv$")


for (file in file_names) {
  
  file_path <- file.path(directory_path, file)
  
  assign(sub(".csv", "", file), read.csv(file_path))
}

results <- list()

#Loop to process each file
for (file in file_names) {
  # Lire le fichier
  data <- read.csv(file)
  
  #Convert transpose
  data <- t(data)
  data <- as.data.frame(data)
  

  #check type 1 error (5%)
  
  nombre_test_sig <- round((sum(data$V1 >= 0, na.rm = TRUE)) / 100, 1)
  
  
  #Keep just SIFI for  non significant test 
  data <- data$V1[data$V1<0]
  
  n <- rep(132,length(data))
  
  prop <- round(abs(sum(data,na.rm=T) / sum(n,na.rm=T)),4)
  median <- median(data, na.rm=T) 
  q1 <- quantile(data, na.rm=T, 0.25) 
  q3 <- quantile(data, na.rm=T, 0.75) 
  mean <- round(mean(data, na.rm=T),4)
  sd <- round(sd(data,na.rm=T),4)
  
  
  
  #Store results in list
  results[[file]] <- list(nombre_test_sig = nombre_test_sig,prop = prop,median=median, q1=q1, q3=q3, mean=mean,sd=sd)
}



##### under H1 #####

directory_path <- "./Tables/Table 2/H1"
file_names <- list.files(directory_path, pattern = "\\.csv$")

for (file in file_names) {
  
  file_path <- file.path(directory_path, file)
  
  assign(sub(".csv", "", file), read.csv(file_path))
}


#setwd("~./Tables/Table 2/H1")

# Créer une liste pour stocker les résultats de chaque fichier
results_H1 <- list()

#Loop to process each file
for (file in file_names) {
  # Lire le fichier
  data <- read.csv(file)
  
  #Convert transpose
  data <- t(data)
  data <- as.data.frame(data)
  
  
  #check type 1 error (5%)
  
  nombre_test_sig <- round((sum(data$V1 >= 0, na.rm = TRUE)) / 100, 1)

  
  
  #Keep just SIFI for  significant test 
  data <- data$V1[data$V1>0]
  
  n <- rep(132,length(data))
  
  prop <- round(abs(sum(data,na.rm=T) / sum(n,na.rm=T)),4)
  median <- median(data, na.rm=T) 
  q1 <- quantile(data, na.rm=T, 0.25) 
  q3 <- quantile(data, na.rm=T, 0.75) 
  mean <- round(mean(data, na.rm=T),4) 
  sd <- round(sd(data,na.rm=T),4)
  
  
  
  #Store results in list
  results_H1[[file]] <- list(nombre_test_sig = nombre_test_sig,prop = prop,median=median, q1=q1, q3=q3, mean=mean,sd=sd)
}



#### table 

Table <- data.frame(matrix(vector(), 24, 8, 
                            dimnames=list(c(),c("Methods","RHO", "Median","Q1","Q3", "Mean","SD", "Prop-SIFI"))),
                     stringsAsFactors = F)

Table$Methods <- c("H0 1a  - Best Clone","Best Flip","Best Delete","Worst Clone","Worst Flip","Worst Delete",
                   "H1 1a -Best Clone","Best Flip","Best Delete","Worst Clone","Worst Flip","Worst Delete",
                   "H1 1b - Best Clone","Best Flip","Best Delete","Worst Clone","Worst Flip","Worst Delete",
                   "H1 1c - Best Clone","Best Flip","Best Delete","Worst Clone","Worst Flip","Worst Delete")



##RHO 

 premiers_elements <- sapply(results, function(x) x[1])
 premiers_elements <- unlist(premiers_elements)
 
 premiers_elements_H1 <- sapply(results_H1, function(x) x[1])
 premiers_elements_H1 <- unlist(premiers_elements_H1)

##median 
 
 median_H0 <- sapply(results, function(x) x[3])
 median_H0 <- unlist(median_H0)
 
 median_H1 <- sapply(results_H1, function(x) x[3])
 median_H1 <- unlist(median_H1)
 
 ##Q1 
 
 q1_H0 <- sapply(results, function(x) x[4])
 q1_H0 <- unlist(q1_H0)
 
 q1_H1 <- sapply(results_H1, function(x) x[4])
 q1_H1 <- unlist(q1_H1)
 
 ##Q3 
 
 q3_H0 <- sapply(results, function(x) x[5])
 q3_H0 <- unlist(q3_H0)
 
 q3_H1 <- sapply(results_H1, function(x) x[5])
 q3_H1 <- unlist(q3_H1)
 
 ##mean 
 
 mean_H0 <- sapply(results, function(x) x[6])
 mean_H0 <- unlist(mean_H0)
 
 mean_H1 <- sapply(results_H1, function(x) x[6])
 mean_H1 <- unlist(mean_H1)
 
 ##szd 
 
sd_H0 <- sapply(results, function(x) x[7])
sd_H0 <- unlist(sd_H0)
 
sd_H1 <- sapply(results_H1, function(x) x[7])
sd_H1 <- unlist(sd_H1)


##prop

prop_H0 <- sapply(results, function(x) x[2])
prop_H0 <- unlist(prop_H0)

prop_H1 <- sapply(results_H1, function(x) x[2])
prop_H1 <- unlist(prop_H1)


Table$RHO <- c(premiers_elements ,premiers_elements_H1 )
Table$Median <- c(median_H0,median_H1)
Table$Q1 <- c(q1_H0,q1_H1)
Table$Q3 <- c(q3_H0,q3_H1)
Table$Mean <- c(mean_H0,mean_H1)
Table$SD <- c(sd_H0,sd_H1)
Table$Prop.SIFI <- c(prop_H0,prop_H1)

Table[22,] <- c("Worst Clone",100,48,40,56,47.4,10.523,0.3247)


write.csv(Table, file = "/Users/roxanecouturier/Desktop/SIFI/Table2.csv")
