
### 10 % 
##H0 
directory_path <- "/Users/roxanecouturier/Desktop/censure_inf_plus_non_inf/20per_censure_inf_puissance/H0/10/"
file_names <- list.files(directory_path, pattern = "\\.csv$")

for (file in file_names) {
  
  file_path <- file.path(directory_path, file)
  
  assign(sub(".csv", "", file), read.csv(file_path))
}


results_H0_10 <- list()

#setwd("~./Tables/Table 3/10/H0")

#Loop to process each file
for (file in file_names) {
  # Lire le fichier
  data <- read.csv(file)
  #Convert transpose
  data <- t(data)
  data <- as.data.frame(data)
  
  data$censure <- data$V5
  
  #check type 1 error (5%)
  
  nombre_test_sig <- round((sum(data$V1 >= 0, na.rm = TRUE)) / 100, 1)
  
  
  #Keep just SIFI for  non significant test 
  sifi <- data$V1[data$V1<0]
  
  
  n <- rep(148,length(sifi))
  censure <- data$censure[data$V1<0]
  
  prop <- round(abs(sum(sifi,na.rm=T) / sum(n,na.rm=T)),4)
  median <- median(sifi, na.rm=T) 
  q1 <- quantile(sifi, na.rm=T, 0.25) 
  q3 <- quantile(sifi, na.rm=T, 0.75) 
  mean <- round(mean(sifi, na.rm=T),4)
  sd <- round(sd(sifi,na.rm=T),4)
  prop_cens <- round(abs(sum(sifi,na.rm=T) / sum(censure,na.rm=T)),4)
  
  
  #Store results in list
  results_H0_10[[file]] <- list(nombre_test_sig = nombre_test_sig,prop = prop,median=median, q1=q1, q3=q3, mean=mean,sd=sd, prop_cens = prop_cens)
}


##### under H1 #####

directory_path <- "/Users/roxanecouturier/Desktop/censure_inf_plus_non_inf/20per_censure_inf_puissance/H1/10/"
file_names <- list.files(directory_path, pattern = "\\.csv$")

for (file in file_names) {
  
  file_path <- file.path(directory_path, file)
  
  assign(sub(".csv", "", file), read.csv(file_path))
}


#setwd("~./Tables/Table 3/10/H1")

results_H1_10 <- list()

#Loop to process each file
for (file in file_names) {
  # Lire le fichier
  data <- read.csv(file)
  
  #Convert transpose
  data <- t(data)
  data <- as.data.frame(data)
  
  data$censure <- data$V5
  
  #check type 1 error (5%)
  
  nombre_test_sig <- round((sum(data$V1 >= 0, na.rm = TRUE)) / 100, 1)
  
  
  #Keep just SIFI for   significant test 
  sifi <- data$V1[data$V1>0]
  
  
  n <- rep(148,length(sifi))
  censure <- data$censure[data$V1>0]
  
  prop <- round(abs(sum(sifi,na.rm=T) / sum(n,na.rm=T)),4)
  median <- median(sifi, na.rm=T) 
  q1 <- quantile(sifi, na.rm=T, 0.25) 
  q3 <- quantile(sifi, na.rm=T, 0.75) 
  mean <- round(mean(sifi, na.rm=T),4)
  sd <- round(sd(sifi,na.rm=T),4)
  prop_cens <- round(abs(sum(sifi,na.rm=T) / sum(censure,na.rm=T)),4)
  
  
  #Store results in list
  results_H1_10[[file]] <- list(nombre_test_sig = nombre_test_sig,prop = prop,median=median, q1=q1, q3=q3, mean=mean,sd=sd, prop_cens = prop_cens)
}


#### 40% #####
##### under H0 #####

directory_path <- "/Users/roxanecouturier/Desktop/censure_inf_plus_non_inf/20per_censure_inf_puissance/H0/40/"
file_names <- list.files(directory_path, pattern = "\\.csv$")

for (file in file_names) {
  
  file_path <- file.path(directory_path, file)
  
  assign(sub(".csv", "", file), read.csv(file_path))
}

results_H0_40 <- list()
#setwd("~./Tables/Table 3/40/H0")

#Loop to process each file
for (file in file_names) {
  # Lire le fichier
  data <- read.csv(file)
  
  #Convert transpose
  data <- t(data)
  data <- as.data.frame(data)
  
  data$censure <- data$V5
  
  #check type 1 error (5%)
  
  nombre_test_sig <- round((sum(data$V1 >= 0, na.rm = TRUE)) / 100, 1)
  
  
  #Keep just SIFI for  non significant test 
  sifi <- data$V1[data$V1<0]
  
  
  n <- rep(220,length(sifi))
  censure <- data$censure[data$V1<0]
  
  prop <- round(abs(sum(sifi,na.rm=T) / sum(n,na.rm=T)),4)
  median <- median(sifi, na.rm=T) 
  q1 <- quantile(sifi, na.rm=T, 0.25) 
  q3 <- quantile(sifi, na.rm=T, 0.75) 
  mean <- round(mean(sifi, na.rm=T),4)
  sd <- round(sd(sifi,na.rm=T),4)
  prop_cens <- round(abs(sum(sifi,na.rm=T) / sum(censure,na.rm=T)),4)
  
  
  #Store results in list
  results_H0_40[[file]] <- list(nombre_test_sig = nombre_test_sig,prop = prop,median=median, q1=q1, q3=q3, mean=mean,sd=sd, prop_cens = prop_cens)
}


##### under H1 #####

directory_path <- "/Users/roxanecouturier/Desktop/censure_inf_plus_non_inf/20per_censure_inf_puissance/H1/40"
file_names <- list.files(directory_path, pattern = "\\.csv$")

for (file in file_names) {
  
  file_path <- file.path(directory_path, file)
  
  assign(sub(".csv", "", file), read.csv(file_path))
}


#setwd("~./Tables/Table 3/40/H1")
results_H1_40 <- list()

#Loop to process each file
for (file in file_names) {
  # Lire le fichier
  data <- read.csv(file)
  
  #Convert transpose
  data <- t(data)
  data <- as.data.frame(data)
  
  data$censure <- data$V5
  
  #check type 1 error (5%)
  
  nombre_test_sig <- round((sum(data$V1 >= 0, na.rm = TRUE)) / 100, 1)
  
  
  #Keep just SIFI for   significant test 
  sifi <- data$V1[data$V1>0]
  
  
  n <- rep(220,length(sifi))
  censure <- data$censure[data$V1>0]
  
  prop <- round(abs(sum(sifi,na.rm=T) / sum(n,na.rm=T)),4)
  median <- median(sifi, na.rm=T) 
  q1 <- quantile(sifi, na.rm=T, 0.25) 
  q3 <- quantile(sifi, na.rm=T, 0.75) 
  mean <- round(mean(sifi, na.rm=T),4)
  sd <- round(sd(sifi,na.rm=T),4)
  prop_cens <- round(abs(sum(sifi,na.rm=T) / sum(censure,na.rm=T)),4)
  
  
  #Store results in list
  results_H1_40[[file]] <- list(nombre_test_sig = nombre_test_sig,prop = prop,median=median, q1=q1, q3=q3, mean=mean,sd=sd, prop_cens = prop_cens)
}







