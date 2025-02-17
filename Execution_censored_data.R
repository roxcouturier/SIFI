library(stringr)

source("functions_sifi.R")
source("functions_simulations.R")


####execution

#set.seed(145) for each scenario 

###scenarios

###10% censored data 
set.seed(145) 
H0_scenario1a_best_flip_10per<- replicate(10000,sim_censored_data_sifi_original(210,210 ,74,74 , 1,1, 491,"best","flip"))
set.seed(145) 
H0_scenario1a_best_clone_10per<- replicate(10000,sim_censored_data_sifi_original(210,210 ,74,74, 1,1,491,"best","clone"))
set.seed(145) 
H0_scenario1a_worst_flip_10per<- replicate(10000,sim_censored_data_sifi_original(210,210 ,74,74, 1,1,491,"worst","flip"))
set.seed(145) 
H0_scenario1a_worst_clone_10per<- replicate(10000,sim_censored_data_sifi_original(210,210 ,74,74, 1,1,491,"worst","clone"))
set.seed(145) 
H0_scenario1a_best_remove_10per<- replicate(10000,sim_censored_data_sifi_remove(210,210 ,74,74, 1,1,491,"best"))
set.seed(145) 
H0_scenario1a_worst_remove_10per<- replicate(10000,sim_censored_data_sifi_remove(210,210 ,74,74, 1,1,491,"worst"))

set.seed(145) 
H1_scenario1a_best_flip_10per<- replicate(10000,sim_censored_data_sifi_original(210,370 ,74,74 , 1,1,710,"best","flip"))
set.seed(145) 
H1_scenario1a_best_clone_10per<- replicate(10000,sim_censored_data_sifi_original(210,370 ,74,74 , 1,1,710,"best","clone"))
set.seed(145) 
H1_scenario1a_worst_flip_10per<- replicate(10000,sim_censored_data_sifi_original(210,370 ,74,74 , 1,1,710,"worst","flip"))
set.seed(145) 
H1_scenario1a_worst_clone_10per<- replicate(10000,sim_censored_data_sifi_original(210,370 ,74,74 , 1,1,710,"worst","clone"))
set.seed(145) 
H1_scenario1a_best_remove_10per<- replicate(10000,sim_censored_data_sifi_remove(210,370 ,74,74 , 1,1,710,"best"))
set.seed(145) 
H1_scenario1a_worst_remove_10per<- replicate(10000,sim_censored_data_sifi_remove(210,370 ,74,74 , 1,1,710,"worst"))


###40% censored data 
set.seed(145) 
H0_scenario1a_best_flip_40per<- replicate(10000,sim_censored_data_sifi_original(210,210 ,110,110 , 1,1, 165,"best","flip"))
set.seed(145) 
H0_scenario1a_best_clone_40per<- replicate(10000,sim_censored_data_sifi_original(210,210 ,110,110, 1,1,165,"best","clone"))
set.seed(145) 
H0_scenario1a_worst_flip_40per<- replicate(10000,sim_censored_data_sifi_original(210,210 ,110,110, 1,1,165,"worst","flip"))
set.seed(145) 
H0_scenario1a_worst_clone_40per<- replicate(10000,sim_censored_data_sifi_original(210,210 ,110,110, 1,1,165,"worst","clone"))
set.seed(145) 
H0_scenario1a_best_remove_40per<- replicate(10000,sim_censored_data_sifi_remove(210,210 ,110,110, 1,1,165,"best"))
set.seed(145) 
H0_scenario1a_worst_remove_40per<- replicate(10000,sim_censored_data_sifi_remove(210,210 ,110,110, 1,1,165,"worst"))

set.seed(145) 
H1_scenario1a_best_flip_40per<- replicate(10000,sim_censored_data_sifi_original(210,370 ,110,110 , 1,1,279,"best","flip"))
set.seed(145) 
H1_scenario1a_best_clone_40per<- replicate(10000,sim_censored_data_sifi_original(210,370 ,110,110 , 1,1,279,"best","clone"))
set.seed(145) 
H1_scenario1a_worst_flip_40per<- replicate(10000,sim_censored_data_sifi_original(210,370 ,110,110 , 1,1,279,"worst","flip"))
set.seed(145) 
H1_scenario1a_worst_clone_40per<- replicate(10000,sim_censored_data_sifi_original(210,370 ,110,110 , 1,1,279,"worst","clone"))
set.seed(145) 
H1_scenario1a_best_remove_40per<- replicate(10000,sim_censored_data_sifi_remove(210,370 ,110,110 , 1,1,279,"best"))
set.seed(145) 
H1_scenario1a_worst_remove_40per<- replicate(10000,sim_censored_data_sifi_remove(210,370 ,110,110 , 1,1,279,"worst"))


###60% censored data 
set.seed(145) 
H0_scenario1a_best_flip_60per<- replicate(10000,sim_censored_data_sifi_original(210,210 ,165,165 , 1,1, 106,"best","flip"))
set.seed(145) 
H0_scenario1a_best_clone_60per<- replicate(10000,sim_censored_data_sifi_original(210,210 ,165,165, 1,1,106,"best","clone"))
set.seed(145) 
H0_scenario1a_worst_flip_60per<- replicate(10000,sim_censored_data_sifi_original(210,210 ,165,165, 1,1,106,"worst","flip"))
set.seed(145) 
H0_scenario1a_worst_clone_60per<- replicate(10000,sim_censored_data_sifi_original(210,210 ,165,165, 1,1,106,"worst","clone"))
set.seed(145) 
H0_scenario1a_best_remove_60per<- replicate(10000,sim_censored_data_sifi_remove(210,210 ,165,165, 1,1,106,"best"))
set.seed(145) 
H0_scenario1a_worst_remove_60per<- replicate(10000,sim_censored_data_sifi_remove(210,210 ,165,165, 1,1,106,"worst"))

set.seed(145) 
H1_scenario1a_best_flip_60per<- replicate(10000,sim_censored_data_sifi_original(210,370 ,165,165 , 1,1,142,"best","flip"))
set.seed(145) 
H1_scenario1a_best_clone_60per<- replicate(10000,sim_censored_data_sifi_original(210,370 ,165,165 , 1,1,142,"best","clone"))
set.seed(145) 
H1_scenario1a_worst_flip_60per<- replicate(10000,sim_censored_data_sifi_original(210,370 ,165,165 , 1,1,142,"worst","flip"))
set.seed(145) 
H1_scenario1a_worst_clone_60per<- replicate(10000,sim_censored_data_sifi_original(210,370 ,165,165 , 1,1,142,"worst","clone"))
set.seed(145) 
H1_scenario1a_best_remove_60per<- replicate(10000,sim_censored_data_sifi_remove(210,370 ,165,165 , 1,1,142,"best"))
set.seed(145) 
H1_scenario1a_worst_remove_60per<- replicate(10000,sim_censored_data_sifi_remove(210,370 ,165,165 , 1,1,142,"worst"))


###80% censored data 
set.seed(145) 
H0_scenario1a_best_flip_80per<- replicate(10000,sim_censored_data_sifi_original(210,210 ,330,330 , 1,1, 44.7,"best","flip"))
set.seed(145) 
H0_scenario1a_best_clone_80per<- replicate(10000,sim_censored_data_sifi_original(210,210 ,330,330, 1,1,44.7,"best","clone"))
set.seed(145) 
H0_scenario1a_worst_flip_80per<- replicate(10000,sim_censored_data_sifi_original(210,210 ,330,330, 1,1,44.7,"worst","flip"))
set.seed(145) 
H0_scenario1a_worst_clone_80per<- replicate(10000,sim_censored_data_sifi_original(210,210 ,330,330, 1,1,44.7,"worst","clone"))
set.seed(145) 
H0_scenario1a_best_remove_80per<- replicate(10000,sim_censored_data_sifi_remove(210,210 ,330,330, 1,1,44.7,"best"))
set.seed(145) 
H0_scenario1a_worst_remove_80per<- replicate(10000,sim_censored_data_sifi_remove(210,210 ,330,330, 1,1,44.7,"worst"))

set.seed(145) 
H1_scenario1a_best_flip_80per<- replicate(10000,sim_censored_data_sifi_original(210,370 ,330,330 , 1,1,87,"best","flip"))
set.seed(145) 
H1_scenario1a_best_clone_80per<- replicate(10000,sim_censored_data_sifi_original(210,370 ,330,330 , 1,1,87,"best","clone"))
set.seed(145) 
H1_scenario1a_worst_flip_80per<- replicate(10000,sim_censored_data_sifi_original(210,370 ,330,330 , 1,1,87,"worst","flip"))
set.seed(145) 
H1_scenario1a_worst_clone_80per<- replicate(10000,sim_censored_data_sifi_original(210,370 ,330,330 , 1,1,87,"worst","clone"))
set.seed(145) 
H1_scenario1a_best_remove_80per<- replicate(10000,sim_censored_data_sifi_remove(210,370 ,330,330 , 1,1,87,"best"))
set.seed(145) 
H1_scenario1a_worst_remove_80per<- replicate(10000,sim_censored_data_sifi_remove(210,370 ,330,330 , 1,1,87,"worst"))


set.seed(145) 
H1_scenario2a_best_flip_80per<- replicate(10000,sim_censored_data_sifi_original(210,370 ,330,330 , 1,0.7,87,"best","flip"))
set.seed(145) 
H1_scenario2a_best_clone_80per<- replicate(10000,sim_censored_data_sifi_original(210,370 ,330,330 , 1,0.7,87,"best","clone"))
set.seed(145) 
H1_scenario2a_worst_flip_80per<- replicate(10000,sim_censored_data_sifi_original(210,370 ,330,330 , 1,0.7,87,"worst","flip"))
set.seed(145) 
H1_scenario2a_worst_clone_80per<- replicate(10000,sim_censored_data_sifi_original(210,370 ,330,330 , 1,0.7,87,"worst","clone"))
set.seed(145) 
H1_scenario2a_best_remove_80per<- replicate(10000,sim_censored_data_sifi_remove(210,370 ,330,330 , 1,0.7,87,"best"))
set.seed(145) 
H1_scenario2a_worst_remove_80per<- replicate(10000,sim_censored_data_sifi_remove(210,370 ,330,330 , 1,0.7,87,"worst"))


set.seed(145) 
H1_scenario2b_best_flip_80per<- replicate(10000,sim_censored_data_sifi_original(210,370 ,330,330 , 1,2,87,"best","flip"))
set.seed(145) 
H1_scenario2b_best_clone_80per<- replicate(10000,sim_censored_data_sifi_original(210,370 ,330,330 , 1,2,87,"best","clone"))
set.seed(145) 
H1_scenario2b_worst_flip_80per<- replicate(10000,sim_censored_data_sifi_original(210,370 ,330,330 , 1,2,87,"worst","flip"))
set.seed(145) 
H1_scenario2b_worst_clone_80per<- replicate(10000,sim_censored_data_sifi_original(210,370 ,330,330 , 1,2,87,"worst","clone"))
set.seed(145) 
H1_scenario2b_best_remove_80per<- replicate(10000,sim_censored_data_sifi_remove(210,370 ,330,330 , 1,2,87,"best"))
set.seed(145) 
H1_scenario2b_worst_remove_80per<- replicate(10000,sim_censored_data_sifi_remove(210,370 ,330,330 , 1,2,87,"worst"))

#names of scenarios

scenarios_names_H0 <- ls(pattern = "^H0_scenario")
scenarios_names_H0

chemin_fichier <- "./"

#each scenario in a CSV file
for (scenario_name in scenarios_names_H0) {
  scenario_data <- get(scenario_name)  #data
  write.csv(scenario_data, file = paste0(chemin_fichier, scenario_name, ".csv"), row.names = FALSE)
}



scenarios_names_H1 <- ls(pattern = "^H1_scenario")
scenarios_names_H1


#each scenario in a CSV file
for (scenario_name in scenarios_names_H1){
  scenario_data <- get(scenario_name)  #data
  write.csv(scenario_data, file = paste0(chemin_fichier, scenario_name, ".csv"), row.names = FALSE)
}

### informative censoring 

library(stringr)

source("/Users/roxanecouturier/Desktop/Doctorat/SIFI/functions/functions_sifi.R")


sim_censored_data_sifi_original <- function(lambdaB, lambdaA ,n1, n2, ka,kb, truncation_time, direction,operation,p){
  
  
  B <- data.frame(trt = rep("B", n1))
  B$time <- rweibull(n1, scale = lambdaB , shape= kb) 
  B$censure <-runif(n1, min = 0, max = truncation_time)     
  B$event <- ifelse(B$time >= B$censure, 1, 0)
  B$event <- ifelse(B$time>=truncation_time,0,1)
  B$time <- pmin(B$time, B$censure)
  
  
  A <- data.frame(trt=rep("A",n2))
  A$time <- rweibull(n2, scale = lambdaA , shape= ka) 
  A$censure <-runif(n2, min = 0, max = truncation_time)      
  A$event <- ifelse(A$time >= A$censure, 1, 0)
  A$event <- ifelse(A$time>=truncation_time,0,1)
  A$time <- pmin(A$time, A$censure)
  
  data=bind_rows(A,B)
  
  indices <- which(data$event == 1)  # Trouver les indices où event = 1
  n_modif <- round(p * length(indices))  # Calculer 10% de ces indices
  indices_to_change <- sample(indices[data$time < median(data$time)], n_modif) #censure les patients avec un temps court 
  data$event[indices_to_change] <- 0  # Modifier ces événements
  
  
  sifi_res <- sifi(sv_data = data[,c(2,4,1)],direction =direction, operation = operation, agnostic = T)
  
  #nb_censure <- sum(data$event==0)
  return(sifi_res)
  
  
}

sim_censored_data_sifi_remove <- function(lambdaB, lambdaA ,n1, n2, ka,kb, truncation_time, direction,p){
  
  
  B <- data.frame(trt = rep("B", n1))
  B$time <- rweibull(n1, scale = lambdaB , shape= kb) 
  B$censure <-runif(n1, min = 0, max = truncation_time)    
  B$event <- ifelse(B$time >= B$censure, 1, 0)
  B$event <- ifelse(B$time>=truncation_time,0,1)
  B$time <- pmin(B$time, B$censure)
  
  
  A <- data.frame(trt=rep("A",n2))
  A$time <- rweibull(n2, scale = lambdaA , shape= ka)
  A$censure <-runif(n2, min = 0, max = truncation_time)     
  A$event <- ifelse(A$time >= A$censure, 1, 0)
  A$event <- ifelse(A$time>=truncation_time,0,1)
  A$time <- pmin(A$time, A$censure)
  
  data=bind_rows(A,B)
  
  indices <- which(data$event == 1)  # Trouver les indices où event = 1
  n_modif <- round(p * length(indices))  # Calculer 10% de ces indices
  indices_to_change <- sample(indices[data$time < median(data$time)], n_modif) #censure les patients avec un temps court 
  data$event[indices_to_change] <- 0  # Modifier ces événements
  
  
  
  sifi_res <-  sifi_remove(sv_data = data[,c(2,4,1)],direction =direction, agnostic = T)
  
  
  
  # nb_censure <- sum(data$event==0)
  # return(list(sifi_res, nb_censure))
  return(sifi_res)
  
}
####execution

#set.seed(145) for each scenario 

###scenarios

###5% censored data + 5% censure inf 

p=0.05
set.seed(145) 
H0_scenario1a_best_flip_10per<- replicate(10000,sim_censored_data_sifi_original(210,210 ,74,74 , 1,1, 557,"best","flip",p))
set.seed(145) 
H0_scenario1a_best_clone_10per<- replicate(10000,sim_censored_data_sifi_original(210,210 ,74,74, 1,1,557,"best","clone",p))
set.seed(145) 
H0_scenario1a_worst_flip_10per<- replicate(10000,sim_censored_data_sifi_original(210,210 ,74,74, 1,1,557,"worst","flip",p))
set.seed(145) 
H0_scenario1a_worst_clone_10per<- replicate(10000,sim_censored_data_sifi_original(210,210 ,74,74, 1,1,557,"worst","clone",p))
set.seed(145) 
H0_scenario1a_best_remove_10per<- replicate(10000,sim_censored_data_sifi_remove(210,210 ,74,74, 1,1,557,"best",p))
set.seed(145) 
H0_scenario1a_worst_remove_10per<- replicate(10000,sim_censored_data_sifi_remove(210,210 ,74,74, 1,1,557,"worst",p))

set.seed(145) 
H1_scenario1a_best_flip_10per<- replicate(10000,sim_censored_data_sifi_original(210,370 ,74,74 , 1,1,769,"best","flip",p))
set.seed(145) 
H1_scenario1a_best_clone_10per<- replicate(10000,sim_censored_data_sifi_original(210,370 ,74,74 , 1,1,769,"best","clone",p))
set.seed(145) 
H1_scenario1a_worst_flip_10per<- replicate(10000,sim_censored_data_sifi_original(210,370 ,74,74 , 1,1,769,"worst","flip",p))
set.seed(145) 
H1_scenario1a_worst_clone_10per<- replicate(10000,sim_censored_data_sifi_original(210,370 ,74,74 , 1,1,769,"worst","clone",p))
set.seed(145) 
H1_scenario1a_best_remove_10per<- replicate(10000,sim_censored_data_sifi_remove(210,370 ,74,74 , 1,1,769,"best",p))
set.seed(145) 
H1_scenario1a_worst_remove_10per<- replicate(10000,sim_censored_data_sifi_remove(210,370 ,74,74 , 1,1,769,"worst",p))


###40% censored data 
p=0.2
set.seed(145) 
H0_scenario1a_best_flip_40per<- replicate(10000,sim_censored_data_sifi_original(210,210 ,110,110 , 1,1, 343,"best","flip",p))
set.seed(145) 
H0_scenario1a_best_clone_40per<- replicate(10000,sim_censored_data_sifi_original(210,210 ,110,110, 1,1,343,"best","clone",p))
set.seed(145) 
H0_scenario1a_worst_flip_40per<- replicate(10000,sim_censored_data_sifi_original(210,210 ,110,110, 1,1,343,"worst","flip",p))
set.seed(145) 
H0_scenario1a_worst_clone_40per<- replicate(10000,sim_censored_data_sifi_original(210,210 ,110,110, 1,1,343,"worst","clone",p))
set.seed(145) 
H0_scenario1a_best_remove_40per<- replicate(10000,sim_censored_data_sifi_remove(210,210 ,110,110, 1,1,343,"best",p))
set.seed(145) 
H0_scenario1a_worst_remove_40per<- replicate(10000,sim_censored_data_sifi_remove(210,210 ,110,110, 1,1,343,"worst",p))

set.seed(145) 
H1_scenario1a_best_flip_40per<- replicate(10000,sim_censored_data_sifi_original(210,370 ,110,110 , 1,1,474,"best","flip",p))
set.seed(145) 
H1_scenario1a_best_clone_40per<- replicate(10000,sim_censored_data_sifi_original(210,370 ,110,110 , 1,1,474,"best","clone",p))
set.seed(145) 
H1_scenario1a_worst_flip_40per<- replicate(10000,sim_censored_data_sifi_original(210,370 ,110,110 , 1,1,474,"worst","flip",p))
set.seed(145) 
H1_scenario1a_worst_clone_40per<- replicate(10000,sim_censored_data_sifi_original(210,370 ,110,110 , 1,1,474,"worst","clone",p))
set.seed(145) 
H1_scenario1a_best_remove_40per<- replicate(10000,sim_censored_data_sifi_remove(210,370 ,110,110 , 1,1,474,"best",p))
set.seed(145) 
H1_scenario1a_worst_remove_40per<- replicate(10000,sim_censored_data_sifi_remove(210,370 ,110,110 , 1,1,474,"worst",p))


scenarios_names_H0 <- ls(pattern = "^H0_scenario")
scenarios_names_H0

chemin_fichier <- "/Users/roxanecouturier/Desktop/censure_inf_plus_non_inf/20per_censure_inf_puissance/H0/"

#each scenario in a CSV file
for (scenario_name in scenarios_names_H0) {
  scenario_data <- get(scenario_name)  #data
  write.csv(scenario_data, file = paste0(chemin_fichier, scenario_name, ".csv"), row.names = FALSE)
}


chemin_fichier <- "/Users/roxanecouturier/Desktop/censure_inf_plus_non_inf/20per_censure_inf_puissance/H1/"



scenarios_names_H1 <- ls(pattern = "^H1_scenario")
scenarios_names_H1


#each scenario in a CSV file
for (scenario_name in scenarios_names_H1){
  scenario_data <- get(scenario_name)  #data
  write.csv(scenario_data, file = paste0(chemin_fichier, scenario_name, ".csv"), row.names = FALSE)
}

