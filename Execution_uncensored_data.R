library(stringr)

source("functions_sifi.R")
source("functions_simulations.R")


####execution

#set.seed(145) for each scenario 

###scenarios
set.seed(145) 
H0_scenario1a_best_flip_66<- replicate(10000,sim_uncensored_data_sifi_original(210,210 ,66, 66, 1,1,"best","flip"))
set.seed(145) 
H0_scenario1a_best_clone_66<- replicate(10000,sim_uncensored_data_sifi_original(210,210 ,66, 66, 1,1,"best","clone"))
set.seed(145) 
H0_scenario1a_worst_flip_66<- replicate(10000,sim_uncensored_data_sifi_original(210,210 ,66, 66, 1,1,"worst","flip"))
set.seed(145) 
H0_scenario1a_worst_clone_66<- replicate(10000,sim_uncensored_data_sifi_original(210,210 ,66, 66, 1,1,"worst","flip"))
set.seed(145) 
H0_scenario1a_best_remove_66<- replicate(10000,sim_uncensored_data_sifi_remove(210,210 ,66, 66, 1,1,"best"))
set.seed(145) 
H0_scenario1a_worst_remove_66<- replicate(10000,sim_uncensored_data_sifi_remove(210,210 ,66, 66, 1,1,"worst"))

set.seed(145) 
H0_scenario1b_best_flip_66<- replicate(10000,sim_uncensored_data_sifi_original(210,210 ,66, 66, 0.7,0.7,"best","flip"))
set.seed(145) 
H0_scenario1b_best_clone_66<- replicate(10000,sim_uncensored_data_sifi_original(210,210 ,66, 66, 0.7,0.7,"best","clone"))
set.seed(145) 
H0_scenario1b_worst_flip_66<- replicate(10000,sim_uncensored_data_sifi_original(210,210 ,66, 66,0.7,0.7,"worst","flip"))
set.seed(145) 
H0_scenario1b_worst_clone_66<- replicate(10000,sim_uncensored_data_sifi_original(210,210 ,66, 66, 0.7,0.7,"worst","flip"))
set.seed(145) 
H0_scenario1b_best_remove_66<- replicate(10000,sim_uncensored_data_sifi_remove(210,210 ,66, 66, 0.7,0.7,"best"))
set.seed(145) 
H0_scenario1b_worst_remove_66<- replicate(10000,sim_uncensored_data_sifi_remove(210,210 ,66, 66, 0.7,0.7,"worst"))


set.seed(145) 
H0_scenario1c_best_flip_66<- replicate(10000,sim_uncensored_data_sifi_original(210,210 ,66, 66, 2,2,"best","flip"))
set.seed(145) 
H0_scenario1c_best_clone_66<- replicate(10000,sim_uncensored_data_sifi_original(210,210 ,66, 66, 2,2,"best","clone"))
set.seed(145) 
H0_scenario1c_worst_flip_66<- replicate(10000,sim_uncensored_data_sifi_original(210,210 ,66, 66, 2,2,"worst","flip"))
set.seed(145) 
H0_scenario1c_worst_clone_66<- replicate(10000,sim_uncensored_data_sifi_original(210,210 ,66, 66, 2,2,"worst","flip"))
set.seed(145) 
H0_scenario1c_best_remove_66<- replicate(10000,sim_uncensored_data_sifi_remove(210,210 ,66, 66, 2,2,"best"))
set.seed(145) 
H0_scenario1c_worst_remove_66<- replicate(10000,sim_uncensored_data_sifi_remove(210,210 ,66, 66,2,2,"worst"))


set.seed(145) 
H0_scenario1a_flip_random_66<- replicate(10000,sim_uncensored_data_sifi_random(210,210 ,66, 66, 1,1,"flip"))
#H0_scenario1a_flip_random_66 <- do.call(rbind, H0_scenario1a_flip_random_66)
#H0_scenario1a_flip_random_66 <- as.data.frame(H0_scenario1a_flip_random_66)

set.seed(145) 
H0_scenario1a_clone_random_66<- replicate(10000,sim_uncensored_data_sifi_random(210,210 ,66, 66, 1,1,"clone"))
# H0_scenario1a_clone_random_66 <- do.call(rbind, H0_scenario1a_clone_random_66)
# H0_scenario1a_clone_random_66 <- as.data.frame(H0_scenario1a_clone_random_66)
set.seed(145) 
H0_scenario1a_remove_random_66<- replicate(10000,sim_uncensored_data_sifi_random_remove(210,210 ,66, 66, 1,1))
# H0_scenario1a_remove_random_66 <- t(H0_scenario1a_remove_random_66) 
# H0_scenario1a_remove_random_66 <- as.data.frame(H0_scenario1a_remove_random_66) 
# H0_scenario1a_remove_random_66 <- data.frame(sapply(H0_scenario1a_remove_random_66,unlist))



####n = 40
set.seed(145) 
H0_scenario1a_best_flip_40<- replicate(10000,sim_uncensored_data_sifi_original(210,210 ,20, 20, 1,1,"best","flip"))
set.seed(145) 
H0_scenario1a_best_clone_40<- replicate(10000,sim_uncensored_data_sifi_original(210,210 ,20, 20, 1,1,"best","clone"))
set.seed(145) 
H0_scenario1a_worst_flip_40<- replicate(10000,sim_uncensored_data_sifi_original(210,210 ,20, 20, 1,1,"worst","flip"))
set.seed(145) 
H0_scenario1a_worst_clone_40<- replicate(10000,sim_uncensored_data_sifi_original(210,210 ,20, 20, 1,1,"worst","flip"))
set.seed(145) 
H0_scenario1a_best_remove_40<- replicate(10000,sim_uncensored_data_sifi_remove(210,210 ,20, 20, 1,1,"best"))
set.seed(145) 
H0_scenario1a_worst_remove_40<- replicate(10000,sim_uncensored_data_sifi_remove(210,210 ,20, 20, 1,1,"worst"))

####n = 80
set.seed(145) 
H0_scenario1a_best_flip_80<- replicate(10000,sim_uncensored_data_sifi_original(210,210 ,40, 40, 1,1,"best","flip"))
set.seed(145) 
H0_scenario1a_best_clone_80<- replicate(10000,sim_uncensored_data_sifi_original(210,210 ,40, 40, 1,1,"best","clone"))
set.seed(145) 
H0_scenario1a_worst_flip_80<- replicate(10000,sim_uncensored_data_sifi_original(210,210 ,40, 40, 1,1,"worst","flip"))
set.seed(145) 
H0_scenario1a_worst_clone_80<- replicate(10000,sim_uncensored_data_sifi_original(210,210 ,40, 40, 1,1,"worst","flip"))
set.seed(145) 
H0_scenario1a_best_remove_80<- replicate(10000,sim_uncensored_data_sifi_remove(210,210 ,40, 40, 1,1,"best"))
set.seed(145) 
H0_scenario1a_worst_remove_80<- replicate(10000,sim_uncensored_data_sifi_remove(210,210 ,40, 40, 1,1,"worst"))

####n = 160
set.seed(145) 
H0_scenario1a_best_flip_160<- replicate(10000,sim_uncensored_data_sifi_original(210,210 ,80, 80, 1,1,"best","flip"))
set.seed(145) 
H0_scenario1a_best_clone_160<- replicate(10000,sim_uncensored_data_sifi_original(210,210 ,80, 80, 1,1,"best","clone"))
set.seed(145) 
H0_scenario1a_worst_flip_160<- replicate(10000,sim_uncensored_data_sifi_original(210,210 ,80, 80, 1,1,"worst","flip"))
set.seed(145) 
H0_scenario1a_worst_clone_160<- replicate(10000,sim_uncensored_data_sifi_original(210,210 ,80, 80, 1,1,"worst","flip"))
set.seed(145) 
H0_scenario1a_best_remove_160<- replicate(10000,sim_uncensored_data_sifi_remove(210,210 ,80, 80, 1,1,"best"))
set.seed(145) 
H0_scenario1a_worst_remove_160<- replicate(10000,sim_uncensored_data_sifi_remove(210,210 ,80, 80, 1,1,"worst"))

####n = 640
set.seed(145) 
H0_scenario1a_best_flip_640<- replicate(10000,sim_uncensored_data_sifi_original(210,210 ,320, 320, 1,1,"best","flip"))
set.seed(145) 
H0_scenario1a_best_clone_640<- replicate(10000,sim_uncensored_data_sifi_original(210,210 ,320, 320, 1,1,"best","clone"))
set.seed(145) 
H0_scenario1a_worst_flip_640<- replicate(10000,sim_uncensored_data_sifi_original(210,210 ,320, 320, 1,1,"worst","flip"))
set.seed(145) 
H0_scenario1a_worst_clone_640<- replicate(10000,sim_uncensored_data_sifi_original(210,210 ,320, 320, 1,1,"worst","flip"))
set.seed(145) 
H0_scenario1a_best_remove_640<- replicate(10000,sim_uncensored_data_sifi_remove(210,210 ,320, 320, 1,1,"best"))
set.seed(145) 
H0_scenario1a_worst_remove_640<- replicate(10000,sim_uncensored_data_sifi_remove(210,210 ,320, 320, 1,1,"worst"))

##random 

####n = 40
set.seed(145) 
H0_scenario1a_flip_random_40<- replicate(10000,sim_uncensored_data_sifi_random(210,210 ,20, 20, 1,1,"flip"))
set.seed(145) 
H0_scenario1a_clone_random_40<-replicate(10000,sim_uncensored_data_sifi_random(210,210 ,20, 20, 1,1,"clone"))
set.seed(145) 
H0_scenario1a_remove_random_40<- replicate(10000,sim_uncensored_data_sifi_random_remove(210,210 ,20, 20, 1,1))

####n = 80
set.seed(145) 
H0_scenario1a_flip_random_80<- replicate(10000,sim_uncensored_data_sifi_random(210,210 ,40, 40, 1,1,"flip"))
set.seed(145) 
H0_scenario1a_clone_random_80<- replicate(10000,sim_uncensored_data_sifi_random(210,210 ,40, 40, 1,1,"clone"))
set.seed(145) 
H0_scenario1a_remove_random_80<- replicate(10000,sim_uncensored_data_sifi_random_remove(210,210 ,40, 40, 1,1))

####n = 160
set.seed(145) 
H0_scenario1a_flip_random_160<-  replicate(10000,sim_uncensored_data_sifi_random(210,210 ,80, 80, 1,1,"flip"))
set.seed(145) 
H0_scenario1a_clone_random_160<-replicate(10000,sim_uncensored_data_sifi_random(210,210 ,80, 80, 1,1,"clone"))
set.seed(145) 
H0_scenario1a_remove_random_160<- replicate(10000,sim_uncensored_data_sifi_random_remove(210,210 ,80, 80, 1,1))

####n = 640
set.seed(145) 
H0_scenario1a_flip_random_640<-  replicate(10000,sim_uncensored_data_sifi_random(210,210 ,320, 320, 1,1,"flip"))
set.seed(145) 
H0_scenario1a_clone_random_640<-replicate(10000,sim_uncensored_data_sifi_random(210,210 ,320, 320, 1,1,"clone"))
set.seed(145) 
H0_scenario1a_remove_random_640<-  replicate(10000,sim_uncensored_data_sifi_random_remove(210,210 ,320, 320, 1,1))




####Under H1 
### set.seed = 145 

set.seed(145) 
H1_scenario1a_best_flip<- replicate(10000,sim_uncensored_data_sifi_original(210,370 ,66, 66, 1,1,"best","flip"))
set.seed(145) 
H1_scenario1a_best_clone<- replicate(10000,sim_uncensored_data_sifi_original(210,370 ,66, 66, 1,1,"best","clone"))
set.seed(145) 
H1_scenario1a_worst_flip<- replicate(10000,sim_uncensored_data_sifi_original(210,370 ,66, 66, 1,1,"worst","flip"))
set.seed(145) 
H1_scenario1a_worst_clone<- replicate(10000,sim_uncensored_data_sifi_original(210,370 ,66, 66, 1,1,"worst","flip"))
set.seed(145) 
H1_scenario1a_best_remove<- replicate(10000,sim_uncensored_data_sifi_remove(210,370 ,66, 66, 1,1,"best"))
set.seed(145) 
H1_scenario1a_worst_remove<- replicate(10000,sim_uncensored_data_sifi_remove(210,370 ,66, 66, 1,1,"worst"))


set.seed(145) 
H1_scenario1b_best_flip<- replicate(10000,sim_uncensored_data_sifi_original(210,370 ,66, 66, 0.7,0.7,"best","flip"))
set.seed(145) 
H1_scenario1b_best_clone<- replicate(10000,sim_uncensored_data_sifi_original(210,370 ,66, 66, 0.7,0.7,"best","clone"))
set.seed(145) 
H1_scenario1b_worst_flip<- replicate(10000,sim_uncensored_data_sifi_original(210,370 ,66, 66,0.7,0.7,"worst","flip"))
set.seed(145) 
H1_scenario1b_worst_clone<- replicate(10000,sim_uncensored_data_sifi_original(210,370 ,66, 66, 0.7,0.7,"worst","flip"))
set.seed(145) 
H1_scenario1b_best_remove<- replicate(10000,sim_uncensored_data_sifi_remove(210,370 ,66, 66, 0.7,0.7,"best"))
set.seed(145) 
H1_scenario1b_worst_remove<- replicate(10000,sim_uncensored_data_sifi_remove(210,370 ,66, 66, 0.7,0.7,"worst"))


set.seed(145) 
H1_scenario1c_best_flip<- replicate(10000,sim_uncensored_data_sifi_original(210,370 ,66, 66, 2,2,"best","flip"))
set.seed(145) 
H1_scenario1c_best_clone<- replicate(10000,sim_uncensored_data_sifi_original(210,370 ,66, 66, 2,2,"best","clone"))
set.seed(145) 
H1_scenario1c_worst_flip<- replicate(10000,sim_uncensored_data_sifi_original(210,370 ,66, 66, 2,2,"worst","flip"))
set.seed(145) 
H1_scenario1c_worst_clone<- replicate(10000,sim_uncensored_data_sifi_original(210,370 ,66, 66, 2,2,"worst","flip"))
set.seed(145) 
H1_scenario1c_best_remove<- replicate(10000,sim_uncensored_data_sifi_remove(210,370 ,66, 66, 2,2,"best"))
set.seed(145) 
H1_scenario1c_worst_remove<- replicate(10000,sim_uncensored_data_sifi_remove(210,370 ,66, 66,2,2,"worst"))


set.seed(145) 
H1_scenario1a_flip_random<- replicate(10000,sim_uncensored_data_sifi_random(210,370 ,66, 66, 1,1,"flip"))
# H1_scenario1a_flip_random <- do.call(rbind, H1_scenario1a_flip_random)
# H1_scenario1a_flip_random <- as.data.frame(H1_scenario1a_flip_random)
set.seed(145) 
H1_scenario1a_clone_random<-replicate(10000,sim_uncensored_data_sifi_random(210,370 ,66, 66, 1,1,"clone"))
# H1_scenario1a_clone_random <- do.call(rbind, H1_scenario1a_clone_random)
# H1_scenario1a_clone_random <- as.data.frame(H1_scenario1a_clone_random)
set.seed(145) 
H1_scenario1a_remove_random<- replicate(10000,sim_uncensored_data_sifi_random_remove(210,370 ,66, 66, 1,1))
# H1_scenario1a_remove_random <- t(H1_scenario1a_remove_random)
# H1_scenario1a_remove_random <- as.data.frame(H1_scenario1a_remove_random)
# H1_scenario1a_remove_random <- data.frame(sapply(H1_scenario1a_remove_random,unlist))

set.seed(145) 
H1_scenario2a_best_flip<- replicate(10000,sim_uncensored_data_sifi_original(210,370 ,66, 66, 1,0.7,"best","flip"))
set.seed(145) 
H1_scenario2a_best_clone<- replicate(10000,sim_uncensored_data_sifi_original(210,370 ,66, 66,1,0.7,"best","clone"))
set.seed(145) 
H1_scenario2a_worst_flip<- replicate(10000,sim_uncensored_data_sifi_original(210,370 ,66, 66, 1, 0.7,"worst","flip"))
set.seed(145) 
H1_scenario2a_worst_clone<- replicate(10000,sim_uncensored_data_sifi_original(210,370 ,66, 66, 1, 0.7,"worst","clone"))
set.seed(145) 
H1_scenario2a_best_remove<- replicate(10000,sim_uncensored_data_sifi_remove(210,370 ,66, 66, 1, 0.7,"best"))
set.seed(145) 
H1_scenario2a_worst_remove <-replicate(10000,sim_uncensored_data_sifi_remove(210,370 ,66, 66,  1,0.7,"worst"))


set.seed(145) 
H1_scenario2b_best_flip<- replicate(10000,sim_uncensored_data_sifi_original(210,370 ,66, 66,1, 2,"best","flip"))
set.seed(145) 
H1_scenario2b_best_clone<- replicate(10000,sim_uncensored_data_sifi_original(210,370 ,66, 66, 1,2,"best","clone"))
set.seed(145) 
H1_scenario2b_worst_flip<- replicate(10000,sim_uncensored_data_sifi_original(210,370 ,66, 66,1, 2,"worst","flip"))
set.seed(145) 
H1_scenario2b_worst_clone<- replicate(10000,sim_uncensored_data_sifi_original(210,370 ,66, 66,1, 2,"worst","clone"))
set.seed(145) 
H1_scenario2b_best_remove<- replicate(10000,sim_uncensored_data_sifi_remove(210,370 ,66, 66, 1,2,"best"))
set.seed(145) 
H1_scenario2b_worst_remove<- replicate(10000,sim_uncensored_data_sifi_remove(210,370 ,66, 66,1, 2,"worst"))


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


