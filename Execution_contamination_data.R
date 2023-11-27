
source("function_simulations_contamination.R")

library(dplyr)
library(survival)
library(survminer)

####  under H0 


set.seed(145)
H0_worst_contamination <- replicate(10000,worst_contamination_H0(210,210,66,66,1,1))

chemin_fichier <- "./"
write.csv(H0_worst_contamination, file = paste0(chemin_fichier,"H0_worst_contamination.csv"), row.names = FALSE)

set.seed(145)
H0_best_contamination <- replicate(10000,best_contamination_H0(210,210,66,66,1,1))
chemin_fichier <- "./"
write.csv(H0_best_contamination, file = paste0(chemin_fichier,"H0_best_contamination.csv"), row.names = FALSE)


#### under H1 

set.seed(145)
worst_contamination_H1 <- replicate(10000,worst_contamination_H1(210,370,66,66,1,1))
chemin_fichier <- "./"
write.csv(worst_contamination_H1, file = paste0(chemin_fichier,"worst_contamination_H1.csv"), row.names = FALSE)

set.seed(145)
best_contamination_H1 <- replicate(10000,best_contamination_H1(210,370,66,66,1,1))
chemin_fichier <- "."
write.csv(best_contamination_H1, file = paste0(chemin_fichier,"best_contamination_H1.csv"), row.names = FALSE)
