#### Sample Size SIFI 
# n = 40 to 640 

#Read files 

library(here)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(hrbrthemes)

files_names <- list.files(path = here("./Figures/Figure 3/SIFI_n_40_640"))
nb_files <- length(files_names)
data_names <- vector("list", length = nb_files)

for (i in 1:nb_files) {
  data <- read.csv(here("./Figures/Figure 3/SIFI_n_40_640", files_names[i]))
  data <- t(data)
  data <- as.data.frame(data)
  
  data_sub <- subset(data, data$V1 < 0)
  data_sub$V1 <- as.factor(data_sub$V1)
  
  #New modified file
  modified_file_name <- paste("modified_", files_names[i], sep = "")
  file <- here("./Figures/Figure 3/SIFI_n_40_640", modified_file_name)
  
  # Écrire le fichier modifié
  write.csv(data_sub, file, row.names = FALSE)
}




# Initialisation des listes pour stocker les données
data_list <- list()

# Liste des noms de fichiers modifiés
modified_files <- list.files(path = here("./Figures/Figure 3/SIFI_n_40_640"), pattern = "^modified_")

#Read all files modified 

for (file_name in modified_files) {
  #read file
  data <- read.csv(here("./Figures/Figure 3/SIFI_n_40_640", file_name))
  #Extract specific name to identify type and file
  type <- gsub("modified_", "", file_name)
  type <- gsub(".csv", "", type)
  #Create a dynamic variable name
  variable_name <- paste(type, "_data", sep = "")
  #Assign data to a variable in the global environment
  assign(variable_name, data)
}



####bases 

best_flip <- data.frame(
  type = c( rep("n = 40", nrow(H0_scenario1a_best_flip_40_data)), rep("n = 80", nrow(H0_scenario1a_best_flip_80_data)),rep("n = 160", nrow(H0_scenario1a_best_flip_160_data)), rep("n = 640", nrow(H0_scenario1a_best_flip_640_data)) ),
  sifi = c(H0_scenario1a_best_flip_40_data$V1,H0_scenario1a_best_flip_80_data$V1,H0_scenario1a_best_flip_160_data$V1,H0_scenario1a_best_flip_640_data$V1)
) 

best_clone <- data.frame(
  type = c( rep("n = 40", nrow(H0_scenario1a_best_clone_40_data)), rep("n = 80", nrow(H0_scenario1a_best_clone_80_data)),rep("n = 160", nrow(H0_scenario1a_best_clone_160_data)), rep("n = 640", nrow(H0_scenario1a_best_clone_640_data)) ),
  sifi = c(H0_scenario1a_best_clone_40_data$V1,H0_scenario1a_best_clone_80_data$V1,H0_scenario1a_best_clone_160_data$V1,H0_scenario1a_best_clone_640_data$V1)
) 


best_remove <- data.frame(
  type = c( rep("n = 40", nrow(H0_scenario1a_best_remove_40_data)), rep("n = 80", nrow(H0_scenario1a_best_remove_80_data)),rep("n = 160", nrow(H0_scenario1a_best_remove_160_data)), rep("n = 640", nrow(H0_scenario1a_best_remove_640_data)) ),
  sifi = c(H0_scenario1a_best_remove_40_data$V1,H0_scenario1a_best_remove_80_data$V1,H0_scenario1a_best_remove_160_data$V1 ,H0_scenario1a_best_remove_640_data$V1)
) 

worst_flip <- data.frame(
  type = c( rep("n = 40", nrow(H0_scenario1a_worst_flip_40_data)), rep("n = 80", nrow(H0_scenario1a_worst_flip_80_data)),rep("n = 160", nrow(H0_scenario1a_worst_flip_160_data)), rep("n = 640", nrow(H0_scenario1a_worst_flip_640_data)) ),
  sifi = c(H0_scenario1a_worst_flip_40_data$V1,H0_scenario1a_worst_flip_80_data$V1,H0_scenario1a_worst_flip_160_data$V1,H0_scenario1a_worst_flip_640_data$V1)
) 

worst_clone <- data.frame(
  type = c( rep("n = 40", nrow(H0_scenario1a_worst_clone_40_data)), rep("n = 80", nrow(H0_scenario1a_worst_clone_80_data)),rep("n = 160", nrow(H0_scenario1a_worst_clone_160_data)), rep("n = 640", nrow(H0_scenario1a_worst_clone_640_data)) ),
  sifi = c(H0_scenario1a_worst_clone_40_data$V1,H0_scenario1a_worst_clone_80_data$V1,H0_scenario1a_worst_clone_160_data$V1,H0_scenario1a_worst_clone_640_data$V1)
) 


worst_remove <- data.frame(
  type = c( rep("n = 40", nrow(H0_scenario1a_worst_remove_40_data)), rep("n = 80", nrow(H0_scenario1a_worst_remove_80_data)),rep("n = 160", nrow(H0_scenario1a_worst_remove_160_data)), rep("n = 640", nrow(H0_scenario1a_worst_remove_640_data)) ),
  sifi = c(H0_scenario1a_worst_remove_40_data$V1,H0_scenario1a_worst_remove_80_data$V1,H0_scenario1a_worst_remove_160_data$V1,H0_scenario1a_worst_remove_640_data$V1)
) 


####graph 

best_flip_graph <- best_flip %>%
  ggplot( aes(x=sifi, fill=type)) +
  geom_histogram( position = "identity",color="#e9ecef", alpha=0.6) +
  scale_fill_manual(values=c("#0754A0","#F12A29","#4233FF","#FF9633"),breaks = c("n = 40", "n = 80", "n = 160", "n = 640")) +
  theme_ipsum() +
  labs(fill="", title = "Best flip Control -> Experimental ") + 
  scale_x_continuous(breaks = seq(min(best_flip$sifi), max(best_flip$sifi), by = 1),
                     labels = seq(min(best_flip$sifi), max(best_flip$sifi), by = 1)) + theme(
                       axis.text.x = element_text(size = 12),   # Increase x-axis label text size
                       axis.text.y = element_text(size = 12),
                       legend.text = element_text(size = 14)
                     )




best_clone_graph <- best_clone %>%
  ggplot( aes(x=sifi, fill=type)) +
  geom_histogram( position = "identity",color="#e9ecef", alpha=0.6) +
  scale_fill_manual(values=c("#0754A0","#F12A29","#4233FF","#FF9633"),breaks = c("n = 40", "n = 80", "n = 160", "n = 640")) +
  theme_ipsum() +
  labs(fill="", title = "Best clone Control -> Experimental ")+ 
  scale_x_continuous(breaks = seq(min(best_clone$sifi), max(best_clone$sifi), by = 4),
                     labels = seq(min(best_clone$sifi), max(best_clone$sifi), by = 4)) + theme(
                       axis.text.x = element_text(size = 12),   # Increase x-axis label text size
                       axis.text.y = element_text(size = 12),
                       legend.text = element_text(size = 14)
                     )





best_remove_graph <- best_remove %>%
  ggplot( aes(x=sifi, fill=type)) +
  geom_histogram( position = "identity",color="#e9ecef", alpha=0.6) +
  scale_fill_manual(values=c("#0754A0","#F12A29","#4233FF","#FF9633"),breaks = c("n = 40", "n = 80", "n = 160", "n = 640")) +
  theme_ipsum() +
  labs(fill="", title = "Best delete Control -> Experimental ")+ 
  scale_x_continuous(breaks = seq(min(best_remove$sifi), max(best_remove$sifi), by = 4),
                     labels = seq(min(best_remove$sifi), max(best_remove$sifi), by = 4)) + theme(
  axis.text.x = element_text(size = 12),   # Increase x-axis label text size
  axis.text.y = element_text(size = 12),
  legend.text = element_text(size = 14)
)





worst_flip_graph <- worst_flip %>%
  ggplot( aes(x=sifi, fill=type)) +
  geom_histogram( position = "identity",color="#e9ecef", alpha=0.6) +
  scale_fill_manual(values=c("#0754A0","#F12A29","#4233FF","#FF9633"),breaks = c("n = 40", "n = 80", "n = 160", "n = 640")) +
  theme_ipsum() +
  labs(fill="", title = "Worst flip Experimental -> Control ")+ 
  scale_x_continuous(breaks = seq(min(worst_flip$sifi), max(worst_flip$sifi), by = 3),
                     labels = seq(min(worst_flip$sifi), max(worst_flip$sifi), by = 3)) + theme(
                       axis.text.x = element_text(size = 12),   # Increase x-axis label text size
                       axis.text.y = element_text(size = 12),
                       legend.text = element_text(size = 14)
                     )




worst_clone_graph <- worst_clone %>%
  ggplot( aes(x=sifi, fill=type)) +
  geom_histogram( position = "identity",color="#e9ecef", alpha=0.6) +
  scale_fill_manual(values=c("#0754A0","#F12A29","#4233FF","#FF9633"),breaks = c("n = 40", "n = 80", "n = 160", "n = 640")) +
  theme_ipsum() +
  labs(fill="", title = "Worst clone Experimental -> Control ")+ 
  scale_x_continuous(breaks = seq(min(worst_clone$sifi), max(worst_clone$sifi), by = 8),
                     labels = seq(min(worst_clone$sifi), max(worst_clone$sifi), by = 8)) + theme(
                       axis.text.x = element_text(size = 12),   # Increase x-axis label text size
                       axis.text.y = element_text(size = 12),
                       legend.text = element_text(size = 14)
                     )





worst_remove_graph <- worst_remove %>%
  ggplot( aes(x=sifi, fill=type)) +
  geom_histogram( position = "identity",color="#e9ecef", alpha=0.6) +
  scale_fill_manual(values=c("#0754A0","#F12A29","#4233FF","#FF9633"),breaks = c("n = 40", "n = 80", "n = 160", "n = 640")) +
  theme_ipsum() +
  labs(fill="", title = "Worst delete Experimental -> Control ")+ 
  scale_x_continuous(breaks = seq(min(worst_remove$sifi), max(worst_remove$sifi), by = 8),
                     labels = seq(min(worst_remove$sifi), max(worst_remove$sifi), by = 8)) + theme(
                       axis.text.x = element_text(size = 12),   # Increase x-axis label text size
                       axis.text.y = element_text(size = 12),
                       legend.text = element_text(size = 14)
                     )



grid.arrange(best_flip_graph,best_clone_graph, best_remove_graph, worst_flip_graph,worst_clone_graph, worst_remove_graph, ncol=3, nrow=2)



