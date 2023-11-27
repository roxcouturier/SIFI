
sim_donnee_sans_censure_utilisation_sifi <- function(lambdaB, lambdaA ,n1, n2, kb,ka){
  # 1/ lambdaB = risk of having the event in arm B ; lambdaB = scale weibull distribution for group B 
  # 1/ lambdaA = risk of having the event in arm A ; lambdaA = scale weibull distribution for group A 
  #n1 = sample size of group B
  #n2 = sample size of group A 
  #kb = shape weibull distribution for group B
  #ka = shape weibull distribution for group A
  
  
  #control arm  
  B <- data.frame(trt = rep("B", n1))
  B$time <- rweibull(n1, scale = lambdaB , shape= kb) /30.5
  B$event = 1
  
  #experimental arm
  
  A <- data.frame(trt=rep("A",n2))
  A$time <- rweibull(n2, scale = lambdaA , shape= ka)  /30.5
  A$event = 1
  
  
  data=bind_rows(A,B)
  
  
  #####!!!! changer les parametres et la fonction qui nous interesse !!!!### 
  
  ###si on veut utiliser le sifi orginal 
  #changer la direction et l'operation souhaitées 
  sifi_res <- sifi(sv_data = data[,c(2,3,1)],direction = "best", operation = "flip", agnostic = T)
  
  ###si on veut utiliser le sifi modifié remove 
  #sifi_res <-  sifi_remove(sv_data = data[,c(2,3,1)],direction = "best", agnostic = T)
  
  
  ### si on veut utiliser sifi random flip/clone 
  # sifi_res <- sifi_random(sv_data = data[,c(2,3,1)], operation = "flip", agnostic = T)
  
  ###si on veut utiliser sifi random remove 
  # sifi_res <- sifi_random_remove(sv_data =  data[,c(2,3,1)], agnostic = T)
  
  
  # return(list(pval, sifi_res))
  return(sifi_res)
  
}


######## Scenarios #######



#### under H0 
####n = 132

H0_scenario1a_best_flip <- c(210,210 ,66, 66, 1,1,"best","flip")
H0_scenario1a_best_clone <- c(210,210 ,66, 66, 1,1,"best","clone")
H0_scenario1a_worst_flip <- c(210,210 ,66, 66, 1,1,"worst","flip")
H0_scenario1a_worst_clone <- c(210,210 ,66, 66, 1,1,"worst","flip")
H0_scenario1a_best_remove <- c(210,210 ,66, 66, 1,1,"best")
H0_scenario1a_worst_remove <- c(210,210 ,66, 66, 1,1,"worst")


H0_scenario1b_best_flip <- c(210,210 ,66, 66, 0.7,0.7,"best","flip")
H0_scenario1b_best_clone <- c(210,210 ,66, 66, 0.7,0.7,"best","clone")
H0_scenario1b_worst_flip <- c(210,210 ,66, 66,0.7,0.7,"worst","flip")
H0_scenario1b_worst_clone <- c(210,210 ,66, 66, 0.7,0.7,"worst","flip")
H0_scenario1b_best_remove <- c(210,210 ,66, 66, 0.7,0.7,"best")
H0_scenario1b_worst_remove <- c(210,210 ,66, 66, 0.7,0.7,"worst")


H0_scenario1c_best_flip <- c(210,210 ,66, 66, 2,2,"best","flip")
H0_scenario1c_best_clone <- c(210,210 ,66, 66, 2,2,"best","clone")
H0_scenario1c_worst_flip <- c(210,210 ,66, 66, 2,2,"worst","flip")
H0_scenario1c_worst_clone <- c(210,210 ,66, 66, 2,2,"worst","flip")
H0_scenario1c_best_remove <- c(210,210 ,66, 66, 2,2,"best")
H0_scenario1c_worst_remove <- c(210,210 ,66, 66,2,2,"worst")


H0_scenario1a_flip_random <- c(210,210 ,66, 66, 1,1,"flip")
H0_scenario1a_clone_random <- c(210,210 ,66, 66, 1,1,"clone")
H0_scenario1a_remove_random  <- c(210,210 ,66, 66, 1,1)

####n = 40
H0_scenario1a_best_flip <- c(210,210 ,20, 20, 1,1,"best","flip")
H0_scenario1a_best_clone <- c(210,210 ,20, 20, 1,1,"best","clone")
H0_scenario1a_worst_flip <- c(210,210 ,20, 20, 1,1,"worst","flip")
H0_scenario1a_worst_clone <- c(210,210 ,20, 20, 1,1,"worst","flip")
H0_scenario1a_best_remove <- c(210,210 ,20, 20, 1,1,"best")
H0_scenario1a_worst_remove <- c(210,210 ,20, 20, 1,1,"worst")

####n = 80
H0_scenario1a_best_flip <- c(210,210 ,40, 40, 1,1,"best","flip")
H0_scenario1a_best_clone <- c(210,210 ,40, 40, 1,1,"best","clone")
H0_scenario1a_worst_flip <- c(210,210 ,40, 40, 1,1,"worst","flip")
H0_scenario1a_worst_clone <- c(210,210 ,40, 40, 1,1,"worst","flip")
H0_scenario1a_best_remove <- c(210,210 ,40, 40, 1,1,"best")
H0_scenario1a_worst_remove <- c(210,210 ,40, 40, 1,1,"worst")

####n = 160
H0_scenario1a_best_flip <- c(210,210 ,80, 80, 1,1,"best","flip")
H0_scenario1a_best_clone <- c(210,210 ,80, 80, 1,1,"best","clone")
H0_scenario1a_worst_flip <- c(210,210 ,80, 80, 1,1,"worst","flip")
H0_scenario1a_worst_clone <- c(210,210 ,80, 80, 1,1,"worst","flip")
H0_scenario1a_best_remove <- c(210,210 ,80, 80, 1,1,"best")
H0_scenario1a_worst_remove <- c(210,210 ,80, 80, 1,1,"worst")

####n = 640
H0_scenario1a_best_flip <- c(210,210 ,320, 320, 1,1,"best","flip")
H0_scenario1a_best_clone <- c(210,210 ,320, 320, 1,1,"best","clone")
H0_scenario1a_worst_flip <- c(210,210 ,320, 320, 1,1,"worst","flip")
H0_scenario1a_worst_clone <- c(210,210 ,320, 320, 1,1,"worst","flip")
H0_scenario1a_best_remove <- c(210,210 ,320, 320, 1,1,"best")
H0_scenario1a_worst_remove <- c(210,210 ,320, 320, 1,1,"worst")



####Under H1 

H1_scenario1a_best_flip <- c(210,370 ,66, 66, 1,1,"best","flip")
H1_scenario1a_best_clone <- c(210,370 ,66, 66, 1,1,"best","clone")
H1_scenario1a_worst_flip <- c(210,370 ,66, 66, 1,1,"worst","flip")
H1_scenario1a_worst_clone <- c(210,370 ,66, 66, 1,1,"worst","flip")
H1_scenario1a_best_remove <- c(210,370 ,66, 66, 1,1,"best")
H1_scenario1a_worst_remove <- c(210,370 ,66, 66, 1,1,"worst")


H1_scenario1b_best_flip <- c(210,370 ,66, 66, 0.7,0.7,"best","flip")
H1_scenario1b_best_clone <- c(210,370 ,66, 66, 0.7,0.7,"best","clone")
H1_scenario1b_worst_flip <- c(210,370 ,66, 66,0.7,0.7,"worst","flip")
H1_scenario1b_worst_clone <- c(210,370 ,66, 66, 0.7,0.7,"worst","flip")
H1_scenario1b_best_remove <- c(210,370 ,66, 66, 0.7,0.7,"best")
H1_scenario1b_worst_remove <- c(210,370 ,66, 66, 0.7,0.7,"worst")


H1_scenario1c_best_flip <- c(210,370 ,66, 66, 2,2,"best","flip")
H1_scenario1c_best_clone <- c(210,370 ,66, 66, 2,2,"best","clone")
H1_scenario1c_worst_flip <- c(210,370 ,66, 66, 2,2,"worst","flip")
H1_scenario1c_worst_clone <- c(210,370 ,66, 66, 2,2,"worst","flip")
H1_scenario1c_best_remove <- c(210,370 ,66, 66, 2,2,"best")
H1_scenario1c_worst_remove <- c(210,370 ,66, 66,2,2,"worst")


H1_scenario1a_flip_random <- c(210,370 ,66, 66, 1,1,"flip")
H1_scenario1a_clone_random <- c(210,370 ,66, 66, 1,1,"clone")
H1_scenario1a_remove_random  <- c(210,370 ,66, 66, 1,1)


H1_scenario2a_best_flip <- c(210,370 ,66, 66, 0.7,1,"best","flip")
H1_scenario2a_best_clone <- c(210,370 ,66, 66,  0.7,1,"best","clone")
H1_scenario2a_worst_flip <- c(210,370 ,66, 66,  0.7,1,"worst","flip")
H1_scenario2a_worst_clone <- c(210,370 ,66, 66,  0.7,1,"worst","flip")
H1_scenario2a_best_remove <- c(210,370 ,66, 66,  0.7,1,"best")
H1_scenario2a_worst_remove <- c(210,370 ,66, 66,  0.7,1,"worst")


H1_scenario2b_best_flip <- c(210,370 ,66, 66, 2,1,"best","flip")
H1_scenario2b_best_clone <- c(210,370 ,66, 66, 2,1,"best","clone")
H1_scenario2b_worst_flip <- c(210,370 ,66, 66, 2,1,"worst","flip")
H1_scenario2b_worst_clone <- c(210,370 ,66, 66, 2,1,"worst","flip")
H1_scenario2b_best_remove <- c(210,370 ,66, 66, 2,1,"best")
H1_scenario2b_worst_remove <- c(210,370 ,66, 66, 2,1,"worst")
