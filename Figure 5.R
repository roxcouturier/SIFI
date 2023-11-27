#### data ####
library(here)
library(survival)
library(ggplot2)
library(survminer)
library(ggforce)
library(ggnewscale)
PFS <- read_delim(here("./Figures/Figure 5/PFS.csv"), delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(PFS)

#Sort the dataset according to follow-up times: give a rank to the patients
#patients with the longest follow-up time = patient with the longest rank 

indices_tri <- order(PFS$time) 
PFS <- PFS[indices_tri, ]
PFS$rank <- seq(1,nrow(PFS),1)

####

n1<-nrow(PFS) #number of lines: patient databases
p = 1 #a single parameter: a single variable treatment



########### Model de cox ############


model<-coxph(Surv(time, event)~ Trt=="A", data=PFS, method="breslow")

##########Martingale residual standardized and adjusted form ##############


XX <- model.matrix(~ Trt, data = PFS)
H<-diag(XX%*%solve(t(XX)%*%XX)%*%t(XX))

#standardized

mresids <- residuals( model, type="martingale" )

sres<-(mresids)/sqrt(1-H)
S=sum(sres^2)/(n1-p)
tm<-mresids/S*sqrt(1-H)
CDM<-(H*tm^2)/((p+1)*(1-H))

#adjusted

Amresids<-(mresids-mean(mresids))/sd(mresids)
AS=sum(Amresids^2)/(n1-p)
Atm<-Amresids/AS*sqrt(1-H)
ACDM<-(H*Atm^2)/((p+1)*(1-H))


##########Plot ##############


PFS$CDM <- CDM

#graph: standardized martingale residuals for the PFS dataset


ggplot(PFS, aes(x = rank, y = CDM, shape = factor(Trt), color = factor(event))) +
  geom_point(size = 2) +
  scale_color_manual(name = "Event", values = c("blue", "red")) +
  xlab("rank") + ylab("CDM") +
  labs(shape = "Treatment")+#, color = "Event") +
  new_scale_color()+
  geom_ellipse(data = NULL, mapping = aes(x0 = 3, y0 = .0526, b = .0005, a = 1.2, angle = 0, color = "Flip"), inherit.aes = FALSE)+
  geom_ellipse(data = NULL, mapping = aes(x0 = 4, y0 = .0517, b = .002, a = 2.7, angle = 0, color = "Clone/Remove"), inherit.aes = FALSE)+
  scale_color_manual(name = "SIFI", values = c("black", "green")) + 
  theme(
    panel.background = element_rect(fill = "white"),  # Set background color to white
    panel.grid.major = element_line(color = "lightgrey", size = 0.5),  # Adjust major grid lines
    panel.grid.minor = element_blank(), # Hide minor grid lines
   axis.text.x = element_text(size = 12),   # Increase x-axis label text size
          axis.text.y = element_text(size = 12)
  ) +
  scale_x_continuous(breaks = seq(0, 155, by = 10)) +  # Adjust x-axis breaks (from 0 to 10 by 1)
  scale_y_continuous(breaks = seq(0, 0.1, by = 0.01)) 
  

#the geom_ellipse are to represent the subjects selected by the SIFI flip/clone/remove
#point chooses geom_ellispe by hand because we know the SIFI which was calculated before

