

library(readxl)
library(ggplot2)

setwd("C:/Users/Danvah/OneDrive/diagnostico_singer/DDVE")
DDVE <- read_excel("perfDDVEp_bait.xlsx")

id <- c()
grupo <- c()
diam <- c()
semana <- c()

for(i in 1:nrow(DDVE)){
  for(j in 2:(ncol(DDVE))){
    if(!is.na(DDVE[i,j])){
      id <- c(id,i)
      grupo <- c(grupo,DDVE$Grupo[i])
      diam <- c(diam,as.numeric(DDVE[i,j]))
      semana <- c(semana,(j+24))
    }
  }
}
DDVE.l <- data.frame(id,grupo,semana,diam)


library(nlme)
fit_1 <- lme(diam ~ I(as.numeric(grupo == 'AIG')) + 
               I(as.numeric(grupo == 'PIG')) +
               I(as.numeric(I(grupo == 'AIG'))*semana) +
               I(as.numeric(I(grupo == 'PIG'))*semana) - 1 , random = ~ 1| id, data=DDVE.l)

summary(fit_1)

dados <- coef(fit_1)


ggplot(data=DDVE.l, aes(x=semana, y=diam, group = grupo,color=grupo)) +
  geom_abline(intercept = 25.533098, slope = -0.463335, color="blue") +
  geom_abline(intercept = 30.347679 , slope = -0.559927, color="red") +
  scale_x_continuous(limits = c(26, 40)) + 
  scale_y_continuous(limits = c(6, 20)) +
  geom_point()+
  scale_color_manual(values = c("blue","red")) 



DDVE.l1 <- subset(DDVE.l, id==1)
ggplot(data=DDVE.l1, aes(x=semana, y=diam )) +
  geom_abline(intercept = 25.533098+1.49045549, slope = -0.463335, color="blue") +
  geom_point(color="blue")+
  scale_x_continuous(limits = c(26, 40)) + 
  scale_y_continuous(limits = c(6, 20)) +
  scale_color_manual(values = c("blue","red")) 



