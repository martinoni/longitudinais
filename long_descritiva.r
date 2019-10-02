

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


ggplot(data=DDVE.l, aes(x=semana, y=diam, group = id,color=grupo)) +
  geom_line()+
  geom_point()+
  scale_color_manual(values = c("blue","red")) 

diam <- c()
grupo <- c()
semana <- c()
id <- c()
for(j in 2:ncol(DDVE)){
  id <- c(id,"mediaAIG")
  grupo <- c(grupo,"AIG")
  semana <- c(semana,(j+24))
  media <- 0
  n <- 0
  for(i in 1:29){
    if(!is.na(DDVE[i,j])){
      media <- media + as.numeric(DDVE[i,j])
      n <- n + 1
    }
  }
  diam <- c(diam,media/n)
  id <- c(id,"mediaPIG")
  grupo <- c(grupo,"PIG")
  semana <- c(semana,(j+24))
  media <- 0
  n <- 0
  for(i in 30:nrow(DDVE)){
    if(!is.na(DDVE[i,j])){
      media <- media + as.numeric(DDVE[i,j])
      n <- n + 1
    }
  }
  diam <- c(diam,media/n)
}

media.df <- data.frame(id,grupo,semana,diam)
tipo <- c(rep("indiv",284),rep("media",30))

DDVE.t <- rbind(DDVE.l,media.df)
DDVE.t <- cbind(DDVE.t,tipo)

ggplot(data=DDVE.t, aes(x=semana, y=diam, group = id,color=grupo)) +
  geom_line(aes(linetype = tipo, size = tipo))+
  geom_point() +
  scale_linetype_manual(values = c("solid", "dashed")) +
  scale_color_manual(values = c("blue","red")) +
  scale_size_manual(values = c(0.5,2))


aig <- subset(DDVE.t, grupo == "AIG")
pig <- subset(DDVE.t, grupo == "PIG")

ggplot(data=aig, aes(x=semana, y=diam, group = id)) +
  geom_line(aes(linetype = tipo, size = tipo),colour="blue")+
  geom_point(colour="blue") +
  scale_linetype_manual(values = c("solid", "dashed")) +
  scale_size_manual(values = c(0.5,2))


ggplot(data=pig, aes(x=semana, y=diam, group = id)) +
  geom_line(aes(linetype = tipo, size = tipo),colour="red")+
  geom_point(colour="red") +
  scale_linetype_manual(values = c("solid", "dashed")) +
  scale_size_manual(values = c(0.5,2))

ggplot(data=media.df, aes(x=semana, y=diam, group = id,color=grupo))+
  geom_line() +
  geom_point() +
  scale_color_manual(values = c("blue","red"))


library(magrittr)
library(dplyr)
aux_AIG <- DDVE.l %>% filter(grupo == "AIG") %>% count(by = semana)
aux_PIG <- DDVE.l %>% filter(grupo == "PIG") %>% count(by = semana)
tabela <- inner_join(aux_AIG, aux_PIG, by = "by")
names(tabela) <- c("Semana", "Quantidade de AIG", "Quantidade de PIG")
tabela







library(plyr)
resumo = ddply(DDVE.t, .(grupo, semana), function(x){
  c(mean=mean(x$diam), sd = sd(x$diam))
})

pd <- position_dodge(width = 0.3)
ggplot(resumo, aes(x=semana, y=mean, color=grupo)) +
  geom_line(aes(linetype=grupo)) +
  geom_point(position=pd) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=0.3, position=pd) +
  scale_color_manual(values = c("blue","red"))






ggplot(data=DDVE.t, aes(x=semana, y=diam, color=grupo)) +
  geom_point() +
  geom_smooth(aes(x=semana),method="loess",se = FALSE,span=1) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  scale_color_manual(values = c("blue","red")) +
  scale_size_manual(values = c(0.5,2))





ggplot(data=aig, aes(x=semana, y=diam)) +
  geom_point(colour="blue") +
  stat_summary(fun.y=mean, geom="line",lwd=1.0) +
  geom_smooth(aes(x=semana),method="loess",se = FALSE,span=1) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  scale_size_manual(values = c(0.5,2))



ggplot(data=pig, aes(x=semana, y=diam)) +
  geom_point(colour="red") +
  stat_summary(fun.y=mean, geom="line",lwd=1.0) +
  geom_smooth(aes(x=semana),method="loess",se = FALSE,span=1) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  scale_size_manual(values = c(0.5,2))





ggplot(data=media.df, aes(x=semana, y=diam, group = id,color=grupo))+
  geom_line() +
  geom_point() +
  scale_color_manual(values = c("blue","red"))





ggplot(data=DDVE.l, aes(x=semana, y=diam, group = grupo,color=grupo)) +
  stat_summary(fun.y=mean, geom="line",lwd=1.0) +
  scale_color_manual(values = c("blue","red")) 





ggplot(data=DDVE.l, aes(x=semana, y=diam, group = grupo,color=grupo)) +
  geom_point()+
  stat_summary(fun.y=mean, geom="line",lwd=1.0) +
  scale_color_manual(values = c("blue","red")) 






ggplot(data=aig, aes(x=semana, y=diam, grup=id)) +
  geom_point(colour="blue") +
  geom_smooth(method="loess",se = FALSE,span=1) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  scale_size_manual(values = c(0.5,2))







library(plotly)
ggplot(data=aig, aes(x=semana, y=diam, grup=id)) +
  geom_point(colour="blue") +
  geom_smooth(method="loess",se = FALSE,span=1) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  scale_size_manual(values = c(0.5,2))
ggplotly()


aig_1 <- subset(aig, id == 1)
ggplot(data=aig_1, aes(x=semana, y=diam)) +
  geom_point(colour="blue") +
  geom_smooth(method="loess",se = FALSE,span=1) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  scale_size_manual(values = c(0.5,2))+
  scale_x_continuous(limits = c(26, 40)) + 
  scale_y_continuous(limits = c(6, 20)) 


aig_22 <- subset(aig, id == 22)
ggplot(data=aig_22, aes(x=semana, y=diam)) +
  geom_point(colour="blue") +
  geom_smooth(method="loess",se = FALSE,span=1) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  scale_size_manual(values = c(0.5,2))+
  scale_x_continuous(limits = c(26, 40)) + 
  scale_y_continuous(limits = c(6, 20)) 


aig_8 <- subset(aig, id == 8)
ggplot(data=aig_8, aes(x=semana, y=diam)) +
  geom_point(colour="blue") +
  geom_smooth(method="loess",se = FALSE,span=1) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  scale_size_manual(values = c(0.5,2))+
  scale_x_continuous(limits = c(26, 40)) + 
  scale_y_continuous(limits = c(6, 20)) 



aig_20 <- subset(aig, id == 20)
ggplot(data=aig_20, aes(x=semana, y=diam)) +
  geom_point(colour="blue") +
  geom_smooth(method="loess",se = FALSE,span=1) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  scale_size_manual(values = c(0.5,2))+
  scale_x_continuous(limits = c(26, 40)) + 
  scale_y_continuous(limits = c(6, 20)) 



aig_28 <- subset(aig, id == 28)
ggplot(data=aig_28, aes(x=semana, y=diam)) +
  geom_point(colour="blue") +
  geom_smooth(method="loess",se = FALSE,span=1) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  scale_size_manual(values = c(0.5,2))+
  scale_x_continuous(limits = c(26, 40)) + 
  scale_y_continuous(limits = c(6, 20)) 





library(plotly)
ggplot(data=pig, aes(x=semana, y=diam, grup=id)) +
  geom_point(colour="red") +
  geom_smooth(method="loess",se = FALSE,span=1) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  scale_size_manual(values = c(0.5,2))
ggplotly()


pig_58 <- subset(pig, id == 58)
ggplot(data=pig_58, aes(x=semana, y=diam)) +
  geom_point(colour="red") +
  geom_smooth(method="loess",se = FALSE,span=1) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  scale_size_manual(values = c(0.5,2))+
  scale_x_continuous(limits = c(26, 40)) + 
  scale_y_continuous(limits = c(6, 20)) 


pig_37<- subset(pig, id == 37)
ggplot(data=pig_37, aes(x=semana, y=diam)) +
  geom_point(colour="red") +
  geom_smooth(method="loess",se = FALSE,span=1) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  scale_size_manual(values = c(0.5,2))+
  scale_x_continuous(limits = c(26, 40)) + 
  scale_y_continuous(limits = c(6, 20)) 


pig_51 <- subset(pig, id == 51)
ggplot(data=pig_51, aes(x=semana, y=diam)) +
  geom_point(colour="red") +
  geom_smooth(method="loess",se = FALSE,span=1) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  scale_size_manual(values = c(0.5,2))+
  scale_x_continuous(limits = c(26, 40)) + 
  scale_y_continuous(limits = c(6, 20)) 


pig_52 <- subset(pig, id == 52)
ggplot(data=pig_52, aes(x=semana, y=diam)) +
  geom_point(colour="red") +
  geom_smooth(method="loess",se = FALSE,span=1) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  scale_size_manual(values = c(0.5,2))+
  scale_x_continuous(limits = c(26, 40)) + 
  scale_y_continuous(limits = c(6, 20)) 


pig_32 <- subset(pig, id == 32)
ggplot(data=pig_32, aes(x=semana, y=diam)) +
  geom_point(colour="red") +
  geom_smooth(method="loess",se = FALSE,span=1) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  scale_size_manual(values = c(0.5,2))+
  scale_x_continuous(limits = c(26, 40)) + 
  scale_y_continuous(limits = c(6, 20)) 


pig_31 <- subset(pig, id == 31)
ggplot(data=pig_31, aes(x=semana, y=diam)) +
  geom_point(colour="red") +
  geom_smooth(method="loess",se = FALSE,span=1) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  scale_size_manual(values = c(0.5,2))+
  scale_x_continuous(limits = c(26, 40)) + 
  scale_y_continuous(limits = c(6, 20)) 



pig_37 <- subset(pig, id == 37)
ggplot(data=pig_37, aes(x=semana, y=diam)) +
  geom_point(colour="red") +
  geom_smooth(method="loess",se = FALSE,span=1) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  scale_size_manual(values = c(0.5,2))+
  scale_x_continuous(limits = c(26, 40)) + 
  scale_y_continuous(limits = c(6, 20)) 

