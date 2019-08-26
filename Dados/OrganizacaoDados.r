library(readxl)
library(ggplot2)

DDVE <- read_excel("C:/Users/rpassos/Desktop/longitudinais-master/Dados/perfDDVEp_bait.xlsx")

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
  geom_point()

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


aux_AIG <- DDVE.l %>% filter(grupo == "AIG") %>% count(by = semana)
aux_PIG <- DDVE.l %>% filter(grupo == "PIG") %>% count(by = semana)
tabela <- inner_join(aux_AIG, aux_PIG, by = "by")
names(tabela) <- c("Semana", "Quantidade de AIG", "Quantidade de PIG")
