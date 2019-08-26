library(readxl)

DDVE <- read_excel("C:/Users/lucasb/Downloads/perfDDVEp_bait.xlsx")

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
