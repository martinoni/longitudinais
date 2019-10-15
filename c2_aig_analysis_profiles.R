library(MASS)
library(Matrix)
library(mvtnorm)
library(fMultivar)
library(mnormt)
library(lattice)
library(lme4)
library(nlme)
library(RLRsim)
library(StatDA)
library(car)

#====================================================================#

# Setting working directory
# 
# setwd("~/Documents/2016/Artigos/Artigo_Julio&Rocha/artigo sweave e rstudio/ArtigoJAS/Example1AIG")

# Reading data set
#dados <- read.table("//Users//franciscomarcelorocha//Documents//2012//Artigo_Julio&Rocha//Programas simulação//dados_PIG_AIG_f.txt",header=T,na="NA")

# dados <- read.table("dados_PIG_AIG_f.txt",header = T,na = "NA")



# setwd("C:/Users/Danvah/Desktop")
# library(readxl)
# dados <- read_excel("AIG_PIG_JAS.xls", skip=1, na="NA")
# dados$Grupo[dados$Grupo=='AIG'] <- 'AIG'
# dados$Grupo[dados$Grupo=='PIG'] <- 'PIG'
# 
# dados <- as.data.frame(dados);
# # dados[is.na(dados)] <- 'NA';



setwd("C:/Users/Danvah/OneDrive/diagnostico_singer/projeto_DDVE")
library(readxl)
dados <- read_excel("perfDDVEp_bait.xlsx")
id <- c(1:61)
dados <- cbind(id,dados)


#====================================================================#
# Separando os dados em AIG e PIG

d <- split(dados,dados$Grupo)
aig <- d$AIG
pig <- d$PIG


dados <- as.data.frame(dados)

#====================================================================#
# Criando variáveis para ajuste dos perfis individuais

x <- 26:40
# Changing week variable to month
#xc <- (x - 26) / 4.29  
xc <- (x - 33) / 4.29  
#xc <-  x  - mean(x)
xc2 <- xc ^ 2
xc3 <- xc ^ 3
np <- 3 # número de parâmetros

######################################################################
############################ Analisando AIG ##########################
######################################################################
nobs.aig <- matrix(0,dim(aig)[1],1)
# Construindo o conjunto de dados para ajustar o polin?mio do 3 grau

for (i in 1:dim(aig)[1]) { 
  yi <- (aig[i,3:17])                       
  num.na <- length(yi[yi == 'NA'])
  # num.na <- length(yi[which(is.na(yi)==TRUE)])
  num.val <- length(yi) - num.na
  nobs.aig[i] <- num.val
}
aig$nobs <- nobs.aig

#====================================================================#

# selecionando os dados para ajuste dos perfis individuais
aig.fitind <- subset(aig,aig$nobs >= (np + 1))

# Gráfico
# 
min.aux <- c("week26","week27","week28","week29","week30","week31",
             "week32","week33","week34","week35","week36","week37",
             "week38","week39","week40")

vecaig <- reshape(aig, direction = "long", varying = 3:17,v.names = "Diameter",
                  timevar = "Weeks", time = as.factor(min.aux))
vecaig <- vecaig[order(vecaig$id), ]
vecaig$weekv <- rep(xc, dim(aig)[1], each = T)
vecaig$subject <- vecaig$ind

vecaig <- na.exclude(vecaig)
# vecaig

g <- xyplot(Diameter ~ weekv |  Grupo, Grupos = id, pch = 16,
            par.settings = standard.theme(quartz,color = F),
            scales = list(x = list(relation = 'same'), 
                          y = list(relation = 'same')),type = 'l',
            lty = 1,as.table = TRUE,
            panel = function(x, y, col, ...) {
              panel.xyplot(x, y, col = col, ...)
              panel.average(x, y, fun = mean, horizontal = F,
                            lwd = 4, col = 'black', ...)   
            },
            ylab = "Aorta diameter (mm/kg)", 
            xlab = expression(bold(paste("Mean Profile in bold"))),
            na.action = na.omit,data = vecaig)

g

#====================================================================#
# excluded individuals treatment AIG
ind.exclude.aig <- dim(subset(aig,aig$nobs < (np+1)))[1]
ind.exclude.aig

#====================================================================#
#
#### Calculando as quantidades básicas para a utilização do método ###

m <- dim(aig.fitind)[1] # Número de indivíduos
peso <- sqrt((m /(m - 1)))
nobst <- sum(aig.fitind$nobs)
#peso.cov<- sqrt((nobs/(nobs-1)))
nu <- nobst - m*np
alfab <- 0.05/(m*np )
#g.cov<-nobs^2 - nobs*np

#====================================================================#
#
## Ajustando modelo normal linear para aig ##
head(aig.fitind);

tabela.aig <- matrix(0,dim(aig.fitind)[1],9)

for (i in 1:dim(aig.fitind)[1]) {
  
  yi <- (aig.fitind[i,3:17])
  nyi <- as.numeric(yi)                       
  fit.aig.ind <- lm(nyi ~ xc + xc2,na.action = na.omit)
  result.aig <- summary(fit.aig.ind)
  sigma2i <- result.aig$sigma ^ 2                        
  
  tabela.aig[i,1:3] <- fit.aig.ind$coef
  X <- model.matrix(fit.aig.ind)
  #   tabela[i,5:8]<- diag(solve(t(X)%*%X))
  tabela.aig[i,4:6] <- (result.aig$coeff[,2]/result.aig$sigma ) ^ 2 #xitxi
  tabela.aig[i,7] <- aig.fitind[i,18]
  tabela.aig[i,8] <- sigma2i
  tabela.aig[i,9] <- sigma2i*(aig.fitind[i,18] - np)
}

tabela.aig[,1:3]

#====================================================================#
# Estimate of the Conditional variance 
sigma2 <- sum(tabela.aig[,9])/nu
#====================================================================#
#
#  Efeitos Fixos

aig.fixed.effects <- matrix(0,np,np)

# Linear coefficient
aig.fixed.effects[1,1] <- mean(tabela.aig[,1])
aig.fixed.effects[1,2] <- ((sqrt(sigma2*sum(tabela.aig[,4])) )/m)
aig.fixed.effects12 <- aig.fixed.effects[1,1] / aig.fixed.effects[1,2]
aig.fixed.effects[1,3] <- ifelse(aig.fixed.effects12 > 0,
                                 (2*pt(-aig.fixed.effects12,df = nu)),
                                 (2*pt(aig.fixed.effects12,df = nu)))
#====================================================================#
# Slope coefficient
aig.fixed.effects[2,1] <- mean(tabela.aig[,2])
aig.fixed.effects[2,2] <- ((sqrt(sigma2*sum(tabela.aig[,5])) )/m)
aig.fixed.effects22 <- aig.fixed.effects[2,1] / aig.fixed.effects[2,2]
aig.fixed.effects[2,3] <- ifelse(aig.fixed.effects22 > 0,
                                 (2*pt(-aig.fixed.effects22,df = nu)),
                                 (2*pt(aig.fixed.effects22,df = nu)))
#====================================================================#
# Quadratic coefficient
aig.fixed.effects[3,1] <- mean(tabela.aig[,3])
aig.fixed.effects[3,2] <- ((sqrt(sigma2*sum(tabela.aig[,6])) )/m)
aig.fixed.effects32 <- aig.fixed.effects[3,1] / aig.fixed.effects[3,2]
aig.fixed.effects[3,3] <- ifelse(aig.fixed.effects32 > 0,
                                 (2*pt(-aig.fixed.effects32,df = nu)),
                                 (2*pt(aig.fixed.effects32,df = nu)))
# #====================================================================#
# # Cubic coefficient
# aig.fixed.effects[4,1] <- mean(tabela.aig[,4])
# aig.fixed.effects[4,2] <- ((sqrt(sigma2*sum(tabela.aig[,7])) )/m)
# aig.fixed.effects42 <- aig.fixed.effects[4,1] / aig.fixed.effects[4,2]
# aig.fixed.effects[4,3] <- ifelse(aig.fixed.effects42 > 0,
#                                  (2*pt(-aig.fixed.effects42,df = nu)),
#                                  (2*pt(aig.fixed.effects42,df = nu)))

aig.fixedeffects <- data.frame(aig.fixed.effects)
colnames(aig.fixedeffects) <- c("Estimate", "s.e.","p-value")
options(scipen=12);
aig.fixedeffects

######################################################################

# Random effects identifications

######################################################################

matrizw <- matrix(0,m,2*np)
# Incluindo na matriz as variáveis w
for (i in 1:np) {
  matrizw[,i] <- tabela.aig[,i]/sqrt(tabela.aig[,(np + i)])  
}

for (j in 1:m) {
  for (k in 1:np) {
    matrizw[j,(k + np)] <- (peso/sqrt(sigma2))*(matrizw[j,k] - 
                                                  mean(matrizw[,k]))
  }
}

aux <- matrix(0,m,np)
for (i in 4:6) { 
  for (j in 1:m) {
    aux[j,(i - np)] <- ifelse(matrizw[j,i] <= qt(alfab,nu) || 
                                matrizw[j,i] >= abs(qt(alfab,nu)),1,0)
  }
}

rand.effect <- apply(aux,2,mean)
rand.effect


var(tabela.aig[,1:3])
######################################################################
######################################################################
######################################################################