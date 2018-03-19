#TCC - PAULINELLY

#limpar workspace
rm(list=ls())

#limpar tela
cat('\014')

#Bibliotecas de interesse
library(mclust)
library(car)
library(lmtest)
library(multcomp)

#FUNCOES
#funcao que ajusta outliers de qualquer amostra
ajustaOutliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  
  for(i in 1:length(y)) {
    #caso o primeiro valor seja NA procura o proximo valor nao NA e coloca
    #no lugar do NA
    if (is.na(y[1]) == TRUE){
      encontrou = FALSE
      cont = 1
      posterior = NA
      #procura o primeiro numero POSTERIOR ao valor atual que nao seja NA
      while (encontrou == FALSE){
        if (is.na(y[1+cont]) == TRUE){
          cont <- cont + 1
        }else{
          posterior <- y[1+cont];
          encontrou <- TRUE
        }
      }
      
      y[1] <- posterior
    }
    
    #caso o ultimo valor seja NA procura o primeiro valor anterior que nao NA e coloca
    #no lugar do NA
    if (is.na(y[length(y)]) == TRUE){
      encontrou <- FALSE
      cont <- 1
      anterior <- NA
      
      #procura o primeiro numero ANTERIOR ao valor atual que nao seja NA
      while (encontrou == FALSE){
        if (is.na(y[length(y)-cont]) == TRUE){
          cont <- cont + 1
        }else{
          anterior <- y[length(y)-cont];
          encontrou <- TRUE
        }
      }
      
      y[length(y)] <- anterior
    }
    
    
    
    if (is.na(y[i])==TRUE){
      encontrou <- FALSE
      cont <- 1
      anterior <- NA
      
      #procura o primeiro numero ANTERIOR ao valor atual que nao seja NA
      while (encontrou == FALSE){
        if (is.na(y[i-cont]) == TRUE){
          cont <- cont + 1
        }else{
          anterior <- y[i-cont];
          encontrou <- TRUE
        }
      }
      
      encontrou = FALSE
      cont = 1
      posterior = NA
      
      #procura o primeiro numero POSTERIOR ao valor atual que nao seja NA
      while (encontrou == FALSE){
        if (is.na(y[i+cont]) == TRUE){
          cont <- cont + 1
        }else{
          posterior <- y[i+cont];
          encontrou <- TRUE
        }
      }
      
      #executa uma media entre o anterior e posterior valor valido na serie e insere no lugar do outlier
      y[i] <- (anterior+posterior)/2
    }
  }
  
  return(y)
}

#Coloca amostra nos intervalos proporcionais entre 0 e 1
padroniza <- function(s){
  retorno <- (s - min(s))/(max(s)-min(s))
  return(retorno)
}

#executando a leitura do arquivo
entrada1 <- read.table("data_30_1.csv", header=T, sep=";")
#entrada1 <- entrada1[1:2]
entrada2 <- read.table("data_30_2.csv", header=T, sep=";")


#separando os conjuntos de dados
ns <- entrada1[1:50,]
np <- entrada1[51:100,]
rs <- entrada1[101:150,]
rp <- entrada1[151:200,]

dadosiniciais <- rbind(ns,np,rs,rp)

boxplot(tempo~base,
        data = dadosiniciais,
        ylab = "Tempo (segundos)",
        names=c("Notebook Paralelo","Notebook Sequencial","Raspberry Paralelo","Raspberry Sequencial"),
        ylim=c(0,1500))

qqPlot(np$tempo, ylab = "Tempo (segundos) para Notebook Paralelo", xlab="")
qqPlot(ns$tempo, ylab = "Tempo (segundos) para Notebook Sequencial", xlab="")
qqPlot(rp$tempo, ylab = "Tempo (segundos) para Raspberry Paralelo", xlab="")
qqPlot(rs$tempo, ylab = "Tempo (segundos) para Raspberry Sequencial", xlab="")

boxplot(np$tempo)
boxplot(ns$tempo)
boxplot(rp$tempo)
boxplot(rs$tempo)

#ajustando outliers
ns[,2] <- ajustaOutliers(ns[,2])
np[,2] <- ajustaOutliers(ajustaOutliers(np[,2]))
rs[,2] <-  ajustaOutliers(rs[,2])
rp[,2] <-  ajustaOutliers(rp[,2])

dadosiniciais <- rbind(ns,np,rs,rp)

# Sumariza as principais medidas estatisticas levando em conta toda a amostra de dados
print(summary(dadosiniciais))

#avaliacao da dispersao dos tempos em cada amostra
#primeira averiguacao da existencia de possiveis outliers
boxplot(tempo~base,
        data = dadosiniciais,
        ylab = "Tempo (segundos)",
        names=c("Notebook Tp","Notebook Tp","Raspberry Tp","Raspberry Ts"),
        ylim=c(0,1500))

#analise individual de cada amostra para averiguacao dos outliers
#este grafico compara uma normal teorica com intervalo de confiança
#com os dados da amostra

qqPlot(np$tempo, ylab = "Tempo (segundos) para Notebook Paralelo", xlab="")
qqPlot(ns$tempo, ylab = "Tempo (segundos) para Notebook Sequencial", xlab="")
qqPlot(rp$tempo, ylab = "Tempo (segundos) para Raspberry Paralelo", xlab="")
qqPlot(rs$tempo, ylab = "Tempo (segundos) para Raspberry Sequencial", xlab="")

boxplot(np$tempo)
boxplot(ns$tempo)
boxplot(rp$tempo)
boxplot(rs$tempo)

#CONFIGURAÇÃO DO EXPERIMENTO-----------------------------------
alpha <- 0.001 #nivel de significancia, para o definicao do intervalo de confianca
#no caso IC - alpha = 0.99, ou seja, 99%
n <- 50 #potencia do teste
#a potencia aumenta conforme aumenta o tamanho da amostra
#desvio padrao alto diminui a potencia do teste
delta <- 0.1 #minimo efeito prático de significancia
#minima variacao que o experimento teve capacidade de captar
a <- 4 #quantidade de grupos

#EXECUTANDO OS TESTES DE COMPARACAO-------------------------------
amostra <- dadosiniciais

#Averiguação de diferenças com ANOVA
modelo <- aov(tempo~base,data = amostra)
print(summary.aov(modelo))

#shapiro-wilk é um teste para numericamente avaliar se uma amostra de dados
#possui aderencia a uma distribuicao normal (p-valor tem que ser menor que 0.05)
sha = shapiro.test(modelo$residuals)
print(sha)

#Averiguação de homocedasticidade
#fligner-killen é um teste para numericamente avaliar se as amostras tem variancias
#nao muito destoantes umas das outras
fli = fligner.test(tempo~base, data = amostra)
print(fli)

plot(x    = modelo$fitted.values,
     y    = modelo$residuals,
     xlab = "Variâncias",
     ylab = "Amostras")

#Averiguação de Independência
#o teste de durbin-watson avalia se houve alguma influencia na coleta dos
#valores da amostra
durbinWatsonTest(modelo)

#DERIVAÇÃO DOS INTERVALOS DE CONFIANÇA----------------------------
#Comparação TODOS X TODOS
sistema_tukey <- glht(modelo, 
                      linfct = mcp(base = "Tukey"))
sistema_tukey_CI <- confint(sistema_tukey, 
                            level = (1-alpha))

print(sistema_tukey_CI)

par(mar = c(5,12,4,2))
plot(sistema_tukey_CI, 
     xlab       = "Tempos (segundos)",
     xlim       = c(-5,1500),
     main=paste("Comparações com",(1-alpha)*100,"% de nível de confiança"))


hist(np$tempo, xlab = "Tp Notebook")
hist(rs$tempo, xlab = "Ts Raspberry")
hist(ns$tempo, xlab = "Ts Notebook")
hist(rp$tempo, xlab = "Tp Raspberry")
