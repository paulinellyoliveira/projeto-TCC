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

#executando a leitura do arquivo
entrada1 <- read.table("data_30_1.csv", header=T, sep=";")
entrada1 <- entrada1[1:2]
entrada2 <- read.table("data_30_2.csv", header=T, sep=";")
entrada3 <- read.table("data_new.csv", header=T, sep=";")

#separando os conjuntos de dados
ns <- entrada3[1:50,]
np <- entrada3[51:100,]
rs <- entrada3[101:150,]
rp <- entrada3[151:200,]

ns1 <- entrada1[1:50,]
np1 <- entrada1[51:100,]
rs1 <- entrada1[101:150,]
rp1 <- entrada1[151:200,]

dadosiniciais <- rbind(ns,np,rs,rp)

boxplot(tempo~base,
        data = dadosiniciais,
        ylab = "Tempo (segundos)",
        names=c("Sd Notebook Ts/Tp","Sd Raspberry Ts/Tp","Sd Tp Raspberry/Notebook","Sd Ts Raspberry/Notebook"),
        ylim=c(1.5,5))


hist(np1$tempo, xlab = "Tp Notebook")
hist(rs1$tempo, xlab = "Ts Raspberry")
hist(ns1$tempo, xlab = "Ts Notebook")
hist(rp1$tempo, xlab = "Tp Raspberry")

hist(entrada2$sd_ts_rasp.not, xlab = "Sd Ts Raspberry/Notebook")
hist(entrada2$sd_tp_rasp.not, xlab = "Sd Tp Raspberry/Notebook")
hist(entrada2$sp_not_tse.tp, xlab = "Sd Notebook Ts/Tp")
hist(entrada2$sd_rasp_ts.tp, xlab = "Sd Raspberry Ts/Tp")
hist(entrada2$ef_not, xlab = "Eficiência Notebook")
hist(entrada2$ef_rasp, xlab = "Eficiencia Raspberry")
xa<-seq(1,50)
cc<-cbind(xa,entrada2$ef_not)
dd<-cbind(xa,entrada2$ef_rasp)

plot(cc,type="o",col="blue",xlab="amostra",ylab="Eficiência (%)")
plot(dd,type="o",col="red",xlab="amostra",ylab="Eficiência (%)")

print(mean(np$tempo))
print(mean(rs$tempo))
print(mean(ns$tempo))
print(mean(rp$tempo))

print(median(np$tempo))
print(median(rs$tempo))
print(median(ns$tempo))
print(median(rp$tempo))

print(sd(np$tempo))
print(sd(rs$tempo))
print(sd(ns$tempo))
print(sd(rp$tempo))

print(mean(entrada2$sd_ts_rasp.not))
print(mean(entrada2$sd_tp_rasp.not))
print(mean(entrada2$sp_not_tse.tp))
print(mean(entrada2$ef_not))
print(mean(entrada2$sd_rasp_ts.tp))
print(mean(entrada2$ef_rasp))

print(median(entrada2$sd_ts_rasp.not))
print(median(entrada2$sd_tp_rasp.not))
print(median(entrada2$sp_not_tse.tp))
print(median(entrada2$ef_not))
print(median(entrada2$sd_rasp_ts.tp))
print(median(entrada2$ef_rasp))

print(sd(entrada2$sd_ts_rasp.not))
print(sd(entrada2$sd_tp_rasp.not))
print(sd(entrada2$sp_not_tse.tp))
print(sd(entrada2$ef_not))
print(sd(entrada2$sd_rasp_ts.tp))
print(sd(entrada2$ef_rasp))
