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
entrada1 <- read.table("data_50_1.csv", header=T, sep=";")
#entrada1 <- entrada1[1:2]
entrada3 <- read.table("data_new2.csv", header=T, sep=";")
entrada2 <- read.table("data_new3.csv", header=T, sep=";")

#separando os conjuntos de dados
ns <- entrada3[1:20,2:3]
np <- entrada3[21:40,2:3]
rs <- entrada3[41:60,2:3]
rp <- entrada3[61:80,2:3]

tns <- entrada1[1:20,2:3]
tnp <- entrada1[21:40,2:3]
trs <- entrada1[41:60,2:3]
trp <- entrada1[61:80,2:3]

aa<-cbind(tns$cidades,tns$tempo)
bb<-cbind(tnp$cidades,tnp$tempo)
cc<-cbind(trs$cidades,trs$tempo)
dd<-cbind(trp$cidades,trp$tempo)

plot(aa,type="o",col="blue",xlab="cidades",ylab="tempo (s)")
plot(bb,type="o",col="red",xlab="cidades",ylab="tempo (s)")
plot(cc,type="o",col="green",xlab="cidades",ylab="tempo (s)")
plot(dd,type="o",col="Purple",xlab="cidades",ylab="tempo (s)")

dotchart(aa)

#yn<-c(seq(5,100,5))
#xn<-c(seq(0,1805,95))

dadosiniciais <- rbind(ns,np,rs,rp)

boxplot(tempo~base,
        data = dadosiniciais,
        ylab = "Tempo (segundos)",
        names=c("Sd Ts Raspberry/Notebook","Sd Tp Raspberry/Notebook","Sd Notebook Ts/Tp","Sd Raspberry Ts/Tp"),
        ylim=c(0,7.8))

hist(np$tempo, xlab = "Sd Tp Raspberry/Notebook")
hist(rs$tempo, xlab = "Sd Notebook Ts/Tp")
hist(ns$tempo, xlab = "Sd Ts Raspberry/Notebook")
hist(rp$tempo, xlab = "Sd Raspberry Ts/Tp")

ef_nt <- cbind(entrada2$cidades[81:100], entrada2$tempo[81:100])
plot(ef_nt,type="o",col="orange",xlab="Cidades",ylab="Percentual (%)")
ef_rp <- cbind(entrada2$cidades[101:120], entrada2$tempo[101:120])
plot(ef_rp,type="o",col="black",xlab="Cidades",ylab="Percentual (%)")

hist(ef_nt, xlab = "Eficiência Notebook")
hist(ef_rp, xlab = "Eficiência Notebook")

#plot(amostra1,col="black",xlim=c(mina,maxa),ylim=c(mina,maxa), xlab="", ylab="")
#par(new=T)
#plot(amostra2,col="black",xlim=c(mina,maxa),ylim=c(mina,maxa), xlab="", ylab="")




#-----------------------------------
tsrn<-entrada2$tempo[1:20]
tprn<-entrada2$tempo[21:40]
ntsp<-entrada2$tempo[41:60]
rpsp<-entrada2$tempo[61:80]

print(mean(tsrn))
print(mean(tprn))
print(mean(ntsp))
print(mean(rpsp))
print(mean(ef_nt))
print(mean(ef_rp))

print(median(tsrn))
print(median(tprn))
print(median(ntsp))
print(median(rpsp))
print(median(ef_nt))
print(median(ef_rp))

print(sd(tsrn))
print(sd(tprn))
print(sd(ntsp))
print(sd(rpsp))
print(sd(ef_nt))
print(sd(ef_rp))

