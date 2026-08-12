library(sciplot)
library(ggplot2)
library(hrbrthemes)
library(dplyr)
library(nlme)
library(car)



citation("nlme")
#Hipótese 1####
#Apenas em teias agregadas fêmeas maiores estarão com machos maiores e fêmeas menores estarão com machos menores


#Importei os dados e transformei a id da teia em variável categórica

teiapar= read.table(file= 'teiapar.txt', header=T, dec=',', stringsAsFactors = T)

summary(teiapar)

teiapar$idteia=factor(teiapar$idteia)

summary(teiapar)

# Fizemos um teste análogo a uma Ancova.   
testex=lm(cefamac~cefafem + situacaoteia+ cefafem : situacaoteia, data= teiapar)

#O resíduo e normalidade não ficaram muito bons, vamos fazer um GLS
plot(testex, which = 1, lty = 0)
plot(testex, which = 2, lty = 0)

#fizemos o GLS e depois olhamos a distribuição e normalidade

testegls= gls(cefamac~cefafem + situacaoteia+ cefafem : situacaoteia, weights = varPower(form= ~cefafem), data= teiapar)
plot(testegls)
qqnorm(testegls)
summary(testegls)

citation()
#na Anova fizemos a tipo 3 pois a tipo 1 e tipo 2 atuam de maneira sequencial enquanto a 3 faz as contas simultaneamente. Fazer as contas de maneira sequencial da problema pq a ordem das contas vai alterar o resultado
Anova(testegls, type = 3)
# A única variável que deu significativa foi o cefalotorax da fêmea. Para ver como os dados se comportam só com a variação do tamanho da fêmea fizemos um gls sem as outras variáveis e plotamos a reta de inclinação
testeinclinacao= gls(cefamac~cefafem, weights = varPower(form= ~cefafem), data= teiapar)
summary(testeinclinacao)
#Ao fazer o segundo teste, no entanto, os valores de inclinação mudaram (a alteração nas variáveis preditoras causou isso). Quando plotamos a linha optamos por tentar colocar a linha na posição que tinha dado no teste com tudo, mas não deu certo e o gráfico ficou estranho, não precisamos colocar a linha
teiapar$cor=ifelse(teiapar$situacaoteia== "a", "red","blue" )
plot(cefamac~cefafem, col=cor, teiapar)
#abline(a=0.106, b=0.167)

#observamos a existência de uma fêmea muito pequena "puxando" os resultados para a significância. Tiramos ele para ver o que aconteceria com o resultado
#O resultado não deu significativo mas cabe mais coletas

teiaparso= teiapar[-c(11,12),]

testeglsso= gls(cefamac~cefafem + situacaoteia+ cefafem : situacaoteia, weights = varPower(form= ~cefafem), data= teiaparso)
plot(testeglsso)

Anova(testeglsso, type=3)

  #hipotese 2####
#Apenas em teias agregadas fêmeas desacompanhadas serão as menores

teiasoz= read.table(file= 'teiasoz.txt', header=T, dec=',', stringsAsFactors = T)

summary(teiasoz)  

teiasoz$acomp.num= as.numeric(teiasoz$acomp)-1

testeh2= glm(acomp.num~cefalotorax+ situacaoteia+ cefalotorax:situacaoteia, family = binomial,teiasoz)
#para olhar o residuo na regressão logistica precisa de um pacote que chama dharma

summary(testeh2)

82.376/71
#´é preciso dividir o residual deviance pelo número de graus de liberdade, se estiver muito longe de 1 +- 0.3 é precisocorrigir





testeh2n= glm(acomp.num~1, family = binomial,teiasoz)
anova(testeh2,testeh2n, test = "Chi")

plot(acomp.num~cefalotorax, teiasoz)

teiasozso=teiasoz[-(45),]

testeh2so= glm(acomp.num~cefalotorax+ situacaoteia+ cefalotorax:situacaoteia, family = binomial,teiasozso)
summary(testeh2so)
testeh2nso= glm(acomp.num~1, family = binomial,teiasozso)

anova(testeh2so,testeh2nso, test = "Chi")
?anova
#Só fariamos se o teste de cima tivesse dado um valor significativo. Nesses testes é possível ver como o modelo se comporta com a remoção de variáveis específicas e a interação. A interação é a primeira a ser removida.
testeh2so.int= glm(acomp.num~cefalotorax+ situacaoteia, family = binomial,teiasozso)
anova(testeh2so,testeh2so.int, test = "Chi")


testeh2so.situ= glm(acomp.num~cefalotorax, family = binomial,teiasozso)
anova(testeh2so.int,testeh2so.situ, test = "Chi")

testeh2so.cefa= glm(acomp.num~situacaoteia, family = binomial,teiasozso)
anova(testeh2so.int,testeh2so.cefa, test = "Chi")

