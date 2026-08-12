library(sciplot)
library(ggplot2)
library(hrbrthemes)
library(dplyr)
library(nlme)
library(car)
library(lme4)


#COLOCAR ID DA AGREG COMO COVARIAVEL

lineplot.CI (coleta, cefafem, data=teiah12)
lineplot.CI (coleta, cefamac, data=teiah12)
lineplot.CI (situacaoteia, cefafem, data=teiah12)
lineplot.CI (situacaoteia, cefamac, data=teiah12)
lineplot.CI ( situacaoteia, coleta, data=teiah12)


citation("nlme")
#Hipótese 1####
#Apenas em teias agregadas fêmeas maiores estarão com machos maiores e fêmeas menores estarão com machos menores


#Importei os dados e transformei a id da teia em variável categórica

teiah12= read.table(file= 'teiah12.txt', header=T, dec=',', stringsAsFactors = T)


teiah122= read.table(file= 'teiah122.txt', header=T, dec=',', stringsAsFactors = T)


summary(teiah122)

teiah122$idteia=factor(teiah122$idteia)
teiah122$coleta=factor(teiah122$coleta)
teiah122$agreg=factor(teiah122$agreg)
summary(teiah12)

# Fizemos um teste análogo a uma Ancova.   
testex2=lm(cefamac~cefafem + situacaoteia+ cefafem : situacaoteia, data=teiah12)

testex2=lm(cefamac~cefafem + situacaoteia+ coleta+ cefafem : situacaoteia, data= teiah12)

#O resíduo e normalidade não ficaram muito bons, vamos fazer um GLS
plot(testex2, which = 1, lty = 0)
plot(testex2, which = 2, lty = 0)
anova(testex2)
#fizemos o GLS e depois olhamos a distribuição e normalidade
#Precisamos testar a distribuição dos resíduos com todas as variáveis no var power para poder entender o quão confuso ele tá

testegls2fem= gls(cefamac~cefafem + situacaoteia+ coleta+ cefafem : situacaoteia, weights = varPower(form= ~cefafem), data= teiah12)

testegls2situ= gls(cefamac~cefafem + situacaoteia+ coleta+ cefafem : situacaoteia, weights = varIdent(form= ~1|situacaoteia), data= teiah12)

testegls2col= gls(cefamac~cefafem + situacaoteia+ coleta+ cefafem : situacaoteia, weights = varIdent(form= ~1|coleta), data= teiah12)

plot(testegls2fem)
qqnorm(testegls2fem)
summary(testegls2fem)

plot(testegls2situ)
qqnorm(testegls2situ)
summary(testegls2situ)


plot(testegls2col)
qqnorm(testegls2col)
summary(testegls2col)


mistoh1= lmer(cefamac~cefafem + situacaoteia+ coleta+ cefafem : situacaoteia+ (1|agreg), data= teiah122)
summary(mistoh1)





#na Anova fizemos a tipo 3 pois a tipo 1 e tipo 2 atuam de maneira sequencial enquanto a 3 faz as contas simultaneamente. Fazer as contas de maneira sequencial da problema pq a ordem das contas vai alterar o resultado
Anova(testegls2fem, type = 3)
#parece que o cefafem deu significativo, e também a coleta e a interação das duaS

# A única variável que deu significativa foi o cefalotorax da fêmea. Para ver como os dados se comportam só com a variação do tamanho da fêmea fizemos um gls sem as outras variáveis e plotamos a reta de inclinação
testeinclinacao12= gls(cefamac~cefafem, weights = varPower(form= ~cefafem), data= teiah12)
summary(testeinclinacao12)
#uquando faz so com as variáveis a significância some vou fazer com a interação e ver o que rola

testeinclinacao22= gls(cefamac~coleta, weights = varPower(form= ~cefafem), data= teiah12)
summary(testeinclinacao22)



testeinclinacao42= gls(cefafem~coleta, weights = varPower(form= ~cefafem), data= teiah12)
summary(testeinclinacao42)



testeinclinacao32= gls(cefamac~cefafem:coleta, weights = varPower(form= ~cefafem), data= teiah12)
summary(testeinclinacao32)

#Ao fazer o segundo teste, no entanto, os valores de inclinação mudaram (a alteração nas variáveis preditoras causou isso). Quando plotamos a linha optamos por tentar colocar a linha na posição que tinha dado no teste com tudo, mas não deu certo e o gráfico ficou estranho, não precisamos colocar a linha
teiah12$cor=ifelse(teiah12$situacaoteia== "a", "red","blue" )
plot(cefamac~cefafem, col=cor, teiah12)
#abline(a=0.106, b=0.167)

#observamos a existência de uma fêmea muito pequena "puxando" os resultados para a significância. Tiramos ele para ver o que aconteceria com o resultado
#O resultado não deu significativo mas cabe mais coletas

teiaparso= teiapar[-c(11,12),]

testeglsso= gls(cefamac~cefafem + situacaoteia+ cefafem : situacaoteia, weights = varPower(form= ~cefafem), data= teiaparso)
plot(testeglsso)

Anova(testeglsso, type=3)

  #hipotese 2####
#Apenas em teias agregadas fêmeas desacompanhadas serão as menores

teiah22= read.table(file= 'teiah22.txt', header=T, dec=',', stringsAsFactors = T)

summary(teiah22)  
teiah22$coleta= factor(teiah22$coleta)
teiah22$agreg= factor(teiah22$agreg)
teiah22$idteia= factor(teiah22$idteia)

teiah22$acomp.num= as.numeric(teiah22$acomp)-1

table(teiah22$id)



testeh22= glmer(acomp.num~cefalotorax+ situacaoteia+ coleta+ cefalotorax:situacaoteia+ (1|agreg), family = binomial,teiah22)





summary(teiah22)
testeh22= glm(acomp.num~cefalotorax+ situacaoteia+ coleta+ cefalotorax:situacaoteia, family = binomial,teiah22)
#para olhar o residuo na regressão logistica precisa de um pacote que chama dharma

summary(testeh22)



143.60/127
#´é preciso dividir o residual deviance pelo número de graus de liberdade, se estiver muito longe de 1 +- 0.3 é precisocorrigir





testeh22n= glm(acomp.num~1, family = binomial,teiah22)
anova(testeh22,testeh22n, test = "Chi")

plot(acomp.num~cefalotorax, teiah22)



?anova
#Só fariamos se o teste de cima tivesse dado um valor significativo. Nesses testes é possível ver como o modelo se comporta com a remoção de variáveis específicas e a interação. A interação é a primeira a ser removida.
testeh22.int= glm(acomp.num~cefalotorax+ situacaoteia+ coleta, family = binomial,teiah22)
anova(testeh22,testeh22.int, test = "Chi")


testeh22.situ= glm(acomp.num~cefalotorax  +coleta, family = binomial,teiah22)
anova(testeh22.int,testeh22.situ, test = "Chi")
summary(testeh22.situ)

testeh22.cefa= glm(acomp.num~situacaoteia + coleta, family = binomial,teiah22)
anova(testeh22.int,testeh22.cefa, test = "Chi")

lineplot.CI(situacaoteia, acomp.num, data= teiah22)
plot(acomp.num~situacaoteia, teiah22)



testeh22gr= glm(acomp.num~cefalotorax , family = binomial,teiah22)
plot(acomp.num~cefalotorax, data= teiah22)
curve(predict(testeh22gr,data.frame(cefalotorax=x), type='resp'),add=T, col='red')

?glm




acomp=(lm(nacomp~cefalotorax+ situacaoteia+ coleta+ cefalotorax:situacaoteia, data=teiah22))
plot(acomp, which=1)
Anova(acomp,t=3)
