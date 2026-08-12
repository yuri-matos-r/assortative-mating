library(sciplot)
library(ggplot2)
library(hrbrthemes)
library(dplyr)
library(nlme)
library(car)
library(lme4)
library(nnet)



lineplot.CI (coleta, cefafem, data=teiah12)
lineplot.CI (coleta, cefamac, data=teiah12)
lineplot.CI (situacaoteia, cefafem, data=teiah12)
lineplot.CI (situacaoteia, cefamac, data=teiah12)
lineplot.CI ( situacaoteia, coleta, data=teiah12)



summary(teiah22$nacomp2[teiah22$situacaoteia=="s"])
#media e desvio padrão do numero de machos por teia agregada
mean(teiah22$nacomp2[teiah22$situacaoteia=="a"])
0.5694444
sd(teiah22$nacomp2[teiah22$situacaoteia=="a"])
1.160699
#media e desvio padrão do numero de machos por teia solitaria
mean(teiah22$nacomp2[teiah22$situacaoteia=="s"])
0.6666667
sd(teiah22$nacomp2[teiah22$situacaoteia=="s"])
0.6288722

#media e desvio padrão do numero de machos por teia por ano 1
mean(teiah22$nacomp2[teiah22$coleta=="1"])
0.28
sd(teiah22$nacomp2[teiah22$coleta=="1"])
0.48

#media e desvio padrão do numero de machos por teia por ano 2
mean(teiah22$nacomp2[teiah22$coleta=="2"])
1.05
sd(teiah22$nacomp2[teiah22$coleta=="2"])
1.22

summary(teiah22)


#Hipótese 1####
#Apenas em teias agregadas fêmeas maiores estarão com machos maiores e fêmeas menores estarão com machos menores


#Importei os dados e transformei a id da teia em variável categórica

teiah12= read.table(file= 'teiah12cq.txt', header=T, dec=',', stringsAsFactors = T)

summary(teiah12)

teiah12$idteia=factor(teiah12$idteia)
teiah12$coleta=factor(teiah12$coleta)
teiah12$agreg=factor(teiah12$agreg)
summary(teiah12)


#para ver se a identidade da agregação tem algum efeito
?lmer
mistoh1= lmer(cefafem~cefamac + situacaoteia+ coleta+ cefamac : situacaoteia+ (1|data)+(1|agreg), data= teiah12)

plot(mistoh1, which = , lty = 0)

mistoh1n= lmer(cefafem~(1|data)+(1|agreg), data= teiah12)

anova(mistoh1, mistoh1n, test= "chi")


















#hipotese 2####
#Apenas em teias agregadas fêmeas desacompanhadas serão as menores

teiah22= read.table(file= 'teiah22q.txt', header=T, dec=',', stringsAsFactors = T)

summary(teiah22)  
teiah22$coleta= factor(teiah22$coleta)
teiah22$agreg= factor(teiah22$agreg)
teiah22$idteia= factor(teiah22$idteia)
teiah22$acomp.num= as.numeric(teiah22$acomp)-1
summary(teiah22)


#para ver se a id da agregacao tem algum efeito como variável aleatória
testeh22.misto= glmer(acomp.num~cefalotorax+ situacaoteia+ coleta+ cefalotorax:situacaoteia+ (1|data)+(1|agreg), family = binomial,teiah22)
summary(testeh22.misto)

testeh22.nulo= glmer(acomp.num~ (1|data)+(1|agreg), family = binomial,teiah22)
anova(testeh22.misto, testeh22.nulo)

testeh22.int= glmer(acomp.num~cefalotorax+ situacaoteia+ coleta+ (1|data)+(1|agreg), family = binomial,teiah22)
anova(testeh22.misto, testeh22.int)

testeh22.cefa= glmer(acomp.num~ situacaoteia+ coleta+ (1|data)+(1|agreg), family = binomial,teiah22)
anova(testeh22.cefa, testeh22.int)


testeh22.teia= glmer(acomp.num~cefalotorax+  coleta+ (1|data)+(1|agreg), family = binomial,teiah22)
anova(testeh22.teia, testeh22.int)

testeh22.col= glmer(acomp.num~cefalotorax+  situacaoteia+ (1|data)+(1|agreg), family = binomial,teiah22)
anova(testeh22.col, testeh22.int)




plot(acomp.num~coleta, teiah22)
lineplot.CI(coleta, acomp.num, data= teiah22)
#para olhar o residuo na regressão logistica precisa de um pacote que chama dharma
wqr;ljjhrqlkwh iigqwrkjqf kugq ikehtgqwvurjgqfvwi ljyhbqouk jgqwvhng iqwkg qjyhwgv lqniu

