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

citation("nlme")
#Hipótese 1####
#Apenas em teias agregadas fêmeas maiores estarão com machos maiores e fêmeas menores estarão com machos menores


#Importei os dados e transformei a id da teia em variável categórica

teiah12= read.table(file= 'teiah12c.txt', header=T, dec=',', stringsAsFactors = T)

summary(teiah12)

teiah12$idteia=factor(teiah12$idteia)
teiah12$coleta=factor(teiah12$coleta)
teiah12$agreg=factor(teiah12$agreg)
summary(teiah12)

#para ver se a identidade da agregação tem algum efeito
?lmer
mistoh1= lmer(cefafem~cefamac + situacaoteia+ coleta+ cefamac : situacaoteia+ (1|agreg), data= teiah12)

plot(mistoh1, which = 2, lty = 0)

mistoh1n= lmer(cefafem~(1|agreg), data= teiah12)

anova(mistoh1, mistoh1n, test= "chi")

mistoh1int= lmer(cefafem~cefamac + situacaoteia+ coleta+ (1|agreg), data= teiah12)
anova(mistoh1int,mistoh1, test= "chi")

mistoh1mac= lmer(cefafem~ situacaoteia+ coleta+ (1|agreg), data= teiah12)
anova(mistoh1mac, mistoh1int, test= "chi")

mistoh1sit= lmer(cefafem~ cefamac+ coleta+ (1|agreg), data= teiah12)
anova(mistoh1sit, mistoh1int, test= "chi")

mistoh1col= lmer(cefafem~ cefamac+situacaoteia+ (1|agreg), data= teiah12)
anova(mistoh1col, mistoh1int, test= "chi")


summary(mistoh1)


summary(teiah12$cefafem)
sd(teiah12$cefafem)
sd(teiah12$cefamac)

#hipotese 2####
#Apenas em teias agregadas fêmeas desacompanhadas serão as menores

teiah22= read.table(file= 'teiah22.txt', header=T, dec=',', stringsAsFactors = T)

summary(teiah22)  
teiah22$coleta= factor(teiah22$coleta)
teiah22$agreg= factor(teiah22$agreg)
teiah22$idteia= factor(teiah22$idteia)
teiah22$acomp.num= as.numeric(teiah22$acomp)-1
summary(teiah22)


#para ver se a id da agregacao tem algum efeito como variável aleatória
testeh22.misto= glmer(acomp.num~cefalotorax+ situacaoteia+ coleta+ cefalotorax:situacaoteia+ (1|agreg), family = binomial,teiah22)
#não teve




summary(teiah22)
testeh22= glm(acomp.num~cefalotorax+ situacaoteia+ coleta+ cefalotorax:situacaoteia, family = binomial,teiah22)
#para olhar o residuo na regressão logistica precisa de um pacote que chama dharma

summary(testeh22)



143.60/127
#´é preciso dividir o residual deviance pelo número de graus de liberdade, se estiver muito longe de 1 +- 0.3 é precisocorrigir


testeh22n= glm(acomp.num~1, family = binomial,teiah22)
anova(testeh22,testeh22n, test = "Chi")

plot(acomp.num~situacaoteia, teiah22)
lineplot.CI(situacaoteia, acomp.num, data =teiah22)


testeh22.int= glm(acomp.num~cefalotorax+ situacaoteia+ coleta, family = binomial,teiah22)

anova(testeh22,testeh22.int, test = "Chi")


testeh22.situ= glm(acomp.num~cefalotorax  +coleta, family = binomial,teiah22)
anova(testeh22.int,testeh22.situ, test = "Chi")

summary(testeh22.situ)

testeh22.cefa= glm(acomp.num~situacaoteia + coleta, family = binomial,teiah22)
anova(testeh22.int,testeh22.cefa, test = "Chi")

summary(testeh22.cefa)

testeh22.col=glm(acomp.num~situacaoteia + cefalotorax, family = binomial,teiah22)
anova(testeh22.int,testeh22.col, test = "Chi")
summary(testeh22)
summary(testeh22.int)

plot()

#gráfico do deepseek

plot(teiah22$cefalotorax, teiah22$acomp.num,
     xlab = "cefalotorax", ylab = "acomp.num")

curve(1 / (1 + exp(-(-4.6807 + 6.0659 * x))), 
      from = min(teiah22$cefalotorax), to = max(teiah22$cefalotorax),
      add = TRUE, col = "red", lwd = 2)



#hipotese3####

#criar teia com o cefalotorax dos machos e retirar as femeas
teiah3= read.table("teiah3.txt", h= T, dec=",", stringsAsFactors = T)
summary(teiah3)
teiah3m= teiah3[teiah3$sexo=="m",] 
teiah3m$idteia=as.factor(teiah3m$idteia)


#criar o valos máximo e médio de cefalotorax de macho em cada teia
#quando faz isso ele automaticamente não repete as teias
maxteia=tapply(teiah3m$cefalotorax, teiah3m$idteia,max)
medteia=tapply(teiah3m$cefalotorax, teiah3m$idteia,mean)
#isso aqu vai pegar só as teias da segunda coleta. fizemos isso de maneira manual olhando a última teia da coleta 1 e a primeira da coleta 2
maxteia2=maxteia[21:length(maxteia)]
medteia2=medteia[21:length(medteia)]


#criar planilha com teias de fêmeas com pelo menos 1 macho na teia e apenas da coleta 2, retirar a femea 116 que tem os machos com posição indefinida
#criamos colunas com as informações de maximo e media de cada teia e uma coluna para saber se o macho estava sozinho ou acompanhado na teia
teiah4m= teiah22[teiah22$nacomp2!=0,]
teiah4m= teiah4m[teiah4m$coleta== 2,]
teiah4m=teiah4m[teiah4m$idteia!=116,]
teiah4m$cefmax=maxteia2
teiah4m$cefamed=medteia2
teiah4m$nacomp3= as.factor(ifelse(teiah4m$nacomp2==1, "solitario", "acompanhado"))

#histograma para saber como que estava a distribuição dos machos
hist(teiah4m$nacomp2, breaks = 16)
?hist




summary(teiah4m)

table(teiah4m$nacomp3, teiah4m$situacaoteia)

#vendo se o tamanho maximo do cefalotorax muda 
teste4= lm(cefmax~nacomp3+situacaoteia+nacomp3:situacaoteia, teiah4m)
plot(teste4, which = 1, lty=0)
plot(teste4, which = 2, lty=0)
anova(teste4)
lineplot.CI(situacaoteia, cefmax, nacomp3, data=teiah4m,lty= 0, lwd= 2, col= c("green4", "purple"), pch=c(16,16))

?lineplot.CI





#vendo se o tamanho medio do cefalotorax muda 
teste5= lm(cefamed~nacomp3+situacaoteia+nacomp3:situacaoteia, teiah4m)
plot(teste5, which = 1, lty=0)
plot(teste5, which = 2, lty=0)
anova(teste5)
lineplot.CI(situacaoteia, cefamed, nacomp3, data=teiah4m)


mean(teiah3m$cefalotorax[teiah3m$situacaoteia=="a"])
mean(teiah3m$cefalotorax[teiah3m$situacaoteia=="s"])

sd(teiah3m$cefalotorax[teiah3m$situacaoteia=="a"])
sd(teiah3m$cefalotorax[teiah3m$situacaoteia=="s"])


mean(teiah4m$cefamed[teiah4m$situacaoteia=="a"])
mean(teiah4m$cefamed[teiah4m$situacaoteia=="s"])

max(teiah4m$cefmax[teiah4m$situacaoteia=="a"])
max(teiah4m$cefmax[teiah4m$situacaoteia=="s"])


#colocando uma coluna de macho acompanhado ou sozinho
teiah22$nacomp3=ifelse(teiah22$nacomp2<3,teiah22$nacomp2, 2 )


summary(teiah22)
teste6=multinom(nacomp3~cefalotorax+ situacaoteia+ coleta+ cefalotorax:situacaoteia,teiah22)

teste6.n= multinom(acomp.num~1,teiah22)
anova(teste6.n, teste6, test= "Chisq")

teste6.int=multinom(acomp.num~cefalotorax+ situacaoteia+ coleta,teiah22)
anova(teste6.int, teste6, test= "Chisq")

teste6.cefa=multinom(acomp.num~ situacaoteia+ coleta,teiah22)
anova(teste6.int, teste6.cefa, test= "Chisq")

teste6situ=multinom(acomp.num~cefalotorax+ coleta,teiah22)
anova(teste6.int, teste6situ, test= "Chisq")

teste6.col=multinom(acomp.num~cefalotorax+ situacaoteia+ coleta,teiah22)
anova(teste6.int, teste6, test= "Chisq")
