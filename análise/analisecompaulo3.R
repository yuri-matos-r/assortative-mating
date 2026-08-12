library(sciplot)
library(ggplot2)
library(hrbrthemes)
library(dplyr)
library(nlme)
library(car)
library(lme4)



lineplot.CI (coleta, cefafem, data=teiah12)
lineplot.CI (coleta, cefamac, data=teiah12)
lineplot.CI (situacaoteia, cefafem, data=teiah12)
lineplot.CI (situacaoteia, cefamac, data=teiah12)
lineplot.CI ( situacaoteia, coleta, data=teiah12)



summary(teiah22$nacomp2[teiah22$situacaoteia=="s"])
mean(teiah22$nacomp2[teiah22$situacaoteia=="a"])
0.5694444
sd(teiah22$nacomp2[teiah22$situacaoteia=="a"])
1.160699
mean(teiah22$nacomp2[teiah22$situacaoteia=="s"])
0.6666667
sd(teiah22$nacomp2[teiah22$situacaoteia=="s"])
0.6288722


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

#Devo colocar a coleta como variável aleatória? na minha cabeça sim, mas ta fora do parentesis
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



#hipotese3 (post roc)####

summary(teiah22)
teiah22$nacomp=teiah22$nacomp2
teiah22$nacomp=as.factor(teiah22$nacomp)
#para ver se a id da agregacao tem algum efeito como variável aleatória


testeh3.misto= lmer(nacomp2~cefalotorax+ situacaoteia+ coleta+ cefalotorax:situacaoteia+ (1|agreg),teiah22)
testeh3= lm(nacomp2~cefalotorax+ situacaoteia+ coleta+ cefalotorax:situacaoteia,teiah22)
plot(testeh3, which = 1, lty=0)
plot(testeh3, which = 2, lty=0)
#isso aqui ficou ruim demais socorro
testeh3gfem= gls(nacomp2~cefalotorax+ situacaoteia+ coleta+ cefalotorax:situacaoteia, weights = varPower(form= ~cefalotorax),teiah22)

plot(testeh3gfem)
qqnorm(testeh3gfem)
summary(testeh3gfem)


testeh3gsitu= gls(nacomp2~cefalotorax+ situacaoteia+ coleta+ cefalotorax:situacaoteia, weights = varIdent(form= ~1|situacaoteia ),teiah22)

plot(testeh3gsitu)
qqnorm(testeh3gsitu)
summary(testeh3gsitu)



testeh3gcol= gls(nacomp2~cefalotorax+ situacaoteia+ coleta+ cefalotorax:situacaoteia, weights = varIdent(form= ~1|coleta ),teiah22)

plot(testeh3gcol)
qqnorm(testeh3gcol)
summary(testeh3gcol)

#como ainda não gostei de nenhum dos plots, tentei corrigir pelo log 




testeh3log= lm(nacomp2~log(cefalotorax)+ situacaoteia+ coleta+ cefalotorax:situacaoteia,teiah22)

plot(testeh3log, which = 1, lty=0)
plot(testeh3log, which = 2, lty=0)

#gostei mais da correção pelo log vou fazer anova tipo 3 "ortogonal"

Anova(testeh3log, type=3)



plot(nacomp2~log(cefalotorax), pch=as.numeric(factor(teiah22$situacaoteia)), teiah22)
m1=lm(nacomp2~log(cefalotorax), teiah22[teiah22$situacaoteia== "a",])
m2=lm(nacomp2~log(cefalotorax), teiah22[teiah22$situacaoteia=="s",])

abline(m1, lty=1)
abline(m2, lty=2)












#hipotese3 sem outlyer (post roc)####

teiah22so=teiah22teiah22[teiah22$nacomp2 < 3, ]
summary(teiah22so)

mistoh3.so= lmer(nacomp2~cefalotorax+ situacaoteia+ coleta+ cefalotorax:situacaoteia+ (1|agreg),teiah22so)
plot(mistoh3.so, which = 2, lty = 0)


mistoh32= glmer(nacomp2~cefalotorax+ situacaoteia+ coleta+ cefalotorax:situacaoteia+ (1|agreg), family = poisson ,teiah22)


testeh32= glm(nacomp2~cefalotorax+ situacaoteia+ coleta+ cefalotorax:situacaoteia, family = poisson ,teiah22)

summary(testeh32)

127/115.55




testeh32n= glm(nacomp2~1, family = poisson ,teiah22)

anova(testeh32,testeh32n, test = "Chi")

testeh32.int= glm(nacomp2~cefalotorax+ situacaoteia+ coleta, family = poisson ,teiah22)
anova(testeh32,testeh32.int, test = "Chi")

testeh32.cefa= glm(nacomp2~ situacaoteia+ coleta, family = poisson ,teiah22)
anova(testeh32.cefa,testeh32.int, test = "Chi")

testeh32.situ= glm(nacomp2~cefalotorax+ coleta, family = poisson ,teiah22)
anova(testeh32.int,testeh32.situ, test = "Chi")

testeh32.col= glm(nacomp2~cefalotorax+ situacaoteia, family = poisson ,teiah22)
anova(testeh32.col,testeh32.int, test = "Chi")



#hipotese3####

teiah3= read.table("teiah3.txt", h= T, dec=",", stringsAsFactors = T)
summary(teiah3)
teiah3m= teiah3[teiah3$sexo=="m",] 

nome=tapply(teiah3m$cefalotorax, teiah3m$idteia,max)
media=tapply(teiah3m$cefalotorax, teiah3m$idteia,mean)


hist(teiah4m$nacomp2, breaks = 16)
?hist


teiah4m= teiah22[teiah22$nacomp2!=0,]
teiah4m= teiah4m[teiah4m$coleta== 2,]
teiah4m=teiah4m[teiah4m$idteia!=116,]
teiah4m$cefmax=nome.coleta2
teiah4m$cefamed=media2
teiah4m$nacomp3= as.factor(ifelse(teiah4m$nacomp2==1, "solitario", "acompanhado"))

nome.coleta2=nome[21:length(nome)]
media2=media[21:length(media)]


summary(teiah4m)

table(teiah4m$nacomp3, teiah4m$situacaoteia)

teste4= lm(cefmax~nacomp3+situacaoteia+nacomp3:situacaoteia, teiah4m)
plot(teste4, which = 1, lty=0)
plot(teste4, which = 2, lty=0)
anova(teste4)
lineplot.CI(situacaoteia, cefmax, nacomp3, data=teiah4m)

teste5= lm(cefamed~nacomp3+situacaoteia+nacomp3:situacaoteia, teiah4m)
plot(teste5, which = 1, lty=0)
plot(teste5, which = 2, lty=0)
anova(teste5)
lineplot.CI(situacaoteia, cefamed, nacomp3, data=teiah4m)



?multinom

teiah22$nacomp3=ifelse(teiah22$nacomp2<3,teiah22$nacomp2, 2 )

install.packages("nnet")
library(nnet)
summary(teiah22)
teste6=multinom(acomp.num~cefalotorax+ situacaoteia+ coleta+ cefalotorax:situacaoteia,teiah22)

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