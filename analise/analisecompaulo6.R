#análise logo depois da reunião, adicionamos a data de coleta como variável aleatória, não utilizar os TXT desses script. eles estão incorretos.




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

teiah12= read.table(file= 'teiah12cd.txt', header=T, dec=',', stringsAsFactors = T)

summary(teiah12)

teiah12$idteia=factor(teiah12$idteia)
teiah12$coleta=factor(teiah12$coleta)
teiah12$agreg=factor(teiah12$agreg)
summary(teiah12)


#para ver se a identidade da agregação tem algum efeito
?lmer
mistoh1= lmer(cefafem~cefamac + situacaoteia+ coleta+ cefamac : situacaoteia+ (1|data)+(1|agreg), data= teiah12)

plot(mistoh1, which = 2, lty = 0)

mistoh1n= lmer(cefafem~(1|data)+(1|agreg), data= teiah12)

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

teiah22= read.table(file= 'teiah22d.txt', header=T, dec=',', stringsAsFactors = T)

summary(teiah22)  
teiah22$coleta= factor(teiah22$coleta)
teiah22$agreg= factor(teiah22$agreg)
teiah22$idteia= factor(teiah22$idteia)
teiah22$acomp.num= as.numeric(teiah22$acomp)-1
summary(teiah22)


#para ver se a id da agregacao tem algum efeito como variável aleatória
testeh22.misto= glmer(acomp.num~cefalotorax+ situacaoteia+ coleta+ cefalotorax:situacaoteia+ (1|data), family = binomial,teiah22)
summary(testeh22.misto)
#não teve
testeh22.nulo= glmer(acomp.num~ (1|data), family = binomial,teiah22)
anova(testeh22.misto, testeh22.nulo)

testeh22.int= glmer(acomp.num~cefalotorax+ situacaoteia+ coleta+ (1|data), family = binomial,teiah22)
anova(testeh22.misto, testeh22.int)

testeh22.cefa= glmer(acomp.num~ situacaoteia+ coleta+ (1|data), family = binomial,teiah22)
anova(testeh22.cefa, testeh22.int)


testeh22.teia= glmer(acomp.num~cefalotorax+  coleta+ (1|data), family = binomial,teiah22)
anova(testeh22.teia, testeh22.int)







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




#grafico1####
plot(teiah22$cefalotorax, teiah22$acomp.num,
     xlab = "",  # Removido temporariamente
     ylab = "",  # Removido temporariamente
     bty = "l",
     xaxt = "n",
     yaxt = "n",
     pch = 21,       # Círculo com contorno e preenchimento
     bg = "darkgray",   # Cor de preenchimento suave
     col = "black",     # Cor do contorno (mais suave que preto)
     cex = 1.5,      # Pontos um pouco maiores
     cex.axis = 1.2) # Tamanho dos números dos eixos

# Adicionar a curva logística
curve(1 / (1 + exp(-(-4.6807 + 6.0659 * x))), 
      from = min(teiah22$cefalotorax), to = max(teiah22$cefalotorax),
      add = TRUE, col = "black", lwd = 2.5)

# Configurar o eixo x com valores em negrito
axis(side = 1,  cex.axis = 1.2)

# Configurar o eixo y com apenas 0 e 1, rotulados como "no" e "yes" em negrito
axis(side = 2, at = c(0, 1), labels = c("No", "Yes"),  cex.axis = 1.2)

# Adicionar títulos dos eixos em negrito
title(xlab = "Female cephalothorax width (cm)", cex.lab = 1.3, line = 2.5)
title(ylab = "Accompanied", cex.lab = 1.3, line = 2.5)


#grafico2####
# Primeiro criar o lineplot SEM os pontos (pch = NA)
lineplot.CI(situacaoteia, acomp.num, data = teiah22,
            xlab = "", 
            ylab = "", 
            bty = "l",
            xaxt = "n",
            pch = NA,            # Sem pontos (vamos adicionar depois)
            col = "black",        # Linha preta
            err.col = "black",    # Barras de erro pretas
            err.lwd = 3,          # Espessura da barra de erro
            cex.axis = 1.2,
            lwd = 2)              # Espessura da linha

# Calcular as médias manualmente
medias <- tapply(teiah22$acomp.num, teiah22$situacaoteia, mean, na.rm = TRUE)

# Adicionar os pontos em PRETO E BRANCO
points(x = c(1, 2), y = medias,
       pch = 21,                 # Círculo com borda e preenchimento
       col = "black",             # Borda preta
       bg = "darkgrey",              # Preenchimento branco
       cex = 2)                   # Tamanho dos pontos

# Configurar o eixo x
axis(side = 1, at = c(1, 2), labels = c("Aggregated", "Isolated"), 
     cex.axis = 1.2)

# Adicionar títulos em preto e negrito
title(xlab = "Spatial situation of the web",  cex.lab = 1.3, line = 2.5, col.lab = "black")
title(ylab = "Proportion of accompained females", cex.lab = 1.3, line = 2.5, col.lab = "black")

