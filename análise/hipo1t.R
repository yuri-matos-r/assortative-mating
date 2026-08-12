teiah12t= read.table(file= 'teiah12t.txt', header=T, dec=',', stringsAsFactors = T)

summary(teiah12t)

teiah12t$idteia=factor(teiah12t$idteia)
teiah12t$coleta=factor(teiah12t$coleta)
teiah12t$agreg=factor(teiah12t$agreg)
summary(teiah12t)


#para ver se a identidade da agregação tem algum efeito
mistoh1t= lmer(cefamac~cefafem + situacaoteia+ coleta+ cefafem : situacaoteia+ (1|agreg), data= teiah12t)
summary(mistoh1)

# Fizemos um teste análogo a uma Ancova.   

testex2t=lm(cefamac~cefafem + situacaoteia+ coleta+ cefafem : situacaoteia, data= teiah12t)

#O resíduo e normalidade não ficaram muito bons, vamos fazer um GLS
plot(testex2t, which = 1, lty = 0)
plot(testex2t, which = 2, lty = 0)
anova(testex2t)
#fizemos o GLS e depois olhamos a distribuição e normalidade
#Precisamos testar a distribuição dos resíduos com todas as variáveis no var power para poder entender o quão confuso ele tá

testegls2tfem= gls(cefamac~cefafem + situacaoteia+ coleta+ cefafem : situacaoteia, weights = varPower(form= ~cefafem), data= teiah12t)

testegls2situt= gls(cefamac~cefafem + situacaoteia+ coleta+ cefafem : situacaoteia, weights = varIdent(form= ~1|situacaoteia), data= teiah12t)

testegls2colt= gls(cefamac~cefafem + situacaoteia+ coleta+ cefafem : situacaoteia, weights = varIdent(form= ~1|coleta), data= teiah12t)

plot(testegls2tfem)
qqnorm(testegls2tfem)
summary(testegls2tfem)

plot(testegls2situt)
qqnorm(testegls2situt)
summary(testegls2situt)

plot(testegls2colt)
qqnorm(testegls2colt)
summary(testegls2colt)






#na Anova fizemos a tipo 3 pois a tipo 1 e tipo 2 atuam de maneira sequencial enquanto a 3 faz as contas simultaneamente. Fazer as contas de maneira sequencial da problema pq a ordem das contas vai alterar o resultado
Anova(testegls2tfem, type = 3)
#parece que o cefafem deu significativo, e também a coleta e a interação das duaS