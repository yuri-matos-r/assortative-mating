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
