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