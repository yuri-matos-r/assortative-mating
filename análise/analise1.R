install.packages("sciplot")
install.packages("lme4")
library(lme4)
library(sciplot)
install.packages("ggplot2")
library(ggplot2)
install.packages("hrbrthemes")
library(hrbrthemes)
install.packages("dplyr")
library(dplyr)


teiapar= teiapar%>%
  mutate(situacaoteia=recode(situacaoteia,
                             "a"= "Agregada",
                             "s"= "Solitária"))
#figura
# Assuming you have a data frame called 'treatment_lines' with columns 'x', 'y', and 'treatment'
tiff("gráfico sic.tiff", w=1800, h= 1200, res= 300)
par(mar=c(5,4,2,2))
ggplot(teiapar, aes(x = cefamac, y = cefafem, color= factor(situacaoteia))) +
  geom_point( size = 2) +
  geom_smooth(aes(color=factor(situacaoteia)), method = "lm", se= F, size= 1)+
  # Add lines for treatments
  theme_classic()+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", size =1.5),
    axis.title.x = element_text(hjust = 0.5, size = 26, margin = margin(t = 14)),
    axis.title.y = element_text(hjust = 0.5, size = 15, margin = margin(r = 10)),
    legend.title = element_text(size= 20),
    legend.text = element_text(size = 20)
    ) +
  labs(y = "Comprimento cefalotorax macho (cm)", x = "Comprimento cefalotorax fêma (cm)", color= "Localização") +
scale_color_manual(values=c("Agregada"= "darkgreen","Solitária"= "blue"))+
  annotate("text", x = -Inf, y = Inf, label = "", hjust = -0.6, vjust = 1.5, size = 6, fontface = "bold")
dev.off()


#hipotese 1

teiapar= read.table(file= 'teiapar.txt', header=T, dec=',', stringsAsFactors = T)

summary(teiapar)

teiapar$idteia=factor(teiapar$idteia)

summary(teiapar)

teiapar$cefafeml = log(teiapar$cefafem)
teiapar$cefamacl = log(teiapar$cefamac)
#nomedoseudataframe$nomedavariavelnova <- scale(nomedodataframe$variavelquequerfazeroscale)

library(lme4)
?lmerControl

teste1=lmer(cefafem~cefamac+situacaoteia+cefamac:situacaoteia+(1|idteia),control = lmerControl("bobyqa"),teiapar)


teste1n=lmer(cefafem~(1|idteia),control = lmerControl('bobyqa'), teiapar)

teste1g=glmer(cefafem~log(cefamac)+situacaoteia+log(cefamac):situacaoteia+(1|idteia),family = Gamma(link = 'identity'), teiapar)

plot(teste1g)

anova(teste1n,teste1,teiapar)

?lmer
summary(teste1)

anova(teste1)
summary()

plot(teste1)
hist(teiapar$cefafem,freq=T)
?hist
library(sciplot)



######################
#######################


testex=lm(cefafem~cefamac + situacaoteia, data= teiapar)
summary(testex)
plot(testex)
anova(testex)
plot(cefafem~cefamac, teiapar)
plot(cefafem~situacaoteia, teiapar)

ggplot(teiapar, aes(x = cefamac, y = cefafem, color = situacaoteia)) +
  geom_point() +  # Gráfico de dispersão
  geom_smooth(method = "lm", se = FALSE) +  # Linha de regressão ajustada
  labs(title = "Efeito de Cefamac e Situacaoteia sobre Cefafem",
       x = "Cefamac (Covariável)",
       y = "Cefafem (Resposta)",
       color = "Situação Teia") +
  theme_minimal()



teste2=glm(cefafeml~cefamacl+situacaoteia+cefamacl:situacaoteia, family = 'gaussian',teiapar)#não ta errado fazer isso mas se o modelo tem distribuiçção normal(a distribuição gauciana) é mais comun usar um lm.
teste1n=glm(cefafeml~1,family = 'gaussian', teiapar)

anova(teste1n,teste2, test= "Chi")
plot(teste2,which=1)
library(sciplot)
lineplot.CI(cefamac, cefafem, situacaoteia, data= teiapar)#não pode usar o lineplot

#FAzer gls
#fazer um plot de ancova
?glmer

####sem outlier

teiaparso= read.table(file= 'teiaparso.txt', header=T, dec=',', stringsAsFactors = T)

summary(teiaparso)

teiaparso$idteia=factor(teiaparso$idteia)
summary(teiapar)
teiapar$cefafeml = log(teiapar$cefafem)
teiapar$cefamacl = log(teiapar$cefamac)
#nomedoseudataframe$nomedavariavelnova <- scale(nomedodataframe$variavelquequerfazeroscale)

library(lme4)
?lmerControl

teste1=lmer(cefafem~cefamac+situacaoteia+cefamac:situacaoteia+(1|idteia),control = lmerControl("bobyqa"),teiaparso)

teste1n=lmer(cefafeml~(1|idteia),control = lmerControl('bobyqa'), teiapar)

teste1gso=glmer(cefafem~cefamac+situacaoteia+cefamac:situacaoteia+(1|idteia),family = Gamma(link = 'identity'), teiaparso)

plot(teste1gso)

anova(teste1n,teste1, var= "chi")

?lmer
summary(teste1)

anova(teste1)
summary()

plot(teste1)
hist(teiapar$cefafem,freq=T)
?hist
library(sciplot)



######################
#######################

teste2=glm(cefafeml~cefamacl+situacaoteia+cefamacl:situacaoteia,family = 'gaussian',teiapar)
teste1n=glm(cefafeml~1,family = 'gaussian', teiapar)

anova(teste1n,teste2, test= "Chi")
plot(teste2,which=1)
library(sciplot)
lineplot.CI(cefamac, cefafem, situacaoteia, data= teiapar)

?glmer







#hipotese 2

teiasoz= read.table(file= 'teiasoz.txt', header=T, dec=',', stringsAsFactors = T)
summary(teiasoz)  
teiasoz$idteia=factor(teiasoz$idteia)
library(nlme)



testeh2= lm(scale(cefalotorax)~situacaoteia+acomp+situacaoteia:acomp, teiasoz)
plot(testeh2, which = 1)
lineplot.CI(  acomp,cefalotorax,situacaoteia, data= teiasoz)

anova(testeh2)



testeh2g=gls(cefalotorax~situacaoteia+acomp+situacaoteia:acomp, weights = varIdent(form = ~1|situacaoteia*acomp), teiasoz)

plot(testeh2g)
##################################
##################################sem outlier


teiasozso= read.table(file= 'teiasozso.txt', header=T, dec=',', stringsAsFactors = T)
summary(teiasozso)  
teiasozso$idteia=factor(teiasozso$idteia)
library(nlme)
library(sciplot)


testeh2so= lm(cefalotorax~situacaoteia+acomp+situacaoteia:acomp, teiasozso)
plot(testeh2so)
lineplot.CI(acomp,cefalotorax,situacaoteia, data= teiasozso)

anova(testeh2so)



testeh2gso=gls(cefalotorax~situacaoteia+acomp+situacaoteia:acomp, weights = varIdent(form = ~1|situacaoteia*acomp), teiasozso)
lineplot.CI(acomp,cefalotorax,situacaoteia, data= teiasozso)
lineplot.CI(acomp,cefalotorax,situacaoteia, data= teiasoz)


plot(testeh2gso)

anova(testeh2gso)
anova(testeh2g)

anova(testeh2gso, testeh2g, test = 'Chi')
summary(testeh2g)
?anova




#resultados da segunda hipótese


lineplot.CI(acomp,cefalotorax,situacaoteia, data= teiasozso)
lineplot.CI(acomp,cefalotorax,situacaoteia, data= teiasoz)
anova(testeh2gso)
anova(testeh2g)
plot(testeh2gso)
plot(testeh2g)


#teste de correlação entre as medidas

cor(teiasoz[,c(5,8,9)])
#                corpo cefalotorax   abdomen
#corpo       1.0000000   0.7457781 0.6383798
#cefalotorax 0.7457781   1.0000000 0.4317170
#abdomen     0.6383798   0.4317170 1.0000000


?cor
cor(teiasoz[,c(5,8,9)],method = c('kendall') )



plot(teiasozso$corpo, teiasozso$abdomen)


?abline
?plot


teiasoz$acompn= as.numeric(teiasoz$acomp)-1
?as.numeric




testeh3=glm(acompn~cefalotorax+situacaoteia+cefalotorax:situacaoteia,family = 'binomial',teiasoz)
testeh3n=glm(acompn~1,family = 'binomial', teiasoz)
anova(testeh3,testeh3n, test = "Chi")

lineplot.CI(cefalotorax,acompn, data= teiasoz)
plot(acompn~cefalotorax,pch = ifelse(situacaoteia == "a", 16, 17), data = teiasoz)
help(plot)
summary(testeh3)
