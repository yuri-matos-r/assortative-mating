library(ggplot2)


teiah22= read.table(file= 'teiah22.txt', header=T, dec=',', stringsAsFactors = T)

summary(teiah22)  
teiah22$coleta= factor(teiah22$coleta)
teiah22$agreg= factor(teiah22$agreg)
teiah22$idteia= factor(teiah22$idteia)

teiah22$acomp.num= as.numeric(teiah22$acomp)-1

testeh22= glm(acomp.num~cefalotorax+ situacaoteia+ coleta+ cefalotorax:situacaoteia, family = binomial,teiah22)


# Criar sequência de valores para cefalotorax
cefalotorax_seq <- seq(min(teiah22$cefalotorax, na.rm = TRUE),
                       max(teiah22$cefalotorax, na.rm = TRUE),
                       length.out = 100)

# Descobrir o valor mais comum de coleta
coleta_mais_comum <- names(which.max(table(teiah22$coleta)))

# Criar dataframe para predição com ambas as situações
# Aqui sei que ele usou a coleta 2 mas não consegui por nada usar os 2 anos
novos_dados <- data.frame(
  cefalotorax = rep(cefalotorax_seq, 2),
  situacaoteia = rep(c("a", "s"), each = length(cefalotorax_seq)),
  coleta = coleta_mais_comum
)

# Fazer predições
predicoes <- predict(testeh22, newdata = novos_dados, type = "response", se.fit = TRUE)

# Adicionar ao dataframe
novos_dados$probabilidade <- predicoes$fit
novos_dados$erro_superior <- predicoes$fit + 1.96 * predicoes$se.fit
novos_dados$erro_inferior <- predicoes$fit - 1.96 * predicoes$se.fit

# Plotar o gráfico com duas curvas
ggplot(novos_dados, aes(x = cefalotorax, y = probabilidade, 
                        color = situacaoteia, fill = situacaoteia)) +
  geom_line(linewidth = 1) +
  geom_point(data = teiah22, aes(x = cefalotorax, y = acomp.num, color = situacaoteia),
             alpha = 0.5, position = position_jitter(height = 0.02)) +
  labs(x = "Cefalotórax", y = "Probabilidade de Acomp.num = 1",
       title = "Curva de Regressão Logística por Situação de Teia",
       color = "Situação Teia", fill = "Situação Teia") +
  scale_color_manual(values = c("a" = "darkgreen", "s" = "blue")) +
  theme_minimal() +
  theme(legend.position = "bottom")








#Tentativa nova
library(ggplot2)

# Criar a curva logística
intercept <- -3.99
inclinacao <- 4.6165

x_vals <- seq(0.29, 0.66, length.out = 100)
curva_logistica <- data.frame(
  cefalotorax = x_vals,
  probabilidade = 1 / (1 + exp(-(intercept + inclinacao * x_vals)))
)

# Criar o gráfico com cores baseadas em "situacaoteia"
ggplot() +
  # Pontos coloridos por situacaoteia
  geom_point(data = teiah22, 
             aes(x = cefalotorax, y = acomp.num, 
                 color = situacaoteia),  # Usando situacaoteia para cores
             size = 2, alpha = 1, shape = 19) +
  
  # Curva logística
  geom_line(data = curva_logistica, 
            aes(x = cefalotorax, y = probabilidade), 
            color = "darkgreen", 
            linewidth = 1.5) +
  
  # Configurações estéticas - cores personalizadas para "a" e "s"
  scale_color_manual(values = c("a" = "#E41A1C",  # Vermelho para agregadas
                                "s" = "#377EB8"), # Azul para solitárias
                     labels = c("a" = "Agregadas", 
                                "s" = "Solitárias"),
                     name = "Situação da Teia") +
  
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1),
                     labels = c("0 (Ausência)", "0.25", "0.5", "0.75", "1 (Presença)")) +
  
  labs(x = "Comprimento do Cefalotórax", 
       y = "Presença/Ausência e Probabilidade",
       title = "Relação entre Tamanho do Cefalotórax e Presença/Ausência",
       subtitle = paste("Curva logística: intercept =", intercept, 
                        ", inclinação =", inclinacao)) +
  
  # Limites
  xlim(0.29, 0.66) +
  ylim(-0.05, 1.05) +
  
  theme_classic() +
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 11),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 10))




#grafico cefacefa






ggplot(teiah12, aes(x = cefamac, y = cefafem, color = situacaoteia)) +
  geom_point(alpha = 0.7, size = 3) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.5) +
  scale_color_manual(values = c("a" = "red", "s" = "blue"),
                     labels = c("a" = "Agregadas", "s" = "Solitárias"),
                     name = "Situação da Teia") +
  labs(x = "Comprimento do Cefalotórax Macho (cefamac)",
       y = "Comprimento do Cefalotórax Fêmea (cefafem)",
       title = "Relação entre Cefalotórax Macho e Fêmea por Situação da Teia") +
  theme_classic() +
  theme(legend.position = "top")
