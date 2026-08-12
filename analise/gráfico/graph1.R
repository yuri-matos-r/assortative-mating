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