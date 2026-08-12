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
