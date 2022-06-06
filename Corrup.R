glimpse(corrupcao)

levels(glimpse(corrupcao$regiao)) #Observando os rÃ³tulos da variÃ¡vel regiao
table(corrupcao$regiao) #Tabela de frequÃªncias da variÃ¡vel regiao

summary(corrupcao)

#Estimando modelo,de forma errada
modelo_corrupcao <- lm(formula = cpi ~ as.numeric(regiao), 
                       data = corrupcao)

# modelo_corrupcao
summary(modelo_corrupcao)

#Calcula intervalos (significância 5%)
confint(modelo_corrupcao, level = 0.95) 

#Plotando os fitted values do modelo_corrupcao considerando a ponderação arbitrária 
corrupcao %>%
  mutate(rotulo = paste(pais, cpi)) %>%
  ggplot(aes(x = as.numeric(regiao), y = cpi, label = rotulo)) +
  geom_point(color = "#FDE725FF") +
  stat_smooth(aes(color = "Fitted Values"),
              method = "lm", 
              formula = y ~ x) +
  labs(x = "Região",
       y = "Corruption Perception Index") +
  scale_x_discrete(labels = c("1" = "América do Sul", 
                              "2" = "Oceania", 
                              "3" = "Europa", 
                              "4" = "EUA e Canadá", 
                              "5" = "Ãsia")) +
  scale_color_manual("Legenda:",
                     values = "#440154FF") +
  geom_text_repel() +
  theme_bw()

