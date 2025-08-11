#Dependências
library(dplyr)
library(dplyr)
library(ggplot2)

#Importando dados
data <- read.csv("data.csv")[c(17, 18)]
summary(data)

#Trocando vírgulas por pontos
data$X.TOTAL <- as.numeric(gsub(",", ".", data$X.TOTAL))

#Testando normalidade e possibilidades de correção
correct <- function (x) {
  l <- list(
    og       = function(x) x,        #Valor original
    log10    = function(x) log10(x), #Correção no log10
    sqrt     = function(x) sqrt(x),  #Correção na raiz quadrada
    exp      = function(x) exp(x),   #Correção no exponencial
    sqr      = function(x) x^2       #Correção na elevação ao quadrado
  )
  
  c <- data.frame(i = character(), y = numeric()) #Criação de data frame para o gráfico
  
  for (i in names(l)){
    y <- l[[i]](x)                                                    #Aplica correção
    p <- shapiro.test(y)$p.value                                      #Executa o teste de Shapiro-Wilk
    r <- ifelse(p >= 0.05, "Há normalidade", "Não há normalidade")    #Checa se o p-valor obtido indica normalidade
    cat(paste0("Correção: ", i, " | p-valor: ", p, " | ", r, "\n\n")) #Retorna resultado organizado no console
    
    c <- rbind(c, data.frame(i, y)) #Salva resultado no data frame
  }
}
correct(data$X.TOTAL)  

#Calculando o p-valor do teste de variação de médias
p.value <- round(wilcox.test(X.TOTAL ~ ESCOLHA, data)$p.value, digits = 4)

#Calculando média e mediana para exibição
r <- data %>%
  group_by(ESCOLHA) %>%
  summarise(
    mean   = mean(X.TOTAL,   na.rm = T),
    median = median(X.TOTAL, na.rm = T)
  )
r <- r %>% 
  tidyr::pivot_longer(cols      = c(mean, median),
                      names_to  = "Type",
                      values_to = "Value")

#Gerando gráfico
ggplot(data) +
  aes(ESCOLHA, X.TOTAL) +
  geom_boxplot(outliers = F) +
  geom_line(data  = r, aes(x = ESCOLHA, y = Value, group = Type, linetype = Type)) +
  geom_point(data = r, aes(x = ESCOLHA, y = Value), size = 2) +
  scale_linetype_manual(
    values   = c("mean" = "solid", "median" = "dashed"),
    labels   = c("mean" = "Média", "median" = "Mediana"),
  ) +
  labs(
    x = "Tipo de nomeação",
    y = "% do orçamento direcionado à Secretaria",
    linetype = "Medida de\ntendência central",
    caption  = paste("p-valor:", p.value)
  ) +
  theme_minimal()
