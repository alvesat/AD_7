## [AD-UFPE-2019] #####
## LISTA 7 ############
## ANTONIO FERNANDES ##

## LINK GITHUB: https://github.com/alvesat/AD_7

##QUESTAO 1#############################################################################

# Abertura do banco em formato .dta (stata)

library(haven)

fair <- read_dta("~/Dados/Listas/AD_7/DATA_L7/fair.dta") 

######## Analise descritiva de todas as variaveis do banco ############################

names(fair) # nomes das variaveis do banco

str(fair) # classe das variaveis do banco

fivenum(fair$YEAR) # descricao da variavel year

fivenum(fair$VOTE) # descricao da variavel vote
hist(fair$VOTE) # histograma da variavel vote


ftable(fair$PARTY) # frequencia da variável party
ftable(fair$PERSON) # frequencia da variavel person

fivenum(fair$DURATION) # descricao da variavel duration
hist(fair$DURATION) # histograma da variavel duration

ftable(fair$WAR) # descricao da variavel war

fivenum(fair$GROWTH) # descricao da variavel growth
mean(fair$GROWTH) # media da variavel growth
hist(fair$GROWTH) # histograma da variavel growth
boxplot(fair$GROWTH) # boxplot da variavel growth

fivenum(fair$INFLATION) # descricao da variavel inflation
mean(fair$INFLATION) # media da variavel inflation
boxplot(fair$INFLATION) # boxplot da variavel inflation
hist(fair$INFLATION) # histograma da variavel inflation

fivenum(fair$GOODNEWS) # descricao da variavel good news
mean(fair$GOODNEWS) # media da variavel good news
hist(fair$GOODNEWS) # histograma da variavel good news

####################### Modelo Regressao Linear (b) ##################################

Linear <- lm(VOTE ~ GROWTH, data = fair) 
summary(Linear) # mostrar resultados do modelo

# Avaliar ajuste do modelo

library(ggplot2) # abrir ggplot

fair$predicted <- predict(Linear)   ##grafico dos residuos
fair$residuals <- residuals(Linear)
ggplot(fair, aes(x = GROWTH, y = VOTE)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +
  geom_segment(aes(xend = GROWTH, yend = predicted), alpha = .2) +
  geom_point(aes(color = abs(residuals), size = abs(residuals))) +
  scale_color_continuous(low = "green", high = "red") +
  guides(color = FALSE, size = FALSE) +
  geom_point(aes(y = predicted), shape = 1) +
  theme_bw()

plot(Linear, which=1, col=c("blue")) # gráfico dos residuos vs linha do ajuste

plot(Linear, which=2, col=c("red")) # Q-Q plot


###### Modelo Regressao Linear com Good News (c) #####################################

Linear_2 <- lm(VOTE ~ GROWTH + GOODNEWS, data = fair) # modelo
summary(Linear_2) # mostrar modelo

# RMSE

library("sjstats") # abrir pacote para calcular o RMSE

rmse(Linear_2) # RMSE do modelo

# Comparar coeficientes padronizados

library('lm.beta') # pacote para comparar os coeficientes padronizados

lm.beta(Linear) # coeficiente padronizado do modelo 1

lm.beta(Linear_2) # coeficiente padronizado do modelo 2

# Intervalo de confiança

confint(Linear_2) # intervalo de confiança do modelo 2

# Avaliar Ajuste do modelo

plot(Linear_2, which=2, col=c("red")) # Q-Q plot

plot(Linear_2, which=1, col=c("blue")) # Residuals vs Fitted Plot

res <- residuals(Linear_2) # atribuir os residuos do modelo 2 em um objeto

shapiro.test(res) # teste de shapiro

mean(res) # media dos residuos

####### Modelo Regressao Linear com Good News e War (d) ###############################

Linear_3 <- lm(VOTE ~ GROWTH + GOODNEWS + WAR, data = fair) # Modelo
summary(Linear_3) # Mostrar modelo

# RMSE

rmse(Linear_3) # RMSE do modelo

# Comparar coeficientes

lm.beta(Linear) # coeficiente padronizado do modelo 1

lm.beta(Linear_2) # coeficiente padronizado do modelo 2

lm.beta(Linear_3) # coeficiente padronizado do modelo 3

# Intervalo de Confiança

confint(Linear_3) # intervalo de confiança do modelo 2

# Avaliar ajuste do modelo

plot(Linear_3, which=2, col=c("red")) # Q-Q plot

plot(Linear_3, which=1, col=c("blue")) # Residuals vs Fitted Plot

res <- residuals(Linear_3) # atribuir os residuos do modelo 3 em um objeto

shapiro.test(res) # teste de shapiro

mean(res) # media dos residuos
 
