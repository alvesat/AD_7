## [AD-UFPE-2019] #####
## LISTA 7 ############
## ANTONIO FERNANDES ##


##QUESTAO 1############

# Abertura do banco em formato .dta (stata)

library(haven)

fair <- read_dta("~/Dados/Listas/AD_7/DATA_L7/fair.dta") 

# Analise descritiva de todas as variaveis do banco

names(fair) # nomes das variaveis do banco

str(fair) # classe das variaveis do banco

fivenum(fair$YEAR) # descricao da variavel year
fivenum(fair$VOTE) # descricao da variavel vote
fivenum(fair$PARTY) # descricao da variavel party
fivenum(fair$PERSON) # descricao da variavel person
fivenum(fair$DURATION) # descricao da variavel duration
fivenum(fair$WAR) # descricao da variavel war
fivenum(fair$GROWTH) #descricao da variavel growth
fivenum(fair$INFLATION) # descricao da variavel inflation
fivenum(fair$GOODNEWS) # descricao da variavel good news

hist(fair$VOTE) # histograma da variavel vote
hist(fair$PARTY) # histograma da variavel party
hist(fair$PERSON) # histograma da variavel person
hist(fair$DURATION) # histograma da variavel duration
hist(fair$WAR) # histograma da variavel war
hist(fair$GROWTH) #histograma da variavel growth
hist(fair$INFLATION) # histograma da variavel inflation
hist(fair$GOODNEWS) # histograma da variavel good news

# Modelo Regressao Linear 

Linear <- lm(VOTE ~ GROWTH, data = fair)

summary(Linear)

# Modelo Regressao Linear com Good News

Linear_2 <- lm(VOTE ~ GROWTH + GOODNEWS, data = fair)

summary(Linear_2)

  # Analise dos residuos https://rpubs.com/iabrady/residual-analysis

plot(Linear_2, which=2, col=c("red")) # Q-Q plot

plot(Linear_2, which=1, col=c("blue")) # Residuals vs Fitted Plot

  #Homocedasticidade
par(mfrow=c(2,2))
plot(Linear_2)

# Modelo Regressao Linear com War

Linear_3 <- lm(VOTE ~ GROWTH + GOODNEWS + WAR, data = fair)

summary(Linear_3)

  # Analise dos residuos https://medium.com/data-distilled/residual-plots-part-4-residuals-vs-leverage-plot-14aeed009ef7

par(mfrow=c(2,2))
plot(Linear_3)


