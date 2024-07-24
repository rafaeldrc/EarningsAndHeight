# instalação e leitura de bibliotecas necessárias
#install.packages("tidyverse")
library(tidyverse)
#install.packages("dplyr")
library(dplyr)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("readxl")
library(readxl)
#install.packages('stargazer')
library(stargazer)
#install.packages('fixest')
library(fixest)

# abrindo a base de dados
data <- read_excel("Earnings_and_Height.xlsx", sheet = "Data")

# a partir do glimpse definiremos como sumarizar as variáveis descritivamente
glimpse(data)

# antes de partirmos para as estatísticas descritivas, vamos fazer as transformações de unidades de medida
# pois sistema imperial não combina com o século XXI :)
data <- data %>% 
  mutate(height_cm = height * 2.54,
         weight_kg = weight *0.453592)

##### 2- ANÁLISE EXPLORATÓRIA DA BASE

# cworker, mrd, occupation, race, region e sex são variáveis categóricas, podemos resumí-las
# a partir de gráficos de barra. Faremos isso para sex e cworker. Para occupation, um gráfico de pizza.

# contando os valores (sexo)
contagem_sexo <- table(data$sex)

# criando o gráfico de barras (sexo)

sexo <- c("Feminino", "Masculino")
quantidade_sexo <- as.numeric(contagem_sexo)
dados_sexo <- data.frame(Sexo = sexo, Quantidade = quantidade_sexo)

dados_sexo$labels <- paste(dados_sexo$Sexo, "\n(", dados_sexo$Quantidade, ")")

ggplot(dados_sexo, aes(x = labels, y = Quantidade, fill = Sexo)) + geom_bar(stat = 'identity') +
  scale_fill_manual(values = c("Feminino" = "pink", "Masculino" = "skyblue")) +
  labs(title = "Quantidade de mulheres e homens",
       x = "Sexo",
       y = "Quantidade") +
  theme_minimal()


# contando os valores (cworker)
contagem_classe <- table(data$cworker)

# criando o gráfico de barras
barplot(contagem_classe,
        main = "Classe do trabalhador",
        xlab = "Classe",
        ylab = "Quantidade",
        col = rainbow(length(contagem_classe)),
        legend = c(paste("Funcionário de empresa privada", "(", contagem_classe[1], ")\n"),
                   paste("Funcionário do Governo Federal americano", "(", contagem_classe[2], ")\n"),
                   paste("Funcionário de um Governo Estadual", "(", contagem_classe[3],")\n"),
                   paste("Funcionário de um Governo Local", "(", contagem_classe[4],")\n"),
                   paste("Funcionário de S.A", "(", contagem_classe[5], ")\n"),
                   paste("Autônomo", "(", contagem_classe[6], ")\n")),
        names.arg = FALSE)
# contando os valores (occupation)
contagem_ocupacao <- table(data$occupation)

# criando o gráfico de pizza
#install.packages('colorRamps')
library(colorRamps)
pie(contagem_ocupacao,
    main = "Ocupação do Trabalhador",
    col = colorRamps::primary.colors(n = length(contagem_ocupacao)),
    labels = "")

legend("topright", legend = c(paste("Executivo/Gerente", "(", contagem_ocupacao[1], ")\n"),
                              paste("Profissional", "(", contagem_ocupacao[2], ")\n"),
                              paste("Técnico", "(", contagem_ocupacao[3],")\n"),
                              paste("Vendas", "(", contagem_ocupacao[4],")\n"),
                              paste("Administração", "(", contagem_ocupacao[5], ")\n"),
                              paste("Serviço doméstico", "(", contagem_ocupacao[6], ")\n"),
                              paste("Serviço de proteção", "(", contagem_ocupacao[7], ")\n"),
                              paste("Outro tipo de serviço", "(", contagem_ocupacao[8], ")\n"),
                              paste("Agricultura", "(", contagem_ocupacao[9], ")\n"),
                              paste("Mecânica", "(", contagem_ocupacao[10], ")\n"),
                              paste("Construção/Mineração", "(", contagem_ocupacao[11], ")\n"),
                              paste("Produção de precisão", "(", contagem_ocupacao[12], ")\n"),
                              paste("Operador de máquina", "(", contagem_ocupacao[13], ")\n"),
                              paste("Transporte", "(", contagem_ocupacao[14], ")\n"),
                              paste("Trabalhador manual", "(", contagem_ocupacao[15], ")\n")),
       fill = colorRamps::primary.colors(n = length(contagem_ocupacao)),
       cex = 0.7)

# observando o rendimento médio por região
data_region <- data %>% 
  group_by(region) %>%
  summarise(mean_earnings = mean(earnings)) %>% 
  mutate(region_name = case_when(
    region == 1 ~ 'Nordeste',
    region == 2 ~ 'Centro-Oeste',
    region == 3 ~ 'Sul',
    region == 4 ~ 'Oeste'
  ))

# note que a região 1 (Nordeste) se destaca com o maior rendimento médio:
data_region

# criando um gráfico de barras para visualizar essa diferença
ggplot(data = data_region) +
  geom_bar(mapping = aes(x = region_name, y = mean_earnings, fill = region_name), stat = 'identity') +
  geom_text(mapping = aes(x = region_name, y = mean_earnings, label = round(mean_earnings, 2)), vjust = -0.5) +
  labs(title = 'Rendimento médio por região dos EUA',
       x = 'Região', y = 'Rendimento médio em US$',
       fill = 'Região')

# Sub-bases por sexo
data_f <- data %>% 
  filter(sex == 0)

data_m <- data %>%
  filter(sex == 1)

# Sub-bases de médias por ano de educação
média_educ_f <- data_f %>%
  group_by(educ) |>
  summarise(round(mean(height_cm), 2), var(height_cm), round(mean(earnings), 2))
colnames(média_educ_f)[2] = "mean_height"
média_educ_f['sexo'] <- "Mulheres"

média_educ_m <- data_m %>%
  group_by(educ) |>
  summarise(round(mean(height_cm), 2), var(height_cm), round(mean(earnings), 2))
colnames(média_educ_m)[2] = "mean_height"
média_educ_m['sexo'] <- "Homens"

média_educ_mf <- bind_rows(média_educ_m, média_educ_f)



# Gráfico de Altura Média por Anos de Educação Formal

ggplot(média_educ_mf, aes(x=educ, y=mean_height, color = sexo)) +
  geom_point() + geom_line() + scale_x_continuous(breaks = seq(0, 20, by = 1)) +
  scale_y_continuous(breaks = seq(150, 190, by = 2)) + theme_classic() +
  labs(title = "Altura Média por Anos de Educação Formal", color = "Sexo",
       x = "Anos de Educação Formal", y = "Altura Média") +
  scale_color_manual(values=c("darkblue", "darkred"))

# Gráficos de desvios da altura média por ocupação
# Para homens:
média_ocupação_m <- data_m %>%
  group_by(occupation) |>
  summarise(round(mean(height_cm), 2), round(mean(earnings), 2),
            round(mean(educ), 2))
colnames(média_ocupação_m)[2] = "mean_height"

média_ocupação_m <- média_ocupação_m %>%
  mutate(occupation_names = case_when(
    occupation == 1 ~ "Executivo/Gerente",
    occupation == 2 ~ "Profissional",
    occupation == 3 ~ "Técnico",
    occupation == 4 ~ "Vendas",
    occupation == 5 ~ "Administração",
    occupation == 6 ~ "Serviço doméstico",
    occupation == 7 ~ "Serviço de proteção",
    occupation == 8 ~ "Outro tipo de serviço",
    occupation == 9 ~ "Agricultura",
    occupation == 10 ~ "Mecânica",
    occupation == 11 ~ "Construção/Mineração",
    occupation == 12 ~ "Produção de precisão",
    occupation == 13 ~ "Operador de máquina",
    occupation == 14 ~ "Transporte",
    occupation == 15 ~ "Trabalhador manual"))

média_ocupação_m <- média_ocupação_m %>%
  mutate(deviation = mean_height - mean(data_m$height_cm))

média_ocupação_m$occupation_names <- factor(média_ocupação_m$occupation_names,
                                            levels = média_ocupação_m$occupation_names)

ggplot(média_ocupação_m, aes(x=occupation_names, y=deviation)) +
  geom_bar(stat="identity", fill="blue4") +
  scale_y_continuous(breaks = seq(-10, 10, by = 0.5)) +
  labs(title = "Desvio da Média de Altura por Ocupação (Homens)",
       x = "Ocupação", y = "Desvio da Média de Altura") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.line = element_line(color = "black", linewidth = 0.5,
                                 linetype = 1))

# Para mulheres
média_ocupação_f <- data_f %>%
  group_by(occupation) |>
  summarise(round(mean(height_cm), 2), round(mean(earnings), 2),
            round(mean(educ), 2))
colnames(média_ocupação_f)[2] = "mean_height"

média_ocupação_f <- média_ocupação_f %>%
  mutate(occupation_names = case_when(
    occupation == 1 ~ "Executivo/Gerente",
    occupation == 2 ~ "Profissional",
    occupation == 3 ~ "Técnico",
    occupation == 4 ~ "Vendas",
    occupation == 5 ~ "Administração",
    occupation == 6 ~ "Serviço doméstico",
    occupation == 7 ~ "Serviço de proteção",
    occupation == 8 ~ "Outro tipo de serviço",
    occupation == 9 ~ "Agricultura",
    occupation == 10 ~ "Mecânica",
    occupation == 11 ~ "Construção/Mineração",
    occupation == 12 ~ "Produção de precisão",
    occupation == 13 ~ "Operador de máquina",
    occupation == 14 ~ "Transporte",
    occupation == 15 ~ "Trabalhador manual"))

média_ocupação_f <- média_ocupação_f %>%
  mutate(deviation = mean_height - mean(data_f$height_cm))

média_ocupação_f$occupation_names <- factor(média_ocupação_f$occupation_names,
                                            levels = média_ocupação_f$occupation_names)

ggplot(média_ocupação_f, aes(x=occupation_names, y=deviation)) +
  geom_bar(stat="identity", fill="red4") +
  scale_y_continuous(breaks = seq(-10, 10, by = 0.5)) +
  labs(title = "Desvio da Média de Altura por Ocupação (Mulheres)",
       x = "Ocupação", y = "Desvio da Média de Altura") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.line = element_line(color = "black", linewidth = 0.5,
                                 linetype = 1))



# HISTOGRAMAS

# Histograma de renda
ggplot(data, aes(x=earnings)) + geom_histogram(binwidth=5000,
                                               color="white", fill="green") +
  scale_y_continuous(breaks = seq(0, 6000, by = 500)) +
  scale_x_continuous(breaks = seq(0, 900000, by = 10000)) +
  labs(title = "Distribuição de Renda",
       x = "Renda", y = "Frequência") +
  stat_bin(binwidth=5000, geom='text', color='black', aes(label=..count..),
           position=position_stack(vjust = 1)) +
  theme_classic()

# Trocando os números de raça, região e estado civíl pelos nomes
data <- data %>%
  mutate(race_name = case_when(
    race == 1 ~ 'Branco',
    race == 2 ~ 'Negro',
    race == 3 ~ 'Hispânico',
    race == 4 ~ 'Outro'))

data <- data %>%
  mutate(region_name = case_when(
    region == 1 ~ 'Nordeste',
    region == 2 ~ 'Centro-Oeste',
    region == 3 ~ 'Sul',
    region == 4 ~ 'Oeste'))

data <- data %>%
  mutate(mrd_name = case_when(
    mrd == 1 ~ 'Casado,\nCônjuge na\nmesma casa',
    mrd == 2 ~ 'Casado,\nCônjuge não mora\nna mesma casa',
    mrd == 3 ~ 'Viúvo',
    mrd == 4 ~ 'Divorciado',
    mrd == 5 ~ 'Separado',
    mrd == 6 ~ 'Nunca casou'))


# Histograma de raça
ggplot(data, aes(x=race_name)) + geom_bar(stat="count", fill="gold") +
  scale_y_continuous(breaks = seq(0, 140000, by = 1000)) +
  labs(title = "Distribuição de Raça",
       x = "Raça", y = "Frequência") +
  geom_text(aes(label = ..count..), stat = "count", vjust = 1, colour = "black") +
  theme_classic()

# Histograma de região
ggplot(data, aes(x=region_name)) + geom_bar(stat="count", fill="brown") +
  scale_y_continuous(breaks = seq(0, 6000, by = 500)) +
  labs(title = "Distribuição de Região",
       x = "Região", y = "Frequência") +
  geom_text(aes(label = ..count..), stat = "count", vjust = 1, colour = "white") +
  theme_classic()

# Histogramas de altura
# Altura para toda a base
ggplot(data, aes(x=height_cm)) + geom_histogram(binwidth=5,
                                                color="white", fill="darkgrey") +
  scale_y_continuous(breaks = seq(0, 10000, by = 500)) +
  scale_x_continuous(breaks = seq(100, 250, by = 5)) +
  labs(title = "Distribuição de Altura",
       x = "Altura (cm)", y = "Frequência") +
  stat_bin(binwidth=5, geom='text', color='black', aes(label=..count..),
           position=position_stack(vjust = 0.99)) +
  theme_classic()
# Altura para homens

ggplot(data_m, aes(x=height_cm)) + geom_histogram(binwidth=5,
                                                  color="white", fill="blue") +
  scale_y_continuous(breaks = seq(0, 10000, by = 500)) +
  scale_x_continuous(breaks = seq(100, 250, by = 5)) +
  labs(title = "Distribuição de Altura (Homens)",
       x = "Altura (cm)", y = "Frequência") +
  stat_bin(binwidth=5, geom='text', color='black', aes(label=..count..),
           position=position_stack(vjust = 0.99)) +
  theme_classic()

# Altura para mulheres

ggplot(data_f, aes(x=height_cm)) + geom_histogram(binwidth=5,
                                                  color="white", fill="red") +
  scale_y_continuous(breaks = seq(0, 10000, by = 500)) +
  scale_x_continuous(breaks = seq(100, 250, by = 5)) +
  labs(title = "Distribuição de Altura (Mulheres)",
       x = "Altura (cm)", y = "Frequência") +
  stat_bin(binwidth=5, geom='text', color='black', aes(label=..count..),
           position=position_stack(vjust = 0.99)) +
  theme_classic()

# Histograma de anos de educação
ggplot(data, aes(x=educ)) + geom_histogram(binwidth=1,
                                           color="white", fill="yellow") +
  scale_y_continuous(breaks = seq(0, 10000, by = 500)) +
  scale_x_continuous(breaks = seq(0, 19, by = 1)) +
  labs(title = "Distribuição de Anos de Educação Formal",
       x = "Anos de Educação Formal", y = "Frequência") +
  stat_bin(binwidth=1, geom='text', color='black', aes(label=..count..),
           position=position_stack(vjust = 0.99)) +
  theme_classic()

# Histograma de estado civíl
ggplot(data, aes(x=mrd_name)) + geom_bar(stat="count", fill="coral") +
  scale_y_continuous(breaks = seq(0, 12000, by = 1000)) +
  labs(title = "Distribuição de Estado Civíl",
       x = "Estado Civíl", y = "Frequência") +
  geom_text(aes(label = ..count..), stat = "count", vjust = 1, color = "black") +
  theme_classic()


# agora, partimos para as variáveis numéricas e vamos analisar suas estatísticas descritivas
# primeiramente, criamos um novo dataframe a partir do original que contenha apenas as variáveis
# propriamente numéricas e rodamos o summary().

numeric_data <- data %>%
  select(age, educ, earnings, height_cm, weight_kg)

# baixamos a biblioteca para tabulação das estatísticas descritivas

#install.packages("pander")
library(pander)

# fazemos o report das estatísticas descritivas
estat_descrit <- pander(summary(numeric_data))


# agora, criamos uma nuvem de pontos para analisar a relação entre salário e altura.
ggplot(data,aes(x = height_cm, y = earnings)) + geom_point() + labs(x = "Altura (centímetros)", y = "Salário, (US$)", title = "Relação entre Altura e Rendimento")

# alternativamente, podemos, para ter uma melhor ideia de como os dados estão distribuídos, usar a função
# geom_jitter() do ggplot2.

ggplot(data, aes(x = height_cm, y = earnings)) + geom_jitter() + labs(x = "Altura (centímetros)", y = "Salário, (US$)", title = "Relação entre Altura e Rendimento")

#plotando com a reta de regressão para visualizar os erros:
ggplot(data, aes(x = height_cm, y = earnings)) + geom_jitter() +
  labs(x = "Altura (centímetros)", y = "Salário, (US$)",title = "Relação entre Altura e Rendimento") +
  geom_smooth(method = 'lm', se = FALSE)

##### 4- ESTIMAÇÕES

# Regressão simples:
reg <- lm(earnings ~ height_cm, data)

stargazer(reg, type = "latex", title = 'Regressão Preliminar',
          dep.var.labels=c("Salário"),
          covariate.labels=c("Altura (cm)", "Constante"),
          omit.stat=c("LL","ser"))

# computando a regressão com desvio padrão robusto a heterocedasticidade
reg_robust <- feols(earnings ~ height_cm, data = data)
summary(reg_robust, vcov = "hetero")

#RESTRIÇÕES

###Regressão simples restrita por sexo:
reg_m <- lm(earnings ~ height_cm, data_m)
reg_f <- lm(earnings ~ height_cm, data_f)

stargazer(reg_m, reg_f, type = "latex", title = 'Regressões por Sexo',
          dep.var.labels=c("Salário"),
          covariate.labels=c("Altura (cm)", "Constante"),
          omit.stat=c("LL","ser"))

# computando a regressão com desvio padrão robusto a heterocedasticidade
reg_m_robust <- feols(earnings ~ height_cm, data = data_m)
reg_f_robust <- feols(earnings ~ height_cm, data = data_f)

summary(reg_m_robust, vcov = "hetero")
summary(reg_f_robust, vcov = "hetero")

###restringindo para outliers em renda, dada a sensibilidade do OLS a outliers e a previamente identificada
# distribuição anormal da renda

no_outliers_in_earnings_data <- data %>% 
  filter(earnings < 84054.750)

reg_no_outliers_in_earnings <- lm(earnings ~ height_cm, data = no_outliers_in_earnings_data)

stargazer::stargazer(reg_no_outliers_in_earnings, type = 'latex', title = 'Regressão sem outliers na renda',
                     dep.var.labels=c("Salário"),
                     covariate.labels=c("Altura (cm)", "Constante"),
                     omit.stat=c("LL","ser"))

# analisando a reta de regressão no eliminando os outliers para entender melhor a variância dos erros
ggplot(no_outliers_in_earnings_data, aes(x = height_cm, y = earnings)) + geom_jitter() +
  labs(x = "Altura (centímetros)", y = "Salário, (US$)",title = "Relação entre Altura e Rendimento") +
  geom_smooth(method = 'lm', se = FALSE)

# computando a regressão com desvio padrão robusto a heterocedasticidade
reg_no_outliers_in_earnings_robust <- feols(earnings ~ height_cm, data = no_outliers_in_earnings_data)
summary(reg_no_outliers_in_earnings_robust, vcov = "hetero")

###restringindo para Nordeste vs resto

reg_Nordeste <- lm(earnings ~ height_cm, data = data, subset = (region == 1))
reg_Nao_Nordeste <- lm(earnings ~ height_cm, data = data, subset = (region != 1))

stargazer::stargazer(reg_Nordeste, reg_Nao_Nordeste, type = 'latex',
                     title = 'Regressão restrita ao nordeste vs às demais regiões',
                     dep.var.labels=c("Salário"),
                     covariate.labels=c("Altura (cm)", "Constante"),
                     omit.stat=c("LL","ser"))

# computando a regressão com desvio padrão robusto a heterocedasticidade
data_Nordeste <- data %>%
  filter(region == 1)

data_Nao_Nordeste <- data %>% 
  filter(region != 1)

reg_Nordeste_robust <- feols(earnings ~ height_cm, data = data_Nordeste)
reg_Nao_Nordeste_robust <- feols(earnings ~ height_cm, data = data_Nao_Nordeste)
summary(reg_Nordeste_robust, vcov = "hetero")
summary(reg_Nao_Nordeste_robust, vcov = "hetero")


###restringindo para mais de 10 anos de educação vs menos

reg_more_10y_educ <- lm(earnings ~ height_cm, data = data, subset = (educ > 10))
reg_10y_less_educ <- lm(earnings ~ height_cm, data = data, subset = (educ <= 10))

stargazer::stargazer(reg_more_10y_educ, reg_10y_less_educ, type = 'latex',
                     title = 'Regressão restrita a mais de 10 anos de educação vs 10 ou menos',
                     dep.var.labels=c("Salário"),
                     covariate.labels=c("Altura (cm)", "Constante"),
                     omit.stat=c("LL","ser"))

# computando a regressão com desvio padrão robusto a heterocedasticidade
data_more_10y_educ <- data %>%
  filter(educ > 10)

data_10y_less_educ <- data %>%
  filter(educ <= 10)

reg_more_10y_educ_robust <- feols(earnings ~ height_cm, data = data_more_10y_educ)
reg_10y_less_educ_robust <- feols(earnings ~ height_cm, data = data_10y_less_educ)
summary(reg_more_10y_educ_robust, vcov = "hetero")
summary(reg_10y_less_educ_robust, vcov = "hetero")

### Excluindo/isolando autônomos
reg_não_auto <- lm(earnings ~ height_cm, data = data, subset = (cworker != 6))
reg_auto <- lm(earnings ~ height_cm, data = data, subset = (cworker == 6))

stargazer(reg_não_auto, reg_auto, type = 'latex',
          title = 'Regressão Isolando Autônomos',
          dep.var.labels=c("Salário"),
          covariate.labels=c("Altura (cm)", "Constante"),
          omit.stat=c("LL","ser"))

# Computando a regressão com desvio padrão robusto a heterocedasticidade
data_não_auto <- data %>%
  filter(cworker != 6)

data_auto <- data %>%
  filter(cworker == 6)

reg_não_auto_robust <- feols(earnings ~ height_cm, data = data_não_auto)
reg_auto_robust <- feols(earnings ~ height_cm, data = data_auto)
summary(reg_não_auto_robust, vcov = "hetero")
summary(reg_auto_robust, vcov = "hetero")


### Brancos vs não-brancos
reg_não_brancos <- lm(earnings ~ height_cm, data = data, subset = (race != 1))
reg_brancos <- lm(earnings ~ height_cm, data = data, subset = (race == 1))

stargazer(reg_não_brancos, reg_brancos, type = 'latex',
          title = 'Regressão Brancos vs Não-Brancos',
          dep.var.labels=c("Salário"),
          covariate.labels=c("Altura (cm)", "Constante"),
          omit.stat=c("LL","ser"))

# Computando a regressão com desvio padrão robusto a heterocedasticidade
data_não_brancos <- data %>%
  filter(race != 1)

data_brancos <- data %>%
  filter(race == 1)

reg_não_brancos_robust <- feols(earnings ~ height_cm, data = data_não_brancos)
reg_brancos_robust <- feols(earnings ~ height_cm, data = data_brancos)
summary(reg_não_brancos_robust, vcov = "hetero")
summary(reg_brancos_robust, vcov = "hetero")


### Casados vs não-casados
reg_não_casados <- lm(earnings ~ height_cm, data = data, subset = (mrd != 1))
reg_casados <- lm(earnings ~ height_cm, data = data, subset = (mrd == 1))

stargazer(reg_não_casados, reg_casados, type = 'latex',
          title = 'Casados vs Não-Casados',
          dep.var.labels=c("Salário"),
          covariate.labels=c("Altura (cm)", "Constante"),
          omit.stat=c("LL","ser"))

# Computando a regressão com desvio padrão robusto a heterocedasticidade
data_não_casados <- data %>%
  filter(mrd != 1)

data_casados <- data %>%
  filter(mrd == 1)

reg_não_casados_robust <- feols(earnings ~ height_cm, data = data_não_casados)
reg_casados_robust <- feols(earnings ~ height_cm, data = data_casados)
summary(reg_não_casados_robust, vcov = "hetero")
summary(reg_casados_robust, vcov = "hetero")

### Adm.-Geren.-Prof. VS Manuais-Técnicas
reg_adm <- lm(earnings ~ height_cm, data = data, subset = (occupation <= 5))
reg_manual <- lm(earnings ~ height_cm, data = data, subset = (occupation > 5))

stargazer(reg_adm, reg_manual, type = 'latex',
          title = 'Adm.-Geren.-Prof. VS Manuais-Técnicas',
          dep.var.labels=c("Salário"),
          covariate.labels=c("Altura (cm)", "Constante"),
          omit.stat=c("LL","ser"))

# Computando a regressão com desvio padrão robusto a heterocedasticidade
data_adm <- data %>%
  filter(occupation <= 5)

data_manual <- data %>%
  filter(occupation > 5)

reg_adm_robust <- feols(earnings ~ height_cm, data = data_adm)
reg_manual_robust <- feols(earnings ~ height_cm, data = data_manual)
summary(reg_adm_robust, vcov = "hetero")
summary(reg_manual_robust, vcov = "hetero")

#### REGRESSÃO MÚLTIPLA
## Com outliers em renda
#fazendo alterações necessárias na base:

#criando uma coluna para NE
data <- data %>%
  dplyr::mutate('NE' = ifelse(region == 1, 1, 0))

#criando uma coluna para Branco
data <- data %>% 
  dplyr::mutate('Branco' = ifelse(race == 1, 1, 0))

#criando uma coluna para Manual-Técnico
data <- data %>% 
  dplyr::mutate('ManualTécnico' = ifelse(occupation > 5, 1, 0))


# Regressores da reg múltipla:
# height_cm, sex, Branco, educ, Manual-Técnico, NE

# rodando a regressão múltipla:
reg_mult <- lm(earnings ~ height_cm + sex + Branco + educ + ManualTécnico + NE, data = data)
summary(reg_mult)
# computando SEs robustos a heterocedasticidade
reg_mult_robust <- fixest::feols(earnings ~ height_cm + sex + Branco + educ + ManualTécnico + NE, data = data)
summary(reg_mult_robust, vcov = 'hetero')

stargazer::stargazer(reg_mult, type = 'latex',
                     title = 'Regressão Múltipla',
                     dep.var.labels=c("Salário"),
                     covariate.labels=c("Altura (cm)", "Gênero", "Branco", "Educ", "ManualTecnico", "NE","Constante"),
                     omit.stat=c("LL","ser"))

## Sem outliers em renda

#fazendo alterações necessárias na base:

#criando uma coluna para NE
no_outliers_in_earnings_data <- no_outliers_in_earnings_data %>%
  dplyr::mutate('NE' = ifelse(region == 1, 1, 0))

#criando uma coluna para Branco
no_outliers_in_earnings_data <- no_outliers_in_earnings_data %>% 
  dplyr::mutate('Branco' = ifelse(race == 1, 1, 0))

#criando uma coluna para Manual-Técnico
no_outliers_in_earnings_data <- no_outliers_in_earnings_data %>% 
  dplyr::mutate('ManualTécnico' = ifelse(occupation > 5, 1, 0))


# Regressores da reg múltipla:
# height_cm, sex, Branco, educ, Manual-Técnico, NE

# rodando a regressão múltipla:
reg_mult_no_outliers <- lm(earnings ~ height_cm + sex + Branco + educ + ManualTécnico + NE, data = no_outliers_in_earnings_data)
summary(reg_mult_no_outliers)
# computando SEs robustos a heterocedasticidade
reg_mult_no_outliers_robust <- fixest::feols(earnings ~ height_cm + sex + Branco + educ + ManualTécnico + NE, data = no_outliers_in_earnings_data)
summary(reg_mult_no_outliers_robust, vcov = 'hetero')

stargazer::stargazer(reg_mult_no_outliers, type = 'latex',
                     title = 'Regressão Múltipla (Sem Outliers de Renda)',
                     dep.var.labels=c("Salário"),
                     covariate.labels=c("Altura (cm)", "Gênero", "Branco", "Educ", "ManualTecnico", "NE","Constante"),
                     omit.stat=c("LL","ser"))


### REGRESSÕES NÃO-LINEARES
## Regressão Logarítmica
reg_mult_log <- lm(log(earnings) ~ height_cm + sex + Branco + educ + ManualTécnico + NE,
                   data = data)
summary(reg_mult_log)

# computando SEs robustos a heterocedasticidade
reg_mult_log_robust <- feols(log(earnings) ~ height_cm + sex + Branco + educ + ManualTécnico + NE,
                             data = data)
summary(reg_mult_log_robust, vcov = 'hetero')

stargazer(reg_mult_log, type = 'latex',
          title = 'Regressão Múltipla (Log Renda)',
          dep.var.labels=c("Log Salário"),
          covariate.labels=c("Altura (cm)", "Gênero", "Branco", "Educ",
                             "ManualTecnico", "NE","Constante"),
          omit.stat=c("LL","ser"))


## Testar se quadrático para mulheres tem melhor aderência aos dados

#fazendo alterações necessárias na base feminina:

#criando uma coluna para NE
data_f <- data_f %>%
  dplyr::mutate('NE' = ifelse(region == 1, 1, 0))

#criando uma coluna para Branco
data_f <- data_f %>% 
  dplyr::mutate('Branco' = ifelse(race == 1, 1, 0))

#criando uma coluna para Manual-Técnico
data_f <- data_f %>% 
  dplyr::mutate('ManualTécnico' = ifelse(occupation > 5, 1, 0))

reg_lin_mulheres <- lm(earnings ~ height_cm + Branco + educ + ManualTécnico + NE, data = data_f)
summary(reg_lin_mulheres)

reg_nao_lin_mulheres <- lm(earnings ~ poly(height_cm, degree = 2, raw = TRUE) + Branco + educ + ManualTécnico + NE, data = data_f)
summary(reg_nao_lin_mulheres)

#regressoes não lineares robustas

fixest::feols(earnings ~ height_cm + Branco + educ + ManualTécnico + NE, data = data_f, vcov = 'hetero')
fixest::feols(earnings ~ poly(height_cm, degree = 2, raw = TRUE) + Branco + educ + ManualTécnico + NE, data = data_f, vcov = 'hetero')

stargazer::stargazer(reg_lin_mulheres, reg_nao_lin_mulheres)
      #alterações no report feitas manualmente no latex
