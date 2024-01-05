
# TESTE DE METODOS ESTATISTICOS PARA A BIOINFORMATICA
# Aluno -> PG 51681_CFCG

# Exercício 1)

# id                                              1
# gender -> variável qualitativa binária          2
# age -> variável quantitativa proporcional       3
# hypertension -> variável qualitativa binária    4
# hear_disease -> variável qualitativa binária    5
# ever_married -> variável qualitativa binária    6
# work_type -> variável qualitativa nomina        7
# residence_type -> variável qualitativa binária  8
# avg_glucose_level -> variável quantitativa nominal  9
# bmi -> variável quantitativa                    10
# smoking_status -> variável qualitativa          11
# stroke -> variável qualitativa binária          12

# Variáveis quantitativas:
summary(Stroke_data_limpo[c(3, 9, 10)], na.rm = T)
sd(Stroke_data_limpo$id, na.rm = T)
sd(Stroke_data_limpo$age, na.rm = T)
sd(Stroke_data_limpo$avg_glucose_level, na.rm = T)
sd(Stroke_data_limpo$bmi, na.rm = T)

# Variáveis qualitativas:
table(Stroke_data_limpo$gender)

hipertensao = as.factor(Stroke_data_limpo$hypertension) 
levels(hipertensao) = c('Não tem', 'Tem')
table(hipertensao)

heart_disease = as.factor(Stroke_data_limpo$heart_disease)
levels(heart_disease) = c('Não tem', 'Tem')
table(heart_disease)

table(Stroke_data_limpo$ever_married)
table(Stroke_data_limpo$work_type)
table(Stroke_data_limpo$Residence_type)
table(Stroke_data_limpo$smoking_status)

stroke = as.factor(Stroke_data_limpo$stroke)
levels(stroke) = c('Não teve', 'Teve')
table(heart_disease)


# Exercício 2)

# A)
hist(Stroke_data_limpo$age, freq = F, main = 'Frequências relativas das idades', xlab = 'Idade', col = 2)

# B)
boxplot(Stroke_data_limpo$age, horizontal = T, col = 4)


# Exercício 3)
# A) 
tapply(Stroke_data_limpo$avg_glucose_level, stroke, summary, na.rm = T)


# B)
boxplot(Stroke_data_limpo$avg_glucose_level~stroke, col = c(2,3))


# C)
# H0 -> Os dois grupos grupos apresentam a mesma variância
# H1 -> Os dois grupos não apresentam a mesma variância
# alpha = 0.05
var.test(Stroke_data_limpo$avg_glucose_level~Stroke_data_limpo$stroke)

# Como o p-value (2.336e-10) < alpha, rejeita-se a hipótese nula, ou seja, os dois grupos não apresentam a mesma variância; F = 0.55298.


# D)
# Para comparar valores medios de duas amostras, com n>30, utiliza-se o t.test()
# H0 -> Os valores medios dos niveis de glicose do grupo com AVC são superiores aos sem AVC 
# H1 -> Os valores medios dos niveis de glicose do grupo com AVC não são superiores aos sem AVC 
# alpha = 0.05

t.test(Stroke_data_limpo$avg_glucose_level~Stroke_data_limpo$stroke, var = F)

# Como o p-value (9.719e-09) < alpha, rejeita-se a hipótese nula, ou seja, os valores médios dos niveis de glucose não são superiores; t = -5.97


# Exercicio 4)
tabela_fumador = table(Stroke_data_limpo$smoking_status)
tabela_fumador

# A)
s = sum(tabela_fumador)
pt = c(1/4, 2/4, 1/4)
fe = s*pt
fe


# B)
barplot(rbind(fe,tabela_fumador), beside = T, col = c(2,3), main = 'Frequência Esperada vs Frequência Observada')


# C)
# H0 -> A distribuicao do tipo de fumador é 1:2:1
# H1 -> A distribuicao do tipo de fumador não é 1:2:1
# alpha = 0.01

chisq.test(tabela_fumador, p = pt)

# Como o p-value (9.533e-05) < alpha, rejeita-se a hipótese nula, portanto, a distribuição do tipo de fumador não é igual a 1:2:1; X-squared = 18.516; Graus de liberdade (df) estão corretos


# Exercício 5)
# H0 -> Existe associação entre o tipo de fumador e a ocorrência de AVC
# H1 -> Não existe associação entre o tipo de fumador e a ocorrência de AVC
# alpha = 0.05

tabela = table(Stroke_data_limpo$smoking_status, stroke)
tabela

chisq.test(tabela)

# Como o p-value (0.003198) < alpha, rejeita-se a hipótese nula, portanto, não existe associação entre o tipo de fumador e a ocorrência de AVC; X-squared = 11.491; Graus de liberdade (df) estão corretos


# Exercício 6)

# A)
boxplot(log(Stroke_data_limpo$avg_glucose_level)~Stroke_data_limpo$smoking_status, col = c(2,3,4))


# B)
# H0 -> Não há diferenças nOs níveis de glucose medio de cada grupo de fumadores
# H1 -> Há diferenças nos níveis de glucose medio de cada grupo de fumadores 
# alpha = 0.01

shapiro.test(log(Stroke_data_limpo$avg_glucose_level))

# No teste de shapiro, p-value (2.2e-16) < alpha, ou seja, o nivel de glucose medio não serve uma distribuição normal

fligner.test(log(Stroke_data_limpo$avg_glucose_level)~Stroke_data_limpo$smoking_status)

# No teste de fligner, p-value (0.08873) > alpha, ou seja, não se rejeita a hipótese nula significando que não existem diferenças nOs níveis de glucose medio de cada grupo de fumadores



# C)
m = aov(log(Stroke_data_limpo$avg_glucose_level)~Stroke_data_limpo$smoking_status)

TukeyHSD(m, conf.level = 0.99)
plot(TukeyHSD(m, conf.level=0.99))

# Observando o gráfico, não há evidência suficiente para rejeitar a hipótese nula



