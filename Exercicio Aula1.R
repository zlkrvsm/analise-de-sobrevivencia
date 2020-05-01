# Definição de funções
grup_traj <- function(data, tempo = tempo, id = id) {
  require(ggplot2)
  require(dplyr)
  data <- mutate(data, status2 = recode(status, `1` = 4, `0` = 1))
  y_breaks <- length(data$id)
  x_breaks <- max(data$tempo)

  ggplot(data) +
    geom_point(aes(tempo, id, shape = status2), size = 3) +
    geom_segment(aes(x = 0, y = id, xend = tempo, yend = id)) +
    scale_x_continuous(breaks = scales::extended_breaks(x_breaks),
                       name = "Meses") +
    scale_y_continuous(breaks = scales::extended_breaks(y_breaks),
                       name = "Paciente") +
    scale_shape_identity() +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank()) +
    coord_cartesian(xlim = c(0.25, x_breaks + 0.5))
}

indiv_traj <- function(data, i) {
  require(ggplot2)
  require(patchwork)

  tmax  <- max(data$tempo.fim)
  t.ini <- data$tempo.ini[[i]]
  t.fim <- data$tempo.fim[[i]]
  censu <- data$status[[i]]

  individuo_n <- tibble(
    tempo2 = seq(0, tmax + 1, 1),
    Yx = ifelse(tempo2 >= t.ini & tempo2 < t.fim, 1, 0),
    Nx = ifelse(tempo2 >= t.fim & t.fim != tmax, 1, 0),
    status = data$status[[i]])

  pontos.Nx <- tibble(
    x = with(individuo_n, ifelse(Nx != lag(Nx), tempo2, NA_integer_)),
    y = with(individuo_n, ifelse(Nx != lag(Nx), 1, NA_integer_)),
    shape = ifelse(data$status[[i]] == 0 & x == data$tempo.fim[[i]], 1, 16))

  pontos.Yx <- tibble(
    x = with(individuo_n, ifelse(Yx != lag(Yx), tempo2, NA_integer_)),
    y = with(individuo_n, ifelse(Yx != lag(Yx), lag(Yx), NA_integer_)),
    shape = ifelse(data$status[[i]] == 0 & x == data$tempo.fim[[i]], 1, 16))

  g <- ggplot(individuo_n) +
    geom_step(aes(tempo2, Nx), direction = "hv", na.rm = TRUE) +
    geom_point(data = pontos.Nx,
               aes(x, y, shape = shape),
               size = 3,
               na.rm = TRUE) +
    scale_shape_identity() +
    scale_x_continuous(breaks = seq(0, tmax, 2),
                       minor_breaks = NULL,
                       limits = c(0, tmax),
                       name = "Meses") +
    scale_y_continuous(breaks = 0:1,
                       limits = c(-1, 2),
                       minor_breaks = NULL,
                       name = expression(N[i](x))) +
    theme_classic() +
    theme(panel.grid = element_blank())

  h <- ggplot(individuo_n) +
    geom_step(aes(tempo2, Yx), direction = "hv", na.rm = TRUE) +
    geom_point(
      data = pontos.Yx,
      aes(x, y, shape = shape),
      size = 3,
      na.rm = TRUE) +
    scale_shape_identity() +
    scale_x_continuous(limits = c(0, tmax), minor_breaks = NULL) +
    scale_y_continuous(
      breaks = 0:1,
      limits = c(-1, 2),
      minor_breaks = NULL,
      name = expression(Y[i](x))) +
    theme_classic() +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      panel.grid = element_blank()) +
    ggtitle(paste0("Paciente ", i))

  wrap_plots(h, g, nrow = 2)
}

# Pacotes ----
library(tidyverse)
library(patchwork)
library(survival)

# Exercício 2.1 ----

# Dados os tempos de observação de aleitamento das seguintes crianças
aleit <- tibble(id     = 1:15,
                tempo  = c(6, 12, 10, 3, 5, 1, 6, 8, 1, 5, 2, 2, 5, 8, 1),
                status = 1)
aleit

# a) Represente graficamente os tempos de observação das crianças
grup_traj(aleit)

# b) Represente as trajetórias dos primeiros cinco indivíduos
aleit_cont <- mutate(aleit,
                     tempo.ini = 0L,
                     tempo.fim = as.integer(tempo),
                     tempo = tempo.fim - tempo.ini,
                     status = 1L)


indiv_traj(aleit_cont, 1)
indiv_traj(aleit_cont, 2)
indiv_traj(aleit_cont, 3)
indiv_traj(aleit_cont, 4)
indiv_traj(aleit_cont, 5)


# c) Como você construíria um banco de dados pelo processo clássico
aleit

# d) E pelo processo de contagem
aleit_cont

# Exercício 2.2 ----
# Considerando agora o tempo de sobrevivência de 15 pacientes de diálise
dial <- tibble(
  id     = 1:15,
  tempo  = c(2L, 4L, 29L, 6L, 3L, 1L, 1L, 2L, 3L, 9L, 10L, 11L, 5L, 5L, 1L),
  status = c(1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1))
dial

# a. Represente graficamente os tempos de observação do paciente, notação clássica
dial_cont <- cbind(dial, tempo.fim = tempo, tempo.ini = 0)
grup_traj(dial_cont)

#b. Represente as trajetórias dos primeiros cinco indivíduos
indiv_traj(dial_cont, 1)
indiv_traj(dial_cont, 2)
indiv_traj(dial_cont, 3)
indiv_traj(dial_cont, 4)
indiv_traj(dial_cont, 5)

# c. Construa um banco de dados para analisar esses dados pelo processo clássico
dial

# d. Reconstrua segundo o processo de contagem utilizando a informação de tempo inicial abaixo
tempo.ini <- c(0, 1, 12, 3, 1, 7, 0, 1, 1, 3, 12, 4, 4, 1, 22)
dial_cont <- tibble(paciente = dial$id,
                    tempo = dial$tempo,
                    tempo.ini,
                    tempo.fim = tempo + tempo.ini,
                    status = dial$status)

dial_cont

# Exercício 2.3 ----
aids <- tibble(id = 1:6,
               tempo = c(4, 6, 12, 3, 6, 9),
               tempo.ini = c(3, 6, 0, 5, 0, 3),
               tempo.fim = tempo + tempo.ini,
               status = c(1, 0, 0, 0, 1, 0))
aids



# Trajetórias
indiv_traj(aids, 1)
indiv_traj(aids, 2)
indiv_traj(aids, 3)
indiv_traj(aids, 4)
indiv_traj(aids, 5)
indiv_traj(aids, 6)

# Exercício 2.4 ----
traj <- c(id = 1,
          tempo.ini = 2,
          tempo.fim = 4,
          tempo = 2,
          status = 1)

# a. Qual foi o mês de entrada do paciente no estudo
traj["tempo.ini"]
# b. Em que mês ocorreu o desfecho
traj["tempo.fim"]
# c. Em que meses o paciente esteve sob o risco de óbito
seq(traj["tempo.ini"], traj["tempo.fim"])

# Exercício 2.5 ----
traj <- c(id = 1,
          tempo.ini = 0,
          tempo.fim = 8,
          tempo = 8,
          status = 1)

# a. Qual foi a data de entrada do paciente no estudo
traj["tempo.ini"]

# b. Em que data ocorreu o desfecho
traj["tempo.fim"]

# Exercício 2.6 ----
# a. Construa o banco de dados no formato clássico
hiv.clas <- tibble(id     = 1:6,
                   tempo  = c(1, 3, 2, 3, 2, 5),
                   status = 1)
hiv.clas

# b. Construa o banco de dados no formato de processo de contagem
hiv.cont <- tibble(id     = 1:6,
                   tempo.ini  = c(0, 0, 1, 3, 2, 2),
                   tempo = c(1, 3, 2, 3, 2, 5),
                   tempo.fim  = tempo.ini + tempo,
                   status = 1)
hiv.cont

# c. Represente graficamente os tempos observados
grup_traj(hiv.cont)

# d. Represente as trajetórias dos 6 indivíduos utilizando N(t) e Y(t)
indiv_traj(hiv.cont, 1)
indiv_traj(hiv.cont, 2)
indiv_traj(hiv.cont, 3)
indiv_traj(hiv.cont, 4)
indiv_traj(hiv.cont, 5)
indiv_traj(hiv.cont, 6)

# e. Quais são os indivíduos em risco no mês 5?
filter(hiv.cont, tempo.ini <= 5, tempo.fim > 5)

# f. Que tipo de censura ocorreu neste estudo?
"Censura Intervalar"

# g. Em que situação ocorreria truncamento?
cat("Ocorreria um truncamento a esquerda quando um paciente testasse positivo
logo que entrar no estudo.")

# Exercicío 2.7 ----
rm(list = ls())

ipec <- read.csv2("ipec.csv")
head(ipec)

# Observe os dados dos pacientes...

# a. No formato clássico
with(ipec, Surv(tempo, status))

# b. No formato de processo de contagem
with(ipec, Surv(ini, fim, status))

# c. Observe a saída nos dois formatos. Quais os tempos e status do
# 1º e 79º pacientes em cada um dos formatos?
ipec %>%
  select(id, tempo, status) %>%
  filter(id %in% c(1, 79))

ipec %>%
  select(id, ini, fim, status) %>%
  filter(id %in% c(1, 79))

# Exercício 2.8 ----

# Organize o banco e faça uma análise exploratório das variáveis de interesse

# a. Verifique as dimensões dos dados
dim(ipec)

# b. Veja quais são as variáveis que constam no arquivo de dados
names(ipec)

# c. Substitua os dados faltantes (Cód 9, 99 ou I) por NA
sapply(ipec, unique)

ipec$anotrat[ipec$anotrat == 9] <- NA
ipec$obito[ipec$obito == "I"] <- NA

# d. Indique ao R as variáveis categóricas que estão erroneamente classificadas
# como numéricas pelo R.
ipec$escola <- factor(
  ipec$escola,
  labels = c("sem", "fundam", "medio", "sup")
)

ipec$risco <- factor(
  ipec$risco,
  labels = c("Homo", "UDI", "Transf", "Cont hetero", "Mut parc", "2 fat")
)

ipec$acompan <- factor(
  ipec$acompan,
  labels = c("Amb", "Int.Post", "Int.Imed")
)

ipec$anotrat <- factor(ipec$anotrat)

ipec$tratam <- factor(
  ipec$tratam,
  labels = c("Nenhum", "Mono", "Combinada", "Potente")
)

ipec$doenca <- factor(
  ipec$doenca,
  labels = c("PCP", "TB pulm", "TB diss", "Toxo", "SK",
             "Outra", "Cand", "Herpes", "Duas", "Def CD4")
  )

ipec$propcp <- factor(
  ipec$propcp,
  labels = c("Sem", "Prim", "Secund", "Ambas"))

# e. Calcule as medidas resumo das variáveis presentes no banco
summary(ipec)

# f. Construa tabelas de frequência para as variáveis de maior interesse
table(ipec$status)
table(ipec$sexo)
table(ipec$escola)
table(ipec$risco)
table(ipec$acompan)
table(ipec$anotrat)
table(ipec$tratam)
table(ipec$doenca)
table(ipec$propcp)

# g. Construa gráficos para o tempo de sobrevivência e para outras variáveis de
# interesse.
boxplot(
  tempo ~ status,
  data = ipec,
  main = "Tempo por status",
  ylab = "Tempo(dias)",
  xlab = "Desfecho")

boxplot(
  tempo ~ sexo,
  data = ipec,
  main = "Tempo por sexo",
  ylab = "Tempo(dias)",
  xlab = "Sexo")

boxplot(
  tempo ~ escola,
  data = ipec,
  main = "Tempo por escolaridade",
  ylab = "Tempo(dias)",
  xlab = "Escolaridade")

boxplot(
  tempo ~ risco,
  data = ipec,
  main = "Tempo por risco",
  ylab = "Tempo(dias)",
  xlab = "Risco")

boxplot(
  tempo ~ acompan,
  data = ipec,
  main = "Tempo por acompanhamento",
  ylab = "Tempo(dias)",
  xlab = "Risco")

boxplot(
  tempo ~ tratam,
  data = ipec,
  main = "Tempo por tratamento",
  ylab = "Tempo(dias)",
  xlab = "Tratamento")

boxplot(
  tempo ~ doenca,
  data = ipec,
  main = "Tempo por doença",
  ylab = "Tempo(dias)",
  xlab = "Doença")

boxplot(
  tempo ~ propcp,
  data = ipec,
  main = "Tempo por propcp",
  ylab = "Tempo(dias)",
  xlab = "Propcp")

hist(
  ipec$tempo,
  main = "Sobrevivência",
  ylab = "Freqüência",
  xlab = "Dias")

hist(
  ipec$tempo/30,
  breaks = seq(0, 120, 6),
  at     = seq(0, 120, 12),
  main   = "Sobrevivência",
  ylab   = "Freqüência",
  xlab   = "Meses")
