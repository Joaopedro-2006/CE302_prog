#Exemplo de estrutura IF-ELSE no R:

faltas = 16
nota = 71
if (nota >= 70 & faltas < 15) {
  result = "aprovado"
} else if (nota < 70) {
  result = "Reprovado por nota"
} else {
  result = "reprovado por faltas"
}

result

#Exercicio saudação
h <- 11

if (h >= 6 & h < 12) {
  saudacao <- "Bom dia!"
} else if (h >= 12 & h < 18) {
  saudacao <- "Boa tarde!"
} else if (h >= 18 & h < 23) {
  saudacao <- "Boa noite!"
} else {
  saudacao <- "Não enviar mensagem!"
  stop("Não enviar mensagem!")
}

saudacao


# Exercício tipo de média

#tipo <- "aritmetica"
#tipo <- "harmonica"
#tipo <- "geometrica"
#tipo <- "ajhfaksfhdkashd"

x <- 1:10

switch(tipo,
       "aritmetica" = {
         mean(x)
       },
       "harmonica" = {
         length(x)/sum(1/x)
       },
       "geometrica" = {
         prod(x)^(1/length(x))
       },
       {
         NA_real_
       })

# Funções vetorizadas

# Usando IF-ELSE vetorial.

notas <- c("João" = 70, "Ana" = 89,
           "Márcia" = 81, "Tiago" = 65,
           "Rodrigo" = 35)

ifelse(notas >= 70, "Aprovado",
       ifelse(notas >= 40, "Exame",
              "Reprovado"))

# Usando SWITCH vetorial.
 
dplyr::case_when(notas >= 70 ~ "Aprovado",
                 notas >= 40 ~ "Exame",
                 TRUE ~ "Reprovado")


## Estruturas de repetição

# Exemplo de estrutura for

for(i in 1:10) {
  print(i^2)
}

#Exemplo de estrutura FOR para taxa de juros

tx_juros = 0.01
n_meses = 12
rend = numeric(n_meses)
rend[1] = 100
for (i in 2:n_meses) {
  rend[i] = rend[i - 1] * (1 + tx_juros)
}

rend


#Exemplo de estrutura WHILE 

n_numbers = 12
total = 0 
i = 1L
while (i > n_numbers) {
  u <- total + runif(1)
  if (sum(u) > 4) break 
  total = u 
  i = i + 1L
}


#Exemplo de estrutura REPEAT
total = 0 
i = 1L
repeat {
  u <- total + runif(1)
  if (sum(u) > 4) break 
  total = u 
  i = i + 1L
}
total

# Exercício · Lançamento de dados

n_max <- 100
tentativas <- 1
while(tentativas < n_max) {
  l1 <- sample(1:6, 3, replace = TRUE) # joga os dados
  l1_ordenado <- sort(l1) # ordenada
  print(l1_ordenado)
  seque <- sum(ifelse(diff(l1_ordenado) == 1, TRUE, FALSE))
  if(seque == 2) break
  tentativas <- tentativas + 1
}
tentativas

tentativas = 1 
repeat {
  l1 = sample(1:6, 3, replace = TRUE)
  l1_ordenado = sort(l1)
  print(l1_ordenado)
  seque = sum(ifelse(diff(l1_ordenado) == 1, TRUE, FALSE))
  if(seque == 2) break
  tentativas = tentativas + 1
}
tentativas

# Exercício · Número médio de tentativas de lançamento de dados

output <- c()
for(i in 1:1000) {
  n_max <- 100
  tentativas <- 1
  while(tentativas < n_max) {
    l1 <- sample(1:6, 3, replace = TRUE) # joga os dados
    l1_ordenado <- sort(l1) # ordenada
    print(l1_ordenado)
    seque <- sum(ifelse(diff(l1_ordenado) == 1, TRUE, FALSE))
    if(seque == 2) break
    tentativas <- tentativas + 1
  } 
  output[i] <- tentativas
}

mean(output)
hist(output)


## Funções

# Formula de baskara

baskara <- function(a = 2, b = 1, c = 0) {
  delta <- b^2 - 4 * a * c
  x <- (-b + c(-1, 1) * sqrt(delta))/(2 * a)
  return(x)
}

x = baskara(a= 2, b = 1, c =0)
x


## Instruções para funções

joga_dados <- function(n_dados, n_max, n_simulacao) {
  output <- c()
  for(i in 1:n_simulacao) {
    tentativas <- 1
    while(tentativas < n_max) {
      l1 <- sample(1:6, n_dados, replace = TRUE) # joga os dados
      l1_ordenado <- sort(l1) # ordenada
      seque <- sum(ifelse(diff(l1_ordenado) == 1, TRUE, FALSE))
      if(seque == c(n_dados-1)) break
      tentativas <- tentativas + 1
    } 
    output[i] <- tentativas
  }
  return(output)
}

jogadas <- joga_dados(n_dados = 3, n_max = 100, n_simulacao = 1000)
jogadas <- joga_dados(n_dados = 4, n_max = 100, n_simulacao = 1000)
mean(jogadas)


## Aspectos avançados

# Calculo do IMC

calcula_imc <- function(peso, altura) {
  imc <- peso/(altura^2)
  limits <- c(0, 18.5, 25, 30, Inf)
  labels <- c("Magreza", "Adequado", "Pré-obeso", "Obesidade")
  classif <- labels[findInterval(imc, limits)]
  return(list(IMC = imc, Classificao = classif))
}
calcula_imc(peso = 86, altura = 1.85)


# Argumento com valor default

calcula_imc <- function(altura, peso = 80) {
  imc <- peso/(altura^2)
  limits <- c(0, 18.5, 25, 30, Inf)
  labels <- c("Magreza", "Adequado", "Pré-obeso", "Obesidade")
  classif <- labels[findInterval(imc, limits)]
  return(list(IMC = imc, Classificao = classif))
}
calcula_imc(altura = 1.80)

## Tratando exceções

calcula_imc <- function(altura, peso = 80) {
  if(altura < 0) stop("Altura deve ser maior do que zero.")  #Aqui
  if(peso < 0) stop("Peso deve ser maior do que zero.")    #Aqui
  imc <- peso/(altura^2)
  limits <- c(0, 18.5, 25, 30, Inf)
  labels <- c("Magreza", "Adequado", "Pré-obeso", "Obesidade")
  classif <- labels[findInterval(imc, limits)]
  return(list(IMC = imc, Classificao = classif))
}
calcula_imc(altura = -1)


## Uso dos ... (passagem implicita de argumentos entre diferentes funções!)

calcula_imc_numero <- function(peso, altura) {
  imc <- peso/(altura^2)
  return(imc)
}

calcula_imc <- function(...) {
  imc <- calcula_imc_numero(...)
  limits <- c(0, 18.5, 25, 30, Inf)
  labels <- c("Magreza", "Adequado", "Pré-obeso", "Obesidade")
  classif <- labels[findInterval(imc, limits)]
  return(list(IMC = imc, Classificao = classif))
}

calcula_imc(peso = 90, altura = 1.70)

