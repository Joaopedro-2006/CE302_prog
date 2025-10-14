formals() #Mostra as variaveis criadas dentro da funçao

for(i in 1:3) {
  if(i == 2) break 
  print(i)}         #O resultado é 1

next   #Ignora a iteração atual e passa para a próxima

ifelse() #Não é uma boa pratica usar dentro de loops aninhados

if (NA) 1 else 0  #Da erro

s <- 0 
for(i in 1:5) { 
  if(i %% 2 == 0) next 
  s <- s + i }
s       #Resposta é 9

?return()

sqrt(-1) #Resposta é NAN

x <- 0 
while(TRUE) { 
  x <- x + 1 
  if(x == 4) break }

x

#o comportamento do switch() com índice numérico fora do intervalo = NULL

x <- 1:4 
ifelse(x > 2, x^2, 0)


x <- 1 
while(x < 3) { 
  x <- x + 1 } 
print(x)

i <- 1 
repeat {
  if(i > 3) break 
  i <- i + 1 } 
i


x = 3
while(x<9) {
  x = x + 1}
print(x)
