#quiz prova 3 ce302
x <- 0
while(TRUE) { 
  x <- x + 1 
  if(x == 4) break 
} 
x

x <- 1:4 
ifelse(x > 2, x^2, 0)

i <- 1 
repeat { 
  if(i > 3) break 
  i <- i + 1 
} 
i

for i in (){
  i <- i+1
}


for(i in 1:3) { 
  if(i == 2) break 
  print(i) }

s <- 0 
for(i in 1:5) { 
  if(i %% 2 == 0) next 
  s <- s + i 
} 
s

x <- 1 
while(x < 3) { 
  x <- x + 1 
} 
print(x)

#com repeat
n_max <- 100
tentativas <- 1

repeat {
  l1 <- sample(1:6, 3, replace = TRUE)
  l1_ordenado <- sort(l1)
  print(l1_ordenado)
  seque <- sum(ifelse(diff(l1_ordenado) == 1, TRUE, FALSE))
  
  if(seque == 2) break
  if(tentativas >= n_max) break
  
  tentativas <- tentativas + 1
}

tentativas