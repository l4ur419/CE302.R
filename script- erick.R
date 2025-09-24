# --- Script para Análise de Matrizes (Questão 6) ---

# 1. Definição das Matrizes A e B (preenchimento por COLUNA)
A <- matrix(c(0, 7, 31, 21, 38,
              0, 50, 27, 13, 23,
              16, 18, 2, 39, 39,
              6, 24, 17, 26, 9,
              26, 44, 9, 11, 23), 
            nrow = 5, ncol = 5, byrow = TRUE)

B <- matrix(c( 44, 41, 32, 4, 31,
                    27, 5, 19, 41, 21,
                    30, 27, 24, 35, 44,
                    33, 41, 37, 0, 10,
                    34, 11, 44, 21, 49), 
            nrow = 5, ncol = 5 , byrow = TRUE)
# 2. Definição da Matriz C = B ⋅ (Bᵀ ⋅ B)⁻¹ ⋅ Bᵀ
B_transposta <- t(B)
C <- B %*% solve(B_transposta %*% B) %*% B_transposta

# 3. Resolução das Questões

# Pergunta 1: log10 do valor absoluto do determinante de C
det_C <- det(C)
log_det_C <- log10(abs(det_C))
cat("1. log10 do valor absoluto do determinante de C:", round(log_det_C, 4), "\n")

# Pergunta 2: log10 do valor absoluto do determinante do produto matricial A ⋅ B
prod_AB <- A %*% B
det_prod_AB <- det(prod_AB)
log_det_prod_AB <- log10(abs(det_prod_AB))
cat("2. log10 do valor absoluto do determinante de A ⋅ B:", round(log_det_prod_AB, 4), "\n")

# Pergunta 3: Soma dos valores absolutos da diagonal de B
diagonal_B <- diag(B)
soma_abs_diagonal_B <- sum(abs(diagonal_B))
cat("3. Soma dos valores absolutos da diagonal de B:", round(soma_abs_diagonal_B, 4), "\n")

# Pergunta 4: Soma dos elementos acima da diagonal de A
elementos_acima_diagonal_A <- A[upper.tri(A)]
soma_acima_diagonal_A <- sum(elementos_acima_diagonal_A)
cat("4. Soma dos elementos acima da diagonal de A:", round(soma_acima_diagonal_A, 4), "\n")

# Pergunta 5: Maior elemento da diagonal do inverso de A ⋅ Bᵀ
prod_A_Btransp <- A %*% t(B)
inversa_prod_A_Btransp <- solve(prod_A_Btransp)
diagonal_inversa <- diag(inversa_prod_A_Btransp)
maior_elemento_diagonal <- max(diagonal_inversa)
cat("5. Maior elemento da diagonal da inversa de A ⋅ Bᵀ:", round(maior_elemento_diagonal, 4), "\n")

# 4. Exibição das Matrizes para Verificação
cat("\n--- Matriz A ---\n")
print(A)
cat("\n--- Matriz B ---\n")
print(B)

cat("\n--- RESPOSTAS FINAIS ---\n")
cat("Resposta 1:", round(log_det_C, 4), "\n")
cat("Resposta 2:", round(log_det_prod_AB, 4), "\n")
cat("Resposta 3:", round(soma_abs_diagonal_B, 4), "\n")
cat("Resposta 4:", round(soma_acima_diagonal_A, 4), "\n")
cat("Resposta 5:", round(maior_elemento_diagonal, 4), "\n")

#---------------------------------------------------------------------------
# --- Script para Análise do Banco 'airquality' (Questão 7) ---
# Adaptado a partir do script base fornecido.

# Carregar o banco de dados airquality
data(airquality)

# 1. Cálculo para toda a variável Ozone

# É crucial remover os valores ausentes (NA) antes dos cálculos.
ozone_completo <- airquality$Ozone[!is.na(airquality$Ozone)]

# Pergunta 1: S² da variável Ozone
# A função var() já usa n-1 no denominador.
S2_ozone <- var(ozone_completo)

# Pergunta 2: DMA da variável Ozone (com n-1)
media_ozone <- mean(ozone_completo)
n_total <- length(ozone_completo) # Usar o comprimento do vetor sem NAs
DMA_ozone <- (1 / (n_total - 1)) * sum(abs(ozone_completo - media_ozone))

# 2. Cálculo apenas para o nível 7 da variável Month
dados_mes7 <- subset(airquality, Month == 7)

# Novamente, remover os NAs da variável Ozone para este subconjunto
ozone_mes7 <- dados_mes7$Ozone[!is.na(dados_mes7$Ozone)]

# Pergunta 3: AS2 no nível 7 da variável Month para a variável Ozone
media_ozone_mes7 <- mean(ozone_mes7)
mediana_ozone_mes7 <- median(ozone_mes7)
desvio_padrao_ozone_mes7 <- sd(ozone_mes7)
AS2_ozone_mes7 <- 3 * (media_ozone_mes7 - mediana_ozone_mes7) / desvio_padrao_ozone_mes7

# Pergunta 4: DMA no nível 7 da variável Month para a variável Ozone (com n-1)
n_mes7 <- length(ozone_mes7) # Usar o comprimento do vetor sem NAs
DMA_ozone_mes7 <- (1 / (n_mes7 - 1)) * sum(abs(ozone_mes7 - media_ozone_mes7))

# 3. Exibição das Respostas Finais
cat("--- RESPOSTAS FINAIS ---\n")
cat("Resposta 1 Questão 7:", round(S2_ozone, 4), "\n")
cat("Resposta 2 Questão 7:", round(DMA_ozone, 4), "\n")
cat("Resposta 3 Questão 7:", round(AS2_ozone_mes7, 4), "\n")
cat("Resposta 4 Questão 7:", round(DMA_ozone_mes7, 4), "\n")

#---------------------------------------------------------------------------------------
# --- Script para Análise do Banco 'CO2' (Questão 8) ---

# 1. Carregar o banco de dados (já vem com o R)
data(CO2)

# --- Análises para toda a variável uptake ---

# Pergunta 1: S² da variável uptake
# A função var() do R já calcula a variância amostral (com n-1)
S2_total <- var(CO2$uptake)

# Pergunta 2: DMA da variável uptake (usando a fórmula com n-1)
media_total <- mean(CO2$uptake)
n_total <- nrow(CO2)
DMA_total <- (1 / (n_total - 1)) * sum(abs(CO2$uptake - media_total))


# --- Análises para o nível 'chilled' da variável Treatment ---

# Filtrar o banco de dados para Treatment == "chilled"
dados_chilled <- subset(CO2, Treatment == "chilled")
conc_chilled <- dados_chilled$conc # A variável de interesse aqui é 'conc'

# Pergunta 3: AS2 no nível 'chilled' da variável Treatment para a variável 'conc'
media_chilled <- mean(conc_chilled)
mediana_chilled <- median(conc_chilled)
desvio_padrao_chilled <- sd(conc_chilled)
AS2_chilled <- 3 * (media_chilled - mediana_chilled) / desvio_padrao_chilled

# Pergunta 4: DMA no nível 'chilled' da variável Treatment para a variável 'conc'
n_chilled <- nrow(dados_chilled)
DMA_chilled <- (1 / (n_chilled - 1)) * sum(abs(conc_chilled - media_chilled))


# --- Exibição dos Resultados Formatados ---
cat("--- Respostas Finais (Questão 8) ---\n\n")

cat(
  "A S² da variável uptake é:",
  sprintf("%.4f", S2_total),
  "\n"
)

cat(
  "O DMA da variável uptake é:",
  sprintf("%.4f", DMA_total),
  "\n"
)

cat(
  "No nível chilled da variável Treatment, o AS² da variável conc é:",
  sprintf("%.4f", AS2_chilled),
  "\n"
)

cat(
  "Considerando apenas o nível chilled da variável Treatment, o DMA da variável conc é:",
  sprintf("%.4f", DMA_chilled),
  "\n"
)

#-------------------------------------------------------------------------------------------------------------
# --- Script para Análise de Notas de Alunos (Questão 1) ---

# 1. Definição dos vetores de dados
Notas <- c(
  João = 81, Maria = 35, José = 95, Ana = 36, Pedro = 73,
  Paula = 37, Carlos = 60, Mariana = 96, Fernando = 36, Luiza = 95
)

Trabalhos <- c(
  João = TRUE, Maria = FALSE, José = TRUE, Ana = FALSE, Pedro = TRUE,
  Paula = FALSE, Carlos = FALSE, Mariana = TRUE, Fernando = FALSE, Luiza = TRUE
)

# 2. Resolução das Questões

# Pergunta 1: A raiz quadrada da média das notas dos alunos é:
raiz_media_total <- sqrt(mean(Notas))

# Pergunta 2: A mediana dos alunos que entregaram o trabalho é:
notas_entregues <- Notas[Trabalhos]
mediana_entregues <- median(notas_entregues)

# Pergunta 3: A maior nota dentre os alunos que não entregaram o trabalho é:
notas_nao_entregues <- Notas[!Trabalhos]
maior_nota_nao_entregues <- max(notas_nao_entregues)

# Pergunta 4: A quantidade de alunos em recuperação que entregou os trabalhos é:
# Condição: Nota entre 40 (inclusivo) e 70 (exclusivo) E trabalho entregue
alunos_recuperacao_entregou <- sum(Notas >= 40 & Notas < 70 & Trabalhos)

# Pergunta 5: O desvio-padrão das notas dos alunos que entregaram os trabalhos é:
desvio_padrao_entregues <- sd(notas_entregues)

# 3. Exibição dos Resultados Formatados
cat("--- Respostas Finais (Questão 1) ---\n\n")

cat(
  "A raiz quadrada da média das notas dos alunos é:",
  sprintf("%.4f", raiz_media_total),
  "\n"
)

cat(
  "A mediana dos alunos que entregaram o trabalho é:",
  sprintf("%.4f", mediana_entregues),
  "\n"
)

cat(
  "A maior nota dentre os alunos que não entregaram o trabalho é:",
  sprintf("%.4f", maior_nota_nao_entregues),
  "\n"
)

cat(
  "A quantidade de alunos em recuperação que entregou os trabalhos é:",
  alunos_recuperacao_entregou,
  "\n"
)

cat(
  "O desvio-padrão das notas dos alunos que entregaram os trabalhos é:",
  sprintf("%.4f", desvio_padrao_entregues),
  "\n"
)
#---------------------------------------------------------------------------
# --- Script para Análise de Notas de Alunos (Questão 2) ---

# 1. Definição dos vetores de dados
Notas <- c(
  João = 42, Maria = 85, José = 63, Ana = 70, Pedro = 65,
  Paula = 85, Carlos = 36, Mariana = 89, Fernando = 45, Luiza = 31
)

Trabalhos <- c(
  João = FALSE, Maria = TRUE, José = FALSE, Ana = TRUE, Pedro = FALSE,
  Paula = TRUE, Carlos = FALSE, Mariana = TRUE, Fernando = FALSE, Luiza = FALSE
)

# 2. Resolução das Questões

# Pergunta 1: A raiz quadrada da média das notas dos alunos é:
raiz_media_total <- sqrt(mean(Notas))

# Pergunta 2: A mediana dos alunos que entregaram o trabalho é:
notas_entregues <- Notas[Trabalhos]
mediana_entregues <- median(notas_entregues)

# Pergunta 3: A maior nota dentre os alunos que não entregaram o trabalho é:
notas_nao_entregues <- Notas[!Trabalhos]
maior_nota_nao_entregues <- max(notas_nao_entregues)

# Pergunta 4: A quantidade de alunos em recuperação que entregou os trabalhos é:
# Condição: Nota entre 40 (inclusivo) e 70 (exclusivo) E trabalho entregue
alunos_recuperacao_entregou <- sum(Notas >= 40 & Notas < 70 & Trabalhos)

# Pergunta 5: O desvio-padrão das notas dos alunos que entregaram os trabalhos é:
desvio_padrao_entregues <- sd(notas_entregues)

# 3. Exibição dos Resultados Formatados
cat("--- Respostas Finais (Questão 2) ---\n\n")

cat(
  "A raiz quadrada da média das notas dos alunos é:",
  sprintf("%.4f", raiz_media_total),
  "\n"
)

cat(
  "A mediana dos alunos que entregaram o trabalho é:",
  sprintf("%.4f", mediana_entregues),
  "\n"
)

cat(
  "A maior nota dentre os alunos que não entregaram o trabalho é:",
  sprintf("%.4f", maior_nota_nao_entregues),
  "\n"
)

cat(
  "A quantidade de alunos em recuperação que entregou os trabalhos é:",
  alunos_recuperacao_entregou,
  "\n"
)

cat(
  "O desvio-padrão das notas dos alunos que entregaram os trabalhos é:",
  sprintf("%.4f", desvio_padrao_entregues),
  "\n"
)
#-------------------------------------------------------------------------------------------
# --- Script para Análise de Notas de Alunos (Questão 3) ---

# 1. Definição dos vetores de dados
Notas <- c(
  João = 71, Maria = 52, José = 70, Ana = 70, Pedro = 72,
  Paula = 80, Carlos = 87, Mariana = 38, Fernando = 98, Luiza = 55
)

Trabalhos <- c(
  João = TRUE, Maria = FALSE, José = TRUE, Ana = TRUE, Pedro = TRUE,
  Paula = TRUE, Carlos = TRUE, Mariana = FALSE, Fernando = TRUE, Luiza = FALSE
)

# 2. Resolução das Questões

# Pergunta 1: A raiz quadrada da média das notas dos alunos é:
raiz_media_total <- sqrt(mean(Notas))

# Pergunta 2: A mediana dos alunos que entregaram o trabalho é:
notas_entregues <- Notas[Trabalhos]
mediana_entregues <- median(notas_entregues)

# Pergunta 3: A maior nota dentre os alunos que não entregaram o trabalho é:
notas_nao_entregues <- Notas[!Trabalhos]
maior_nota_nao_entregues <- max(notas_nao_entregues)

# Pergunta 4: A quantidade de alunos em recuperação que entregou os trabalhos é:
# Condição: Nota entre 40 (inclusivo) e 70 (exclusivo) E trabalho entregue
alunos_recuperacao_entregou <- sum(Notas >= 40 & Notas < 70 & Trabalhos)

# Pergunta 5: O desvio-padrão das notas dos alunos que entregaram os trabalhos é:
desvio_padrao_entregues <- sd(notas_entregues)

# 3. Exibição dos Resultados Formatados
cat("--- Respostas Finais (Questão 3) ---\n\n")

cat(
  "A raiz quadrada da média das notas dos alunos é:",
  sprintf("%.4f", raiz_media_total),
  "\n"
)

cat(
  "A mediana dos alunos que entregaram o trabalho é:",
  sprintf("%.4f", mediana_entregues),
  "\n"
)

cat(
  "A maior nota dentre os alunos que não entregaram o trabalho é:",
  sprintf("%.4f", maior_nota_nao_entregues),
  "\n"
)

cat(
  "A quantidade de alunos em recuperação que entregou os trabalhos é:",
  alunos_recuperacao_entregou,
  "\n"
)

cat(
  "O desvio-padrão das notas dos alunos que entregaram os trabalhos é:",
  sprintf("%.4f", desvio_padrao_entregues),
  "\n"
)

