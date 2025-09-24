#PROVA 2
#QUESTÃO 1 -------------------------------------------------------------------
# Dados
Notas <- c(João = 81, Maria = 35, José = 95, Ana = 36, Pedro = 73, 
           Paula = 37, Carlos = 60, Mariana = 96, Fernando = 36, Luiza = 95)

Trabalhos <- c(João = TRUE, Maria = FALSE, José = TRUE, Ana = FALSE, Pedro = TRUE, 
               Paula = FALSE, Carlos = FALSE, Mariana = TRUE, Fernando = FALSE, Luiza = TRUE)

# 1. Raiz quadrada da média de todas as notas
media_geral <- mean(Notas)
resp1 <- sqrt(media_geral)

# 2. Mediana dos alunos que entregaram o trabalho
notas_entregaram <- Notas[Trabalhos]
resp2 <- median(notas_entregaram)

# 3. Maior nota entre os que não entregaram
notas_nao_entregaram <- Notas[!Trabalhos]
resp3 <- max(notas_nao_entregaram)

# 4. Quantidade de alunos em recuperação que entregaram trabalhos
# Recuperação: 40 <= nota < 70
recuperacao_entregaram <- sum(notas_entregaram >= 40 & notas_entregaram < 70)
resp4 <- recuperacao_entregaram

# 5. Desvio-padrão amostral das notas dos que entregaram
resp5 <- sd(notas_entregaram)

# Arredondar para 4 casas decimais
resp1 <- round(resp1, 4)
resp2 <- round(resp2, 4)
resp3 <- round(resp3, 4)
resp5 <- round(resp5, 4)

# Exibir resultados
cat("Resposta 1:", resp1, "\n")
cat("Resposta 2:", resp2, "\n")
cat("Resposta 3:", resp3, "\n")
cat("Resposta 4:", resp4, "\n")
cat("Resposta 5:", resp5, "\n")

#QUESTAO 2 ------------------------------------------------------------
# Dados
Notas <- c(João = 42, Maria = 85, José = 63, Ana = 70, Pedro = 65, 
           Paula = 85, Carlos = 36, Mariana = 89, Fernando = 45, Luiza = 31)

Trabalhos <- c(João = FALSE, Maria = TRUE, José = FALSE, Ana = TRUE, Pedro = FALSE, 
               Paula = TRUE, Carlos = FALSE, Mariana = TRUE, Fernando = FALSE, Luiza = FALSE)

# 1. Raiz quadrada da média de todas as notas
media_geral <- mean(Notas)
resp1 <- sqrt(media_geral)

# 2. Mediana dos alunos que entregaram o trabalho
notas_entregaram <- Notas[Trabalhos]
resp2 <- median(notas_entregaram)

# 3. Maior nota entre os que não entregaram
notas_nao_entregaram <- Notas[!Trabalhos]
resp3 <- max(notas_nao_entregaram)

# 4. Quantidade de alunos em recuperação que entregaram trabalhos
# Recuperação: 40 <= nota < 70
recuperacao_entregaram <- sum(notas_entregaram >= 40 & notas_entregaram < 70)
resp4 <- recuperacao_entregaram

# 5. Desvio-padrão amostral das notas dos que entregaram
resp5 <- sd(notas_entregaram)

# Arredondar para 4 casas decimais
resp1 <- round(resp1, 4)
resp2 <- round(resp2, 4)
resp3 <- round(resp3, 4)
resp5 <- round(resp5, 4)

# Exibir resultados
cat("Resposta 1:", resp1, "\n")
cat("Resposta 2:", resp2, "\n")
cat("Resposta 3:", resp3, "\n")
cat("Resposta 4:", resp4, "\n")
cat("Resposta 5:", resp5, "\n")

#QUESTAO 3 -----------------------------------------------------------------------------------
# Dados
Notas2 <- c(João = 71, Maria = 52, José = 70, Ana = 70, Pedro = 72, 
           Paula = 80, Carlos = 87, Mariana = 38, Fernando = 98, Luiza = 55)

Trabalhos2 <- c(João = TRUE, Maria = FALSE, José = TRUE, Ana = TRUE, Pedro = TRUE, 
               Paula = TRUE, Carlos = TRUE, Mariana = FALSE, Fernando = TRUE, Luiza = FALSE)

# 1. Raiz quadrada da média de todas as notas
media_geral2 <- mean(Notas)
resp1 <- sqrt(media_geral)

# 2. Mediana dos alunos que entregaram o trabalho
notas_entregaram <- Notas[Trabalhos]
resp2 <- median(notas_entregaram)

# 3. Maior nota entre os que não entregaram
notas_nao_entregaram <- Notas[!Trabalhos]
resp3 <- max(notas_nao_entregaram)

# 4. Quantidade de alunos em recuperação que entregaram trabalhos
# Recuperação: 40 <= nota < 70
recuperacao_entregaram <- sum(notas_entregaram >= 40 & notas_entregaram < 70)
resp4 <- recuperacao_entregaram

# 5. Desvio-padrão amostral das notas dos que entregaram
resp5 <- sd(notas_entregaram)

# Arredondar para 4 casas decimais
resp1 <- round(resp1, 4)
resp2 <- round(resp2, 4)
resp3 <- round(resp3, 4)
resp5 <- round(resp5, 4)

# Exibir resultados
cat("Resposta 1:", resp1, "\n")
cat("Resposta 2:", resp2, "\n")
cat("Resposta 3:", resp3, "\n")
cat("Resposta 4:", resp4, "\n")
cat("Resposta 5:", resp5, "\n")

#QUESTÕES 4 -------------------------------------------------------------------------
# Definição das matrizes A e B
# Os elementos são listados por coluna (padrão do R)
A <- matrix(c(
  14, 29, 22, 25, 30,
  49, 39, 41, 18, 21,
  15, 38, 18, 2, 8,
  3, 15, 25, 20, 39,
  25, 39, 12, 37, 1
), nrow = 5, ncol = 5, byrow = TRUE)

B <- matrix(c(
  17, 2, 12, 20, 23,
  29, 37, 24, 5, 33,
  3, 3, 29, 26, 20,
  29, 19, 32, 12, 41,
  0, 14, 48, 5, 50
), nrow = 5, ncol = 5, byrow = TRUE)

# --- Questão 1: O log10 do valor absoluto do determinante de C ---
# C = B %*% solve(t(B) %*% B) %*% t(B)
# Como B é uma matriz quadrada e de posto completo, C é a matriz identidade.
# O determinante da matriz identidade é 1, e log10(1) = 0.
# A linha abaixo calcula C para demonstrar, mas o resultado pode ter imprecisões numéricas.
C <- B %*% solve(t(B) %*% B) %*% t(B)
det_C <- det(C)
# O valor será muito próximo de 1, resultando em um log10 muito próximo de 0.
log_det_C <- log10(abs(det_C))


# --- Questão 2: O log10 do valor absoluto do determinante de (A * B) ---
produto_AB <- A %*% B
det_AB <- det(produto_AB)
log_det_AB <- log10(abs(det_AB))


# --- Questão 3: A soma dos valores absolutos da diagonal da matriz B ---
soma_diag_B <- sum(abs(diag(B)))


# --- Questão 4: A soma dos elementos que estão acima da diagonal de A ---
# A função upper.tri(A) cria uma matriz lógica.
# A[upper.tri(A)] extrai os elementos da parte triangular superior.
soma_acima_diag_A <-sum (A[upper.tri(A)])




# --- Questão 5: O maior elemento da diagonal do inverso de (A * B^T) ---
produto_A_Bt <- A %*% t(B)
inverso_A_Bt <- solve(produto_A_Bt)
maior_elem_diag_inverso <- max(diag(inverso_A_Bt))


# --- Impressão dos resultados formatados ---
cat("Respostas para a Questão 4:\n")
cat("--------------------------------\n")

cat(
  "O log10 do valor absoluto do determinante de C é:",
  sprintf("%.4f", log_det_C),
  "\n"
)

cat(
  "O log10 do valor absoluto do determinante do produto matricial entre A e B é:",
  sprintf("%.4f", log_det_AB),
  "\n"
)

cat(
  "A soma dos valores absolutos da diagonal da matriz B é:",
  sprintf("%.4f", soma_diag_B),
  "\n"
)

cat(
  "A soma dos elementos que estão acima da diagonal de A é:",
  sprintf("%.4f", soma_acima_diag_A),
  "\n"
)

cat(
  "O maior elemento da diagonal do inverso do produto matricial entre A e o transposto de B é:",
  sprintf("%.4f", maior_elem_diag_inverso),
  "\n"
)

#QUESTÃO 5 -----------------------------------------------------------
# Definição das matrizes A e B da Questão 5
# Os elementos são listados por coluna (padrão do R)
A <- matrix(c(
  26, 17, 21, 22, 16,
  16, 13, 12, 8, 40,
  5, 0, 40, 6, 40,
  43, 35, 29, 10, 0,
  11, 32, 6, 5, 15
), nrow = 5, ncol = 5 , byrow = TRUE)

B <- matrix(c(
  27, 15, 20, 26, 27,
  11, 48, 5, 20, 34,
  0, 40, 24, 39, 6,
  30, 23, 35, 35, 18,
  48, 24, 21, 25, 38
), nrow = 5, ncol = 5 , byrow = TRUE)

# --- Questão 1: O log10 do valor absoluto do determinante de C ---
# C = B %*% solve(t(B) %*% B) %*% t(B)
# Como B é uma matriz quadrada e de posto completo, C é a matriz identidade.
# O determinante da matriz identidade é 1, e log10(1) = 0.
# A linha abaixo calcula C para demonstrar.
C <- B %*% solve(t(B) %*% B) %*% t(B)
det_C <- det(C)
log_det_C <- log10(abs(det_C))


# --- Questão 2: O log10 do valor absoluto do determinante de (A * B) ---
produto_AB <- A %*% B
det_AB <- det(produto_AB)
log_det_AB <- log10(abs(det_AB))


# --- Questão 3: A soma dos valores absolutos da diagonal da matriz B ---
soma_diag_B <- sum(abs(diag(B)))


# --- Questão 4: A soma dos elementos que estão acima da diagonal de A ---
soma_acima_diag_A <- sum(A[upper.tri(A)])


# --- Questão 5: O maior elemento da diagonal do inverso de (A * B^T) ---
produto_A_Bt <- A %*% t(B)
inverso_A_Bt <- solve(produto_A_Bt)
maior_elem_diag_inverso <- max(diag(inverso_A_Bt))


# --- Impressão dos resultados formatados ---
cat("Respostas para a Questão 5:\n")
cat("--------------------------------\n")

cat(
  "O log10 do valor absoluto do determinante de C é:",
  sprintf("%.4f", log_det_C),
  "\n"
)

cat(
  "O log10 do valor absoluto do determinante do produto matricial entre A e B é:",
  sprintf("%.4f", log_det_AB),
  "\n"
)

cat(
  "A soma dos valores absolutos da diagonal da matriz B é:",
  sprintf("%.4f", soma_diag_B),
  "\n"
)

cat(
  "A soma dos elementos que estão acima da diagonal de A é:",
  sprintf("%.4f", soma_acima_diag_A),
  "\n"
)

cat(
  "O maior elemento da diagonal do inverso do produto matricial entre A e o transposto de B é:",
  sprintf("%.4f", maior_elem_diag_inverso),
  "\n"
)

#QUESTÃO 6 --------------------------------------------------------------------------------------------
# Definição das matrizes A e B da Questão 6
# Os elementos são listados por coluna (padrão do R)
A <- matrix(c(
  0, 7, 31, 21, 38,
  0, 50, 27, 13, 23,
  16, 18, 2, 39, 39,
  6, 24, 17, 26, 9,
  26, 44, 9, 11, 23
), nrow = 5, ncol = 5, byrow = TRUE)

B <- matrix(c(
  44, 41, 32, 4, 31,
  27, 5, 19, 41, 21,
  30, 27, 24, 35, 44,
  33, 41, 37, 0, 10,
  34, 11, 44, 21, 49
), nrow = 5, ncol = 5 , byrow = TRUE)

# --- Questão 1: O log10 do valor absoluto do determinante de C ---
# C = B %*% solve(t(B) %*% B) %*% t(B)
# Como B é uma matriz quadrada e de posto completo, C é a matriz identidade.
# O determinante da matriz identidade é 1, e log10(1) = 0.
# A linha abaixo calcula C para demonstrar.
C <- B %*% solve(t(B) %*% B) %*% t(B)
det_C <- det(C)
log_det_C <- log10(abs(det_C))


# --- Questão 2: O log10 do valor absoluto do determinante de (A * B) ---
produto_AB <- A %*% B
det_AB <- det(produto_AB)
log_det_AB <- log10(abs(det_AB))


# --- Questão 3: A soma dos valores absolutos da diagonal da matriz B ---
soma_diag_B <- sum(abs(diag(B)))


# --- Questão 4: A soma dos elementos que estão acima da diagonal de A ---
soma_acima_diag_A <- sum(A[upper.tri(A)])


# --- Questão 5: O maior elemento da diagonal do inverso de (A * B^T) ---
produto_A_Bt <- A %*% t(B)
inverso_A_Bt <- solve(produto_A_Bt)

maior_elem_diag_inverso <- max(diag(inverso_A_Bt))


# --- Impressão dos resultados formatados ---
cat("Respostas para a Questão 6:\n")
cat("--------------------------------\n")

cat(
  "O log10 do valor absoluto do determinante de C é:",
  sprintf("%.4f", log_det_C),
  "\n"
)

cat(
  "O log10 do valor absoluto do determinante do produto matricial entre A e B é:",
  sprintf("%.4f", log_det_AB),
  "\n"
)

cat(
  "A soma dos valores absolutos da diagonal da matriz B é:",
  sprintf("%.4f", soma_diag_B),
  "\n"
)

cat(
  "A soma dos elementos que estão acima da diagonal de A é:",
  sprintf("%.4f", soma_acima_diag_A),
  "\n"
)

cat(
  "O maior elemento da diagonal do inverso do produto matricial entre A e o transposto de B é:",
  sprintf("%.4f", maior_elem_diag_inverso),
  "\n"
)

