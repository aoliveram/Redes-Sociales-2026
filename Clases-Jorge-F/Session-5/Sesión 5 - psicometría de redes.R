################################################################################
# Doctorado en Ciencias de la Complejidad Social
# Teoria de redes 
# Jorge Fábrega
# Sesión 5 - PSICOMETRÍA DE REDES Y REDES DE VARIABLES
# ==============================================================================
# 0. PREPARACIÓN
# ==============================================================================

rm(list = ls())

set.seed(1234)

library(qgraph)
library(psych)
library(bootnet)
library(EGAnet)
library(IsingFit)

# ==============================================================================
# MÓDULO 1
# DE CORRELACIONES A DEPENDENCIAS CONDICIONALES
# ==============================================================================

# ___________________________________//_________________________________________
# ___________________________________//_________________________________________

# ------------------------------------------------------------------------------
# 1. UNA RED DE VARIABLES
# ------------------------------------------------------------------------------

# Hasta ahora podemos haber pensado una red como:
#
# nodo = actor
# arista = relación entre actores
#
# En esta sesión:
#
# nodo = variable
# arista = asociación condicional entre variables
#
# Vamos a trabajar inicialmente con cinco síntomas:
#
# Ansiedad
# Preocupacion
# Insomnio
# Fatiga
# Irritabilidad


# ------------------------------------------------------------------------------
# 2. PRIMER PROBLEMA: CORRELACIÓN NO IMPLICA UNA RELACIÓN PROPIA ENTRE DOS NODOS
# ------------------------------------------------------------------------------
# Construimos un ejemplo muy pequeño con tres variables.
# La estructura generadora es:
# A  --->  B  --->  C
# A afecta estadísticamente a B, y B a C.
# Por esta razón, A y C también estarán correlacionadas.

n <- 1000

A <- rnorm(n)

B <- 0.8 * A + rnorm(n, sd = 0.6)

C <- 0.8 * B + rnorm(n, sd = 0.6)

datos_ABC <- data.frame(A, B, C)

# Correlaciones simples
round(cor(datos_ABC), 3)


# ------------------------------------------------------------------------------
# 3. LA CORRELACIÓN ENTRE A Y C
# ------------------------------------------------------------------------------

cor(datos_ABC$A, datos_ABC$C)

# La correlación es positiva.
# Si construyéramos una red usando únicamente correlaciones, dibujaríamos:
# A ----- C
# La pregunta relevante es:
# ¿A y C siguen asociados una vez que conocemos B?

# ------------------------------------------------------------------------------
# 4. CORRELACIÓN PARCIAL: CONTROLAR POR B
# ------------------------------------------------------------------------------

# Una forma intuitiva de calcular una correlación parcial:
# 1. explicar A usando B;
# 2. guardar aquello de A que B no explica;
# 3. explicar C usando B;
# 4. guardar aquello de C que B no explica;
# 5. correlacionar ambos residuos.

modelo_A_B <- lm(A ~ B, data = datos_ABC)

modelo_C_B <- lm(C ~ B, data = datos_ABC)

residuo_A <- residuals(modelo_A_B)

residuo_C <- residuals(modelo_C_B)

cor(residuo_A, residuo_C)

# La asociación A-C cae fuertemente.
# Esta es la intuición básica de una dependencia condicional:
# ¿qué relación queda entre dos variables después de considerar las demás?


# ------------------------------------------------------------------------------
# 5. LA MISMA IDEA CON psych::partial.r
# ------------------------------------------------------------------------------

R_ABC <- cor(datos_ABC)
R_ABC

# Correlación parcial A-C controlando B:
psych::partial.r(
  data = R_ABC,
  x = c(1, 3),
  y = 2
)

# ___________________________________//_________________________________________
# ___________________________________//_________________________________________

# ------------------------------------------------------------------------------
# 6. DE TRES VARIABLES A UN SISTEMA
# ------------------------------------------------------------------------------
# Ahora simulamos datos más parecidos a una aplicación psicométrica.
# Generamos aleatoriamente síntomas y los asociamos a dos dimensiones latentes:
#
# 1. ansiedad
# 2. depresión
#
# Insomnio tendrá relación con ambas dimensiones y funcionará como un puente.

n <- 500

factor_ansiedad <- rnorm(n)
factor_depresion <- rnorm(n)

data_continua <- data.frame(
  Anhedonia = 0.75 * factor_depresion +
    rnorm(n, sd = 0.45),

  Insomnio = 0.55 * factor_depresion +
    0.35 * factor_ansiedad +
    rnorm(n, sd = 0.45),

  Fatiga = 0.80 * factor_depresion +
    rnorm(n, sd = 0.45),

  Irritabilidad = 0.65 * factor_ansiedad +
    rnorm(n, sd = 0.45),

  Ansiedad = 0.90 * factor_ansiedad +
    rnorm(n, sd = 0.40),

  Preocupacion = 0.80 * factor_ansiedad +
    rnorm(n, sd = 0.40),

  Nerviosismo = 0.85 * factor_ansiedad +
    rnorm(n, sd = 0.40),

  Inquietud = 0.75 * factor_ansiedad +
    rnorm(n, sd = 0.45)
)

head(data_continua)
dim(data_continua)


# ------------------------------------------------------------------------------
# 7. NORMALIZAMOS PARA FACILITAR LA COMPARACIÓN
# ------------------------------------------------------------------------------

data_continua_z <- as.data.frame(scale(data_continua))

# media 0
round(
  apply(data_continua_z, 2, mean), # 2 es columna, 1 es fila
  3
)

# varianza 1
round(
  apply(data_continua_z, 2, sd),
  3
)


# ------------------------------------------------------------------------------
# 8. PRIMERA RED: CORRELACIONES SIMPLES
# ------------------------------------------------------------------------------

R <- cor(data_continua_z)

round(R, 2)

qgraph(
  R,
  layout = "spring",
  labels = colnames(data_continua_z),
  minimum = 0.10,
  cut = 0,
  vsize = 7,
  legend = FALSE,
  title = "Red de correlaciones"
)

# Esta red representa asociaciones marginales.
# Una conexión puede reflejar:
# - una relación propia entre dos variables;
# - una asociación transmitida por otras variables;
# - una causa común;
# - combinaciones de los mecanismos anteriores.


# ------------------------------------------------------------------------------
# 9. MATRIZ DE PRECISIÓN
# ------------------------------------------------------------------------------

# En un modelo Gaussiano, una forma de recuperar la estructura condicional
# es invertir la matriz de correlaciones.

Omega <- solve(R) # Omega = R^{-1}

round(Omega, 2)

# ------------------------------------------------------------------------------
# 10. DE LA MATRIZ DE PRECISIÓN A CORRELACIONES PARCIALES
# ------------------------------------------------------------------------------
# La correlación parcial entre i y j puede calcularse como:
# rho_ij|resto =
#
#            - omega_ij
# --------------------------------
# sqrt(omega_ii * omega_jj)

p <- ncol(Omega)

P <- matrix(
  0,
  nrow = p,
  ncol = p
)

rownames(P) <- colnames(data_continua_z)
colnames(P) <- colnames(data_continua_z)

for (i in 1:p) {

  for (j in 1:p) {

    if (i == j) {

      P[i, j] <- 1

    } else {

      P[i, j] <-
        -Omega[i, j] /
        sqrt(Omega[i, i] * Omega[j, j])

    }
  }
}

round(P, 2)


# ------------------------------------------------------------------------------
# 11. RED DE DEPENDENCIAS CONDICIONALES
# ------------------------------------------------------------------------------

qgraph(
  P,
  layout = "spring",
  labels = colnames(data_continua_z),
  minimum = 0.05,
  cut = 0,
  vsize = 7,
  legend = FALSE,
  title = "Red de correlaciones parciales"
)

# Ahora cada arista se interpreta como una asociación entre dos variables
# después de considerar simultáneamente las demás variables del sistema.

# ------------------------------------------------------------------------------
# 12. COMPARAR CORRELACIÓN SIMPLE Y CORRELACIÓN PARCIAL
# ------------------------------------------------------------------------------
# Ejemplo: Ansiedad y Fatiga

R["Ansiedad", "Fatiga"]

P["Ansiedad", "Fatiga"]

# Ejemplo: Insomnio y Fatiga

R["Insomnio", "Fatiga"]

P["Insomnio", "Fatiga"]

# Algunas relaciones se reducen mucho al condicionar.
# Otras conservan una asociación sustantiva.


# ------------------------------------------------------------------------------
# 13. INTERPRETACIÓN COMO PAIRWISE MARKOV RANDOM FIELD
# ------------------------------------------------------------------------------
# En un Pairwise Markov Random Field:
#
# ausencia de arista i-j equivale a Xi independiente de Xj, 
# condicional a todas las demás variables.
#
# En la práctica, con datos muestrales, los valores rara vez son exactamente 0.
# Más adelante veremos cómo la regularización permite estimar redes sparse.


# ___________________________________//_________________________________________
# ___________________________________//_________________________________________

# ------------------------------------------------------------------------------
# 14. VARIABLES BINARIAS: PREPARAMOS UN EJEMPLO
# ------------------------------------------------------------------------------

# El modelo de Ising se aplica a variables binarias.
#
# Convertiremos cada síntoma continuo en:
#
# 0 = valor bajo
# 1 = valor alto
#
# usando la mediana de cada variable como punto de corte.
#
# Esto es sólo un procedimiento pedagógico para obtener datos binarios.
# En una aplicación real los datos binarios normalmente provendrían
# directamente de respuestas sí/no, presencia/ausencia, etc.

data_binaria <- data.frame(
  lapply(
    data_continua,
    function(x) as.integer(x > median(x))
  )
)

head(data_binaria)

table(data_binaria$Ansiedad)


# ------------------------------------------------------------------------------
# 15. CORRELACIONES ENTRE VARIABLES BINARIAS
# ------------------------------------------------------------------------------

round(
  cor(data_binaria),
  2
)

# Volvemos a encontrar muchas asociaciones.
# Ahora queremos estimar cuáles persisten condicionalmente en un sistema binario.


# ------------------------------------------------------------------------------
# 16. MODELO DE ISING
# ------------------------------------------------------------------------------

# Un modelo de Ising tiene la forma:
#
# P(X = x) =
#
#       1
#      --- exp[
#       Z
#
#          sum_i tau_i x_i
#            +
#          sum_{i<j} omega_ij x_i x_j
#
#      ]
#
# tau_i     = tendencia propia del nodo
# omega_ij  = interacción entre los nodos i y j
#
# omega_ij = 0 implica ausencia de arista.


# ------------------------------------------------------------------------------
# 17. ESTIMAR EL MODELO DE ISING
# ------------------------------------------------------------------------------

ising_fit <- IsingFit::IsingFit(
  data_binaria,
  plot = FALSE
)

# Matriz de interacciones estimadas:
round(
  ising_fit$weiadj,
  2
)


# ------------------------------------------------------------------------------
# 18. VISUALIZAR LA RED DE ISING
# ------------------------------------------------------------------------------

qgraph(
  ising_fit$weiadj,
  layout = "spring",
  labels = colnames(data_binaria),
  vsize = 7,
  legend = FALSE,
  title = "Modelo de Ising"
)

# Las aristas representan interacciones condicionales entre variables binarias.


# ------------------------------------------------------------------------------
# 19. OBSERVAR LOS THRESHOLDS DEL MODELO DE ISING
# ------------------------------------------------------------------------------

# Dependiendo de la versión instalada de IsingFit, los thresholds
# aparecen en el objeto estimado.

ising_fit$thresholds

# El threshold captura la propensión propia de cada nodo hacia uno de sus estados.

exp(ising_fit$thresholds[1])/(1+exp(ising_fit$thresholds[1])) # anhedonia

exp(ising_fit$thresholds[5])/(1+exp(ising_fit$thresholds[5])) # ansiedad


# ------------------------------------------------------------------------------
# 20. UNA MIRADA LOCAL AL MODELO DE ISING
# ------------------------------------------------------------------------------

# Para entender la relación con una regresión logística,
# estimemos la probabilidad de Ansiedad alta usando el resto de las variables.

modelo_logit_ansiedad <- glm(
  Ansiedad ~
    Anhedonia +
    Insomnio +
    Fatiga +
    Irritabilidad +
    Preocupacion +
    Nerviosismo +
    Inquietud,
  data = data_binaria,
  family = binomial()
)

summary(modelo_logit_ansiedad)

# Regresión logística ordinaria
coef(modelo_logit_ansiedad)

# Coeficientes nodewise antes de simetrizar (LASSO)
colnames(ising_fit$asymm.weights) <- colnames(data_binaria)
rownames(ising_fit$asymm.weights) <- colnames(data_binaria)
round(ising_fit$asymm.weights["Ansiedad", ], 3)

# Red Ising final
round(ising_fit$weiadj["Ansiedad", ], 3)

# al simetrizar en el modelo de Ising combinamos. Por ejemplo: (0.828+0.503)/2
round(
  ising_fit$asymm.weights[
    c("Ansiedad", "Insomnio"),
    c("Ansiedad", "Insomnio")
  ],
  3
)  

# La intuición es:
#
# cada nodo binario puede modelarse localmente como una función logística
# de los demás nodos.
#
# Ising combina estas relaciones locales dentro de un modelo conjunto.

# ___________________________________//_________________________________________
# ___________________________________//_________________________________________

# Ahora volvemos a las variables continuas y abordamos un problema adicional:
# con muchas variables y una muestra finita, aparecen muchas asociaciones
# pequeñas que pueden ser producto del ruido muestral.

# ==============================================================================
# MÓDULO 2
# GGM, REGULARIZACIÓN, EGA, CENTRALIDAD Y ESTABILIDAD
# ==============================================================================


# ------------------------------------------------------------------------------
# 22. GAUSSIAN GRAPHICAL MODEL
# ------------------------------------------------------------------------------

# Para variables continuas aproximadamente gaussianas:
# X ~ N(mu, Sigma)
# y usamos:
# Omega = Sigma^{-1}
#
# Los elementos fuera de la diagonal de Omega contienen la información
# necesaria para obtener correlaciones parciales.


# ------------------------------------------------------------------------------
# 23. ¿CUÁNTAS ARISTAS PODRÍAMOS ESTIMAR?
# ------------------------------------------------------------------------------

p <- ncol(data_continua_z)

p

posibles_aristas <- p * (p - 1) / 2

posibles_aristas

# Con 8 variables existen 28 posibles aristas.
# Con 50 variables:

50 * 49 / 2

# 1225 posibles aristas.
# El número de parámetros crece rápidamente con p.


# ------------------------------------------------------------------------------
# 24. UNA RED DE CORRELACIONES PARCIALES SIN REGULARIZAR
# ------------------------------------------------------------------------------

qgraph(
  P,
  layout = "spring",
  labels = colnames(data_continua_z),
  minimum = 0,
  cut = 0,
  vsize = 7,
  legend = FALSE,
  title = "GGM sin regularización"
)

# Incluso asociaciones pequeñas aparecen como aristas.


# ------------------------------------------------------------------------------
# 25. REGULARIZACIÓN: LA IDEA
# ------------------------------------------------------------------------------

# Queremos equilibrar:
#
# 1. ajuste a los datos;
# 2. parsimonia.
#
# La penalización LASSO incorpora:
# lambda * sum |omega_ij|
#
# y empuja relaciones pequeñas hacia cero.
#
# El resultado es una red sparse.


# ------------------------------------------------------------------------------
# 26. GRAPHICAL LASSO CON qgraph
# ------------------------------------------------------------------------------

# EBICglasso estima una secuencia de soluciones regularizadas
# y selecciona una usando EBIC.
#
# gamma controla cuánto se penaliza la complejidad.
#
# gamma = 0.50 es una elección habitual en aplicaciones psicométricas.

R <- cor(data_continua_z)

# la penalización la controlamos con gamma

red_g50 <- qgraph::EBICglasso(
  S = R,
  n = nrow(data_continua_z),
  gamma = 0.50
)

sum(red_g50[upper.tri(red_g50)] != 0)

round(
  red_g50,
  2
)


# ------------------------------------------------------------------------------
# 27. VISUALIZAR LA RED REGULARIZADA
# ------------------------------------------------------------------------------

qgraph(
  red_g50,
  layout = "spring",
  labels = colnames(data_continua_z),
  vsize = 7,
  legend = FALSE,
  title = "GGM regularizado: EBICglasso, gamma 0.5"
)

# Varias relaciones pequeñas quedan exactamente en cero.


# ------------------------------------------------------------------------------
# 28. ¿QUÉ HACE gamma?
# ------------------------------------------------------------------------------

# Comparemos una selección menos conservadora y otra más conservadora.

red_gamma_0 <- qgraph::EBICglasso(
  S = R,
  n = nrow(data_continua_z),
  gamma = 0
)

red_gamma_05 <- qgraph::EBICglasso(
  S = R,
  n = nrow(data_continua_z),
  gamma = 0.50
)

# Número de aristas diferentes de cero:

sum(red_gamma_0[upper.tri(red_gamma_0)] != 0)

sum(red_gamma_05[upper.tri(red_gamma_05)] != 0)

# gamma mayor exige mayor evidencia para conservar complejidad.


# ------------------------------------------------------------------------------
# 29. ESTIMAR LA MISMA RED CON bootnet
# ------------------------------------------------------------------------------

network_ggm <- bootnet::estimateNetwork(
  data_continua_z,
  default = "EBICglasso",
  corMethod = "cor_auto"
)

network_ggm


# ------------------------------------------------------------------------------
# 30. VISUALIZAR EL OBJETO bootnet
# ------------------------------------------------------------------------------

plot(
  network_ggm,
  layout = "spring"
)

# Qué modelo usar?

?bootnet::estimateNetwork

# ___________________________________//_________________________________________
# ___________________________________//_________________________________________

# ------------------------------------------------------------------------------
# 31. STRENGTH CENTRALITY
# ------------------------------------------------------------------------------

# Para una red ponderada:
#
# Strength_i = sum_j |w_ij|
#
# La calculamos directamente desde la matriz regularizada.

strength_manual <- rowSums(abs(red_gamma_05))

sort(
  strength_manual,
  decreasing = TRUE
)


# ------------------------------------------------------------------------------
# 32. CENTRALIDAD CON qgraph / bootnet
# ------------------------------------------------------------------------------

centralityPlot(
  network_ggm,
  include = "Strength",
  scale = "raw0"
)

# Aquí "central" significa:
# el nodo mantiene asociaciones condicionales relativamente fuertes
# con otras variables de la red.


# ------------------------------------------------------------------------------
# 33. EXPLORATORY GRAPH ANALYSIS
# ------------------------------------------------------------------------------

# Recuperamos ahora explícitamente la lógica del script adjunto.
#
# EGA:
#
# datos
#  ->
# red estimada
#  ->
# detección de comunidades
#  ->
# dimensiones empíricas

ega_correlacion <- EGAnet::EGA(
  data_continua_z
)

ega_correlacion


# ------------------------------------------------------------------------------
# 34. OBSERVAR LA RED ESTIMADA POR EGA
# ------------------------------------------------------------------------------

qgraph(
  ega_correlacion$network,
  layout = "spring",
  labels = colnames(data_continua_z),
  vsize = 7,
  legend = FALSE,
  title = "Exploratory Graph Analysis"
)


# ------------------------------------------------------------------------------
# 35. COMUNIDADES EN EGA
# ------------------------------------------------------------------------------

# comunidades según EGA

ega_correlacion$dim.variables

# La expectativa en los datos simulados es recuperar aproximadamente:
#
# dimensión depresiva:
#   Anhedonia, Insomnio, Fatiga
#
# dimensión de ansiedad:
#   Irritabilidad, Ansiedad, Preocupacion, Nerviosismo, Inquietud
#
# Insomnio puede comportarse como nodo puente porque fue generado
# con componentes de ambos factores, pero no olviden que todos 
# estos son datos ficticios


# ------------------------------------------------------------------------------
# 36. CONTRASTE: DATOS SIN ESTRUCTURA
# ------------------------------------------------------------------------------

# Contrastemos con una red aleatoria sin correlaciones:

data_aleatoria <- data.frame(
  Anhedonia = rnorm(n),
  Insomnio = rnorm(n),
  Fatiga = rnorm(n),
  Irritabilidad = rnorm(n),
  Ansiedad = rnorm(n),
  Preocupacion = rnorm(n),
  Nerviosismo = rnorm(n),
  Inquietud = rnorm(n)
)

ega_aleatorio <- EGAnet::EGA(
  data_aleatoria
)

ega_aleatorio


# ------------------------------------------------------------------------------
# 37. COMPARAR LAS DOS REDES EGA
# ------------------------------------------------------------------------------

qgraph(
  ega_aleatorio$network,
  layout = "spring",
  labels = colnames(data_aleatoria),
  vsize = 7,
  legend = FALSE,
  title = "EGA: datos independientes"
)

qgraph(
  ega_correlacion$network,
  layout = "spring",
  labels = colnames(data_continua_z),
  vsize = 7,
  legend = FALSE,
  title = "EGA: datos con estructura"
)


# ------------------------------------------------------------------------------
# 38. COMPARACIÓN CON ANÁLISIS FACTORIAL
# ------------------------------------------------------------------------------

# Los mismos datos fueron generados originalmente usando dos factores latentes.
#
# Veamos qué encuentra un análisis factorial exploratorio.

fa_parallel <- psych::fa.parallel(
  data_continua_z,
  fa = "fa",
  fm = "minres"
)

# Estimamos dos factores para comparar con el proceso generador:

fa_2 <- psych::fa(
  data_continua_z,
  nfactors = 2,
  rotate = "oblimin",
  fm = "minres"
)

print(
  fa_2$loadings,
  cutoff = 0.20
)

# EGA y análisis factorial están mirando la estructura de dependencia
# desde representaciones diferentes.
#
# En estos datos sabemos que existen dos factores porque nosotros
# construimos el proceso generador.
#
# Con datos reales, la estructura observada por sí sola no determina
# automáticamente cuál interpretación sustantiva generó esa distribución.


# ------------------------------------------------------------------------------
# 39. INCERTIDUMBRE DE LAS ARISTAS: BOOTSTRAP NO PARAMÉTRICO
# ------------------------------------------------------------------------------

# Una red estimada depende de una muestra.
#
# Re-muestreamos individuos y volvemos a estimar la red.

set.seed(1234)

boot_edges <- bootnet::bootnet(
  network_ggm,
  nBoots = 500,
  type = "nonparametric",
  nCores = 1
)

# En una aplicación final puede aumentarse a 1000 o más réplicas.


# ------------------------------------------------------------------------------
# 40. VISUALIZAR INCERTIDUMBRE DE LOS PESOS
# ------------------------------------------------------------------------------

plot(
  boot_edges,
  labels = FALSE,
  order = "sample"
)

# Un intervalo amplio indica que el peso estimado es impreciso.


# ------------------------------------------------------------------------------
# 41. ¿SON DOS ARISTAS DIFERENTES ENTRE SÍ?
# ------------------------------------------------------------------------------

# bootnet permite comparar pesos mediante bootstrap.
#
# El gráfico de diferencias puede producirse con:

plot(
  boot_edges,
  "edge",
  plot = "difference",
  onlyNonZero = TRUE,
  order = "sample"
)


# ------------------------------------------------------------------------------
# 42. ESTABILIDAD DE LA CENTRALIDAD
# ------------------------------------------------------------------------------

# Ahora eliminamos progresivamente casos y volvemos a estimar la red.
# Queremos saber si el ranking de strength se conserva.

set.seed(1234)

boot_cases <- bootnet::bootnet(
  network_ggm,
  nBoots = 500,
  type = "case",
  statistics = c("strength"),
  nCores = 1
)


# ------------------------------------------------------------------------------
# 43. VISUALIZAR LA ESTABILIDAD DE STRENGTH
# ------------------------------------------------------------------------------

plot(
  boot_cases,
  statistics = "strength"
)


# ------------------------------------------------------------------------------
# 44. COEFFICIENTE CS
# ------------------------------------------------------------------------------

bootnet::corStability(
  boot_cases
)

# El CS-coefficient resume cuánto de la muestra podemos eliminar
# manteniendo una correlación suficientemente alta con la centralidad original.
#
# Como criterio práctico frecuente:
#
# CS < 0.25    -> baja estabilidad
# CS > 0.25    -> interpretable con cautela
# CS > 0.50    -> estabilidad preferible


# ------------------------------------------------------------------------------
# 45. COMPARAR DOS SUBMUESTRAS
# ------------------------------------------------------------------------------

# Generaremos ahora dos grupos.
#
# Grupo A mantiene los datos originales.
#
# Grupo B tendrá una relación adicional más fuerte entre
# Preocupacion e Insomnio.

n_grupo <- 350

# Grupo A
A_ansiedad <- rnorm(n_grupo)

A_depresion <- rnorm(n_grupo)

grupo_A <- data.frame(
  Anhedonia = 0.75 * A_depresion + rnorm(n_grupo, sd = 0.45),
  Insomnio = 0.55 * A_depresion + 0.35 * A_ansiedad + rnorm(n_grupo, sd = 0.45),
  Fatiga = 0.80 * A_depresion + rnorm(n_grupo, sd = 0.45),
  Irritabilidad = 0.65 * A_ansiedad + rnorm(n_grupo, sd = 0.45),
  Ansiedad = 0.90 * A_ansiedad + rnorm(n_grupo, sd = 0.40),
  Preocupacion = 0.80 * A_ansiedad + rnorm(n_grupo, sd = 0.40),
  Nerviosismo = 0.85 * A_ansiedad + rnorm(n_grupo, sd = 0.40),
  Inquietud = 0.75 * A_ansiedad + rnorm(n_grupo, sd = 0.45)
)

# Grupo B
B_ansiedad <- rnorm(n_grupo)

B_depresion <- rnorm(n_grupo)

B_preocupacion <- 0.80 * B_ansiedad + rnorm(n_grupo, sd = 0.40)

grupo_B <- data.frame(
  Anhedonia = 0.75 * B_depresion + rnorm(n_grupo, sd = 0.45),
  Insomnio = 0.55 * B_depresion +
    0.35 * B_ansiedad +
    0.45 * B_preocupacion +
    rnorm(n_grupo, sd = 0.45),
  Fatiga = 0.80 * B_depresion + rnorm(n_grupo, sd = 0.45),
  Irritabilidad = 0.65 * B_ansiedad + rnorm(n_grupo, sd = 0.45),
  Ansiedad = 0.90 * B_ansiedad + rnorm(n_grupo, sd = 0.40),
  Preocupacion = B_preocupacion,
  Nerviosismo = 0.85 * B_ansiedad + rnorm(n_grupo, sd = 0.40),
  Inquietud = 0.75 * B_ansiedad + rnorm(n_grupo, sd = 0.45)
)


# ------------------------------------------------------------------------------
# 46. ESTIMAR UNA RED PARA CADA GRUPO
# ------------------------------------------------------------------------------

network_A <- bootnet::estimateNetwork(
  grupo_A,
  default = "EBICglasso",
  corMethod = "cor_auto"
)

network_B <- bootnet::estimateNetwork(
  grupo_B,
  default = "EBICglasso",
  corMethod = "cor_auto"
)


# ------------------------------------------------------------------------------
# 47. COMPARACIÓN VISUAL CON EL MISMO LAYOUT
# ------------------------------------------------------------------------------

# Guardamos un layout común para facilitar comparación.

layout_comun <- qgraph::averageLayout(
  network_A$graph,
  network_B$graph
)

qgraph(
  network_A$graph,
  layout = layout_comun,
  labels = colnames(grupo_A),
  vsize = 7,
  legend = FALSE,
  title = "Grupo A"
)

qgraph(
  network_B$graph,
  layout = layout_comun,
  labels = colnames(grupo_B),
  vsize = 7,
  legend = FALSE,
  title = "Grupo B"
)

# El layout común reduce diferencias puramente gráficas.
#
# Aun así, observar dos dibujos distintos no constituye por sí solo
# una prueba estadística de diferencia entre redes.


# ------------------------------------------------------------------------------
# 48. NETWORK COMPARISON TEST
# ------------------------------------------------------------------------------

if (requireNamespace("NetworkComparisonTest", quietly = TRUE)) {

  set.seed(1234)

  nct_resultado <- NetworkComparisonTest::NCT(
    grupo_A,
    grupo_B,
    it = 500,
    binary.data = FALSE,
    paired = FALSE,
    test.edges = TRUE,
    edges = "all",
    progressbar = TRUE
  )

  # Diferencia en estructura global
  nct_resultado$glstrinv.real

  nct_resultado$glstrinv.pval

  # Diferencia en fuerza global
  nct_resultado$glstrinv.sep

  nct_resultado$glstrinv.pval

  # Resultados del test
  summary(nct_resultado)

}

# Observar el M del output que representa la mayor diferencia absoluta entre 
# pesos de aristas equivalentes, p pequeño implica que no son iguales. 

