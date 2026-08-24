# ==============================================================================
# TEORÍA DE REDES SOCIALES - DCCS
# SESIÓN 3: CENTRALIDAD, COHESIÓN, COMUNIDADES, ROLES Y EMBEDDINGS
# ==============================================================================
# La secuencia completa de la sesión fue:
#
# RED
#   |
#   +--> descripción básica
#   |      grado, densidad, componentes, distancias, transitividad
#   |
#   +--> CENTRALIDAD
#   |      grado
#   |      intermediación
#   |      cercanía
#   |      eigenvector
#   |
#   +--> COHESIÓN
#   |      cliques
#   |      k-cores
#   |
#   +--> COMUNIDADES
#   |      modularidad
#   |      Louvain
#   |      Walktrap
#   |      Greedy modularity
#   |
#   +--> ROLES
#   |      centralidad interna
#   |      participación entre comunidades
#   |
#   +--> EMBEDDINGS
#          embedding espectral
#          random walks
#          node2vec
#
#
# Pregunta final:
#
# "¿Qué significa ocupar una posición importante en una red?"
#
# La respuesta depende de qué propiedad estructural queremos representar:
#
# acceso
# intermediación
# proximidad
# prestigio
# cohesión
# pertenencia comunitaria
# rol
# similitud estructural
#
# ==============================================================================
# Red utilizada:
# Zachary's Karate Club (Zachary, 1977)
# 34 miembros de un club de karate y 78 relaciones sociales.
# https://networks.skewed.de/net/karate
#
# La red también está incorporada en igraph como "Zachary".
#
# Referencias principales:
# Freeman, L. C. (1979). Centrality in Social Networks: Conceptual Clarification.
# Borgatti, S. P. (2005). Centrality and Network Flow.
# Newman, M. E. J. (2006). Modularity and Community Structure in Networks.
# Grover, A. & Leskovec, J. (2016). node2vec.
#
# ==============================================================================

library(igraph)

# Fijamos una semilla para que layouts, random walks y embeddings
# sean reproducibles en la medida en que los paquetes lo permitan.

set.seed(1234)

# ==============================================================================
# 1. CARGAR Y ENTENDER LA BASE ANTES DE CALCULAR CENTRALIDADES
# ==============================================================================

g <- make_graph("Zachary")

g


# ------------------------------------------------------------------------------
# 1.1 ¿Qué representa esta red?
# ------------------------------------------------------------------------------

# Nodos: miembros del club.
# Aristas: relaciones sociales observadas entre miembros.
# Tipo de red: no dirigida.
# Peso: no ponderada para este ejercicio.

is_directed(g)

# Número de nodos

n <- vcount(g)
n

# Número de aristas

m <- ecount(g)
m

# La versión de uso más extendido de la red de Zachary tiene:
# n = 34
# m = 78
#
# Verificamos que estamos trabajando con esa versión.

stopifnot(n == 34)
stopifnot(m == 78)


# ------------------------------------------------------------------------------
# 1.2 Identificadores de los nodos
# ------------------------------------------------------------------------------

V(g)$name <- as.character(1:vcount(g))
V(g)$name
V(g)$name[1]  <- "Mr. Hi"
V(g)$name[34] <- "John A."
V(g)$name

# Para trabajar cómodamente creamos un vector con nombres.

nodos <- V(g)$name

# ------------------------------------------------------------------------------
# 1.3 Lista de aristas
# ------------------------------------------------------------------------------

aristas <- as_data_frame(g, what = "edges")
head(aristas, 15)

dim(aristas)


# ------------------------------------------------------------------------------
# 1.4 Matriz de adyacencia
# ------------------------------------------------------------------------------

# Convertimos la red en una matriz A.
#
# A_ij = 1 si existe una relación entre i y j
# A_ij = 0 en otro caso

A <- as_adjacency_matrix(g, sparse = FALSE)
A <- as.matrix(A)

A[1:10, 1:10]

# La red es no dirigida.
# Por tanto, la matriz debe ser simétrica.

all(A == t(A))

# No existen loops.
# La diagonal debe ser cero.

diag(A)


# ==============================================================================
# 2. DESCRIPCIÓN ESTRUCTURAL BÁSICA DE LA RED
# ==============================================================================


# ------------------------------------------------------------------------------
# 2.1 Grado calculado directamente desde la matriz
# ------------------------------------------------------------------------------

# Para una red no dirigida:
#
# k_i = sum_j A_ij
#
# Es decir, sumamos cada fila de la matriz de adyacencia.

grado_manual <- rowSums(A)

grado_manual

# Construimos una tabla simple.

tabla_grado <- data.frame(
  nodo = nodos,
  grado = grado_manual
)

tabla_grado <- tabla_grado[order(-tabla_grado$grado), ]

tabla_grado


# Verificación con igraph

grado_igraph <- degree(g)

all(grado_manual == grado_igraph)


# ------------------------------------------------------------------------------
# 2.2 Distribución de grado
# ------------------------------------------------------------------------------

# ¿Cuántos nodos tienen grado 1, 2, 3, ...?

tabla_distribucion_grado <- table(grado_manual)
tabla_distribucion_grado

# Proporción de nodos para cada grado.

prop.table(tabla_distribucion_grado)

# Visualización.

barplot(
  tabla_distribucion_grado,
  xlab = "Grado k",
  ylab = "Número de nodos",
  main = "Distribución de grado - Zachary Karate Club"
)


# ------------------------------------------------------------------------------
# 2.3 Grado promedio
# ------------------------------------------------------------------------------

grado_promedio_manual <- mean(grado_manual)
grado_promedio_manual

# En una red no dirigida:
#
# sum_i k_i = 2m
#
# Por tanto:
#
# grado promedio = 2m/n

grado_promedio_formula <- 2 * m / n
grado_promedio_formula

all.equal(grado_promedio_manual, grado_promedio_formula)


# ------------------------------------------------------------------------------
# 2.4 Densidad desde la definición
# ------------------------------------------------------------------------------

# Número máximo de aristas en una red simple no dirigida:
#
# n(n-1)/2

max_aristas <- n * (n - 1) / 2
max_aristas

# Densidad:
#
# D = m / [n(n-1)/2]
#   = 2m / [n(n-1)]

densidad_manual <- m / max_aristas
densidad_manual

# Verificamos con igraph.

densidad_igraph <- edge_density(g)
densidad_igraph

all.equal(densidad_manual, densidad_igraph)


# ------------------------------------------------------------------------------
# 2.5 Componentes: construir primero la idea de alcanzabilidad
# ------------------------------------------------------------------------------

# Dos nodos pertenecen al mismo componente si existe algún camino entre ellos.
#
# Una manera algebraica de pensarlo:
#
# A       informa caminos de longitud 1
# A^2     informa caminatas de longitud 2
# A^3     informa caminatas de longitud 3
# ...
#
# Si para algún r <= n-1:
#
# (A^r)_ij > 0
#
# entonces existe un camino que conecta i con j.

# Comenzamos diciendo que cada nodo puede alcanzarse a sí mismo.

alcanzable <- diag(n) > 0

# Caminos de longitud 1.

potencia_A <- A
alcanzable <- alcanzable | (potencia_A > 0)

# Incorporamos sucesivamente caminos más largos.
#
# Para este tamaño de red podemos hacerlo explícitamente.
# No es el procedimiento más eficiente para una red grande.

for (paso in 2:(n - 1)) {

  potencia_A <- potencia_A %*% A

  alcanzable <- alcanzable | (potencia_A > 0)
}

# ¿Cuántos nodos alcanza cada nodo?

rowSums(alcanzable)

# Si todos alcanzan a los 34 nodos, la red está conectada.

all(rowSums(alcanzable) == n)

    # Para ver lo que pasas cuando hay más de un componente 
    # agreguemos un nodo sin conexiones

    g1 <- g
    g1 <- add_vertices(g1, 1)
    A1 <- as.matrix(as_adjacency_matrix(g1))
    n1 <- nrow(A1)

    # Cada nodo puede alcanzarse a sí mismo.

    alcanzable1 <- diag(n1) > 0

    # Caminos de longitud 1.

    potencia_A1 <- A1
    alcanzable1 <- alcanzable1 | (potencia_A1 > 0)
    for (paso in 2:(n1 - 1)) {
      potencia_A1 <- potencia_A1 %*% A1
      alcanzable1 <- alcanzable1 | (potencia_A1 > 0)
    }
    rowSums(alcanzable1)

# Volvamos sobre la red original
# Construimos etiquetas de componente a partir de la matriz de alcanzabilidad.

componente_manual <- rep(NA_integer_, n)
numero_componente <- 0

for (i in 1:n) {

  if (is.na(componente_manual[i])) {

    numero_componente <- numero_componente + 1

    miembros <- which(alcanzable[i, ])

    componente_manual[miembros] <- numero_componente
  }
}

componente_manual
numero_componente


# Verificación con igraph.

comp <- components(g)

comp$no
comp$csize
comp$membership

comp1 <- components(g1)


# ------------------------------------------------------------------------------
# 2.6 Distancias geodésicas: cálculo explícito con Floyd-Warshall
# ------------------------------------------------------------------------------

# D_ij comienza en:
#
# 0      si i = j
# 1      si i y j son vecinos
# Inf    si todavía no conocemos un camino

D <- matrix(Inf, nrow = n, ncol = n)

diag(D) <- 0

D[A == 1] <- 1

# Algoritmo de Floyd-Warshall.
#
# Para cada posible nodo intermediario k:
#
# d(i,j) = min[d(i,j), d(i,k) + d(k,j)]

for (k in 1:n) {

  for (i in 1:n) {

    for (j in 1:n) {

      alternativa <- D[i, k] + D[k, j]

      if (alternativa < D[i, j]) {

        D[i, j] <- alternativa
      }
    }
  }
}

D[1:10, 1:10]

# Verificación con igraph.

D_igraph <- distances(g)

all(D == D_igraph)


# ------------------------------------------------------------------------------
# 2.7 Distancia promedio y diámetro
# ------------------------------------------------------------------------------

# Tomamos únicamente el triángulo superior de la matriz,
# porque d(i,j) = d(j,i).

distancias_unicas <- D[upper.tri(D)]

mean(distancias_unicas)

diametro_manual <- max(distancias_unicas)
diametro_manual

# Verificación.

mean_distance(g)
diameter(g)


# ------------------------------------------------------------------------------
# 2.8 Transitividad global
# ------------------------------------------------------------------------------

# Para una red simple no dirigida:
#
# trace(A^3) / 6
#
# entrega el número de triángulos.

A2 <- A %*% A
A3 <- A2 %*% A

numero_triangulos <- sum(diag(A3)) / 6
numero_triangulos

# Número de triples conectados:
#
# para cada nodo i existen choose(k_i, 2) pares de vecinos.

triples_conectados <- sum(grado_manual * (grado_manual - 1) / 2)
triples_conectados

# Transitividad:
#
# T = 3 * triángulos / triples conectados

transitividad_manual <- 3 * numero_triangulos / triples_conectados
transitividad_manual

# Verificación.

transitivity(g, type = "global")


# ------------------------------------------------------------------------------
# 2.9 Primera visualización
# ------------------------------------------------------------------------------

# Conservaremos el mismo layout para todas las figuras posteriores.
# De esa manera, cuando cambie el tamaño o el color del nodo,
# estaremos comparando la misma geometría.

set.seed(1234)
layout_base <- layout_with_fr(g)

plot(
  g,
  layout = layout_base,
  vertex.size = 18,
  vertex.label.cex = 0.8,
  main = "Zachary Karate Club"
)


# ==============================================================================
# 3. CENTRALIDAD DE GRADO
# ==============================================================================

# Pregunta:
#
# ¿Cuántas conexiones directas tiene cada nodo?
#
# Ya la calculamos desde la matriz:
#
# k_i = sum_j A_ij

centralidad_grado <- grado_manual

tabla_centralidades <- data.frame(
  nodo = nodos,
  grado = centralidad_grado
)

tabla_centralidades[order(-tabla_centralidades$grado), ][1:10, ]


# ------------------------------------------------------------------------------
# 3.1 Grado normalizado
# ------------------------------------------------------------------------------

# Máximo grado posible = n - 1

grado_normalizado_manual <- centralidad_grado / (n - 1)

grado_normalizado_manual

# Verificación.

grado_normalizado_igraph <- degree(g, normalized = TRUE)

all.equal(
  as.numeric(grado_normalizado_manual),
  as.numeric(grado_normalizado_igraph)
)


# ------------------------------------------------------------------------------
# 3.2 Visualización
# ------------------------------------------------------------------------------

plot(
  g,
  layout = layout_base,
  vertex.size = 8 + 2 * centralidad_grado,
  vertex.label.cex = 0.8,
  main = "Tamaño del nodo proporcional al grado"
)


# ==============================================================================
# 4. CENTRALIDAD DE CERCANÍA
# ==============================================================================

# Pregunta:
#
# ¿Qué tan lejos está un nodo del resto?
#
# Para una red conectada:
#
# C_C(i) = (n-1) / sum_j d(i,j)

suma_distancias <- rowSums(D)

cercania_manual <- (n - 1) / suma_distancias

cercania_manual


# Verificación con igraph.

cercania_igraph <- closeness(g, normalized = TRUE)

all.equal(
  as.numeric(cercania_manual),
  as.numeric(cercania_igraph)
)


# Añadimos a la tabla.

tabla_centralidades$cercania <- cercania_manual

tabla_centralidades[
  order(-tabla_centralidades$cercania),
][1:10, ]


plot(
  g,
  layout = layout_base,
  vertex.size = 8 + 15 * cercania_igraph,
  vertex.label.cex = 0.8,
  main = "Tamaño del nodo proporcional al grado"
)

# ==============================================================================
# 5. CENTRALIDAD DE INTERMEDIACIÓN
# ==============================================================================

# Pregunta:
#
# ¿En qué proporción de los caminos geodésicos entre otros nodos aparece i?
#
# Freeman:
#
# C_B(v) =
# sum_{s != v != t} sigma_st(v) / sigma_st
#
# donde:
#
# sigma_st    = número de caminos geodésicos entre s y t
# sigma_st(v) = número de esos caminos que pasan por v
#
# Vamos a construirlo paso a paso.


# ------------------------------------------------------------------------------
# 5.1 Número de caminos geodésicos entre cada par
# ------------------------------------------------------------------------------

# Creamos una matriz sigma.
#
# sigma[s,t] = número de caminos más cortos desde s hasta t.

sigma <- matrix(0, nrow = n, ncol = n)

for (s in 1:n) {

  # Existe exactamente un camino de longitud 0 desde s hasta sí mismo.

  sigma[s, s] <- 1

  # Ordenamos los nodos por distancia desde s.

  orden <- order(D[s, ])

  for (w in orden) {

    if (w == s) {
      next
    }

    # Predecesores de w en un camino geodésico desde s:
    #
    # nodos vecinos de w que están exactamente un paso más cerca de s.

    vecinos_w <- which(A[w, ] == 1)

    predecesores <- vecinos_w[
      D[s, vecinos_w] == D[s, w] - 1
    ]

    # Cada camino geodésico hacia un predecesor
    # puede extenderse una arista hasta w.

    sigma[s, w] <- sum(sigma[s, predecesores])
  }
}

sigma[1:10, 1:10]


# ------------------------------------------------------------------------------
# 5.2 Betweenness a partir de sigma y D
# ------------------------------------------------------------------------------

betweenness_manual <- rep(0, n)

for (v in 1:n) {

  acumulado <- 0

  for (s in 1:(n - 1)) {

    for (t in (s + 1):n) {

      # No contamos pares en los que v sea origen o destino.

      if (s == v || t == v) {
        next
      }

      # v puede pertenecer a un camino geodésico s-t sólo si:
      #
      # d(s,t) = d(s,v) + d(v,t)

      if (D[s, t] == D[s, v] + D[v, t]) {

        caminos_via_v <- sigma[s, v] * sigma[v, t]

        total_caminos <- sigma[s, t]

        acumulado <- acumulado + caminos_via_v / total_caminos
      }
    }
  }

  betweenness_manual[v] <- acumulado
}

betweenness_manual


# Verificación con igraph.
# Para una red no dirigida, igraph cuenta cada par no ordenado una vez.

betweenness_igraph <- betweenness(
  g,
  directed = FALSE,
  normalized = FALSE
)

max(abs(betweenness_manual - betweenness_igraph))

all.equal(
  as.numeric(betweenness_manual),
  as.numeric(betweenness_igraph),
  tolerance = 1e-8
)


# Añadimos a la tabla.

tabla_centralidades$intermediacion <- betweenness_manual

tabla_centralidades[
  order(-tabla_centralidades$intermediacion),
][1:10, ]


# ------------------------------------------------------------------------------
# 5.3 Visualización de brokerage
# ------------------------------------------------------------------------------

plot(
  g,
  layout = layout_base,
  vertex.size = 8 + sqrt(betweenness_manual + 1) * 3,
  vertex.label.cex = 0.8,
  main = "Tamaño proporcional a intermediación"
)


# ==============================================================================
# 6. CENTRALIDAD DE EIGENVECTOR
# ==============================================================================

# Idea:
#
# un nodo es central si está conectado con otros nodos centrales.
#
# x_i = (1/lambda) * sum_j A_ij x_j
#
# En forma matricial:
#
# A x = lambda x


# ------------------------------------------------------------------------------
# 6.1 Power iteration desde cero
# ------------------------------------------------------------------------------

# Partimos asignando igual centralidad a todos.

x <- rep(1, n)

# Normalizamos.

x <- x / sqrt(sum(x^2))

# Iteramos.
#
# Multiplicar A %*% x hace que cada nodo reciba
# la suma de los valores actuales de sus vecinos.

for (iteracion in 1:1000) {

  x_nuevo <- as.numeric(A %*% x)

  x_nuevo <- x_nuevo / sqrt(sum(x_nuevo^2))

  cambio <- max(abs(x_nuevo - x))

  x <- x_nuevo

  if (cambio < 1e-10) {
    break
  }
}

iteracion
cambio

eigen_manual <- x

# Escalamos para que el máximo sea 1,
# siguiendo una convención fácil de interpretar.

eigen_manual <- eigen_manual / max(eigen_manual)

eigen_manual


# ------------------------------------------------------------------------------
# 6.2 Verificación con igraph
# ------------------------------------------------------------------------------

eig_igraph <- eigen_centrality(
  g,
  directed = FALSE
)

eigen_igraph <- eig_igraph$vector

# El signo de un eigenvector puede invertirse sin cambiar su significado.
# En este caso esperamos valores positivos.

max(abs(eigen_manual - eigen_igraph))


# Añadimos a la tabla.

tabla_centralidades$eigenvector <- eigen_manual


# ==============================================================================
# 7. COMPARAR DISTINTAS IDEAS DE "IMPORTANCIA"
# ==============================================================================

# Ordenamos separadamente por cada centralidad.

top_grado <- tabla_centralidades[
  order(-tabla_centralidades$grado),
][1:10, c("nodo", "grado")]

top_intermediacion <- tabla_centralidades[
  order(-tabla_centralidades$intermediacion),
][1:10, c("nodo", "intermediacion")]

top_cercania <- tabla_centralidades[
  order(-tabla_centralidades$cercania),
][1:10, c("nodo", "cercania")]

top_eigenvector <- tabla_centralidades[
  order(-tabla_centralidades$eigenvector),
][1:10, c("nodo", "eigenvector")]

top_grado
top_intermediacion
top_cercania
top_eigenvector


# ------------------------------------------------------------------------------
# 7.1 Correlaciones entre centralidades
# ------------------------------------------------------------------------------

cor(
  tabla_centralidades[, c(
    "grado",
    "intermediacion",
    "cercania",
    "eigenvector"
  )]
)


# ------------------------------------------------------------------------------
# 7.2 Rankings
# ------------------------------------------------------------------------------

tabla_centralidades$rank_grado <-
  rank(-tabla_centralidades$grado, ties.method = "min")

tabla_centralidades$rank_intermediacion <-
  rank(-tabla_centralidades$intermediacion, ties.method = "min")

tabla_centralidades$rank_cercania <-
  rank(-tabla_centralidades$cercania, ties.method = "min")

tabla_centralidades$rank_eigenvector <-
  rank(-tabla_centralidades$eigenvector, ties.method = "min")

tabla_centralidades[
  order(tabla_centralidades$rank_grado),
]


# Pregunta para discutir:
#
# ¿Qué nodos cambian más de posición según la definición de centralidad?
#
# La respuesta obliga a distinguir:
#
# grado          -> acceso/visibilidad directa
# betweenness    -> intermediación
# closeness      -> proximidad global
# eigenvector    -> prestigio recursivo


# ==============================================================================
# 8. COHESIÓN: CLIQUES
# ==============================================================================

# Un clique es un subconjunto de nodos donde todos están conectados con todos.


# ------------------------------------------------------------------------------
# 8.1 Triángulos desde cero
# ------------------------------------------------------------------------------

# Un triángulo es un clique de tamaño 3.
# Ya sabemos contar el número total mediante trace(A^3)/6.
#
# Ahora identificaremos explícitamente cuáles son.

triangulos <- data.frame(
  nodo1 = character(0),
  nodo2 = character(0),
  nodo3 = character(0)
)

for (i in 1:(n - 2)) {

  for (j in (i + 1):(n - 1)) {

    for (k in (j + 1):n) {

      if (
        A[i, j] == 1 &&
        A[i, k] == 1 &&
        A[j, k] == 1
      ) {

        triangulos <- rbind(
          triangulos,
          data.frame(
            nodo1 = nodos[i],
            nodo2 = nodos[j],
            nodo3 = nodos[k]
          )
        )
      }
    }
  }
}

nrow(triangulos)

numero_triangulos

head(triangulos, 20)


# ------------------------------------------------------------------------------
# 8.2 Cliques maximales con igraph
# ------------------------------------------------------------------------------

# Enumerar todos los cliques maximales requiere un algoritmo especializado.
# Aquí sí utilizamos igraph.

cliques_maximos <- max_cliques(g)

length(cliques_maximos)

# Tamaños.

tamanos_cliques <- sapply(cliques_maximos, length)

sort(tamanos_cliques, decreasing = TRUE)

# Mostramos los mayores.

cliques_maximos[
  order(tamanos_cliques, decreasing = TRUE)
][1:10]


# ==============================================================================
# 9. COHESIÓN: K-CORES
# ==============================================================================

# Un k-core es un subgrafo donde cada nodo posee al menos k
# vecinos dentro de ese mismo subgrafo.


# ------------------------------------------------------------------------------
# 9.1 Construcción explícita de los k-cores
# ------------------------------------------------------------------------------

# Calcularemos, para cada k, qué nodos sobreviven al procedimiento:
#
# 1. eliminar nodos con grado interno < k
# 2. recalcular grados
# 3. repetir hasta que ya no sea necesario eliminar más nodos

coreness_manual <- rep(0, n)

grado_maximo <- max(grado_manual)

for (k in 1:grado_maximo) {

  activos <- rep(TRUE, n)

  repetir <- TRUE

  while (repetir) {

    indices_activos <- which(activos)

    A_activa <- A[indices_activos, indices_activos, drop = FALSE]

    grados_internos <- rowSums(A_activa)

    eliminar_local <- which(grados_internos < k)

    if (length(eliminar_local) == 0) {

      repetir <- FALSE

    } else {

      eliminar_global <- indices_activos[eliminar_local]

      activos[eliminar_global] <- FALSE
    }

    if (sum(activos) == 0) {
      repetir <- FALSE
    }
  }

  # Los nodos que sobreviven pertenecen al k-core.

  coreness_manual[activos] <- k
}

coreness_manual


# Verificación con igraph.

coreness_igraph <- coreness(g)

all(coreness_manual == coreness_igraph)


tabla_centralidades$coreness <- coreness_manual

tabla_centralidades[
  order(-tabla_centralidades$coreness, -tabla_centralidades$grado),
][1:15, ]


# Visualización.

plot(
  g,
  layout = layout_base,
  vertex.size = 10 + 5 * coreness_manual,
  vertex.label.cex = 0.8,
  main = "Tamaño proporcional a coreness"
)


# ==============================================================================
# 10. COMUNIDADES: PRIMERO LA IDEA
# ==============================================================================

# Una comunidad es, intuitivamente, un conjunto de nodos
# más densamente conectado internamente que con el resto.
#
# Pero para convertir esa intuición en un criterio
# necesitamos una función objetivo.
#
# Una de las más usadas es modularidad.


# ==============================================================================
# 11. MODULARIDAD
# ==============================================================================

# Newman:
#
# Q = (1/2m) sum_ij [A_ij - k_i k_j/(2m)] delta(c_i,c_j)
#
# La lógica es:
#
# A_ij                  = vínculo observado
#
# k_i k_j/(2m)         = vínculo esperado bajo un modelo nulo
#                        que preserva la secuencia de grados
#
# delta(c_i,c_j) = 1   = i y j pertenecen a la misma comunidad


# ------------------------------------------------------------------------------
# 11.1 Primero obtenemos una partición
# ------------------------------------------------------------------------------

# Usaremos Louvain para disponer de una primera partición.
#
# Más adelante compararemos algoritmos.

com_louvain <- cluster_louvain(g)

membership_louvain <- membership(com_louvain)

membership_louvain

length(unique(membership_louvain))


# ------------------------------------------------------------------------------
# 11.2 Calcular modularidad desde la fórmula
# ------------------------------------------------------------------------------

Q_manual <- 0

for (i in 1:n) {

  for (j in 1:n) {

    observado <- A[i, j]

    esperado <- grado_manual[i] * grado_manual[j] / (2 * m)

    misma_comunidad <- as.numeric(
      membership_louvain[i] == membership_louvain[j]
    )

    Q_manual <- Q_manual +
      (observado - esperado) * misma_comunidad
  }
}

Q_manual <- Q_manual / (2 * m)

Q_manual


# Verificación.

Q_igraph <- modularity(g, membership_louvain)
Q_igraph

all.equal(Q_manual, Q_igraph)


# ==============================================================================
# 12. DETECCIÓN DE COMUNIDADES: LOUVAIN
# ==============================================================================

com_louvain

membership_louvain

modularity(com_louvain)

sizes(com_louvain)


# Visualización.

plot(
  com_louvain,
  g,
  layout = layout_base,
  vertex.label.cex = 0.8,
  main = "Comunidades: Louvain"
)


# ==============================================================================
# 13. DETECCIÓN DE COMUNIDADES: WALKTRAP
# ==============================================================================

# Walktrap utiliza caminatas aleatorias cortas.
#
# Intuición:
# una caminata tiende a permanecer durante varios pasos
# dentro de una zona densamente conectada.

com_walktrap <- cluster_walktrap(
  g,
  steps = 4
)

membership_walktrap <- membership(com_walktrap)

membership_walktrap

modularity(com_walktrap)

sizes(com_walktrap)


plot(
  com_walktrap,
  g,
  layout = layout_base,
  vertex.label.cex = 0.8,
  main = "Comunidades: Walktrap"
)


# ==============================================================================
# 14. DETECCIÓN DE COMUNIDADES: GREEDY MODULARITY
# ==============================================================================

# Greedy modularity comienza con comunidades pequeñas
# y realiza fusiones que mejoran Q.

com_greedy <- cluster_fast_greedy(g)

membership_greedy <- membership(com_greedy)

membership_greedy

modularity(com_greedy)

sizes(com_greedy)


plot(
  com_greedy,
  g,
  layout = layout_base,
  vertex.label.cex = 0.8,
  main = "Comunidades: Greedy modularity"
)


# ==============================================================================
# 15. COMPARAR ALGORITMOS DE COMUNIDADES
# ==============================================================================

comparacion_comunidades <- data.frame(
  nodo = nodos,
  louvain = as.integer(membership_louvain),
  walktrap = as.integer(membership_walktrap),
  greedy = as.integer(membership_greedy)
)

comparacion_comunidades


# Número de comunidades.

data.frame(
  algoritmo = c("Louvain", "Walktrap", "Greedy modularity"),
  n_comunidades = c(
    length(unique(membership_louvain)),
    length(unique(membership_walktrap)),
    length(unique(membership_greedy))
  ),
  modularidad = c(
    modularity(com_louvain),
    modularity(com_walktrap),
    modularity(com_greedy)
  )
)


# Comparación formal entre particiones.
# Adjusted Rand Index = 1 si las particiones coinciden exactamente.

compare(
  membership_louvain,
  membership_walktrap,
  method = "adjusted.rand"
)

compare(
  membership_louvain,
  membership_greedy,
  method = "adjusted.rand"
)

compare(
  membership_walktrap,
  membership_greedy,
  method = "adjusted.rand"
)


# ==============================================================================
# 16. ROLES ESTRUCTURALES DENTRO Y ENTRE COMUNIDADES
# ==============================================================================

# Comunidad y rol estructural son conceptos distintos.
#
# Vamos a construir dos medidas sencillas:
#
# 1. within-module degree z-score:
#    ¿qué tan conectado está i dentro de su comunidad?
#
# 2. participation coefficient:
#    ¿cómo distribuye i sus vínculos entre distintas comunidades?


# ------------------------------------------------------------------------------
# 16.1 Grado interno a la comunidad
# ------------------------------------------------------------------------------

comunidad <- membership_louvain

grado_interno <- rep(0, n)

for (i in 1:n) {

  vecinos_i <- which(A[i, ] == 1)

  grado_interno[i] <- sum(
    comunidad[vecinos_i] == comunidad[i]
  )
}

grado_interno


# ------------------------------------------------------------------------------
# 16.2 Within-module degree z-score
# ------------------------------------------------------------------------------

z_modulo <- rep(NA_real_, n)

comunidades_existentes <- sort(unique(comunidad))

for (c in comunidades_existentes) {

  miembros_c <- which(comunidad == c)

  valores_c <- grado_interno[miembros_c]

  media_c <- mean(valores_c)

  sd_c <- sd(valores_c)

  if (sd_c == 0) {

    z_modulo[miembros_c] <- 0

  } else {

    z_modulo[miembros_c] <-
      (valores_c - media_c) / sd_c
  }
}

z_modulo


# ------------------------------------------------------------------------------
# 16.3 Participation coefficient
# ------------------------------------------------------------------------------

# P_i = 1 - sum_s (k_is / k_i)^2
#
# k_is = vínculos de i con comunidad s
#
# P cercano a 0:
# casi todos los vínculos permanecen en una sola comunidad.
#
# P más alto:
# vínculos distribuidos entre varias comunidades.

participacion <- rep(0, n)

for (i in 1:n) {

  k_i <- grado_manual[i]

  suma_cuadrados <- 0

  for (c in comunidades_existentes) {

    miembros_c <- which(comunidad == c)

    k_is <- sum(A[i, miembros_c])

    suma_cuadrados <- suma_cuadrados + (k_is / k_i)^2
  }

  participacion[i] <- 1 - suma_cuadrados
}

participacion


# ------------------------------------------------------------------------------
# 16.4 Tabla de roles
# ------------------------------------------------------------------------------

tabla_roles <- data.frame(
  nodo = nodos,
  comunidad = comunidad,
  grado = grado_manual,
  grado_interno = grado_interno,
  z_modulo = z_modulo,
  participacion = participacion,
  intermediacion = betweenness_manual
)

tabla_roles[
  order(-tabla_roles$participacion),
]


# Preguntas:
#
# ¿Qué nodos combinan alto grado interno y baja participación?
# ¿Qué nodos conectan varias comunidades?
# ¿Coinciden alta participación y alta betweenness?


# ==============================================================================
# 17. INTRODUCCIÓN A GRAPH EMBEDDINGS
# ==============================================================================

# Hasta ahora un nodo ha sido representado mediante:
#
# - una fila de A
# - varias centralidades
# - una comunidad
# - un rol
#
# Un embedding busca:
#
# f: V -> R^d
#
# es decir, representar cada nodo mediante un vector de números.


# ==============================================================================
# 18. UN EMBEDDING ESPECTRAL SIMPLE
# ==============================================================================

# Veamos como construir coordenadas desde la estructura de la matriz de adyacencia.
#
# Descomponemos:
#
# A = U Lambda U'
#
# Los eigenvectors resumen direcciones importantes de variación estructural.

descomposicion <- eigen(A, symmetric = TRUE)

valores_propios <- descomposicion$values
vectores_propios <- descomposicion$vectors

valores_propios[1:10]


# ------------------------------------------------------------------------------
# 18.1 Primeras dos dimensiones
# ------------------------------------------------------------------------------

# Utilizamos los dos eigenvectors asociados a los eigenvalues
# de mayor magnitud.

orden_eigen <- order(abs(valores_propios), decreasing = TRUE)

indices_2d <- orden_eigen[1:2]

embedding_espectral <- vectores_propios[, indices_2d, drop = FALSE]

colnames(embedding_espectral) <- c("dim1", "dim2")

embedding_espectral <- data.frame(
  nodo = nodos,
  dim1 = embedding_espectral[, 1],
  dim2 = embedding_espectral[, 2],
  comunidad = comunidad
)

embedding_espectral


# Visualización.

plot(
  embedding_espectral$dim1,
  embedding_espectral$dim2,
  type = "n",
  xlab = "Dimensión 1",
  ylab = "Dimensión 2",
  main = "Embedding espectral de la red"
)

text(
  embedding_espectral$dim1,
  embedding_espectral$dim2,
  labels = embedding_espectral$nodo
)


# ==============================================================================
# 19. RANDOM WALKS: LA PIEZA CENTRAL ANTES DE NODE2VEC
# ==============================================================================

# Node2vec no trabaja directamente con A como el embedding anterior.
#
# Genera secuencias de nodos mediante caminatas aleatorias.
#
# Primero construiremos una caminata aleatoria no sesgada.


# ------------------------------------------------------------------------------
# 19.1 Una caminata aleatoria desde cero
# ------------------------------------------------------------------------------

set.seed(1234)

inicio <- 1

walk_length <- 20

walk_simple <- rep(NA_integer_, walk_length)

walk_simple[1] <- inicio

for (paso in 2:walk_length) {

  actual <- walk_simple[paso - 1]

  vecinos_actual <- which(A[actual, ] == 1)

  siguiente <- sample(
    vecinos_actual,
    size = 1
  )

  walk_simple[paso] <- siguiente
}

walk_simple

nodos[walk_simple]


# ------------------------------------------------------------------------------
# 19.2 Matriz de transición de un random walk simple
# ------------------------------------------------------------------------------

# Desde i elegimos uniformemente entre sus k_i vecinos.
#
# P_ij = A_ij / k_i

P <- matrix(0, nrow = n, ncol = n)

for (i in 1:n) {

  P[i, ] <- A[i, ] / grado_manual[i]
}

# Cada fila debe sumar 1.

rowSums(P)

P[1:10, 1:10]


# ==============================================================================
# 20. NODE2VEC: ¿POR QUÉ p Y q?
# ==============================================================================

# En un random walk simple:
#
# P(siguiente = x | actual = v)
#
# depende sólo de v.
#
# Node2vec introduce memoria de un paso.
#
# La probabilidad de ir desde v hacia x depende también
# del nodo anterior t.
#
# Peso no normalizado:
#
# alpha_pq(t,x) =
#
# 1/p    si x = t
# 1      si x está conectado con t
# 1/q    si x está a distancia 2 de t
#
# Luego esos pesos se normalizan.


# ------------------------------------------------------------------------------
# 20.1 Ejemplo concreto de probabilidades node2vec
# ------------------------------------------------------------------------------

# Escogemos una transición:
#
# nodo anterior = 1
# nodo actual   = 2

anterior <- 1
actual <- 2

candidatos <- which(A[actual, ] == 1)

candidatos


# Elegimos primero p = 1 y q = 1.
#
# Con estos valores no existe sesgo adicional.

p <- 1
q <- 1

pesos <- rep(NA_real_, length(candidatos))

for (r in seq_along(candidatos)) {

  x <- candidatos[r]

  if (x == anterior) {

    pesos[r] <- 1 / p

  } else if (A[anterior, x] == 1) {

    pesos[r] <- 1

  } else {

    pesos[r] <- 1 / q
  }
}

probabilidades <- pesos / sum(pesos)

data.frame(
  candidato = candidatos,
  peso = pesos,
  probabilidad = probabilidades
)


# ------------------------------------------------------------------------------
# 20.2 Favorecer exploración local
# ------------------------------------------------------------------------------

# Una q > 1 penaliza movimientos hacia nodos
# alejados del nodo anterior.

p <- 1
q <- 4

pesos_local <- rep(NA_real_, length(candidatos))

for (r in seq_along(candidatos)) {

  x <- candidatos[r]

  if (x == anterior) {

    pesos_local[r] <- 1 / p

  } else if (A[anterior, x] == 1) {

    pesos_local[r] <- 1

  } else {

    pesos_local[r] <- 1 / q
  }
}

prob_local <- pesos_local / sum(pesos_local)

data.frame(
  candidato = candidatos,
  peso = pesos_local,
  probabilidad = prob_local
)


# ------------------------------------------------------------------------------
# 20.3 Favorecer exploración hacia afuera
# ------------------------------------------------------------------------------

# Una q < 1 aumenta el peso relativo de nodos
# alejados del nodo anterior.

p <- 1
q <- 0.25

pesos_exploracion <- rep(NA_real_, length(candidatos))

for (r in seq_along(candidatos)) {

  x <- candidatos[r]

  if (x == anterior) {

    pesos_exploracion[r] <- 1 / p

  } else if (A[anterior, x] == 1) {

    pesos_exploracion[r] <- 1

  } else {

    pesos_exploracion[r] <- 1 / q
  }
}

prob_exploracion <- pesos_exploracion /
  sum(pesos_exploracion)

data.frame(
  candidato = candidatos,
  peso = pesos_exploracion,
  probabilidad = prob_exploracion
)


# ==============================================================================
# 21. UNA CAMINATA NODE2VEC CONSTRUIDA EXPLÍCITAMENTE
# ==============================================================================

set.seed(1234)

p <- 1
q <- 0.5

walk_length <- 20

walk_n2v <- rep(NA_integer_, walk_length)

# Primer nodo.

walk_n2v[1] <- 1

# Primer movimiento:
# todavía no existe un nodo anterior.
# Elegimos uniformemente entre los vecinos.

vecinos_1 <- which(A[walk_n2v[1], ] == 1)

walk_n2v[2] <- sample(
  vecinos_1,
  size = 1
)

# Desde el tercer paso aplicamos el sesgo p-q.

for (paso in 3:walk_length) {

  anterior <- walk_n2v[paso - 2]

  actual <- walk_n2v[paso - 1]

  candidatos <- which(A[actual, ] == 1)

  pesos <- rep(NA_real_, length(candidatos))

  for (r in seq_along(candidatos)) {

    x <- candidatos[r]

    if (x == anterior) {

      pesos[r] <- 1 / p

    } else if (A[anterior, x] == 1) {

      pesos[r] <- 1

    } else {

      pesos[r] <- 1 / q
    }
  }

  probabilidades <- pesos / sum(pesos)

  walk_n2v[paso] <- sample(
    candidatos,
    size = 1,
    prob = probabilidades
  )
}

walk_n2v

nodos[walk_n2v]


# ==============================================================================
# 22. DE RANDOM WALKS A CONTEXTOS
# ==============================================================================

# Supongamos una caminata:
#
# 1 - 3 - 4 - 8 - 2 - 1
#
# Con una ventana de tamaño 2, para el nodo 4:
#
# contexto = {1, 3, 8, 2}
#
# Node2vec genera muchas caminatas y utiliza estas co-ocurrencias
# como un problema análogo a word2vec/skip-gram.
#
# Nodos que aparecen en contextos similares
# terminan con vectores similares.


# ==============================================================================
# 23. NODE2VEC EN R
# ==============================================================================

# Ahora que ya vimos:
#
# - random walks
# - transición sesgada
# - papel de p
# - papel de q
# - idea de contexto
#
# usamos una implementación disponible en R.


# ------------------------------------------------------------------------------
# 23.1 Preparar la edge list
# ------------------------------------------------------------------------------

edges_node2vec <- data.frame(
  from = as.character(aristas$from),
  to = as.character(aristas$to)
)

head(edges_node2vec)

dim(edges_node2vec)


# ------------------------------------------------------------------------------
# 23.2 Cargar paquete
# ------------------------------------------------------------------------------

library(node2vec)


# ------------------------------------------------------------------------------
# 23.3 Aprender un embedding pequeño
# ------------------------------------------------------------------------------

# Para fines docentes usamos:
#
# dimensión = 8
# 20 caminatas por nodo
# longitud = 20
#
# Una aplicación real puede usar otras configuraciones.

set.seed(1234)

emb_n2v <- node2vecR(
  edges_node2vec,
  p = 1,
  q = 1,
  directed = "undirected",
  num_walks = 20,
  walk_length = 20,
  dim = 8
)

dim(emb_n2v)

emb_n2v[1:10, ]

rownames(emb_n2v)


# ==============================================================================
# 24. EMBEDDING NODE2VEC EN DOS DIMENSIONES
# ==============================================================================

# El embedding tiene 8 dimensiones.
#
# Para verlo en una pantalla necesitamos reducirlo a 2.
# Esta reducción sirve para visualización.
#
# No debemos confundir:
#
# embedding original de 8 dimensiones
#
# con
#
# proyección visual a 2 dimensiones.


# ------------------------------------------------------------------------------
# 24.1 PCA sobre el embedding
# ------------------------------------------------------------------------------

pca_n2v <- prcomp(
  emb_n2v,
  center = TRUE,
  scale. = FALSE
)

summary(pca_n2v)

coords_n2v <- pca_n2v$x[, 1:2]

head(coords_n2v)


# El orden de filas de emb_n2v viene dado por sus rownames.
# Lo vinculamos con los nodos de la red.

ids_embedding <- rownames(emb_n2v)

indice_grafo <- match(
  ids_embedding,
  nodos
)

comunidad_embedding <- comunidad[indice_grafo]


# Visualización sin usar color como requisito interpretativo.
# Etiquetamos los nodos.

plot(
  coords_n2v[, 1],
  coords_n2v[, 2],
  type = "n",
  xlab = "PC1",
  ylab = "PC2",
  main = "Node2vec: proyección 2D mediante PCA"
)

text(
  coords_n2v[, 1],
  coords_n2v[, 2],
  labels = ids_embedding
)


# ==============================================================================
# 25. SIMILITUD ENTRE NODOS EN EL EMBEDDING
# ==============================================================================

# Dos nodos próximos en el espacio vectorial
# tienen representaciones node2vec similares.
#
# Calculamos distancia euclidiana entre embeddings.

dist_emb <- as.matrix(
  dist(emb_n2v)
)

dist_emb[1:10, 1:10]


# ------------------------------------------------------------------------------
# 25.1 Vecinos vectoriales de un nodo
# ------------------------------------------------------------------------------

# Elegimos, por ejemplo, el nodo "1".

nodo_objetivo <- "1"

fila_objetivo <- which(
  rownames(emb_n2v) == nodo_objetivo
)

distancias_objetivo <- dist_emb[
  fila_objetivo,
]

orden_similares <- order(
  distancias_objetivo
)

data.frame(
  nodo = rownames(emb_n2v)[orden_similares],
  distancia_embedding = distancias_objetivo[orden_similares]
)[1:10, ]


# ==============================================================================
# 26. COMPARAR CENTRALIDAD, COMUNIDAD, ROL Y EMBEDDING
# ==============================================================================

# Unimos las dimensiones estudiadas.

tabla_final <- data.frame(
  nodo = nodos,
  grado = grado_manual,
  intermediacion = betweenness_manual,
  cercania = cercania_manual,
  eigenvector = eigen_manual,
  coreness = coreness_manual,
  comunidad_louvain = comunidad,
  grado_interno = grado_interno,
  z_modulo = z_modulo,
  participacion = participacion
)

tabla_final


# ------------------------------------------------------------------------------
# 26.1 Incorporar coordenadas del embedding
# ------------------------------------------------------------------------------

tabla_embedding <- data.frame(
  nodo = rownames(emb_n2v),
  emb_PC1 = coords_n2v[, 1],
  emb_PC2 = coords_n2v[, 2]
)

tabla_final <- merge(
  tabla_final,
  tabla_embedding,
  by = "nodo",
  all.x = TRUE,
  sort = FALSE
)

tabla_final


# ------------------------------------------------------------------------------
# 26.2 Preguntas de interpretación
# ------------------------------------------------------------------------------

# 1. ¿Los nodos con mayor grado pertenecen al mismo core?
#
# 2. ¿Los nodos de mayor betweenness tienen participación alta entre comunidades?
#
# 3. ¿Los nodos de la misma comunidad aparecen próximos en el embedding?
#
# 4. ¿Existen nodos estructuralmente similares que pertenecen a comunidades
#    distintas?
#
# 5. ¿Qué información aporta el embedding que no era evidente
#    mirando solamente una centralidad?


# ==============================================================================
# 27. TRES NODOS PARA INTERPRETAR EN PROFUNDIDAD
# ==============================================================================

# Elegimos automáticamente:
#
# A. mayor grado
# B. mayor intermediación
# C. mayor participación entre comunidades
#
# No esperamos necesariamente tres nodos distintos.
# Eso también es un resultado.

nodo_mayor_grado <- tabla_final$nodo[
  which.max(tabla_final$grado)
]

nodo_mayor_intermediacion <- tabla_final$nodo[
  which.max(tabla_final$intermediacion)
]

nodo_mayor_participacion <- tabla_final$nodo[
  which.max(tabla_final$participacion)
]

nodos_seleccionados <- unique(
  c(
    nodo_mayor_grado,
    nodo_mayor_intermediacion,
    nodo_mayor_participacion
  )
)

tabla_final[
  tabla_final$nodo %in% nodos_seleccionados,
]


# ==============================================================================
# 28. EJERCICIO
# ==============================================================================

# A partir de la tabla_final y de las visualizaciones:
#
# A. Identifique los cinco nodos principales según:
#    - grado
#    - intermediación
#    - cercanía
#    - eigenvector
#
# B. Explique por qué los rankings difieren.
#
# C. Compare Louvain, Walktrap y Greedy modularity:
#    - número de comunidades
#    - modularidad
#    - nodos cuya asignación cambia
#
# D. Identifique:
#    - un nodo central dentro de su comunidad
#    - un broker entre comunidades
#    - un nodo periférico
#
# E. Examine el embedding node2vec:
#    - ¿qué nodos aparecen próximos?
#    - ¿esa proximidad refleja comunidad, centralidad o rol?
#
# F. Cambie los parámetros de node2vec:
#
#    p = 1, q = 4
#
#    y después:
#
#    p = 1, q = 0.25
#
#    Compare los embeddings.
#
# G. Explique por qué modificar q cambia el tipo de estructura
#    que las caminatas tienden a explorar.


# ==============================================================================
# 29. EXTENSIÓN: COMPARAR DOS NODE2VEC CON DISTINTO q
# ==============================================================================

# Caso A: exploración más local.

set.seed(1234)

emb_local <- node2vecR(
  edges_node2vec,
  p = 1,
  q = 4,
  directed = "undirected",
  num_walks = 20,
  walk_length = 20,
  dim = 8
)


# Caso B: exploración más hacia afuera.

set.seed(1234)

emb_exploracion <- node2vecR(
  edges_node2vec,
  p = 1,
  q = 0.25,
  directed = "undirected",
  num_walks = 20,
  walk_length = 20,
  dim = 8
)


# Reducimos ambos a dos dimensiones para observarlos.

pca_local <- prcomp(
  emb_local,
  center = TRUE,
  scale. = FALSE
)

pca_exploracion <- prcomp(
  emb_exploracion,
  center = TRUE,
  scale. = FALSE
)

coords_local <- pca_local$x[, 1:2]
coords_exploracion <- pca_exploracion$x[, 1:2]


# Graficamos por separado.

plot(
  coords_local[, 1],
  coords_local[, 2],
  type = "n",
  xlab = "PC1",
  ylab = "PC2",
  main = "Node2vec: q = 4"
)

text(
  coords_local[, 1],
  coords_local[, 2],
  labels = rownames(emb_local)
)


plot(
  coords_exploracion[, 1],
  coords_exploracion[, 2],
  type = "n",
  xlab = "PC1",
  ylab = "PC2",
  main = "Node2vec: q = 0.25"
)

text(
  coords_exploracion[, 1],
  coords_exploracion[, 2],
  labels = rownames(emb_exploracion)
)


