## ---------------------------------------------------------------------------
## Analisis de red de nominaciones (generador de nombres)
## Fuente: libro-redes-sociales-2026/datos/encuesta_nominaciones.xlsx
##
## 1. Matriz de adyacencia dirigida a partir de nom_1..nom_7
## 2. Densidad, grado de entrada/salida, reciprocidad y balance
## 3. ERGM: edges + mutual + gwesp + nodematch(disciplina) + nodematch(sexo)
## 4. Interpretacion cautelosa de coeficientes
## ---------------------------------------------------------------------------

library(readxl)
library(igraph)
library(network)
library(sna)
library(ergm)

set.seed(20260819)

ruta <- "libro-redes-sociales-2026/datos/encuesta_nominaciones.xlsx"
df <- read_excel(ruta)

# -----------------------------------------------------------------------
# 0. Atributos de los actores y validacion
# -----------------------------------------------------------------------
actores <- df[, c("nombre", "sexo", "edad", "disciplina")]
actores$disciplina <- factor(actores$disciplina)
actores$sexo <- factor(actores$sexo)

cols_nom <- paste0("nom_", 1:7)
stopifnot(all(cols_nom %in% names(df)))

# lista de aristas en formato largo (ego -> alter), quitando NA
edges_largo <- do.call(rbind, lapply(cols_nom, function(cn) {
  data.frame(from = df$nombre, to = df[[cn]], stringsAsFactors = FALSE)
}))
edges_largo <- edges_largo[!is.na(edges_largo$to), ]

# todos los nominados deben pertenecer al set de 30 actores (ya validado)
stopifnot(all(edges_largo$to %in% actores$nombre))
stopifnot(!any(edges_largo$from == edges_largo$to))  # sin autonominaciones

cat("N actores:", nrow(actores), "\n")
cat("N nominaciones (aristas dirigidas, sin duplicar):", nrow(edges_largo), "\n")

# -----------------------------------------------------------------------
# 1. Matriz de adyacencia dirigida
# -----------------------------------------------------------------------
g <- graph_from_data_frame(edges_largo, directed = TRUE, vertices = actores)

adj <- as_adjacency_matrix(g, sparse = FALSE)
adj <- adj[actores$nombre, actores$nombre]  # fijar orden segun encuesta

cat("\n--- Matriz de adyacencia (dirigida) ---\n")
cat("Dimensiones:", paste(dim(adj), collapse = " x "), "\n")
cat("Suma de la matriz (= n aristas):", sum(adj), "\n")

# -----------------------------------------------------------------------
# 2. Densidad, grados, reciprocidad y balance
# -----------------------------------------------------------------------
n <- igraph::vcount(g)
n_aristas <- igraph::ecount(g)
densidad <- igraph::edge_density(g, loops = FALSE)

grado_salida <- igraph::degree(g, mode = "out")
grado_entrada <- igraph::degree(g, mode = "in")

tabla_grados <- data.frame(
  nombre = actores$nombre,
  disciplina = actores$disciplina,
  sexo = actores$sexo,
  grado_salida = grado_salida[actores$nombre],
  grado_entrada = grado_entrada[actores$nombre]
)
tabla_grados <- tabla_grados[order(-tabla_grados$grado_entrada), ]

cat("\n--- Densidad ---\n")
cat(sprintf("Densidad = %.4f  (n aristas = %d de %d pares posibles)\n",
            densidad, n_aristas, n * (n - 1)))

cat("\n--- Grado de entrada / salida (ordenado por grado de entrada) ---\n")
print(tabla_grados, row.names = FALSE)

cat("\n--- Resumen de grados ---\n")
cat("Grado de salida: media =", round(mean(grado_salida), 2),
    " sd =", round(sd(grado_salida), 2), "\n")
cat("Grado de entrada: media =", round(mean(grado_entrada), 2),
    " sd =", round(sd(grado_entrada), 2), "\n")

# reciprocidad: proporcion de diadas mutuas sobre diadas con al menos una arista
recip_edgewise <- reciprocity(g)  # igraph: proporcion de aristas que son reciprocas

# censo diadico (mutuas, asimetricas, nulas) via sna, y reciprocidad "dyadic"
net_sna <- network(adj, directed = TRUE, matrix.type = "adjacency")
dyad_cens <- sna::dyad.census(net_sna)
recip_dyadic <- sna::grecip(net_sna, measure = "dyadic")   # (mutuas+nulas)/total diadas
recip_dyadic_nonnull <- sna::grecip(net_sna, measure = "dyadic.nonnull")  # mutuas/(mutuas+asimetricas)

cat("\n--- Reciprocidad y balance diadico ---\n")
cat("Reciprocidad 'edgewise' (igraph):", round(recip_edgewise, 3),
    "-> de todas las aristas dirigidas, esta proporcion tiene su reciproca\n")
cat("Censo diadico (mutuas, asimetricas, nulas):\n")
print(dyad_cens)
cat("Reciprocidad diadica (mutuas+nulas / total diadas):", round(recip_dyadic, 3), "\n")
cat("Reciprocidad diadica no-nula (mutuas / (mutuas+asimetricas)):",
    round(recip_dyadic_nonnull, 3), "\n")

# balance/transitividad como indicador estructural adicional (no es balance
# estructural formal de Heider, que requiere lazos signados; aqui se reporta
# la transitividad dirigida como proxy descriptivo, se detalla en la
# interpretacion)
transitividad <- transitivity(g, type = "global")
cat("Transitividad global (proxy descriptivo, no balance signado):",
    round(transitividad, 3), "\n")

# -----------------------------------------------------------------------
# 3. ERGM
# -----------------------------------------------------------------------
net <- network(adj, directed = TRUE, matrix.type = "adjacency")
network::set.vertex.attribute(net, "disciplina", as.character(actores$disciplina))
network::set.vertex.attribute(net, "sexo", as.character(actores$sexo))
network::set.vertex.attribute(net, "edad", actores$edad)

modelo <- ergm(
  net ~ edges +
    mutual +
    gwesp(decay = 0.5, fixed = TRUE) +
    nodematch("disciplina") +
    nodematch("sexo"),
  control = control.ergm(
    MCMLE.maxit = 40,
    MCMC.burnin = 20000,
    MCMC.interval = 2000,
    seed = 20260819
  )
)

cat("\n--- Resumen del ERGM ---\n")
print(summary(modelo))

cat("\n--- Odds ratios (exp(coef)) ---\n")
print(round(exp(coef(modelo)), 3))

cat("\n--- Diagnostico de convergencia MCMC ---\n")
mcmc_diag <- tryCatch(mcmc.diagnostics(modelo, which = "plots" ), error = function(e) e)

cat("\n--- Bondad de ajuste (grado entrada/salida, ESP, distancia geodesica) ---\n")
gof_modelo <- tryCatch(
  gof(modelo, GOF = ~ idegree + odegree + espartners + distance),
  error = function(e) {
    cat("gof() fallo:", conditionMessage(e), "\n")
    NULL
  }
)
if (!is.null(gof_modelo)) {
  print(gof_modelo)
}

cat("\n--- Fin del script ---\n")
