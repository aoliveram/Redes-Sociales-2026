# compara_redes.R — Compara tres modelos clásicos de red
# Uso (desde la carpeta raíz del repositorio):
#   Rscript libro-redes-sociales-2026/scripts/compara_redes.R

suppressPackageStartupMessages(library(igraph))
set.seed(2026)

n <- 200   # nodos
m <- 600   # aristas (aprox. las mismas en los tres modelos)

redes <- list(
  aleatoria     = sample_gnm(n, m),                      # Erdős–Rényi G(n,m)
  mundo_pequeno = sample_smallworld(1, n, 3, p = 0.05),  # Watts–Strogatz
  libre_escala  = sample_pa(n, m = 3, directed = FALSE)  # Barabási–Albert
)

resumen <- data.frame(
  red           = names(redes),
  nodos         = sapply(redes, vcount),
  aristas       = sapply(redes, ecount),
  densidad      = round(sapply(redes, edge_density), 4),
  transitividad = round(sapply(redes, transitivity), 4),
  dist_media    = round(sapply(redes, mean_distance), 2),
  grado_max     = sapply(redes, function(g) max(degree(g)))
)

print(resumen, row.names = FALSE)

salida <- "libro-redes-sociales-2026/outputs/comparacion_redes_R.csv"
write.csv(resumen, salida, row.names = FALSE)
cat("\nGuardado en", salida, "\n")
