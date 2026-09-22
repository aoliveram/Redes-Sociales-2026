# ==============================================================================
# DCCS - TEORÍA DE REDES
# Jorge Fábrega
#
# SESIÓN 6
# CONTAGIO SIMPLE Y MODELO SIR SOBRE REDES
# ==============================================================================
#
# Objetivos:
#
# 1. Construir dos redes con la misma cantidad de nodos.
# 2. Representar los estados S, I y R.
# 3. Entender manualmente una iteración del modelo SIR.
# 4. Automatizar posteriormente exactamente ese mismo mecanismo.
# 5. Reconstruir visualmente la dinámica período a período.
# 6. Comparar cómo cambia el contagio según:
#       - la topología;
#       - el nodo donde comienza;
#       - beta;
#       - gamma.
#
# IMPORTANTE:
#
# Primero construiremos UNA iteración manualmente.
# Sólo después automatizaremos el proceso.
#
# La idea es entender qué está haciendo R antes de pedirle
# que lo repita muchas veces.
#
# ==============================================================================
rm(list=ls())

library(igraph)

set.seed(1234)


# ==============================================================================
# 1. PARÁMETROS GENERALES DEL EXPERIMENTO
# ==============================================================================

n <- 24


# ------------------------------------------------------------------------------
# Probabilidad de transmisión
# ------------------------------------------------------------------------------

beta <- 0.25

# Ojo:
# beta representa la probabilidad de transmisión POR CONTACTO I-S.
#
# Es decir, un nodo infectado no contagia automáticamente a todos sus vecinos.
# Cada vínculo entre un infectado y un susceptible constituye una oportunidad
# independiente de transmisión.
#
# Aquí es donde pueden después jugar con otros valores para ver qué pasa.
#
# Prueben, por ejemplo:
#
# beta <- 0.10
# beta <- 0.40
# beta <- 0.70


# ------------------------------------------------------------------------------
# Probabilidad de recuperación
# ------------------------------------------------------------------------------

gamma <- 0.15

# gamma representa la probabilidad de que un nodo infectado
# pase a recuperado durante un período.
#
# Prueben después:
#
# gamma <- 0.05
# gamma <- 0.30
# gamma <- 0.60
#
# Pregunta:
# ¿Qué debería ocurrir si aumentamos gamma manteniendo beta constante?


# ------------------------------------------------------------------------------
# Número máximo de períodos
# ------------------------------------------------------------------------------

T_max <- 20


# ------------------------------------------------------------------------------
# Nodo donde comienza la epidemia
# ------------------------------------------------------------------------------

nodo_inicial <- 1

# Ojo:
# esta será una de nuestras principales variables experimentales.
#
# Después pueden cambiar solamente esto:
#
# nodo_inicial <- 5
# nodo_inicial <- 12
# nodo_inicial <- 20
#
# y observar qué cambia manteniendo TODO lo demás constante.


# ==============================================================================
# 2. CONSTRUIR DOS TOPOLOGÍAS
# ==============================================================================


# ==============================================================================
# 2.1 RED ALEATORIA
# ==============================================================================

set.seed(100)

# Generaremos la red hasta obtener una red conectada.
#
# ¿Por qué?
# Porque queremos que, en principio, exista algún camino
# entre cualquier par de nodos.
#
# Si hubiera componentes separados, algunos nodos serían
# estructuralmente imposibles de alcanzar desde la semilla inicial.

repeat {
  
  g_aleatoria <- sample_gnp(
    n = n,
    p = 0.14,
    directed = FALSE,
    loops = FALSE
  )
  
  if (is_connected(g_aleatoria)) {
    break
  }
}


# Revisemos si efectivamente tenemos un solo componente

components(g_aleatoria)$no


# Visualización

plot(
  g_aleatoria,
  vertex.label = V(g_aleatoria)$name,
  vertex.size = 25,
  main = "Topología A: red aleatoria"
)


# ------------------------------------------------------------------------------
# Algunas características básicas
# ------------------------------------------------------------------------------

degree(g_aleatoria)

mean(degree(g_aleatoria))


# Ojo:
# aquí estamos recordando algo visto en sesiones anteriores.
#
# El proceso dinámico que estudiaremos ahora ocurre SOBRE
# una estructura que ya podemos describir mediante grado,
# centralidad, componentes, etc.


# ==============================================================================
# 2.2 RED CON COMUNIDADES
# ==============================================================================

set.seed(200)


# Tres comunidades de ocho nodos

tam_comunidades <- c(8, 8, 8)


# Probabilidad de vínculos dentro de comunidades

p_in <- 0.40


# Probabilidad de vínculos entre comunidades

p_out <- 0.03


# Ojo:
# aquí también pueden experimentar.
#
# Por ejemplo, ¿qué ocurriría si hacemos:
#
# p_out <- 0.01
#
# ¿Y si hacemos:
#
# p_out <- 0.15
#
# Estamos modificando cuán aisladas están las comunidades entre sí.


P <- matrix(
  p_out,
  nrow = 3,
  ncol = 3
)

diag(P) <- p_in


# Generamos hasta obtener una red conectada

repeat {
  
  g_comunidades <- sample_sbm(
    n = n,
    pref.matrix = P,
    block.sizes = tam_comunidades,
    directed = FALSE,
    loops = FALSE
  )
  
  if (is_connected(g_comunidades)) {
    break
  }
}


components(g_comunidades)$no


plot(
  g_comunidades,
  vertex.label = V(g_comunidades)$name,
  vertex.size = 25,
  main = "Topología B: red con comunidades"
)


degree(g_comunidades)

mean(degree(g_comunidades))


# ==============================================================================
# 2.3 OBSERVAR AMBAS REDES JUNTAS
# ==============================================================================

par(mfrow = c(1, 2))

plot(
  g_aleatoria,
  vertex.label = V(g_aleatoria)$name,
  vertex.size = 22,
  main = "Red aleatoria"
)

plot(
  g_comunidades,
  vertex.label = V(g_comunidades)$name,
  vertex.size = 22,
  main = "Red con comunidades"
)

par(mfrow = c(1, 1))


# Pregunta para antes de simular:
#
# Si beta y gamma fueran exactamente iguales en ambas redes,
# ¿esperarían que la epidemia se comportara igual?
#
# ¿Por qué?


# ==============================================================================
# 3. ESTADOS DEL MODELO SIR
# ==============================================================================

# Utilizaremos tres estados:
#
# S = Susceptible
# I = Infectado
# R = Recuperado


estado_inicial <- rep("S", n)

estado_inicial[nodo_inicial] <- "I"

estado_inicial


# Tenemos entonces:
#
# 23 susceptibles
# 1 infectado
# 0 recuperados


table(estado_inicial)


# ==============================================================================
# 4. UNA ITERACIÓN SIR PASO A PASO
# ==============================================================================

# Antes de automatizar el modelo vamos a construir
# manualmente UN período.
#
# Para esta demostración usaremos deliberadamente DOS infectados.
#
# La razón es puramente pedagógica:
# queremos tener una probabilidad razonable de observar simultáneamente
# contagios, recuperaciones y nodos que continúan infectados.


# ------------------------------------------------------------------------------
# 4.1 Estado inicial especial para la demostración
# ------------------------------------------------------------------------------

estado <- rep("S", n)

estado[c(1, 5)] <- "I"

estado

table(estado)


# Ojo:
# esta modificación corresponde SÓLO a esta demostración manual.
#
# Cuando hagamos la simulación completa volveremos a utilizar
# un único nodo inicial.


# ------------------------------------------------------------------------------
# 4.2 Fijar semilla para que el ejemplo sea reproducible
# ------------------------------------------------------------------------------

set.seed(123)

# Aquí fijamos deliberadamente el azar.
#
# Queremos que todos veamos exactamente la misma realización
# mientras aprendemos el mecanismo.
#
# Más adelante permitiremos que el proceso vuelva a ser estocástico.


# ------------------------------------------------------------------------------
# 4.3 Identificar infectados
# ------------------------------------------------------------------------------

infectados <- which(
  estado == "I"
)

infectados


# En este momento los únicos nodos capaces de transmitir
# son los que aparecen en este vector.


# ==============================================================================
# 4.4 BUSCAR VECINOS SUSCEPTIBLES
# ==============================================================================

for (i in infectados) {
  
  vecinos_i <- neighbors(
    g_aleatoria,
    i
  )
  
  vecinos_i <- as.integer(
    vecinos_i
  )
  
  susceptibles_i <- vecinos_i[
    estado[vecinos_i] == "S"
  ]
  
  cat(
    "\nNodo infectado:",
    i,
    "\n"
  )
  
  cat(
    "Vecinos susceptibles:",
    susceptibles_i,
    "\n"
  )
}


# Ojo:
# beta todavía NO ha actuado.
#
# Hasta aquí sólo hemos usado la topología para determinar
# quién puede potencialmente contagiar a quién.


# ==============================================================================
# 4.5 TRANSMISIÓN
# ==============================================================================

nuevos_infectados <- c()


for (i in infectados) {
  
  vecinos_i <- as.integer(
    neighbors(
      g_aleatoria,
      i
    )
  )
  
  susceptibles_i <- vecinos_i[
    estado[vecinos_i] == "S"
  ]
  
  
  for (j in susceptibles_i) {
    
    # ---------------------------------------------------------------
    # Sorteo Bernoulli
    # ---------------------------------------------------------------
    #
    # 1 = transmisión
    # 0 = no transmisión
    
    contagio <- rbinom(
      n = 1,
      size = 1,
      prob = beta
    )
    
    
    cat(
      "Contacto",
      i,
      "->",
      j,
      ":",
      contagio,
      "\n"
    )
    
    
    if (contagio == 1) {
      
      nuevos_infectados <- c(
        nuevos_infectados,
        j
      )
    }
  }
}


# Un susceptible podría haber sido expuesto
# por más de un vecino infectado.

nuevos_infectados <- unique(
  nuevos_infectados
)


# Resultado pedagógicamente más claro que imprimir NULL

if (length(nuevos_infectados) == 0) {
  
  cat(
    "\nEn este período no hubo nuevos contagios.\n"
  )
  
} else {
  
  cat(
    "\nNuevos infectados:",
    nuevos_infectados,
    "\n"
  )
}


# Ojo:
# beta se aplica a CONTACTOS I-S.
#
# Esta es una entrada explícita de la estructura de red
# en el proceso epidémico.


# ==============================================================================
# 4.6 RECUPERACIÓN
# ==============================================================================

recuperados <- c()


for (i in infectados) {
  
  recuperacion <- rbinom(
    n = 1,
    size = 1,
    prob = gamma
  )
  
  
  cat(
    "Recuperación del nodo",
    i,
    ":",
    recuperacion,
    "\n"
  )
  
  
  if (recuperacion == 1) {
    
    recuperados <- c(
      recuperados,
      i
    )
  }
}


if (length(recuperados) == 0) {
  
  cat(
    "\nEn este período ningún infectado se recuperó.\n"
  )
  
} else {
  
  cat(
    "\nNodos que se recuperaron:",
    recuperados,
    "\n"
  )
}


# Ojo:
# recuperación y contagio son procesos distintos.
#
# beta controla I -> S
# gamma controla I -> R


# ==============================================================================
# 4.7 ACTUALIZACIÓN SIMULTÁNEA
# ==============================================================================

estado_nuevo <- estado


estado_nuevo[
  nuevos_infectados
] <- "I"


estado_nuevo[
  recuperados
] <- "R"


estado_nuevo

table(estado_nuevo)


# IMPORTANTE:
#
# estamos actualizando todos los estados al FINAL del período.
#
# Un nodo que se contagia durante t NO empieza inmediatamente
# a contagiar dentro de ese mismo período.
#
# Comenzará a hacerlo en t + 1.
#
# Esto evita que el orden del loop determine artificialmente
# el resultado de la epidemia.


# ==============================================================================
# 5. AUTOMATIZAR EL MISMO PROCESO
# ==============================================================================

# Ahora vamos a encapsular exactamente el mecanismo anterior
# dentro de una función.
#
# No estamos introduciendo un modelo nuevo.
#
# Estamos pidiéndole a R que repita automáticamente
# lo que acabamos de hacer manualmente.


simular_SIR <- function(
    g,
    nodo_inicial,
    beta,
    gamma,
    T_max = 20,
    seed = NULL
) {
  
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  
  n <- vcount(g)
  
  
  # ---------------------------------------------------------------------------
  # Estado inicial
  # ---------------------------------------------------------------------------
  
  estado <- rep(
    "S",
    n
  )
  
  estado[
    nodo_inicial
  ] <- "I"
  
  
  # Aquí guardaremos toda la historia de la epidemia
  
  historia <- list()
  
  historia[[1]] <- estado
  
  
  # ---------------------------------------------------------------------------
  # Iteraciones
  # ---------------------------------------------------------------------------
  
  for (t in 1:T_max) {
    
    
    infectados <- which(
      estado == "I"
    )
    
    
    # Si ya no existen infectados,
    # el proceso terminó.
    
    if (length(infectados) == 0) {
      break
    }
    
    
    nuevos_infectados <- c()
    
    recuperados <- c()
    
    
    # =========================================================================
    # A. TRANSMISIÓN
    # =========================================================================
    
    for (i in infectados) {
      
      
      vecinos_i <- as.integer(
        neighbors(
          g,
          i
        )
      )
      
      
      susceptibles_i <- vecinos_i[
        estado[vecinos_i] == "S"
      ]
      
      
      for (j in susceptibles_i) {
        
        
        contagio <- rbinom(
          n = 1,
          size = 1,
          prob = beta
        )
        
        
        if (contagio == 1) {
          
          nuevos_infectados <- c(
            nuevos_infectados,
            j
          )
        }
      }
    }
    
    
    nuevos_infectados <- unique(
      nuevos_infectados
    )
    
    
    # =========================================================================
    # B. RECUPERACIÓN
    # =========================================================================
    
    for (i in infectados) {
      
      
      recuperacion <- rbinom(
        n = 1,
        size = 1,
        prob = gamma
      )
      
      
      if (recuperacion == 1) {
        
        recuperados <- c(
          recuperados,
          i
        )
      }
    }
    
    
    # =========================================================================
    # C. ACTUALIZACIÓN SIMULTÁNEA
    # =========================================================================
    
    estado_nuevo <- estado
    
    
    estado_nuevo[
      nuevos_infectados
    ] <- "I"
    
    
    estado_nuevo[
      recuperados
    ] <- "R"
    
    
    estado <- estado_nuevo
    
    
    # =========================================================================
    # D. GUARDAR LA HISTORIA
    # =========================================================================
    
    historia[[length(historia) + 1]] <- estado
  }
  
  
  return(historia)
}


# ==============================================================================
# 6. SIMULAR SOBRE LA RED ALEATORIA
# ==============================================================================

# Volvemos ahora al experimento original:
# UNA sola semilla inicial.


nodo_inicial <- 1


hist_aleatoria <- simular_SIR(
  g = g_aleatoria,
  nodo_inicial = nodo_inicial,
  beta = beta,
  gamma = gamma,
  T_max = T_max,
  seed = 999
)


length(hist_aleatoria)


# Ojo:
# length(hist_aleatoria) indica cuántos estados temporales
# quedaron almacenados.
#
# Puede ser menor que T_max + 1 si la epidemia
# desapareció antes.


# ==============================================================================
# 7. VISUALIZACIÓN DE LA DINÁMICA
# ==============================================================================

# Fijamos un layout.
#
# Esto es importante porque queremos que cada nodo permanezca
# aproximadamente en el mismo lugar en todos los períodos.
#
# De lo contrario, parecería que la red cambia,
# cuando en realidad sólo están cambiando los estados.


set.seed(500)

layout_A <- layout_with_fr(
  g_aleatoria
)


# ------------------------------------------------------------------------------
# Colores para los estados
# ------------------------------------------------------------------------------

colores_estado <- function(estado) {
  
  ifelse(
    estado == "S",
    "lightgray",
    ifelse(
      estado == "I",
      "tomato",
      "lightblue"
    )
  )
}


# ==============================================================================
# 7.1 t = 0
# ==============================================================================

plot(
  g_aleatoria,
  layout = layout_A,
  vertex.color = colores_estado(
    hist_aleatoria[[1]]
  ),
  vertex.size = 28,
  vertex.label = V(g_aleatoria)$name,
  main = "Red aleatoria: t = 0"
)


legend(
  "topleft",
  legend = c(
    "Susceptible",
    "Infectado",
    "Recuperado"
  ),
  pch = 21,
  pt.bg = c(
    "lightgray",
    "tomato",
    "lightblue"
  ),
  pt.cex = 2,
  bty = "n"
)


# ==============================================================================
# 7.2 t = 1
# ==============================================================================

if (length(hist_aleatoria) >= 2) {
  
  plot(
    g_aleatoria,
    layout = layout_A,
    vertex.color = colores_estado(
      hist_aleatoria[[2]]
    ),
    vertex.size = 28,
    vertex.label = V(g_aleatoria)$name,
    main = "Red aleatoria: t = 1"
  )
}


# ==============================================================================
# 7.3 t = 2
# ==============================================================================

if (length(hist_aleatoria) >= 3) {
  
  plot(
    g_aleatoria,
    layout = layout_A,
    vertex.color = colores_estado(
      hist_aleatoria[[3]]
    ),
    vertex.size = 28,
    vertex.label = V(g_aleatoria)$name,
    main = "Red aleatoria: t = 2"
  )
}


# Pregunta:
#
# Antes de ejecutar el período siguiente,
# observen los nodos infectados.
#
# ¿Qué nodos tienen alguna posibilidad de contagiarse?
#
# La respuesta debe poder deducirse mirando solamente
# los vecinos de los infectados actuales.


# ==============================================================================
# 8. PELÍCULA DEL CONTAGIO
# ==============================================================================

for (t in seq_along(hist_aleatoria)) {
  
  estado_t <- hist_aleatoria[[t]]
  
  
  plot(
    g_aleatoria,
    layout = layout_A,
    vertex.color = colores_estado(
      estado_t
    ),
    vertex.size = 28,
    vertex.label = V(g_aleatoria)$name,
    main = paste0(
      "Red aleatoria: t = ",
      t - 1
    )
  )
  
  
  legend(
    "topleft",
    legend = c(
      "S",
      "I",
      "R"
    ),
    pch = 21,
    pt.bg = c(
      "lightgray",
      "tomato",
      "lightblue"
    ),
    pt.cex = 1.5,
    bty = "n"
  )
  
  
  Sys.sleep(0.8)
}


# Ojo:
# lo que estamos viendo es UNA realización posible del proceso.
#
# Si cambiamos la semilla aleatoria,
# podemos obtener otra trayectoria aun manteniendo:
#
#   - la misma red;
#   - el mismo beta;
#   - el mismo gamma;
#   - el mismo nodo inicial.


# ==============================================================================
# 9. CONTAR S, I Y R EN CADA PERÍODO
# ==============================================================================

resumen_SIR <- function(historia) {
  
  do.call(
    rbind,
    lapply(
      seq_along(historia),
      function(t) {
        
        estados <- historia[[t]]
        
        data.frame(
          t = t - 1,
          S = sum(
            estados == "S"
          ),
          I = sum(
            estados == "I"
          ),
          R = sum(
            estados == "R"
          )
        )
      }
    )
  )
}


serie_A <- resumen_SIR(
  hist_aleatoria
)


serie_A


# ==============================================================================
# 9.1 CURVA SIR
# ==============================================================================

matplot(
  serie_A$t,
  serie_A[, c(
    "S",
    "I",
    "R"
  )],
  type = "l",
  lty = 1,
  lwd = 3,
  xlab = "Tiempo",
  ylab = "Número de nodos"
)


legend(
  "right",
  legend = c(
    "S",
    "I",
    "R"
  ),
  lty = 1,
  lwd = 3
)


# Aquí estamos observando el mismo proceso de dos maneras:
#
# 1. la película sobre la red;
# 2. la evolución agregada de S, I y R.
#
# La primera muestra DÓNDE ocurre el contagio.
# La segunda muestra CUÁNTO contagio hay en cada período.


# ==============================================================================
# 10. REPETIR SOBRE LA RED CON COMUNIDADES
# ==============================================================================

hist_comunidades <- simular_SIR(
  g = g_comunidades,
  nodo_inicial = nodo_inicial,
  beta = beta,
  gamma = gamma,
  T_max = T_max,
  seed = 999
)


set.seed(500)

layout_B <- layout_with_fr(
  g_comunidades
)


for (t in seq_along(hist_comunidades)) {
  
  estado_t <- hist_comunidades[[t]]
  
  
  plot(
    g_comunidades,
    layout = layout_B,
    vertex.color = colores_estado(
      estado_t
    ),
    vertex.size = 28,
    vertex.label = V(g_comunidades)$name,
    main = paste0(
      "Red con comunidades: t = ",
      t - 1
    )
  )
  
  
  legend(
    "topleft",
    legend = c(
      "S",
      "I",
      "R"
    ),
    pch = 21,
    pt.bg = c(
      "lightgray",
      "tomato",
      "lightblue"
    ),
    pt.cex = 1.5,
    bty = "n"
  )
  
  
  Sys.sleep(0.8)
}


# ==============================================================================
# 11. RESUMIR LA RED CON COMUNIDADES
# ==============================================================================

serie_B <- resumen_SIR(
  hist_comunidades
)


serie_B


# ==============================================================================
# 12. TAMAÑO FINAL DEL CONTAGIO
# ==============================================================================

alcanzados_A <- sum(
  tail(
    hist_aleatoria,
    1
  )[[1]] != "S"
)


alcanzados_B <- sum(
  tail(
    hist_comunidades,
    1
  )[[1]] != "S"
)


alcanzados_A

alcanzados_B


cat(
  "\nRed aleatoria:",
  alcanzados_A,
  "nodos alcanzados de",
  n,
  "\n"
)


cat(
  "Red con comunidades:",
  alcanzados_B,
  "nodos alcanzados de",
  n,
  "\n"
)


# Ojo:
# con una única simulación todavía NO podemos concluir
# que una topología produce sistemáticamente más difusión.
#
# Estamos mirando una realización particular.
#
# Más adelante podríamos repetir el proceso cientos de veces
# y comparar distribuciones del tamaño final de la epidemia.


# ==============================================================================
# 13. ¿LA EPIDEMIA TERMINÓ?
# ==============================================================================

estado_final_A <- tail(
  hist_aleatoria,
  1
)[[1]]


estado_final_B <- tail(
  hist_comunidades,
  1
)[[1]]


sum(
  estado_final_A == "I"
)


sum(
  estado_final_B == "I"
)


# Si todavía existen nodos I, llegar a T_max
# simplemente interrumpió nuestra observación.
#
# Si I = 0, entonces la epidemia efectivamente se extinguió.


# ==============================================================================
# 14. EXPERIMENTO: CAMBIAR LA SEMILLA
# ==============================================================================

# Hasta aquí hemos mantenido:
#
# beta constante
# gamma constante
# topología constante
#
# Ahora vamos a modificar solamente el nodo inicial.


nodo_inicial <- 1


hist_1 <- simular_SIR(
  g = g_comunidades,
  nodo_inicial = nodo_inicial,
  beta = beta,
  gamma = gamma,
  T_max = T_max,
  seed = 500
)


nodo_inicial <- 8


hist_8 <- simular_SIR(
  g = g_comunidades,
  nodo_inicial = nodo_inicial,
  beta = beta,
  gamma = gamma,
  T_max = T_max,
  seed = 500
)


nodo_inicial <- 16


hist_16 <- simular_SIR(
  g = g_comunidades,
  nodo_inicial = nodo_inicial,
  beta = beta,
  gamma = gamma,
  T_max = T_max,
  seed = 500
)


# ==============================================================================
# 14.1 COMPARAR TAMAÑO FINAL
# ==============================================================================

c(
  semilla_1 = sum(
    tail(
      hist_1,
      1
    )[[1]] != "S"
  ),
  
  semilla_8 = sum(
    tail(
      hist_8,
      1
    )[[1]] != "S"
  ),
  
  semilla_16 = sum(
    tail(
      hist_16,
      1
    )[[1]] != "S"
  )
)


# Pregunta:
#
# Si beta y gamma son exactamente iguales,
# ¿por qué puede cambiar el resultado?
#
# Aquí empieza a aparecer explícitamente
# la posición estructural de la semilla.


# ==============================================================================
# 15. ELEGIR SEMILLAS USANDO CENTRALIDAD
# ==============================================================================


# ------------------------------------------------------------------------------
# 15.1 Nodo de mayor grado
# ------------------------------------------------------------------------------

grado <- degree(
  g_comunidades
)


grado


nodo_mayor_grado <- which.max(
  grado
)


nodo_mayor_grado


# Prueba:
#
# usa ahora:
#
# nodo_inicial <- nodo_mayor_grado
#
# ¿La epidemia crece más rápidamente?
# ¿Alcanza más nodos?
#
# Ojo: una única simulación no basta para afirmar
# que necesariamente será así.


# ------------------------------------------------------------------------------
# 15.2 Nodo de mayor intermediación
# ------------------------------------------------------------------------------

intermediacion <- betweenness(
  g_comunidades,
  directed = FALSE,
  normalized = TRUE
)


intermediacion


nodo_mayor_intermediacion <- which.max(
  intermediacion
)


nodo_mayor_intermediacion


# Desafío:
#
# Prueba calculando la centralidad de intermediación y eligiendo
# el nodo más central como primer infectado.
#
# ¿Qué cambia en el proceso?
#
# ¿Logra el contagio cruzar comunidades con mayor facilidad?
#
# ¿Por qué un nodo que conecta regiones distintas podría ser
# especialmente relevante para una dinámica de difusión?


# ==============================================================================
# 16. EXPERIMENTOS PARA HACER EN CASA
# ==============================================================================


# ------------------------------------------------------------------------------
# EXPERIMENTO 1
# ------------------------------------------------------------------------------
#
# Mantén fija la red y el nodo inicial.
#
# Cambia beta:
#
# beta <- 0.10
# beta <- 0.30
# beta <- 0.60
#
# ¿Qué cambia?


# ------------------------------------------------------------------------------
# EXPERIMENTO 2
# ------------------------------------------------------------------------------
#
# Mantén beta constante.
#
# Cambia gamma:
#
# gamma <- 0.05
# gamma <- 0.30
# gamma <- 0.70
#
# ¿Qué cambia?


# ------------------------------------------------------------------------------
# EXPERIMENTO 3
# ------------------------------------------------------------------------------
#
# Mantén beta y gamma constantes.
#
# Cambia solamente:
#
# nodo_inicial
#
# ¿Qué papel juega la posición estructural de la semilla?


# ------------------------------------------------------------------------------
# EXPERIMENTO 4
# ------------------------------------------------------------------------------
#
# Compara como semillas:
#
# 1. un nodo aleatorio;
# 2. el nodo de mayor grado;
# 3. el nodo de mayor intermediación.
#
# ¿Cuál parece producir mayor difusión?


# ------------------------------------------------------------------------------
# EXPERIMENTO 5
# ------------------------------------------------------------------------------
#
# En la red SBM cambia:
#
# p_out
#
# Prueba:
#
# p_out <- 0.01
# p_out <- 0.05
# p_out <- 0.15
#
# ¿Qué sucede cuando las comunidades quedan
# cada vez más conectadas entre sí?


# ------------------------------------------------------------------------------
# EXPERIMENTO 6
# ------------------------------------------------------------------------------
#
# Ejecuta exactamente el mismo modelo cambiando solamente:
#
# seed
#
# ¿Obtienes siempre el mismo resultado?
#
# Esta es una propiedad fundamental de los modelos
# estocásticos de difusión.


# ==============================================================================
# 17. IDEA CENTRAL
# ==============================================================================

# El resultado que queremos retener es:
#
#
#       REGLA DE TRANSMISIÓN
#                 +
#       ESTRUCTURA DE LA RED
#                 +
#       POSICIÓN DE LA SEMILLA
#                 +
#              AZAR
#
#                 |
#                 v
#
#       DINÁMICA DE DIFUSIÓN
#
#
# Incluso cuando beta y gamma permanecen constantes,
# cambiar la topología o el lugar donde comienza el proceso
# puede modificar profundamente su evolución.
#
# ==============================================================================