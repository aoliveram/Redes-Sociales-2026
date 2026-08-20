# compara_redes.py — Compara tres modelos clásicos de red
# Uso (desde la carpeta raíz del repositorio):
#   python3 libro-redes-sociales-2026/scripts/compara_redes.py
# Requiere: pip install networkx

import csv

import networkx as nx

n, m = 200, 600  # nodos y aristas (aprox. las mismas en los tres modelos)

redes = {
    "aleatoria": nx.gnm_random_graph(n, m, seed=2026),            # Erdős–Rényi G(n,m)
    "mundo_pequeno": nx.watts_strogatz_graph(n, 6, 0.05, seed=2026),  # Watts–Strogatz
    "libre_escala": nx.barabasi_albert_graph(n, 3, seed=2026),    # Barabási–Albert
}

filas = []
for nombre, G in redes.items():
    filas.append({
        "red": nombre,
        "nodos": G.number_of_nodes(),
        "aristas": G.number_of_edges(),
        "densidad": round(nx.density(G), 4),
        "transitividad": round(nx.transitivity(G), 4),
        "dist_media": round(nx.average_shortest_path_length(G), 2),
        "grado_max": max(d for _, d in G.degree()),
    })

for fila in filas:
    print(fila)

salida = "libro-redes-sociales-2026/outputs/comparacion_redes_python.csv"
with open(salida, "w", newline="") as f:
    w = csv.DictWriter(f, fieldnames=filas[0].keys())
    w.writeheader()
    w.writerows(filas)

print(f"\nGuardado en {salida}")
