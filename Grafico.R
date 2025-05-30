#Leer archivos.

nodos <- read.csv("nodos.csv")
aristas <- read.csv("aristas.csv")

#Preparar nodos.
library(dplyr)

nodos_vis <- nodos %>%
  mutate(
    label = nombre,             # Mostrar el nombre completo del curso
    value = creditos,           # Tamaño del nodo
    group = area,               # Agrupar por disciplina/área
    shape = ifelse(bloque <= 2, "dot", "square"),  # Ejemplo: dots para primeros bloques
    color = case_when(          # Color según cantidad de horas
      horas <= 2 ~ "lightblue",
      horas <= 4 ~ "skyblue",
      horas <= 6 ~ "deepskyblue",
      TRUE ~ "dodgerblue"
    )
  ) %>%
  select(id, label, group, shape, color, value)

#Preparar aristas.

aristas_vis <- aristas %>%
  rename(from = NECESITA, to = MATERIA) %>%
  mutate(
    arrows = ifelse(TIPO == "REQUISITO", "to", ""),  # Requisito = flecha, Correq = sin flecha
    dashes = TIPO == "CORREQUISITO"
  )

# Corequisitos bidireccionales (doble entrada)
coreq <- aristas_vis %>%
  filter(dashes == TRUE) %>%
  mutate(tmp = from, from = to, to = tmp) %>%
  select(-tmp)

edges_vis <- bind_rows(aristas_vis, coreq)

#Grafico
library(visNetwork)
print(
visNetwork(nodos_vis, edges_vis) %>%
  visEdges(smooth = FALSE) %>%
  visNodes(font = list(size = 14)) %>%
  visOptions(
    highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
    selectedBy = list(variable = "group", multiple = TRUE),
    nodesIdSelection = TRUE
  ) %>%
  visInteraction(navigationButtons = TRUE) %>%
  visLayout(randomSeed = 123)
)

#Nota: Bueno agregue un print aquí