#Leer archivos.

nodos <- read.csv("nodos.csv")
aristas <- read.csv("aristas.csv")

#Preparar nodos.
library(dplyr)

nodos_vis <- nodos %>%
  mutate(
    label = nombre,             
    value = creditos,           
    group = area,               
    shape = case_when(
      bloque == 1 ~ "dot",
      bloque == 2 ~ "square",
      bloque == 3 ~ "triangle",
      bloque == 4 ~ "star",
      bloque == 5 ~ "diamond",
      bloque == 6 ~ "ellipse",
      TRUE ~ "box"  # para bloques mayores a 6
    ),
    color = case_when(
      horas == 0 ~ "#ffe0e0",  # rosa claro
      horas == 1 ~ "#ffd699",  # naranja claro
      horas == 2 ~ "#ffff99",  # amarillo claro
      horas == 3 ~ "#c8facc",  # verde menta
      horas == 4 ~ "#80deea",  # celeste medio
      horas == 5 ~ "#4fc3f7",  # celeste oscuro
      horas == 6 ~ "#0288d1",  # azul profundo
      TRUE ~ "gray"            # fallback color
    ),
    title = paste0(
      "<b>", nombre, "</b><br>",
      "Sigla: ", id, "<br>",
      "Bloque: ", bloque, "<br>",
      "Horas: ", horas, "<br>",
      "Créditos: ", creditos, "<br>",
      "Área: ", area
    )
  ) %>%
  select(id, label, group, shape, color, value, title)


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

grafico <- visNetwork(nodos_vis, edges_vis) %>%
  visEdges(smooth = FALSE) %>%
  visNodes(font = list(size = 14)) %>%
  visOptions(
    highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
    selectedBy = list(variable = "group", multiple = TRUE),
    nodesIdSelection = TRUE
  ) %>%
  visInteraction(navigationButtons = TRUE) %>%
  visLayout(randomSeed = 123)

grafico  # Grafico para ponerlo bien en el RMarkdown


#Nota: ya quedo con mas

