# Limpiar todo
rm(list = ls())

# Librer?as
library(ggplot2)
library(dplyr)
library(stringr)
library(cowplot)
library(magick)
library(googlesheets4)
library(lubridate)
library(ggsankeyfier)

# Fuentes
library(sysfonts)
library(showtext)
dir <- paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/Fonts/")
font_add(family = "font_title",
         bold = file.path(dir, "CreatoDisplay-ExtraBold.otf"),
         regular = file.path(dir, "CreatoDisplay-Regular.otf"))
font_add("font_subtitle", file.path(dir, "CreatoDisplay-Regular.otf"))
font_add(family = "font_body",
         regular = file.path(dir, "RobotoSlab-Regular.ttf"),
         bold = file.path(dir, "RobotoSlab-Bold.ttf"))
showtext_auto()

# Colores
Paleta2 <- c("#1e7b34", "#119ca0", "#b8d6ac", "#6963aa",
             "#7c428a", "#4c2158", "#c72a29", "#ec6230", "#cbc2ce")

Colores_nodos <- c(
  "Consultas totales" = "#6963AA",
  "Violencia familiar y por motivos de género" = "#ec6230",
  "Otros motivos" = "#cbc2ce",
  "Derivadas a Fiscalías (previas medidas pertinentes)" = "#119ca0",
  "Competencia de la Unidad de Flagrancia" = "#c72a29"
)

Colores_flow <- c(
  "Consultas totales" = "#6963AA",
  "Violencia familiar y por motivos de género" = "#ec6230",
  "Otros motivos" = "#FFFFFF",
  "Derivadas a Fiscalías (previas medidas pertinentes)" = "#119ca0",
  "Competencia de la Unidad de Flagrancia" = "#c72a29"
)

Data <- pivot_stages_longer(
  data.frame(
    Consultas = "Consultas totales",
    Motivo = c("Violencia familiar y por motivos de género", "Violencia familiar y por motivos de género", "Otros motivos"),
    Resultado = c("Competencia de la Unidad de Flagrancia", "Derivadas a Fiscalías (previas medidas pertinentes)", NA),
    Valor = c(84, 336, 826)
  ),
  stages_from = c("Consultas", "Motivo", "Resultado"),
  values_from = "Valor"
) %>%
  mutate(Label = ifelse(node != "Otros motivos", as.character(node), NA),
         Label2 = ifelse(node == "Otros motivos", as.character(node), NA),
         Valor_label = ifelse(!is.na(node), formatC(Valor, format="fg", big.mark=".", decimal.mark=","), NA))

grafico <- ggplot(Data, aes(x = stage, y = Valor, group = node, connector = connector,
                            edge_id = edge_id, fill = node)) +
  geom_sankeyedge(aes(fill = factor(edge_id)),
                  position = position_sankey(v_space = 100, order = "descending"), alpha = 0.5) +
  scale_fill_manual(values = c(
    "1" = "#ec6230",
    "2" = "#cbc2ce",
    "3" = "#c72a29",
    "4" = "#119ca0",
    "5" = "#FFFFFF"
  ), na.value = "white") +
  ggnewscale::new_scale_fill() +
  geom_sankeynode(
    aes(fill = node),
    position = position_sankey(v_space = 100, order = "descending", width = 0.20),
    colour = NA
  ) +
  scale_fill_manual(values = Colores_nodos, na.value = "white") +
  geom_text(data = ~ subset(.x, stage == "Consultas"), aes(label = str_wrap(Label, 15)), stat = "sankeynode", 
            position = position_sankey(v_space = 100, order = "descending", nudge_x = -0.15), hjust = 1, size = 5,
            family = "font_title", color = "black", lineheight = 0.9, fontface = "bold") +
  geom_text(data = ~ subset(.x, stage == "Motivo"), aes(label = str_wrap(Label, 20)), stat = "sankeynode", 
            position = position_sankey(v_space = 100, order = "descending", nudge_x = -0.15), hjust = 1, size = 5,
            family = "font_title", color = "black", lineheight = 0.9, fontface = "bold") +
  geom_text(data = ~ subset(.x, stage == "Motivo"), aes(label = str_wrap(Label2, 20)), stat = "sankeynode", 
            position = position_sankey(v_space = 100, order = "descending", nudge_x = 0.15), hjust = 0, size = 5,
            family = "font_title", color = "black", lineheight = 0.9, fontface = "bold") +
  geom_text(data = ~ subset(.x, stage == "Resultado"), aes(label = str_wrap(Label, 20)), stat = "sankeynode", 
            position = position_sankey(v_space = 100, order = "descending", nudge_x = 0.15), hjust = 0, size = 5,
            family = "font_title", color = "black", lineheight = 0.9, fontface = "bold") +
  geom_text(aes(label = ifelse(after_stat(node_size) == 1246, formatC(after_stat(node_size), format = "fg", big.mark=".", decimal.mark=","),
                               paste0(formatC(after_stat(node_size) / 1246 * 100, format = "f", digits = 1, decimal.mark = ","), "%"))), stat = "sankeynode",
            position = position_sankey(v_space = 100, order = "descending", nudge_y = 7.5), vjust=0, hjust = 0.5, size = 4,
            family = "font_body", color = "white", fontface = "bold") +
  geom_text(aes(label = ifelse(after_stat(node_size) == 1246, "",
                               paste0("(", formatC(after_stat(node_size), format = "fg", big.mark=".", decimal.mark = ","), ")"))), stat = "sankeynode",
            position = position_sankey(v_space = 100, order = "descending", nudge_y = -7.5), vjust=1, hjust = 0.5, size = 3,
            family = "font_body", color = "white") +
  scale_fill_manual( values = Colores_nodos, na.value = "white" ) +
  scale_x_discrete(expand = expansion(mult = c(0.25, 0.4))) +
  theme_void() +
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.background = element_rect(fill="white", color=NA)
  )

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=10, height=6)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=10, height=6)

