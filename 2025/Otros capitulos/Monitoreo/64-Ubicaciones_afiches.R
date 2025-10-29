# Limpiar todo
rm(list = ls())

# Librer?as
library(ggplot2)
library(dplyr)
library(stringr)
library(cowplot)
library(magick)

# Fuentes
library(showtext)
font_add_google("Source Sans 3", "font_sans")
font_add_google("Source Serif 4", "font_serif")
showtext_auto()

# Crear datos
Data <- data.frame(Categoria = c("Ingreso", "Sala de profesores", "Cartelera",
                                 "Otro", "Pasillos", "Preceptoría", "Patio", "Baños", "Aulas"),
                   Cantidad = c(10, 9, 8, 7, 6, 4, 3, 2, 1)) %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  group_by(Categoria) %>%
  summarise(Cantidad = sum(Cantidad)) %>%
  ungroup %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  arrange(Cantidad) %>%
  mutate(Ord = row_number()) %>%
  mutate(Ord = ifelse(Categoria == "Otro", max(Ord) + 1, Ord))

# Definir colores
Paleta <- c("#206170", "#5ec5d4", "#a782ec", "#852f8c", "#0f216d", "#2b42a0",
            "#ff9d27", "#ff621d", "#f93e35", "#d3335e", "#cbc2ce")

# Gráfico
grafico <- ggplot(Data, aes(x=Porcentaje, y=reorder(Categoria, Ord))) +
  geom_col(data = subset(Data, Categoria == "Otro"), fill = "#cbc2ce") +
  geom_col(data = subset(Data, Categoria != "Otro"), aes(fill=Cantidad)) +
  scale_fill_gradient(low="#95a1d0", high="#2b42a0") +
  theme_light() +
  labs(y=str_wrap("Lugar", 25), x="Porcentaje",
       title=str_wrap("¿En qué lugares fueron colocados los afiches?",
                      width=60),
       subtitle="Respuesta de opción múltiple") +
  geom_text(aes(label = formatC(Cantidad, big.mark = ".", decimal.mark = ",")), color = "black",
            size=4, family="font_sans", hjust = 0, nudge_x = 0.5, nudge_y = -0.15) +
  geom_text(aes(label = paste0(formatC(round(Porcentaje,1), big.mark = ".", decimal.mark = ","), "%")), color = "black",
            size=6, family="font_sans", fontface="bold", hjust = 0, nudge_x = 0.5, nudge_y = 0.15) +
  scale_x_continuous(limits = c(0, round(max(Data$Porcentaje) * 1.3, 0)),
                     labels = function(z) paste0(z, "%")) +
  scale_y_discrete(labels = function(z) str_wrap(z, 30)) +
  theme(text=element_text(family="font_sans"), legend.position="none",
        plot.title = element_text(size=15, family="font_serif", margin=margin(b=10), face="italic", hjust=0.5, color="grey30"),
        plot.title.position = "plot",
        plot.subtitle = element_text(size=12, family="font_serif", margin=margin(b=10), hjust=0.5, color="grey30"),
        legend.background = element_blank(), legend.box.background = element_rect(color = "black"),
        legend.box.margin=margin(5,5,5,5),
        panel.grid = element_blank(),
        panel.grid.major.x = element_line(color="grey95", linewidth = 0.5),
        axis.text.x = element_text(size=15, margin = margin(t=10,r=0,b=0,l=0)),
        axis.text.y = element_text(size=20, margin = margin(t=0,r=10,b=0,l=0)),
        axis.title.x = element_text(size=20, margin = margin(t=15, r=0, b=0, l=0)),
        axis.title.y = element_text(size=20, margin = margin(t=0, r=15, b=0, l=0)))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=10, height=7)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=10, height=7)