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
Data <- data.frame(Tematica = c("Violencia laboral", "Protocolo de MI, ASI y VdG en espacios educativos",
                                "Conducta empresarial con perspectiva en DDHH y género",
                                "Sistema Único de Denuncias", "Violencia obstétrica", "Transversalización de género",
                                "Protocolo de intervención ante VdG en salud", "Aproximaciones iniciales a la perspectiva de género",
                                "Identidad de Género"),
                   Cantidad = c(21, 1, 4, 1, 1, 2, 3, 4, 2)) %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  arrange(Porcentaje) %>%
  mutate(Ord = row_number())

# Definir colores
Paleta <- c("#206170", "#5ec5d4", "#a782ec", "#852f8c", "#0f216d", "#2b42a0",
            "#ff9d27", "#ff621d", "#f93e35", "#d3335e", "#cbc2ce")

# Gráfico
grafico <- ggplot(Data, aes(x=Porcentaje, y=reorder(Tematica, Ord))) +
  geom_col(aes(fill=Cantidad), width=0.8) +
  scale_fill_gradient(low="#d086d6", high="#852f8c") +
  theme_light() +
  labs(y=str_wrap("Temática de la capacitación", 25), x="Porcentaje") +
  geom_text(aes(label = formatC(Cantidad, big.mark = ".", decimal.mark = ",")), color = "grey30",
            size=4, family="font_sans", hjust = 0, nudge_x = 1, nudge_y = -0.2) +
  geom_text(aes(label = paste0(formatC(round(Porcentaje,1), big.mark = ".", decimal.mark = ","), "%")), color = "black",
            size=5, family="font_sans", fontface="bold", hjust = 0, nudge_x = 1, nudge_y = 0.2) +
  scale_x_continuous(limits = c(0, round(max(Data$Porcentaje) * 1.2, -1)),
                     labels = function(z) paste0(z, "%")) +
  scale_y_discrete(labels = function(z) str_wrap(z, 30)) +
  theme(text=element_text(family="font_sans"), legend.position="none",
        plot.title = element_text(size=40, family="font_sans", face="bold"),
        plot.subtitle = element_text(size=25, family="font_sans", face="italic", margin=margin(t=5,r=0,b=20,l=0)),
        legend.background = element_blank(), legend.box.background = element_rect(color = "black"),
        legend.box.margin=margin(5,5,5,5),
        panel.grid = element_blank(),
        panel.grid.major.x = element_line(color="grey95", linewidth = 0.5),
        axis.text.x = element_text(size=15, margin = margin(t=10,r=0,b=0,l=0)),
        axis.text.y = element_text(size=12, margin = margin(t=0,r=10,b=0,l=0)),
        axis.title.x = element_text(size=20, margin = margin(t=15, r=0, b=0, l=0)),
        axis.title.y = element_text(size=20, margin = margin(t=0, r=15, b=0, l=0)))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=10, height=6)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=10, height=6)