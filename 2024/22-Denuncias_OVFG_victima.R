# Limpiar todo
rm(list = ls())

# Librerías
library(ggplot2)
library(dplyr)
library(stringr)
library(cowplot)
library(magick)
library(ggtext)

# Fuentes
library(showtext)
font_add_google("Barlow", "font")
showtext_auto()

# Leer datos
Raw <- read.csv(file=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Datos/Denuncias_victima.csv"))

# Modificar datos
Data1 <- Raw %>%
  filter(Año == 2024) %>%
  group_by(Año, Género, Rango_etario, Ord_rango_etario) %>%
  summarise(Cantidad = sum(Frecuencia))
Data1 <- Data1 %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Data1$Cantidad)) %>% 
  mutate(Cantidad = ifelse(Género == "Mujeres", Cantidad * (-1), Cantidad),
         Porcentaje = ifelse(Género == "Mujeres", Porcentaje * (-1), Porcentaje))

Data2 <- Raw %>%
  filter(Año == 2023) %>%
  group_by(Año, Género, Rango_etario, Ord_rango_etario) %>%
  summarise(Cantidad = sum(Frecuencia))
Data2 <- Data2 %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Data2$Cantidad)) %>% 
  mutate(Cantidad = ifelse(Género == "Mujeres", Cantidad * (-1), Cantidad),
         Porcentaje = ifelse(Género == "Mujeres", Porcentaje * (-1), Porcentaje))

# Definir colores
Colores <- c("Mujeres" = "#f2904c",
             "Varones" = "#a5549c")

# Texto
Texto <- paste0("<span style='font-size:30pt; color:#6e3169'>**",
                formatC(round(abs(sum((Data1 %>% filter(Año == 2024, Género == "Mujeres",
                                                        Rango_etario %in% c("22-29 años", "30-39 años", "40-49 años")))$Porcentaje)),1),
                        big.mark = ".", decimal.mark = ","),
                "%**</span><br>",
                "<span style='font-size:10pt'>",
                "de las personas que</span><br>",
                "<span style='font-size:10pt'>",
                "realizaron la denuncia son</span><br>",
                "<span style='font-size:10pt; color:#6e3169'>**mujeres entre 22 y 49 años**</span>")

# Grafico 1
grafico1 <- ggplot(Data1, aes(x=Porcentaje, y=reorder(Rango_etario, Ord_rango_etario), fill=Género)) +
  geom_col(position = "stack") +
  annotate(geom="rect", ymin=5.5, ymax=8.5, xmin=min(Data1$Porcentaje), xmax=0, linetype=2, color="grey", fill=NA) +
  geom_textbox(aes(y=3, x=-15), label=Texto, label.color = NA, family="font", halign = 0.5, fill=NA, color="white", text.color="black",
               show.legend=FALSE, fill=NA, size=4) +
  theme_light() +
  labs(title="2.024", x="Porcentaje", y="Rango etario") +
  geom_text(aes(label = paste0(formatC(round(abs(Porcentaje),1), big.mark = ".", decimal.mark = ","), "%")), family="font", size=2,
            hjust = ifelse(Data1$Género == "Mujeres", ifelse(abs(Data1$Porcentaje) <= 1.6, 1.2, -0.2),
                           ifelse(Data1$Porcentaje <= 1.6, -0.2, 1.2))) +
  scale_x_continuous(limits=c(min(Data1$Porcentaje) - 3, max(Data1$Porcentaje) + 3), labels = function(z) paste0(abs(z), "%")) +
  scale_fill_manual(name = "Género",
                    values = Colores) +
  theme(text=element_text(family="font"),
        legend.position = "bottom",
        legend.justification = "right",
        legend.title = element_text(size=10, family="font"),
        legend.text = element_text(size=12, family="font"),
        plot.title = element_text(size=20, family="font", face="bold", hjust=0.5),
        plot.title.position = "plot",
        plot.subtitle = element_text(size=15, family="font"),
        plot.caption = element_text(size=12, family="font", face="italic"),
        panel.grid.major = element_line(colour = "#F5F5F5"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(size=12, margin = margin(t=10,r=0,b=5,l=0)),
        axis.text.y = element_text(size=12, margin = margin(t=0,r=10,b=0,l=5)),
        axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15))

# Grafico 2
grafico2 <- ggplot(Data2, aes(x=Porcentaje, y=reorder(Rango_etario, Ord_rango_etario), fill=Género)) +
  geom_col(position = "stack") +
  theme_light() +
  labs(title="2.023") +
  scale_x_continuous(limits=c(min(Data2$Porcentaje) - 3, max(Data1$Porcentaje) + 3), labels = function(z) paste0(abs(z), "%")) +
  scale_fill_manual(values = Colores) +
  theme(text=element_text(family="font"),
        legend.position = "none",
        plot.title = element_text(size=20, family="font", face="bold", hjust=0.5),
        plot.title.position = "plot",
        plot.subtitle = element_text(size=15, family="font"),
        plot.caption = element_text(size=12, family="font", face="italic"),
        legend.title = element_blank(),
        panel.grid.major = element_line(colour = "#F5F5F5"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(size=8, margin = margin(t=10,r=0,b=5,l=0)),
        axis.text.y = element_text(size=8, margin = margin(t=0,r=10,b=0,l=5)),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = margin(t=20, r=100, b=10, l=100))

# Layout
grafico <- plot_grid(grafico2, grafico1, nrow=2, rel_heights = c(2,3))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=8, height=8)
ggsave(filename = paste0(filename, ".pdf"), path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/pdf/"),
       plot=grafico, dpi=72, width=8, height=8)