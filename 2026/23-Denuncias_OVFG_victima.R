# Limpiar todo
rm(list = ls())

# Librer?as
library(ggplot2)
library(dplyr)
library(stringr)
library(cowplot)
library(magick)
library(ggtext)

# Fuentes
library(showtext)
font_add_google("Source Sans 3", "font_sans")
font_add_google("Source Serif 4", "font_serif")
showtext_auto()

# Años
Año_1 <- 2026
Año_2 <- 2025

# Leer datos
Raw <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1Cfbecjc5DLo3uGsMEHscsfUC9YOtnKtFvt1bOZI_B4c/edit?usp=sharing",
                  sheet = "Persona_que_denuncia")

# Diccionarios
Rangos_etarios <- data.frame(Ord = 1:12,
                             Rango = c("0-5 años", "6-10 años", "11-14 años", "15-17 años",
                                       "18-21 años", "22-29 años", "30-39 años", "40-49 años",
                                       "50-59 años", "60-74 años", "Más de 74 años","Sin especificar"))

# Modificar datos
Data1 <- Raw %>%
  filter(Año == Año_1) %>%
  group_by(Año, Género, Rango_etario) %>%
  summarise(Cantidad = sum(Frecuencia)) %>%
  ungroup %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>% 
  mutate(Cantidad = ifelse(Género == "Mujeres", Cantidad * (-1), Cantidad),
         Porcentaje = ifelse(Género == "Mujeres", Porcentaje * (-1), Porcentaje)) %>%
  left_join(Rangos_etarios, by = c("Rango_etario" = "Rango")) %>%
  arrange(Año, Ord)

# Definir colores
Paleta <- c("#206170", "#5ec5d4", "#a782ec", "#852f8c", "#0f216d", "#2b42a0",
            "#ff9d27", "#ff621d", "#f93e35", "#d3335e", "#cbc2ce")

Colores <- c("Mujeres" = "#ff621d",
             "Varones" = "#852f8c")

# Texto
Texto <- paste0("<span style='font-size:30pt; color:#0f216d'>**",
                formatC(round(abs(sum((Data1 %>% filter(Año == Año_1, Género == "Mujeres",
                                                        Rango_etario %in% c("22-29 años", "30-39 años", "40-49 años")))$Porcentaje)),1),
                        big.mark = ".", decimal.mark = ","),
                "%**</span><br>",
                "<span style='font-size:10pt'>",
                "de las personas que</span><br>",
                "<span style='font-size:10pt'>",
                "realizaron la denuncia son</span><br>",
                "<span style='font-size:10pt; color:#0f216d'>**mujeres entre 22 y 49 años**</span>")

# Grafico 1
grafico <- ggplot(Data1, aes(x=Porcentaje, y=reorder(Rango_etario, Ord), fill=Género)) +
  geom_col(position = "stack") +
  annotate(geom="rect", ymin=5.5, ymax=8.5, xmin=min(Data1$Porcentaje), xmax=0,
           linetype=2, color="grey", fill=NA) +
  geom_textbox(aes(y=3, x=-15), label=Texto, label.color = NA, family="font", halign = 0.5,
               fill=NA, color="white", text.color="black",
               show.legend=FALSE, fill=NA, size=4) +
  theme_light() +
  labs(x="Porcentaje", y="Rango etario") +
  geom_text(aes(label = paste0(formatC(round(abs(Porcentaje),1), big.mark = ".", decimal.mark = ","), "%")), family="font", size=2,
            hjust = ifelse(Data1$Género == "Mujeres", ifelse(abs(Data1$Porcentaje) <= 1.6, 1.2, -0.2),
                           ifelse(Data1$Porcentaje <= 1.6, -0.2, 1.2))) +
  scale_x_continuous(limits=c(min(Data1$Porcentaje) - 3, max(Data1$Porcentaje) + 3), labels = function(z) paste0(abs(z), "%")) +
  scale_fill_manual(name = "Género",
                    values = Colores) +
  theme(text=element_text(family="font_sans"),
        legend.position = "top",
        legend.justification = "right",
        legend.title = element_text(size=10, family="font_serif"),
        legend.text = element_text(size=12, family="font_sans"),
        plot.title = element_blank(),
        plot.title.position = "plot",
        plot.subtitle = element_text(size=15, family="font_sans"),
        plot.caption = element_text(size=12, family="font_sans", face="italic"),
        panel.grid.major = element_line(colour = "#F5F5F5"),
        panel.grid.major.y = element_line(color="grey95", linewidth = 0.5),
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(size=12, margin = margin(t=10,r=0,b=5,l=0)),
        axis.text.y = element_text(size=12, margin = margin(t=0,r=10,b=0,l=5)),
        axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=8, height=5)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=8, height=5)

