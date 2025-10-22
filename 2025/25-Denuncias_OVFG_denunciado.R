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

# Leer datos
Raw <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1Cfbecjc5DLo3uGsMEHscsfUC9YOtnKtFvt1bOZI_B4c/edit?usp=sharing",
                  sheet = "Persona_denunciada")

Data1 <- Raw %>%
  filter(Año == 2024, Rango_etario != "Sin especificar") %>%
  group_by(Año, Género, Rango_etario, Ord_rango_etario) %>%
  summarise(Cantidad = sum(Frecuencia))
Data1 <- Data1 %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Data1$Cantidad)) %>% 
  mutate(Cantidad = ifelse(Género == "Mujeres", Cantidad * (-1), Cantidad),
         Porcentaje = ifelse(Género == "Mujeres", Porcentaje * (-1), Porcentaje))

SE1 <- Raw %>%
  filter(Año == 2024, Rango_etario == "Sin especificar") %>%
  group_by(Año, Género) %>%
  summarise(Cantidad = sum(Frecuencia))

Data2 <- Raw %>%
  filter(Año == 2023, Rango_etario != "Sin especificar") %>%
  group_by(Año, Género, Rango_etario, Ord_rango_etario) %>%
  summarise(Cantidad = sum(Frecuencia))
Data2 <- Data2 %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Data2$Cantidad)) %>% 
  mutate(Cantidad = ifelse(Género == "Mujeres", Cantidad * (-1), Cantidad),
         Porcentaje = ifelse(Género == "Mujeres", Porcentaje * (-1), Porcentaje))

SE2 <- Raw %>%
  filter(Año == 2023, Rango_etario == "Sin especificar") %>%
  group_by(Año, Género) %>%
  summarise(Cantidad = sum(Frecuencia))

# Colores
Paleta <- c("#206170", "#5ec5d4", "#a782ec", "#852f8c", "#0f216d", "#2b42a0",
            "#ff9d27", "#ff621d", "#f93e35", "#d3335e", "#cbc2ce")

Colores <- c("Mujeres" = "#ff621d",
             "Varones" = "#852f8c")

# Texto
Texto <- paste0("<span style='font-size:30pt; color:#0f216d'>**",
                formatC(round(abs(sum((Data1 %>% filter(Año == 2024, Género == "Varones",
                                                        Rango_etario %in% c("22-29 años", "30-39 años")))$Porcentaje)),1),
                        big.mark = ".", decimal.mark = ",", format="fg"),
                "%**</span><br>",
                "<span style='font-size:10pt'>",
                "de las personas</span><br>",
                "<span style='font-size:10pt'>",
                "denunciadas son</span><br>",
                "<span style='font-size:10pt; color:#0f216d'>**varones entre 22 y 39 años**</span>")

# Grafico 1
grafico1 <- ggplot(Data1, aes(x=Porcentaje, y=reorder(Rango_etario, Ord_rango_etario), fill=Género)) +
  geom_col(position = "stack") +
  annotate(geom="rect", ymin=2.5, ymax=4.5, xmin=0, xmax=max(Data1$Porcentaje), linetype=2, color="grey", fill=NA) +
  geom_textbox(aes(y=6, x=20), label=Texto, label.color = NA, family="font_sans", halign = 0.5, fill=NA, color="white", text.color="black",
               show.legend=FALSE, fill=NA, size=4) +
  theme_light() +
  labs(title="2.025", x="Porcentaje", y="Rango etario") +
  geom_text(aes(label = paste0(formatC(round(abs(Porcentaje),1), big.mark = ".", decimal.mark = ","), "%")), family="font_sans", size=2,
            hjust = ifelse(Data1$Género == "Mujeres", ifelse(abs(Data1$Porcentaje) <= 1.6, 1.2, -0.2),
                           ifelse(Data1$Porcentaje <= 1.6, -0.2, 1.2))) +
  annotate(geom="text", x=18, y=1.5, label = paste0("Edad sin especificar: \n",
                                                    "Mujeres: ",
                                                    formatC(SE1$Cantidad[1], big.mark = ".", decimal.mark = ",", format="fg"), "\n",
                                                    "Varones: ",
                                                    formatC(SE1$Cantidad[2], big.mark = ".", decimal.mark = ",", format="fg")),
           family="font", size=2.5) +
  scale_x_continuous(limits=c(min(Data1$Porcentaje) - 3, max(Data1$Porcentaje) + 3), labels = function(z) paste0(abs(z), "%")) +
  scale_fill_manual(name = "Género",
                    values = Colores) +
  theme(text=element_text(family="font_sans"),
        legend.position = "bottom",
        legend.justification = "right",
        legend.title = element_text(size=10, family="font_serif"),
        legend.text = element_text(size=12, family="font_sans"),
        plot.title = element_text(size=20, family="font_serif", face="bold", hjust=0.5),
        plot.title.position = "plot",
        plot.subtitle = element_text(size=15, family="font_sans"),
        plot.caption = element_text(size=12, family="font_sans", face="italic"),
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
  labs(title="2.024") +
  annotate(geom="text", x=18, y=1.5, label = paste0("Edad sin especificar: \n",
                                                      "Mujeres: ",
                                                      formatC(SE2$Cantidad[1], big.mark = ".", decimal.mark = ","), "\n",
                                                      "Varones: ",
                                                      formatC(SE2$Cantidad[2], big.mark = ".", decimal.mark = ",")),
           family="font_sans", size=2) +
  scale_x_continuous(limits=c(min(Data1$Porcentaje) - 3, max(Data1$Porcentaje) + 3), labels = function(z) paste0(abs(z), "%")) +
  scale_fill_manual(values = Colores) +
  theme(text=element_text(family="font_sans"),
        legend.position = "none",
        plot.title = element_text(size=20, family="font_serif", face="bold", hjust=0.5),
        plot.title.position = "plot",
        plot.subtitle = element_text(size=15, family="font_sans"),
        plot.caption = element_text(size=12, family="font_sans", face="italic"),
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
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=8, height=8)