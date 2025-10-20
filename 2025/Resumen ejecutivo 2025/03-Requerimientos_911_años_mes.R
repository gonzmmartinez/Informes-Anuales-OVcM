# Limpiar todo
rm(list = ls())

# Funciones
`%ni%` <- Negate(`%in%`)

# Librerías
library(ggplot2)
library(dplyr)
library(stringr)
library(directlabels)
library(ggrepel)
library(googlesheets4)
library(ggtext)

# Fuentes
library(showtext)
font_add_google("Source Sans 3", "font_sans")
font_add_google("Source Serif 4", "font_serif")
showtext_auto()

# Leer datos
Raw <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1fX8iWndJKs_UTTcB1SoU5tpTK7ysVvxJeyVAE0C5gro/edit?usp=sharing",
                  sheet = "Mes")

# Diccionarios
Mes <- data.frame(Mes = c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio",
                          "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"),
                  Mes_num = 1:12,
                  Semestre_num = rep(c(1,2), each=6))

Data <- Raw %>%
  filter(Tipo != "Abuso sexual") %>%
  mutate(Año = as.factor(Año)) %>%
  group_by(Año, Mes) %>%
  summarise(Cantidad = sum(Cantidad)) %>%
  ungroup %>%
  left_join(Mes, by="Mes")

Labels <- Data %>%
  mutate(Año = as.factor(Año)) %>%
  group_by(Año) %>%
  filter(Mes_num == max(Mes_num)) %>%
  ungroup %>%
  mutate(xpos = Mes_num+0.4,
         Label = str_replace(as.character(Año), "^(\\d)(\\d+)$", "\\1.\\2")) %>%
  mutate(ypos = case_when(Año == 2023 ~ Cantidad - 500,
                          Año == 2025 ~ Cantidad + 500,
                          .default = Cantidad))

# Colores
Colores <- c()

# Gráfico
grafico <- ggplot(Data, aes(x=reorder(Mes, Mes_num), y=Cantidad, group=Año)) +
  geom_line(aes(color=Año), linewidth=2) +
  geom_point(aes(color=Año), size=3) +
  geom_label(data=Labels, aes(x=xpos, y=ypos, label=Label, fill=Año), color="white",
             family="font_serif", alpha=0.75) +
  geom_textbox(inherit.aes = FALSE, aes(x=8.5, y=22500, label="Mayor cantidad de requerimientos en la **segunda mitad del año**"),
               family="font", color="black", width=unit(0.6, "npc"), halign=0.5, size=5, lineheight=1,
               box.colour = NA, fill=NA) +
  labs(title="",
       x="Mes", y="Cantidad") +
  theme_light() +
  scale_x_discrete(labels = function(z) str_sub(z, 1, 3), expand = expansion(add = c(0.5, 1))) +
  scale_y_continuous(limits=c(5000, 25000), labels = function(z) formatC(z, format="fg", big.mark = ".", decimal.mark = ",")) +
  theme(text=element_text(family="font_sans"), legend.position="none",
        plot.title = element_text(size=20, family="font_sans", face="bold"),
        plot.subtitle = element_text(size=15, family="font_sans"),
        plot.caption = element_text(size=12, family="font_sans", face="italic"),
        panel.grid.major = element_line(colour = "grey95"),
        axis.text.x = element_text(size=15, margin = margin(t=10,r=0,b=5,l=0)),
        axis.text.y = element_text(size=15, margin = margin(t=0,r=10,b=0,l=5)),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=12, height=7)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=12, height=7)
