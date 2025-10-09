# Limpiar todo
rm(list = ls())

# Librerías
library(ggplot2)
library(dplyr)
library(stringr)
library(cowplot)
library(magick)
library(googlesheets4)
library(ggtext)

# Fuentes
library(showtext)
font_add_google("Source Sans 3", "font_sans")
font_add_google("Source Serif 4", "font_serif")
showtext_auto()

# Leer datos
Raw <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1fX8iWndJKs_UTTcB1SoU5tpTK7ysVvxJeyVAE0C5gro/edit?usp=sharing",
                  sheet = "Dia") %>%
  filter(Accion == "Llamadas", Tipo != "Abuso sexual")

Dia_lvl <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado", "Domingo")
Mes_lvl <- c("Junio", "Mayo", "Abril", "Marzo", "Febrero", "Enero")

Data <- Raw %>%
  filter(Año == 2025) %>%
  mutate(Dia = factor(Dia, levels = Dia_lvl)) %>%
  mutate(Mes = factor(Mes, levels= Mes_lvl)) %>%
  group_by(Año, Mes, Dia) %>%
  summarise(Cantidad = sum(Cantidad)) %>%
  group_by(Año) %>%
  mutate(Porcentaje = Cantidad/sum(Cantidad) * 100) %>%
  ungroup

Total <- Data %>%
  filter(Dia %in% c("Sábado", "Domingo")) %>%
  group_by(Dia) %>%
  summarise(Cantidad = sum(Cantidad),
            Porcentaje = sum(Porcentaje))

Total_anterior <- Raw %>%
  filter(Año == 2025) %>%
  mutate(Dia = factor(Dia, levels = Dia_lvl)) %>%
  mutate(Mes = factor(Mes, levels = Mes_lvl)) %>%
  group_by(Año, Mes, Dia) %>%
  summarise(Cantidad = sum(Cantidad)) %>%
  group_by(Año) %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  ungroup %>%
  filter(Dia %in% c("Sábado", "Domingo")) %>%
  group_by(Dia) %>%
  summarise(Cantidad = sum(Cantidad),
            Porcentaje = sum(Porcentaje))

# Gráfico
grafico1 <- ggplot(Data, aes(x=Dia, y=Mes, fill=Cantidad)) +
  geom_tile() +
  geom_text(aes(label = formatC(Cantidad, big.mark=".", decimal.mark=",")), family="font_sans", size=3, color="black", alpha=0.7) +
  annotate(geom="rect", ymin=0.5, ymax=6.5, xmin=5.5, xmax=7.5, color="#6e3169", fill="#6e3169", alpha=0.1) +
  labs(x="Día de la semana", y="Mes") +
  scale_fill_gradient2(low="#1daa6a", mid="#FAF99F", high="#a5549c", midpoint=mean(Data$Cantidad)) +
  theme_light() +
  theme(text=element_text(family="font_sans"), legend.position="none",
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        panel.grid.major = element_line(colour = "#F5F5F5"),
        plot.background = element_rect(fill = "white", color="white"),
        panel.border = element_blank(),
        axis.text.x = element_text(size=12, family="font_sans", margin = margin(t=10,r=0,b=0,l=0)),
        axis.text.y = element_text(size=12, family="font_sans", margin = margin(t=0,r=5,b=0,l=5)),
        axis.title.x = element_text(size=15, family="font_sans", margin = margin(t=5,r=0,b=5,l=0)),
        axis.title.y = element_text(size=15, family="font_sans", margin = margin(t=0,r=5,b=0,l=0)))

grafico2 <- ggplot(x=1:2, y=1:2) +
  geom_textbox(aes(x=1.5, y=1.5,
                label=paste0("<span style='font-size:40pt; color:#6e3169'>**",
                             formatC(round(sum(Total$Porcentaje),0), big.mark=".", decimal.mark=","),
                             "%**</span><br>",
                             "<span style='font-size:8pt'>(",
                             formatC(sum(Total$Cantidad), big.mark=".", decimal.mark=",", format="fg"),
                             ")</span><br>",
                             "<span style='font-size:12pt'>de las llamadas por</span><br>",
                             "<span style='font-size:12pt; color:#6e3169'>**violencia de género**</span><br>",
                             "<span style='font-size:12pt'>y </span><span style='font-size:12pt; color:#6e3169'>**violencia familiar**</span><br>",
                             "<span style='font-size:12pt'>se registraron los días</span><br>",
                             "<span style='font-size:12pt; color:#6e3169; text-decoration: underline'>**sábado y domingo**</span><br>")),
                label.color = NA, family="font_sans", halign = 0.5, fill=NA, color="white", text.color="black",
                show.legend=FALSE, fill=NA, size=4) +
  theme_void() +
  theme(plot.background = element_rect(fill = "white", colour = "white"),
        panel.border = element_blank())

grafico <- plot_grid(grafico1, grafico2, ncol=2,
                     rel_widths = c(5,1))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=10, height=4.5)
ggsave(filename = paste0(filename, ".pdf"), path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=10, height=4.5)