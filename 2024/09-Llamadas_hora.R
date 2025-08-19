# Limpiar todo
rm(list = ls())

# Librerías
library(ggplot2)
library(dplyr)
library(stringr)
library(cowplot)
library(magick)

# Fuentes
library(showtext)
font_add_google("Barlow", "font")
showtext_auto()

# Leer datos
Raw <- read.csv(file=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Datos/Requerimientos_911_horas.csv")) %>%
  filter(Accion == "Llamadas", Tipo == "Violencia de género")

Dia_lvl <- c("Domingo", "Sábado", "Viernes", "Jueves", "Miércoles", "Martes", "Lunes")
Mes_lvl <- c("Junio", "Mayo", "Abril", "Marzo", "Febrero", "Enero")

Data <- Raw %>%
  filter(Año == 2024) %>%
  mutate(Hora = factor(Hora)) %>%
  mutate(Mes = factor(Mes, levels= Mes_lvl)) %>%
  group_by(Año, Mes, Hora) %>%
  summarise(Cantidad = sum(Cantidad)) %>%
  ungroup %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad))

Total <- Data %>%
  filter(Hora %in% c(0,1,2,20,21,22,23))

# Gráfico
grafico1 <- ggplot(Data, aes(x=Hora, y=Mes, fill=Cantidad)) +
  geom_tile() +
  geom_text(aes(label = formatC(Cantidad, big.mark=".", decimal.mark=",")), family="font", size=3, color="black", alpha=0.7) +
  annotate(geom="rect", xmin=c(0.5, 20.5), ymax=6.5, ymin=0.5, xmax=c(3.5,24.5), color="#6e3169", fill="#6e3169", alpha=0.1) +
  labs(x="Hora", y="Mes") +
  scale_fill_gradient2(low="#1daa6a", mid="#FAF99F", high="#a5549c", midpoint=mean(Data$Cantidad)) +
  theme_light() +
  theme(text=element_text(family="font"), legend.position="none",
        plot.title = element_text(size=20, family="font", face="bold"),
        plot.subtitle = element_text(size=15, family="font"),
        plot.caption = element_text(size=10, family="font", face="italic"),
        panel.grid.major = element_line(colour = "#F5F5F5"),
        axis.text.x = element_text(size=15, margin = margin(t=10,r=0,b=0,l=0)),
        axis.text.y = element_text(size=15, margin = margin(t=0,r=5,b=0,l=5)),
        axis.title.x = element_text(size=20, margin = margin(t=5,r=0,b=5,l=0)),
        axis.title.y = element_text(size=20, margin = margin(t=0,r=5,b=0,l=0)))

grafico2 <- ggplot(x=1:2, y=1:2) +
  geom_textbox(aes(x=1.5, y=1.5,
                   label=paste0("<span style='font-size:40pt; color:#6e3169'>**",
                                formatC(round(sum(Total$Porcentaje),1), big.mark=".", decimal.mark=","),
                                "%**</span><br>",
                                "<span style='font-size:8pt'>(",
                                formatC(sum(Total$Cantidad), big.mark=".", decimal.mark=","),
                                ")</span><br>",
                                "<span style='font-size:12pt'>de las llamadas por</span><br>",
                                "<span style='font-size:12pt; color:#6e3169'>**violencia de género**</span><br>",
                                "<span style='font-size:12pt'>se registraron entre las</span><br>",
                                "<span style='font-size:12pt; color:#6e3169; text-decoration: underline'>**20:00 y 3:00 hs**</span><br>")),
               label.color = NA, family="font", halign = 0.5, fill=NA, color="white", text.color="black",
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
       plot=grafico, dpi=100, width=13, height=5)
ggsave(filename = paste0(filename, ".pdf"), path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/pdf/"),
       plot=grafico, dpi=72, width=13, height=5)