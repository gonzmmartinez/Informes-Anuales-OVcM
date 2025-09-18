# Limpiar todo
rm(list = ls())

# Librerías
library(tidyverse)
library(ggtext)
library(ggparliament)
library(cowplot)

# Funciones
`%nin%` = Negate(`%in%`)

# Fuentes
library(showtext)
font_add_google("Barlow", "font")
showtext_auto()

# Cargar datos
Raw1 <- data.frame(Capacitado = factor(c("Mujeres", "Varones"), levels=c("Mujeres", "Varones")),
                  Cantidad = c(18,42)) %>%
  mutate(xmid = c(9/60, 39/60)) %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  mutate(Label = paste0("<span style='font-size:17pt'>**",
                        formatC(round(Porcentaje,1), big.mark = ".", decimal.mark = ","),
                        "%**</span><span style='font-size:10pt'> (",
                        Cantidad,
                        ")</span>"))

# Create the data frame to be used
Data1 <- parliament_data(election_data = Raw1,
                        type = "semicircle",
                        parl_rows = 5,
                        party_seats = Raw1$Cantidad)

# Cargar datos
Raw2 <- data.frame(Capacitado = factor(c("Mujeres", "Varones"), levels=c("Mujeres", "Varones")),
                  Cantidad = c(3,20)) %>%
  mutate(xmid = c(1.5/23, 13/23)) %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  mutate(Label = paste0("<span style='font-size:17pt'>**",
                        formatC(round(Porcentaje,1), big.mark = ".", decimal.mark = ","),
                        "%**</span><span style='font-size:10pt'> (",
                        Cantidad,
                        ")</span>"))

# Create the data frame to be used
Data2 <- parliament_data(election_data = Raw2,
                        type = "semicircle",
                        parl_rows = 3,
                        party_seats = Raw2$Cantidad)

# Colores
Paleta <- c("#2a1f82", "#3f3af2", "#a4fffd", "#e3e2ef", "#27391C", "#1F7D53")
Colores <- c("Mujeres" <- "#1F7D53",
             "Varones" <- "#2a1f82")

grafico1 <- ggplot(Data1, aes(x = x, y = y, colour=Capacitado)) +
  geom_parliament_seats(size= 6) +
  annotate(geom="text", x=0, y=0.1, label="30%", family="font", fontface="bold", size=10, color="#1F7D53") +
  theme_ggparliament() +
  labs(title="Cámara de Diputados") +
  scale_fill_manual(values = c("#1F7D53", "#2a1f82")) +
  scale_colour_manual(values = c("#1F7D53", "#2a1f82")) +
  coord_fixed(ratio = 1) +
  theme(text=element_text(family="font"),
        legend.position = "bottom",
        legend.justification = "right",
        plot.title = element_text(family="font", size=25, face="bold", hjust=0.5, margin=margin(t=12.5, b=20)),
        plot.subtitle = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size=10, family="font"),
        legend.box.margin=margin(5,5,5,5),
        legend.key.spacing.x = unit(0.25, "cm"),
        plot.background = element_rect(fill = "white", colour = NA),
        plot.margin = unit(c(1,1,1,1), "cm"))

grafico2 <- ggplot(Data2, aes(x = x, y = y, colour=Capacitado)) +
  geom_parliament_seats(size= 10) +
  annotate(geom="text", x=0, y=0.05, label="13%", family="font", fontface="bold", size=10, color="#1F7D53") +
  theme_ggparliament() +
  labs(title="Cámara de Senadores") +
  scale_fill_manual(values = Colores) +
  scale_colour_manual(values = Colores) +
  scale_y_continuous(expand = c(0.09,0.09)) +
  coord_fixed(ratio = 0.9) +
  theme(text=element_text(family="font"),
        legend.position = "none",
        plot.title = element_text(family="font", size=25, face="bold", hjust=0.5, margin=margin(b=15)),
        plot.subtitle = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size=15),
        plot.background = element_rect(fill = "white", colour = NA),
        plot.margin = unit(c(1,1,1.5,1), "cm"))

# Layout
grafico <- plot_grid(grafico2, grafico1, ncol=2,
                     rel_widths = c(1,1),
                     align = "v") +
  theme(plot.background = element_rect(fill = "white", colour = NA))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=10, height=4)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=10, height=4)