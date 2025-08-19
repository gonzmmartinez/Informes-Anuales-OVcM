# Limpiar todo
rm(list = ls())

# Librerías
library(ggplot2)
library(dplyr)
library(stringr)
library(cowplot)
library(magick)
library(ggtext)
library(ggparliament)

# Fuentes
library(showtext)
font_add_google("Barlow", "font")
showtext_auto()

Raw <- data.frame(Capacitado = factor(c("Sí", "No"), levels=c("Sí", "No")),
                  Cantidad = c(22,1)) %>%
  mutate(xmid = c(11.5/23, 22.5/23)) %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  mutate(Label = ifelse(Porcentaje > 5,
                        paste0("<span style='font-size:17pt'>**",
                               formatC(round(Porcentaje,1), big.mark = ".", decimal.mark = ","),
                               "%**</span><span style='font-size:10pt'> (",
                               Cantidad,
                               ")</span>"),
                        paste0("<span style='font-size:10pt'>**",
                               formatC(round(Porcentaje,1), big.mark = ".", decimal.mark = ","),
                               "%**</span><br><span style='font-size: 6pt'> (",
                               Cantidad,
                               ")</span>")))

# Create the data frame to be used
Data <- parliament_data(election_data = Raw,
                        type = "semicircle", # Parliament type
                        parl_rows = 3,      # Number of rows of the parliament
                        party_seats = Raw$Cantidad) # Seats per party

# Definir colores
Colores <- c("#e54c7c", "#6e3169", "#f2904c", "#ffd241", "#1daa6a", "#4cb2f2", "#f24c7e", "#747264", "#e6e8eb")

# Gráfico1
grafico1 <- ggplot(Data, aes(x = x, y = y, colour=Capacitado)) +
  geom_parliament_seats(size= 10) +
  theme_ggparliament() +
  scale_fill_manual(values = c("#f2904c", "#c2c2c2")) +
  scale_colour_manual(values = c("#f2904c", "#c2c2c2")) +
  labs(title="Cámara de Senadores") +
  theme(text=element_text(family="font"),
        legend.position = "right",
        plot.title = element_text(family="font", size=25, face="bold", hjust=0.5, margin=margin(b=20, t=10)),
        plot.subtitle = element_text(family="font", size=15, face="italic", hjust=0.5),
        legend.title = element_blank(),
        legend.text = element_text(size=15),
        legend.box.margin=margin(5,5,5,5),
        legend.key.spacing.y = unit(0.25, "cm"),
        plot.background = element_rect(fill = "white", colour = NA))

grafico2 <- ggplot(Raw, aes(y="", x=Cantidad, fill=Capacitado)) +
  geom_col(position = position_fill(reverse = TRUE), width=0.5) +
  geom_richtext(aes(y=1, x=xmid, label=Label),
            family="font", size=3, color = "black",
            label.color = NA,
            show.legend=FALSE, fill=NA) +
  scale_fill_manual(values = c("#f2904c", "#c2c2c2")) +
  theme_void() +
  theme(legend.position = "none",
        plot.margin = margin(l=-10, r=45))

# Layout
grafico <- plot_grid(grafico1, grafico2, ncol=1, rel_heights = c(1,0.3)) +
  theme(plot.background = element_rect(fill = "white", colour=NA))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=8, height=5)
ggsave(filename = paste0(filename, ".svg"), path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/SVG/"),
       plot=grafico, dpi=72, width=8, height=5)