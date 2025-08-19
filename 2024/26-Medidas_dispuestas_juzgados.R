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

# Crear datos
Raw <- read.csv(file=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Datos/Medidas_dispuestas_juzgados.csv"))

Data1 <- Raw %>%
  filter(Año == 2023) %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  arrange(Cantidad) %>%
  mutate(Ord = row_number()) %>%
  mutate(Ord = ifelse(Medida == "Otras", 0, Ord))

Data2 <- Raw %>%
  filter(Año == 2024) %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  arrange(Cantidad) %>%
  mutate(Ord = row_number()) %>%
  mutate(Ord = ifelse(Medida == "Otras", 0, Ord))

# Gráfico
grafico1 <- ggplot(Data1, aes(x=Cantidad, y=reorder(Medida, Ord))) +
  geom_col(aes(fill=Cantidad)) +
  scale_fill_gradient(high="#6e3169", low="#e54c7c") +
  theme_classic() +
  labs(title="2.023", subtitle="Todo el año", y="Tipo de medida dispuesta", x="Cantidad") +
  geom_text(aes(label = formatC(Cantidad, big.mark = ".", decimal.mark = ",")), color = ifelse(Data1$Cantidad <= 3000, "black", "white"),
            size=8, family="font", fontface="bold", hjust = ifelse(Data1$Cantidad <= 3000, -0.5, 1.5)) +
  scale_y_discrete(labels = function(z) str_wrap(z, 30)) +
  theme(text=element_text(family="font"), legend.position="none",
        plot.title = element_text(size=40, family="font", face="bold"),
        plot.subtitle = element_text(size=25, family="font", face="italic", margin=margin(t=5,r=0,b=20,l=0)),
        legend.background = element_blank(), legend.box.background = element_rect(color = "black"),
        legend.box.margin=margin(5,5,5,5),
        axis.text.x = element_text(size=15, margin = margin(t=10,r=0,b=0,l=0)),
        axis.text.y = element_text(size=20, margin = margin(t=0,r=10,b=0,l=0)),
        axis.title.x = element_text(size=20, margin = margin(t=15, r=0, b=0, l=0)),
        axis.title.y = element_text(size=20, margin = margin(t=0, r=15, b=0, l=0)))

# Gráfico
grafico2 <- ggplot(Data2, aes(x=Cantidad, y=reorder(Medida, Ord))) +
  geom_col(aes(fill=Cantidad)) +
  scale_fill_gradient(high="#316e6e", low="#72bf90") +
  theme_classic() +
  labs(title="2.024", subtitle="Enero-septiembre", y="Tipo de medida dispuesta", x="Cantidad") +
  geom_text(aes(label = formatC(Cantidad, big.mark = ".", decimal.mark = ",")), color = ifelse(Data2$Cantidad <= 3000, "black", "white"),
            size=8, family="font", fontface="bold", hjust = ifelse(Data2$Cantidad <= 3000, -0.5, 1.5)) +
  scale_y_discrete(labels = function(z) str_wrap(z, 30)) +
  theme(text=element_text(family="font"), legend.position="none",
        plot.title = element_text(size=40, family="font", face="bold"),
        plot.subtitle = element_text(size=25, family="font", face="italic", margin=margin(t=5,r=0,b=20,l=0)),
        legend.background = element_blank(), legend.box.background = element_rect(color = "black"),
        legend.box.margin=margin(5,5,5,5),
        axis.text.x = element_text(size=15, margin = margin(t=10,r=0,b=0,l=0)),
        axis.text.y = element_text(size=15, margin = margin(t=0,r=10,b=0,l=0)),
        axis.title.x = element_text(size=20, margin = margin(t=15, r=0, b=0, l=0)),
        axis.title.y = element_blank())

grafico <- plot_grid(grafico1, grafico2, nrow=1) +
  theme(plot.background = element_rect(fill="white", color=NA))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=20, height=22)
ggsave(filename = paste0(filename, ".pdf"), path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/pdf/"),
       plot=grafico, dpi=72, width=20, height=22)