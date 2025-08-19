# Limpiar todo
rm(list = ls())

# Librerías
library(ggplot2)
library(dplyr)
library(stringr)
library(cowplot)
library(magick)
library(ggfittext)

# Fuentes
library(showtext)
font_add_google("Barlow", "font")
showtext_auto()

# Crear datos
Data <- read.csv(file=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Datos/Consignas_policiales.csv")) %>%
  mutate(Año = as.factor(formatC(Año, big.mark = ".", decimal.mark = ",")),
         Tipo = factor(Tipo, levels=c("Fija","Ambulatoria","Personalizada")))

# Definir colores
Colores <- c("Fija" = "#ec6489",
             "Ambulatoria" = "#f2904c",
             "Personalizada" = "#72bf90")

# Titulo
titulo <- ggplot() +
  labs(title="Consignas policiales a víctimas y agresores por tipo de consigna (VIF-VG)",
       subtitle="Años 2021-2023. Provincia de Salta") +
  theme_void() +
  theme(plot.title=element_text(family="font", size=20, face="bold"),
        plot.subtitle=element_text(family="font", size=15),
        plot.margin = margin(t=15, r=0, b=0, l=10))

# Grafico 1
grafico1 <- ggplot(Data %>% filter(Sujeto == "Agresor"), aes(x=Año, y=Cantidad, fill=Tipo)) +
  geom_col() +
  geom_text(aes(label = formatC(Cantidad, big.mark = ".", decimal.mark = ",")), vjust = c(-1,-1,-1,-1,2,2,2,2,2), size= 5,
            color="black", family="font") +
  facet_wrap(~Tipo, ncol=1, scales="free") +
  labs(title="Agresores") +
  scale_fill_manual(values=Colores) +
  theme_light() +
  theme(legend.position = "none",
        plot.title = element_text(size= 20, family= "font", face="bold", hjust = 0.5, margin = margin(t=0,r=0,b=10,l=0)),
        strip.text.x = element_text(size=12, family="font", face="bold"),
        axis.title.x = element_text(size=15, family="font"),
        axis.title.y = element_text(size=15, family="font"),
        axis.text.x = element_text(size=10, family="font"),
        axis.text.y = element_text(size=10, family="font"),
        panel.grid = element_line(colour = "#F5F5F5"))

# Grafico 2
grafico2 <- ggplot(Data %>% filter(Sujeto == "Víctima"), aes(x=Año, y=Cantidad, fill=Tipo)) +
  geom_col() +
  geom_text(aes(label = formatC(Cantidad, big.mark = ".", decimal.mark = ",")), vjust = 2, size = 5, color="black", family="font") +
  facet_wrap(~Tipo, ncol=1, scales="free")  +
  labs(title="Víctimas") +
  scale_fill_manual(values=Colores) +
  scale_y_continuous(labels = function(z) formatC(z, big.mark = ".", decimal.mark = ",", format="d")) +
  theme_light() +
  theme(legend.position = "none",
        plot.title = element_text(size= 20, family= "font", face="bold", hjust = 0.5, margin = margin(t=0,r=0,b=10,l=0)),
        strip.text.x = element_text(size=12, family="font", face="bold"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size=15, family="font"),
        axis.text.x = element_text(size=10, family="font"),
        axis.text.y = element_text(size=10, family="font"),
        panel.grid = element_line(colour = "#F5F5F5"))

grafico <- plot_grid(grafico1, grafico2, ncol=2)

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=10, height=8)
ggsave(filename = paste0(filename, ".pdf"), path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/pdf/"),
       plot=grafico, dpi=72, width=10, height=8)