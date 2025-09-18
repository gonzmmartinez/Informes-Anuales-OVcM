# Limpiar todo
rm(list = ls())

# Funciones
`%ni%` <- Negate(`%in%`)

# Librerías
library(ggplot2)
library(dplyr)
library(stringr)
library(directlabels)

# Fuentes
library(showtext)
font_add_google("Barlow", "font")
showtext_auto()

# Leer datos
Raw <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1Cfbecjc5DLo3uGsMEHscsfUC9YOtnKtFvt1bOZI_B4c/edit?usp=sharing",
                  sheet = "Ingresadas") %>%
  filter(Tipo != "No configura VFG")

Data <- Raw %>%
  mutate(Año = factor(Año), Trimestre = factor(Trimestre)) %>%
  filter(Año %in% c(2024, 2025)) %>%
  filter(Tipo %in% c("Familiar", "Género")) %>%
  group_by(Año, Trimestre) %>%
  summarise(Cantidad = sum(Frecuencia)) %>%
  ungroup %>%
  mutate(Orden = row_number(),
         Label = paste0(Trimestre, "-", str_sub(Año, 3,4)))

# Colores
Colores <- c("#6e3169", "#ec6489")

# Gr?fico
grafico <- ggplot(Data, aes(x=reorder(Label, Orden), y=Cantidad, group="1")) +
  geom_vline(xintercept=4.5, linetype=1, color="lightgrey") +
  geom_col(aes(alpha=Cantidad), fill=Colores[1]) +
  geom_text(aes(y=4000, label=formatC(Cantidad, big.mark = ".", decimal.mark = ",")), family="font", color="white", size=7) +
  annotate(geom="text", x=c(2.5, 5.5), y=-2000, label=c("2.024", "2.025"), size=7.5, color="grey20", family="font") +
  annotate(geom="segment", x=c(0.4, 4.5), xend=c(0.4, 4.5), y=-2500, yend=0, linetype=1, color="lightgrey", linewidth=0.5) +
  labs(title="",
       x="Trimestre-Año", y="Cantidad") +
  scale_y_continuous(labels = function(z) formatC(z, big.mark = ".", decimal.mark = ",", format="d")) +
  scale_x_discrete(labels = paste0(c(1:4, 1:2), "°")) +
  coord_cartesian(xlim=c(1,6), ylim=c(0, 10000), clip="off") +
  theme_light() +
  scale_alpha_continuous(range=c(0.5, 1)) +
  theme(text=element_text(family="font"), legend.position="none",
        plot.title = element_text(size=20, family="font", face="bold"),
        plot.subtitle = element_text(size=15, family="font"),
        plot.caption = element_text(size=12, family="font", face="italic"),
        panel.grid.major = element_line(colour = "#F5F5F5"),
        axis.text.x = element_text(size=15, margin = margin(t=10,r=0,b=5,l=0)),
        axis.text.y = element_text(size=15, margin = margin(t=0,r=10,b=0,l=5)),
        axis.title.x = element_text(size=20, margin=margin(t=40)),
        axis.title.y = element_text(size=20),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=12, height=7)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=12, height=7)