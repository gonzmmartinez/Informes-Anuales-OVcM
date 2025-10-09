# Limpiar todo
rm(list = ls())

# Librerías
library(ggplot2)
library(dplyr)
library(stringr)
library(cowplot)
library(magick)
library(googlesheets4)
library(tidyr)

# Fuentes
library(showtext)
font_add_google("Source Sans 3", "font_sans")
font_add_google("Source Serif 4", "font_serif")
showtext_auto()

# Leer datos
Raw <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1fX8iWndJKs_UTTcB1SoU5tpTK7ysVvxJeyVAE0C5gro/edit?usp=sharing",
                  sheet = "Hora") %>%
  filter(Accion == "Llamadas", Tipo != "Abuso sexual")

Data <- Raw %>%
  group_by(Año, Hora) %>%
  summarise(Cantidad = sum(Cantidad)) %>%
  mutate(Hora = factor(Hora),
         Año = formatC(Año, big.mark=".", decimal.mark=",")) %>%
  group_by(Año) %>%
  mutate(Porcentaje = Cantidad/sum(Cantidad) * 100) %>%
  ungroup %>%
  mutate(Año = ifelse(Año %in% c("2.023", "2.025"), paste0(Año, "*"), Año))

Test <- Raw %>%
  group_by(Año, Hora) %>%
  summarise(Cantidad = sum(Cantidad)) %>%
  mutate(Año = formatC(Año, big.mark=".", decimal.mark=",")) %>%
  ungroup %>%
  pivot_wider(
    names_from = Año,
    values_from = Cantidad
  )

# Gráfico
grafico <- ggplot(Data, aes(x="", y=Hora, fill=Porcentaje)) +
  geom_tile() +
  facet_wrap(~Año, nrow=1) +
  geom_text(aes(label=paste0(round(Porcentaje,1),"%")), family="font", size=3, color="black", alpha=0.7) +
  labs(y="Hora del día",
       caption="* las proporciones se calculan en base a los datos correspondientes al primer trimestre únicamente.") +
  scale_y_discrete(limits = rev) + 
  scale_fill_gradient2(low="#1daa6a", mid="#FAF99F", high="#a5549c", midpoint=mean(Data$Porcentaje)) +
  theme_light() +
  theme(text=element_text(family="font_sans"), legend.position="none",
        plot.title = element_text(size=20, family="font_sans", face="bold"),
        plot.subtitle = element_text(size=15, family="font_sans"),
        plot.caption = element_text(size=10, family="font_sans", face="italic"),
        panel.grid.major = element_line(colour = "#F5F5F5"),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size=15, family="font_sans", margin = margin(t=0,r=5,b=0,l=5)),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=20, family="font_sans", margin = margin(t=0,r=5,b=0,l=0)),
        strip.background = element_rect(color=NA, fill="#FE6244"),
        strip.text = element_text(size=20, color="white", family="font_serif", face="bold"))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=10, height=10)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=10, height=10)