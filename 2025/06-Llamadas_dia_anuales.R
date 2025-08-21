# Limpiar todo
rm(list = ls())

# Librerías
library(ggplot2)
library(dplyr)
library(stringr)
library(cowplot)
library(magick)
library(googlesheets4)

# Fuentes
library(showtext)
font_add_google("Barlow", "font")
showtext_auto()

# Leer datos
Raw <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1fX8iWndJKs_UTTcB1SoU5tpTK7ysVvxJeyVAE0C5gro/edit?usp=sharing",
                  sheet = "Dia") %>%
  filter(Accion == "Llamadas", Tipo == "Violencia de género")

Levels <- c("Domingo", "Sábado", "Viernes", "Jueves", "Miércoles", "Martes", "Lunes")

Data <- Raw %>%
  group_by(Año, Dia) %>%
  summarise(Cantidad = sum(Cantidad)) %>%
  mutate(Dia = factor(Dia, levels=Levels),
         Año = formatC(Año, big.mark=".", decimal.mark=",")) %>%
  group_by(Año) %>%
  mutate(Porcentaje = Cantidad/sum(Cantidad) * 100) %>%
  ungroup %>%
  mutate(Año = ifelse(Año %in% c("2.023", "2.025"), paste0(Año, "*"), Año))

# Gráfico
grafico <- ggplot(Data, aes(x="", y=Dia, fill=Porcentaje)) +
  geom_tile() +
  facet_wrap(~Año, nrow=1) +
  geom_text(aes(label = paste0(formatC(round(Porcentaje,1), big.mark=".", decimal.mark=","),"%")),
            family="font", size=5, color="black", alpha=0.7) +
  labs(x="Año", y="Día de la semana",
       caption="* las proporciones se calculan en base a los datos correspondientes al primer trimestre únicamente.") +
  scale_fill_gradient2(low="#1daa6a", mid="#FAF99F", high="#a5549c", midpoint=mean(Data$Porcentaje)) +
  theme_light() +
  theme(text=element_text(family="font"), legend.position="none",
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        plot.caption = element_text(size=10, family="font", face="italic"),
        strip.text = element_text(size=15, family="font", face="bold"),
        panel.grid.major = element_line(colour = "#F5F5F5"),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size=15, margin = margin(t=0,r=5,b=0,l=5)),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=15, margin = margin(t=0,r=5,b=0,l=0)),
        axis.ticks.x = element_blank())

# Guardar gr?fico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=12, height=7)
ggsave(filename = paste0(filename, ".pdf"), path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=12, height=7)