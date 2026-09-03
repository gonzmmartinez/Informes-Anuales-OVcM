# Limpiar todo
rm(list = ls())

# Funciones
`%ni%` <- Negate(`%in%`)

# Librerías
library(ggplot2)
library(dplyr)
library(stringr)
library(directlabels)
library(googlesheets4)
library(lubridate)

# Fuentes
library(showtext)
font_add_google("Source Sans 3", "font_sans")
font_add_google("Source Serif 4", "font_serif")
showtext_auto()

# Diccionarios
Mes_ord <- data.frame(Mes = c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio",
                     "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"),
             Ord = 1:12)

# Leer datos
Raw <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1mUMxGbv3x1hoVxWbTfAquSDR25YWnSDTL8T5MFkllvU/edit?usp=sharing",
                  sheet = "SUD_db_completa")

Data <- Raw %>%
  filter(Año %in% c(2025, 2026), Tipo %ni% c("Penal")) %>%
  group_by(Año, Mes) %>%
  summarise(Cantidad = sum(Cantidad), .groups = "drop") %>%
  left_join(Mes_ord, by = "Mes") %>%
  arrange(Año, Ord) %>%
  mutate(
    Label = formatC(
      Cantidad,
      big.mark = ".",
      decimal.mark = ",",
      format = "fg"
    ),
    Ord = row_number()
  )

# Definir colores
Paleta <- c("#206170", "#5ec5d4", "#a782ec", "#852f8c", "#0f216d", "#2b42a0",
            "#ff9d27", "#ff621d", "#f93e35", "#d3335e", "#cbc2ce")

# Gráfico
grafico <- ggplot(Data, aes(x=reorder(Label, Ord), y=Cantidad)) +
  geom_col(aes(fill=Cantidad), width=0.9) +
  geom_text(aes(label=Label), size=5, family="font_sans",
            fontface="bold", color="white", hjust=0.5, nudge_y=-200) +
  scale_y_continuous(labels = function(z) formatC(z, big.mark = ".", decimal.mark=",", format="fg")) +
  scale_x_discrete(labels = c(str_to_title(month(1:12, label = TRUE, abbr = TRUE, locale = "Spanish_Argentina.utf8")), 
                              str_to_title(month(1:6, label = TRUE, abbr = TRUE, locale = "Spanish_Argentina.utf8")))) +
  scale_fill_gradient(low="#e999af", high="#d3335e") +
  labs(title="",
       x="Mes/Año", y="Cantidad") +
  annotate(geom="text", y=-700, x=c(6.5, 15.5), label=c(2025,2026),
           size=8, color="black", family="font_sans") +
  annotate(geom="segment", x=c(1,13,18), xend=c(1,13,18), y=-500, yend=-900, color="grey", linewidth=0.25) +
  theme_light() +
  coord_cartesian(ylim = c(-100, 5000), xlim=c(0.25, 18.75), clip="off", expand=FALSE) +
  scale_alpha_continuous(range=c(0.5, 1)) +
  theme(text=element_text(family="font_sans"), legend.position="none",
        plot.title = element_text(size=20, family="font_sans", face="bold"),
        plot.subtitle = element_text(size=15, family="font_sans"),
        plot.caption = element_text(size=12, family="font_sans", face="italic"),
        panel.grid.major = element_line(colour = "grey95"),
        axis.text.x = element_text(size=15, margin = margin(t=5,r=0,b=5,l=0)),
        axis.text.y = element_text(size=15, margin = margin(t=0,r=10,b=0,l=5)),
        axis.title.x = element_text(size=20, margin=margin(t=40)),
        axis.title.y = element_text(size=20),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=14, height=7)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=14, height=7)
