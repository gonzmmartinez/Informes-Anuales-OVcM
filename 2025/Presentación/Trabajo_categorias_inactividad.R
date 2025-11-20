# Limpiar todo
rm(list = ls())

# Funciones
`%ni%` <- Negate(`%in%`)

# Librerías
library(ggplot2)
library(jsonlite)
library(dplyr)
library(stringr)
library(readr)
library(eph)
library(janitor)
library(tidyr)
library(lubridate)
library(ggrepel)

# Fuentes
library(showtext)
font_add_google("Source Sans 3", "font_sans")
font_add_google("Source Serif 4", "font_serif")
showtext_auto()

# Leer datos
Raw <- read.csv(file=paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/Datos/Datos_EPH.csv")) %>%
  filter(ANO4 == 2025)

# Filtrar datos
Data0 <- Raw %>%
  filter(AGLOMERADO == 23, CH06 >= 14) %>%
  organize_labels(type = "individual")

Data0 <- organize_cno(Data0)

Data <- calculate_tabulates(
  base = Data0 %>% filter(CAT_INAC != "0"),
  x = "CAT_INAC",
  y = "CH04",
  weights = "PONDERA") %>%
  rename(Categoria = "CAT_INAC/CH04") %>%
  mutate(Categoria = case_when(Categoria == "Jubilado / pensionado" ~ "Jubilados/as Pensionados/as",
                               Categoria == "Estudiante" ~ "Estudiantes",
                               Categoria == "Ama de casa" ~ "Responsables del hogar",
                               .default = "Otras categorías de inactividad")) %>%
  group_by(Categoria) %>%
  summarise(Mujer = sum(Mujer),
            Varon = sum(Varon)) %>%
  ungroup %>%
  pivot_longer(cols = c("Varon", "Mujer"),
               names_to = "Género",
               values_to = "Cantidad") %>%
  mutate(Género = paste0(Género, "es")) %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  mutate(Categoria = factor(Categoria, levels=c("Estudiantes", "Jubilados/as Pensionados/as", "Responsables del hogar", "Otras categorías de inactividad")))

# Colores
Paleta <- c("#206170", "#5ec5d4", "#a782ec", "#852f8c", "#0f216d", "#2b42a0",
            "#ff9d27", "#ff621d", "#f93e35", "#d3335e", "#cbc2ce")

# Colores
Colores <- c("Varones" = "#852f8c",
             "Mujeres" = "#ff621d")

# Grafico
grafico <- ggplot(Data, aes(x=Categoria, y=Porcentaje, fill=Género)) +
  annotate(geom="rect", xmin=2.45, xmax=3.55, ymin=-5, ymax=26, fill="#cbc2ce", alpha=0.5, color=NA) +
  geom_col(width=0.75, position=position_dodge(width=0.75)) +
  geom_text(aes(label=paste0(formatC(round(Porcentaje,1), big.mark = ".", decimal.mark = ","), "%"), 
                group=Género), color="white", vjust=2, position=position_dodge(width=0.75),
            fontface="bold", size=7, family="font_sans") +
  scale_y_continuous(labels = function(z) paste0(z, "%"), breaks=seq(from=0, to=25, by=5)) +
  scale_x_discrete(labels = function(z) str_wrap(z, width=25)) +
  scale_fill_manual(values=Colores) +
  coord_cartesian(ylim = c(0, 27.5), xlim=c(0.5, 4.5), clip="off", expand=FALSE) +
  labs(x="Categoría de inactividad", y="Porcentaje") +
  theme_light() +
  theme(text=element_text(family="font_sans"),
        legend.position="bottom",
        legend.justification = "center",
        legend.title = element_text(size=10, family="font_serif"),
        legend.text = element_text(size=12, family="font_sans"),
        legend.key.spacing.x = unit(0.5, "cm"),
        legend.background = element_blank(),
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        plot.caption = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_text(size=17, margin = margin(t=10,r=0,b=5,l=0)),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        plot.background = element_rect(fill=NA, color=NA),
        panel.background = element_rect(fill=NA, color=NA))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=10, height=6)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=10, height=6)