# Limpiar todo
rm(list = ls())

# Librerías
library(tidyverse)
library(ggtext)

# Funciones
`%ni%` = Negate(`%in%`)

# Fuentes
library(sysfonts)
library(showtext)
dir <- paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/Fonts/")
font_add(family = "font_title",
         bold = file.path(dir, "CreatoDisplay-ExtraBold.otf"),
         regular = file.path(dir, "CreatoDisplay-Regular.otf"))
font_add("font_subtitle", file.path(dir, "CreatoDisplay-Regular.otf"))
font_add(family = "font_body",
         regular = file.path(dir, "RobotoSlab-Regular.ttf"),
         bold = file.path(dir, "RobotoSlab-Bold.ttf"))
showtext_auto()

# Cargar datos
Raw <- read.csv(file=paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/Datos/Registro_Asistencias_Completo_2026.csv")) %>%
  rename(Tipo = "Tipo.de.violencia..indicar.la.principal.")

# Modificar datos
Data <- Raw %>%
  mutate(Tipo = case_when(Tipo %in% c("Psicológica", "Psicologica") ~ "Psicológica",
                          Tipo %in% c("Económica y patrimonial", "economica",
                                      "Patrimonial", "Economica") ~ "Económica / patrimonial",
                          Tipo %in% c("Física", "Fisica") ~ "Física",
                          Tipo %in% c("Sexual", "SEXUAL") ~ "Sexual",
                          Tipo %in% c("Sin dato", "") ~ "Sin dato")) %>%
  filter(Tipo != "Sin dato") %>%
  group_by(Tipo) %>%
  summarise(Cantidad = n()) %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  ungroup %>%
  arrange(desc(Porcentaje)) %>%
  mutate(Ord = row_number())

Sin_dato <- Raw %>%
  filter(Tipo %in% c("Sin dato", "")) %>%
  summarise(Cantidad = n()) %>%
  filter(Cantidad > 0) %>%
  mutate(Texto = paste0("Sin dato: ", Cantidad)) %>%
  pull(Texto) %>%
  (\(x) if (length(x) == 0) NA_character_ else x)()

# Colores
Paleta <- c("#1e7b34", "#119ca0", "#b8d6ac", "#6963aa",
            "#7c428a", "#4c2158", "#c72a29", "#ec6230", "#cbc2ce")

# Gr?fico
grafico <- ggplot(Data, aes(x=Porcentaje, y=reorder(Tipo, -Ord))) +
  geom_col(aes(fill = Cantidad), width=0.9) +
  geom_text(aes(label=paste0(formatC(round(Porcentaje,1), format="fg", big.mark = ".", decimal.mark = ","), "%")),
            nudge_x = max(Data$Porcentaje)*0.02, nudge_y = 0.05, hjust=0, family="font_body", color="black", size=7.5, vjust=0, fontface="bold") +
  geom_text(aes(label=formatC(Cantidad, format="fg", big.mark=".", decimal.mark=",")),
            nudge_x = max(Data$Porcentaje)*0.02, nudge_y = -0.05, hjust=0, family="font_body", color="black", size=5, vjust=1) +
  annotate(geom="text", y=0.75, x=40, label=Sin_dato, size=4,
           family="font_body", fontface="italic", color="darkgrey") +
  labs(title="",
       y="Tipo de violencia", x="Porcentaje") +
  theme_light() +
  scale_fill_gradient(low="#B6B3D5", high="#6963aa") +
  scale_y_discrete(labels = function(z) str_wrap(z, width=10)) +
  scale_x_continuous(labels = function(z) paste0(round(z,2), "%"),
                     limits=c(0,max(Data$Porcentaje)*1.25)) +
  theme(text=element_text(family="font_body"), legend.position="none",
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        plot.caption = element_blank(),
        panel.grid.major = element_line(colour = "grey95"),
        panel.grid = element_blank(),
        axis.text.x = element_text(family="font_title", size=10, margin = margin(t=10,r=0,b=5,l=0)),
        axis.text.y = element_text(family="font_title", size=15, margin = margin(t=0,r=10,b=0,l=5)),
        axis.title.x = element_text(family="font_title", size=12.5),
        axis.title.y = element_text(family="font_title", size=12.5))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=8, height=5)
ggsave(filename = paste0(filename, ".pdf"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=8, height=5)