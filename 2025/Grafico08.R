# Limpiar todo
rm(list = ls())

# Librerías
library(tidyverse)
library(ggtext)

# Funciones
`%nin%` = Negate(`%in%`)

# Fuentes
library(showtext)
font_add_google("Poppins", "font")
showtext_auto()

# Cargar datos
Raw <- read.csv(file=paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/Datos/Registro_Asistencias_Completo_2025.csv"))

# Modificar datos
Data <- Raw %>%
  mutate(Vinculo = case_when(Vinculo == "Otro" ~ "Otro",
                             Vinculo == "Funcionario público" ~ "Funcionario Público",
                             Vinculo == "Ex pareja" ~ "Ex pareja",
                             Vinculo == "Madre/madrastra/tutora" ~ "Madre, madrastra, o tutora",
                             Vinculo == "Otro familiar" ~ "Otro familiar",
                             Vinculo == "Pareja actual" ~ "Pareja actual",
                             Vinculo == "Padre/padrastro/tutor" ~ "Padre, padrastro, o tutor",
                             Vinculo == "Superior jerárquico" ~ "Superior jerárquico",
                             Vinculo == "Sin dato" ~ "Sin dato",
                             Vinculo == "Compañero/a de trabajo" ~ "Compañero/a de trabajo",
                             Vinculo == "Familiar de la pareja/ex pareja" ~ "Familiar de la pareja/ex pareja",)) %>%
  filter(Vinculo != "Sin dato") %>%
  group_by(Vinculo) %>%
  summarise(Cantidad = n()) %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  ungroup %>%
  arrange(desc(Porcentaje)) %>%
  mutate(Ord = row_number())

Sin_dato <- Raw %>%
  filter(Vinculo == "Sin dato")

# Definir colores
Colores <- c("Mujer" = "#8cb369",
             "Varón" = "#f4a259",
             "Mujer trans" = "#bc4b51",
             "Otro" = "#735751")

# Gr?fico
grafico <- ggplot(Data, aes(x=Porcentaje, y=reorder(Vinculo, -Ord))) +
  geom_col(aes(alpha = Cantidad), fill="#8cb369") +
  geom_text(aes(hjust=ifelse(Porcentaje<10,-0.25,1.25), label=paste0(round(Porcentaje,2), "%")),
            family="font", color="black", size=4, vjust=-0.5, fontface="bold") +
  geom_text(aes(hjust=ifelse(Porcentaje<10,-1.25,2), label=Cantidad),
            family="font", color="black", size=3, vjust=1.25) +
  annotate(geom="text", y=0.75, x=50, label=paste0("Sin dato: ", nrow(Sin_dato)), size=4,
           family="font", fontface="italic", color="darkgrey") +
  labs(title="",
       y="Vínculo con la persona agresora", x="Porcentaje") +
  theme_light() +
  scale_alpha_continuous(range=c(0.5, 1)) +
  scale_y_discrete(labels = function(z) str_wrap(z, width=20)) +
  scale_x_continuous(labels = function(z) paste0(round(z,2), "%")) +
  theme(text=element_text(family="font"), legend.position="none",
        plot.title = element_text(size=20, family="font", face="bold"),
        plot.subtitle = element_text(size=15, family="font"),
        plot.caption = element_text(size=12, family="font", face="italic"),
        panel.grid.major = element_line(colour = "#F5F5F5"),
        axis.text.x = element_text(size=10, margin = margin(t=10,r=0,b=5,l=0)),
        axis.text.y = element_text(size=12, margin = margin(t=0,r=10,b=0,l=5)),
        axis.title.x = element_text(size=12.5),
        axis.title.y = element_text(size=12.5))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/"),
       plot=grafico, dpi=100, width=8, height=6.5)