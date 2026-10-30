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
  filter(Municipio %in% c("Capital", "Rosario de la Frontera",
                          "Cachi", "San Ramón de la Nueva Orán")) %>%
  rename(Tipo = "Tipo.de.violencia..indicar.la.principal.",
         Modalidad = "Modalidad.de.la.Violencia..indicar.la.principal.") %>%
  mutate(Tipo = case_when(Tipo %in% c("Psicológica", "Psicologica") ~ "Psicológica",
                          Tipo %in% c("Económica y patrimonial", "economica",
                                      "Patrimonial", "Economica") ~ "Económica / patrimonial",
                          Tipo %in% c("Física", "Fisica") ~ "Física",
                          Tipo %in% c("Sexual", "SEXUAL") ~ "Sexual",
                          Tipo %in% c("Sin dato", "") ~ "Sin dato"),
         Modalidad = case_when(Modalidad %in% c("Doméstica", "Domestica", "domestica", "Intra familia") ~ "Doméstica",
                               Modalidad %in% c("En el ambito digital") ~ "Digital",
                               Modalidad %in% c("En el espacio público / acoso callejero", "En el espacio publico") ~ "En el espacio público / acoso callejero",
                               Modalidad %in% c("OTROS") ~ "Otra",
                               Modalidad %in% c("Laboral") ~ "Laboral",
                               Modalidad %in% c("Institucional") ~ "Institucional",
                               Modalidad %in% c("Mediática") ~ "Mediática",
                               Modalidad %in% c("Sin dato", "") ~ "Sin dato"))

# Modificar datos
Data1 <- Raw %>%
  filter(Tipo != "Sin dato") %>%
  group_by(Tipo) %>%
  summarise(Cantidad = n()) %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  ungroup %>%
  arrange(desc(Porcentaje)) %>%
  mutate(Ord = row_number())

Sin_dato1 <- Raw %>%
  filter(Tipo %in% c("Sin dato", "")) %>%
  summarise(Cantidad = n()) %>%
  filter(Cantidad > 0) %>%
  mutate(Texto = paste0("Sin dato: ", Cantidad)) %>%
  pull(Texto) %>%
  (\(x) if (length(x) == 0) NA_character_ else x)()

# Modificar datos
Data2 <- Raw %>%
  filter(Modalidad != "Sin dato") %>%
  group_by(Modalidad) %>%
  summarise(Cantidad = n()) %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  ungroup %>%
  mutate(Modalidad = ifelse(Cantidad < 10, "Otra", Modalidad)) %>%
  group_by(Modalidad) %>%
  summarise(Cantidad = sum(Cantidad),
            Porcentaje = sum(Porcentaje)) %>%
  ungroup %>%
  arrange(desc(Porcentaje)) %>%
  mutate(Ord = row_number())

Sin_dato2 <- Raw %>%
  filter(Modalidad %in% c("Sin dato", "")) %>%
  summarise(Cantidad = n()) %>%
  filter(Cantidad > 0) %>%
  mutate(Texto = paste0("Sin dato: ", Cantidad)) %>%
  pull(Texto) %>%
  (\(x) if (length(x) == 0) NA_character_ else x)()

# Colores
Paleta <- c("#1e7b34", "#119ca0", "#b8d6ac", "#6963aa",
            "#7c428a", "#4c2158", "#c72a29", "#ec6230", "#cbc2ce")

# Gr?fico
grafico1 <- ggplot(Data1, aes(x=Porcentaje, y=reorder(Tipo, -Ord))) +
  geom_col(aes(fill = Cantidad), width=0.9) +
  geom_text(aes(label=paste0(formatC(round(Porcentaje,1), format="fg", big.mark = ".", decimal.mark = ","), "%")),
            nudge_x = max(Data1$Porcentaje)*0.02, nudge_y = 0.05, hjust=0, family="font_body", color="black", size=5, vjust=0, fontface="bold") +
  geom_text(aes(label=formatC(Cantidad, format="fg", big.mark=".", decimal.mark=",")),
            nudge_x = max(Data1$Porcentaje)*0.02, nudge_y = -0.05, hjust=0, family="font_body", color="black", size=3, vjust=1) +
  annotate(geom="text", y=0.75, x=max(Data1$Porcentaje)*1.1, label=Sin_dato1, size=4,
           family="font_body", fontface="italic", color="darkgrey", hjust=1) +
  labs(title="Tipos de violencia",
       y="Tipo de violencia", x="Porcentaje") +
  theme_light() +
  scale_fill_gradient(low="#B6B3D5", high="#6963aa") +
  scale_y_discrete(labels = function(z) str_wrap(z, width=10)) +
  scale_x_continuous(labels = function(z) paste0(round(z,2), "%"),
                     limits=c(0,max(Data1$Porcentaje)*1.20)) +
  theme(text=element_text(family="font_body"), legend.position="none",
        plot.title = element_text(family="font_title", face="bold", size=20, margin=margin(b=10), hjust=0.5),
        plot.title.position = "panel",
        plot.subtitle = element_blank(),
        plot.caption = element_blank(),
        panel.grid.major = element_line(colour = "grey95"),
        panel.grid = element_blank(),
        axis.text.x = element_text(family="font_title", size=10, margin = margin(t=10,r=0,b=0,l=0)),
        axis.text.y = element_text(family="font_title", size=15, margin = margin(t=0,r=10,b=0,l=0)),
        axis.title = element_blank())

# Gr?fico 2
grafico2 <- ggplot(Data2, aes(x=Porcentaje, y=reorder(Modalidad, -Ord))) +
  geom_col(aes(fill = Porcentaje), width=0.9) +
  geom_text(aes(label=paste0(formatC(round(Porcentaje,1), format="fg", big.mark = ".", decimal.mark = ","), "%")),
            nudge_x = max(Data2$Porcentaje)*0.02, nudge_y = 0.05, hjust=0,
            family="font_body", color="black", size=5, vjust=0, fontface="bold") +
  geom_text(aes(label=formatC(Cantidad, format="fg", big.mark=".", decimal.mark=",")),
            nudge_x = max(Data2$Porcentaje)*0.02, nudge_y = -0.05, hjust=0,
            family="font_body", color="black", size=3, vjust=1) +
  annotate(geom="text", y=0.75, x=max(Data2$Porcentaje)*1.1, label=Sin_dato2, size=4,
           family="font_body", fontface="italic", color="darkgrey", hjust=1) +
  labs(title="Modalidades de violencia",
       y="Modalidad de violencia", x="Porcentaje") +
  theme_light() +
  scale_fill_gradient(low="#F5AE94", high="#ec6230") +
  scale_y_discrete(labels = function(z) str_wrap(z, width=25)) +
  scale_x_continuous(breaks = seq(0, 100, by=20),
                     labels = function(z) paste0(round(z,2), "%"),
                     limits=c(0,max(Data2$Porcentaje)*1.20),
                     expand = c(0.025, 0)) +
  theme(text=element_text(family="font_body"), legend.position="none",
        plot.title = element_text(family="font_title", face="bold", size=20, margin=margin(b=10), hjust=0.5),
        plot.title.position = "panel",
        plot.subtitle = element_blank(),
        plot.caption = element_blank(),
        panel.grid.major = element_line(colour = "grey95"),
        panel.grid = element_blank(),
        axis.text.x = element_text(family="font_title", size=10, margin = margin(t=10,r=0,b=0,l=0)),
        axis.text.y = element_text(family="font_title", size=15, margin = margin(t=0,r=10,b=0,l=0), hjust=1),
        axis.title = element_blank())

# Layout
grafico <- plot_grid(grafico1, grafico2, nrow=1,
                     rel_widths = c(0.85,1)) +
  theme(plot.background = element_rect(fill = "white", colour = NA))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=14, height=5)
ggsave(filename = paste0(filename, ".pdf"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=14, height=5)