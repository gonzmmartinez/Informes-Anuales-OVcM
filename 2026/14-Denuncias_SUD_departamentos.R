# Limpiar todo
rm(list = ls())

# Funciones
`%ni%` <- Negate(`%in%`)

# Librerías
library(ggplot2)
library(dplyr)
library(stringr)
library(directlabels)
library(ggrepel)
library(googlesheets4)

# Fuentes
library(showtext)
font_add_google("Source Sans 3", "font_sans")
font_add_google("Source Serif 4", "font_serif")
showtext_auto()

# Leer datos
Raw <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1mUMxGbv3x1hoVxWbTfAquSDR25YWnSDTL8T5MFkllvU/edit?usp=sharing",
                  sheet = "SUD_db_completa")

Poblacion <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1mUMxGbv3x1hoVxWbTfAquSDR25YWnSDTL8T5MFkllvU/edit?usp=sharing",
                        sheet = "Poblacion")

Data <- Raw %>%
  filter(Año %in% c(2024, 2025), Tipo %in% c("Género", "Familiar", "No penal")) %>%
  group_by(Año, Departamento) %>%
  summarise(Cantidad = sum(Cantidad)) %>%
  ungroup %>%
  left_join(Poblacion, by=c("Año", "Departamento")) %>%
  rename(Cantidad = "Cantidad.x", Poblacion = "Cantidad.y") %>%
  mutate(Tasa = 100 * Cantidad / Poblacion) %>%
  group_by(Año) %>%
  arrange(desc(Tasa)) %>%
  mutate(Ord = row_number(),
         Dept_facet = factor(paste(Departamento, Año),
                             levels = paste(Departamento, Año)[order(Ord)])) %>%
  ungroup() %>%
  mutate(Año = factor(case_when(Año == 2024 ~ "2.024 (todo el año)",
                                Año == 2025 ~ "2.025 (primer semestre)"),
                      levels = c("2.025 (primer semestre)", "2.024 (todo el año)")))

# Colores
Paleta <- c("#206170", "#5ec5d4", "#a782ec", "#852f8c", "#0f216d", "#2b42a0",
            "#ff9d27", "#ff621d", "#f93e35", "#d3335e", "#cbc2ce")

# Gráfico
grafico <- ggplot(Data, aes(x=Dept_facet, y=Cantidad)) +
  geom_col(fill="#25879e") +
  geom_text(aes(label=formatC(Cantidad, big.mark=".", decimal.mark=",", format="fg")),
            color="#25879e", size=3, nudge_y=1000, family="font_sans") +
  geom_point(aes(y=Tasa*5000), size=5, color="#d3335e") +
  geom_text(aes(y = Tasa*5000, label=formatC(round(Tasa,2), big.mark=".", decimal.mark=",", format="fg")),
            color="#d3335e", size=4, nudge_y=1500, family="font_sans") +
  labs(title="",
       x="Departamento", y="Cantidad de denuncias") +
  facet_wrap(~Año, nrow=2, scales="free_x") +
  theme_light() +
  scale_x_discrete(labels = function(z) str_sub(z, start=1, end=-6)) +
  scale_y_continuous(limits=c(0, 23000), labels = function(z) formatC(z, format="fg", big.mark = ".", decimal.mark = ","),
                     sec.axis = sec_axis(transform=~./5000, name=str_wrap("Tasa de denuncias por cada 100 habitantes", 30))) +
  theme(text=element_text(family="font_sans"), legend.position="none",
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        plot.caption = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major = element_line(colour = "grey95"),
        axis.text.x = element_text(size=12, margin = margin(t=5,r=0,b=5,l=0), angle=45, hjust=1),
        axis.text.y = element_text(size=15, margin = margin(t=0,r=10,b=0,l=5)),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20, margin=margin(r=10, l=10)),
        axis.title.y.right = element_text(size=20, margin=margin(r=10, l=10)),
        strip.background = element_rect(color=NA, fill="#cbc2ce"),
        strip.text = element_text(size=15, color="black", family="font_serif",
                                  face="bold", margin=margin(t=10, b=10)))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=14, height=10)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=14, height=10)
