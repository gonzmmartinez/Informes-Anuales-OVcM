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

# Leer datos
Raw <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1mUMxGbv3x1hoVxWbTfAquSDR25YWnSDTL8T5MFkllvU/edit?usp=sharing",
                  sheet = "SUD_db_completa")

Poblacion <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1mUMxGbv3x1hoVxWbTfAquSDR25YWnSDTL8T5MFkllvU/edit?usp=sharing",
                        sheet = "Poblacion")

Data <- Raw %>%
  filter(Año %in% c(2025, 2026), Tipo %in% c("Género", "Familiar", "No penal")) %>%
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
  mutate(Año = factor(case_when(Año == 2025 ~ "2025 (todo el año)",
                                Año == 2026 ~ "2026 (primer semestre)"),
                      levels = c("2026 (primer semestre)", "2025 (todo el año)")))

# Colores
Paleta <- c("#1e7b34", "#119ca0", "#b8d6ac", "#6963aa",
            "#7c428a", "#4c2158", "#c72a29", "#ec6230", "#cbc2ce")

# Gráfico
grafico <- ggplot(Data, aes(x=Dept_facet, y=Cantidad)) +
  geom_col(fill="#119ca0") +
  geom_text(aes(label=formatC(Cantidad, big.mark=".", digits=0, decimal.mark=",", format="f")),
            color="#119ca0", size=3, nudge_y=1000, family="font_body") +
  geom_point(aes(y=Tasa*5000), size=5, color="#7c428a") +
  geom_text(aes(y = Tasa*5000, label=formatC(Tasa, digits=2, big.mark=".", decimal.mark=",", format="f")),
            color="#7c428a", size=4, nudge_y=2000, family="font_body") +
  labs(title="",
       x="Departamento", y="Cantidad de denuncias") +
  facet_wrap(~Año, nrow=2, scales="free_x") +
  theme_light() +
  scale_x_discrete(labels = function(z) str_sub(z, start=1, end=-6)) +
  scale_y_continuous(limits=c(0, 27000), labels = function(z) formatC(z, format="fg", big.mark = ".", decimal.mark = ","),
                     sec.axis = sec_axis(transform=~./5000, name=str_wrap("Tasa de denuncias por cada 100 habitantes", 30))) +
  theme(text=element_text(family="font_body"), legend.position="none",
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        plot.caption = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major = element_line(colour = "grey95"),
        axis.text.x = element_text(family="font_title", size=12, margin = margin(t=5,r=0,b=5,l=0), angle=45, hjust=1),
        axis.text.y = element_text(family="font_title", size=10, margin = margin(t=0,r=10,b=0,l=5)),
        axis.title.x = element_text(family="font_title", size=20),
        axis.title.y = element_text(family="font_title", size=20, margin=margin(r=10, l=10)),
        axis.title.y.right = element_text(family="font_title", size=20, margin=margin(r=10, l=10)),
        strip.background = element_rect(color=NA, fill="#cbc2ce"),
        strip.text = element_text(size=15, color="black", family="font_title",
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
