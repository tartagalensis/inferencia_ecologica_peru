#### ANALISIS RESULTADOS INFERENCIA ECOLOGICA
#### Author: Franco Galeano
#### Init: 8 Junio 2020

########################################################
### El objetivo de este script es realizar tablas y
###un diagrama de sankey con los resultados del script s05
#########################################################

library(ggalluvial) #paquete para diagramas de sankey
library(ggrepel)
library(janitor)
library(magick)
library(tidyverse)

# Tabla en numeros enteros para el documento
tabla_enteros <- read_csv("data/inferenciaEcoResultados.csv") %>%
  rename("1ra Vuelta / 2da Vuelta" = "indice",
         "Castillo (PL)" = "perulibre_balot",
         "Fujimori (FP)" = "fuerzapopular_balot",
         "No Voto / Blanco" = "NoVoto_Blanco") %>%
  adorn_totals("row") %>%
  mutate(Total = rowSums(.[2:4])) %>%
  print()


## Tabla en porcentajes para el documento
tabla_pcts <- read_csv("data/inferenciaEcoResultados.csv") %>%
  rename("1ra Vuelta / 2da Vuelta" = "indice",
         "Castillo (PL)" = "perulibre_balot",
         "Fujimori (FP)" = "fuerzapopular_balot",
         "No Voto / Blanco" = "NoVoto_Blanco") %>%
  adorn_totals("row") %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting() %>%
  #adorn_ns() %>%
  print()


######################
### DIAGRAMA DE SANKEY
######################
paso <- tibble(paso = c("Castillo (PL)", "Fujimori (FP)",
                        "López Aliaga (RP)", "de Soto (AP)",
                        "Lescano (AP)","Mendoza (JxP)",
                        "Acuña (APRA)","Forsyth (VN)", "Otros","Urresti (PP)","No votó/Blanco"))

base_sankey <- read_csv("data/inferenciaEcoResultados.csv") %>%
  select("Castillo (PL)" = perulibre_balot,
         "Fujimori (FP)" = fuerzapopular_balot,
         "No Voto / Blanco" = NoVoto_Blanco) %>%
  bind_cols(paso) %>%
  select(paso, everything()) %>%
  gather(key="generales", value = "freq", -paso) %>%
  arrange(desc(paso))%>%
  print()

plot_sankey <- base_sankey %>%
  ggplot(aes(axis1 = paso, axis2 = generales,
             y = freq)) +
  scale_x_discrete(limits = c("1ra Vuelta", "2da Vuelta"), expand = c(0.4, .03)) +
  geom_alluvium(aes(fill = freq)) +
  geom_stratum() +
  geom_text_repel(aes(label = ifelse(paso == paso, paso, NA)),
    stat = "stratum",infer.label = TRUE,size = 5, width = 1/4,
                  nudge_x = -.5,direction = "y",family = "Encode Sans Normal") +
  geom_text_repel(aes(label = ifelse(generales == generales, generales, NA)),
                  stat = "stratum",infer.label = TRUE,size = 5, width = 1/4,
                  nudge_x = .4,direction = "y", family = "Encode Sans Normal") +
  theme_minimal() +
  labs(title = "Transferencia de votos - Elecciones presidenciales Perú (2021)",
       caption= "@Tartagalensis") +
   theme(text = element_text(family =  "Encode Sans Normal"),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 12),
        legend.position="none",
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        )



## SAVE
ggsave(plot = plot_sankey, filename = "plots/sankey_peru.png",
       width = 10, height = 7, units = "in")

## TRIM
image_read("plots/sankey_peru.png") %>%
  image_trim() %>%
  image_write("plots/sankey_peru.png")

