## Data Wrangling - Inferencia Ecológica Perú
#### Author: Franco Galeano
#### Init: 08 Jun 2021

## Importamos libs y paquetes
library(janitor)
library(tidyverse)


### IMPORTAR DATA

##Primera vuelta
eleccion_pv <- read_csv("data/primera_vuelta.csv") %>%
  clean_names() %>%
  mutate(votos_positivos_pv = partido_nacionalista_peruano + el_frente_amplio_por_justicia_vida_y_libertad +
          partido_morado + peru_patria_segura + victoria_nacional + accion_popular + avanza_pais_partido_de_integracion_social +
           podemos_peru + juntos_por_el_peru + partido_popular_cristiano_ppc + fuerza_popular + union_por_el_peru +
           renovacion_popular + renacimiento_unido_nacional + partido_democratico_somos_peru + partido_politico_nacional_peru_libre +
           democracia_directa + alianza_para_el_progreso,
         votos_pv = votos_positivos_pv + votos_en_blanco,
         otros_pv = partido_morado + partido_popular_cristiano_ppc + partido_nacionalista_peruano + union_por_el_peru +
           renacimiento_unido_nacional + el_frente_amplio_por_justicia_vida_y_libertad + peru_patria_segura + democracia_directa +
           partido_democratico_somos_peru) %>%
  select(ubigeo, electores_pv = num_electores,
         perulibre_pv = partido_politico_nacional_peru_libre, fuerzapopular_pv = fuerza_popular,
         renovpop_pv = renovacion_popular, avanzapais_pv = avanza_pais_partido_de_integracion_social,
         ap_pv =  accion_popular, juntosperu_pv = juntos_por_el_peru, apra_pv = alianza_para_el_progreso,
         victorianac_pv = victoria_nacional, pp_pv =podemos_peru, otros_pv, blancos_pv = votos_en_blanco,
         votos_positivos_pv, votos_pv ) %>%
  print()

#Balotaje
eleccion_balot <- read_csv("data/segunda_vuelta.csv") %>%
  mutate(votos_positivos_balot = votos_fuerzapopular + votos_perulibre,
         votos_balot = votos_positivos_balot + votos_blanco) %>%
  select(ubigeo, departamento, provincia, distrito,
         electores_balot = num_electores,
         fuerzapopular_balot = votos_fuerzapopular,
         perulibre_balot = votos_perulibre,
         blancos_balot = votos_blanco,
         votos_positivos_balot,
         votos_balot) %>%
  print()

base_join <- eleccion_pv %>%
  left_join(eleccion_balot) %>%
  drop_na() %>%
  #mutate(diferencia = electores_pv - electores_balot) %>%
  select(ubigeo, electores_pv,
         perulibre_pv, fuerzapopular_pv, renovpop_pv, avanzapais_pv, ap_pv, juntosperu_pv,
         apra_pv,victorianac_pv,otros_pv,podemos_pv = pp_pv,
         perulibre_balot,fuerzapopular_balot) %>%
  print()



# write_csv
base_join %>% write_csv("data/base_inferencia_ecologica_Peru2021.csv")

