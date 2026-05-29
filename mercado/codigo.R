
library(officer)
library(tidytext)
library(tidyverse)
library(wordcloud)
library(RColorBrewer)

doc <- read_docx("mercados.docx")

texto_completo <- docx_summary(doc) %>% 
  filter(content_type == "paragraph") %>% 
  select(text)

# ==============================================================================
# 3. LIMPIEZA ESPECÍFICA DEL FOCUS GROUP
# ==============================================================================
# Eliminamos las etiquetas de los entrevistados (E1:, E2:, E3:) para que no cuenten
texto_limpio <- texto_completo %>%
  mutate(text = str_remove_all(text, "^E[0-9]:\\s*"))

# ==============================================================================
# 4. TOKENIZACIÓN (Separar el texto en palabras individuales)
# ==============================================================================
palabras_separadas <- texto_limpio %>% 
  unnest_tokens(output = palabra, input = text)

# ==============================================================================
# 5. ELIMINACIÓN DE PALABRAS SIN VALOR (Stop Words y Muletillas)
# ==============================================================================
# Descargamos el diccionario oficial de palabras de parada en español (de, que, el, un, etc.)
stop_words_es <- get_stopwords(language = "es")

# Creamos tu propia lista de muletillas típicas del lenguaje hablado en entrevistas
# (Puedes agregar aquí dentro de las comillas cualquier otra palabra que quieras borrar)
muletillas_focus <- data.frame(word = c(
  "sí", "bueno", "ahí", "así", "cada", "hola", "tal", "aquí", "acá", 
  "entonces", "bueno", "pues", "creo", "digo", "ejemplo", "visto",
  "gracias", "tardes", "nombre", "tengo", "años"
))

# Unimos ambos filtros de limpieza
todas_las_palabras_vacias <- bind_rows(stop_words_es, muletillas_focus)

# Aplicamos el filtro para dejar solo palabras con lógica y valor analítico
palabras_finales <- palabras_separadas %>%
  anti_join(todas_las_palabras_vacias, by = c("palabra" = "word")) %>% 
  filter(!str_detect(palabra, "^[0-9]+$")) # Borra números sueltos (como edades o bloques)

# ==============================================================================
# 6. CONTEO DE FRECUENCIAS (Opcional, para ver la lista en consola)
# ==============================================================================
frecuencia <- palabras_finales %>%
  count(palabra, sort = TRUE)

print("Las 20 palabras más repetidas con significado real:")
print(head(frecuencia, 20))

# ==============================================================================
# 7. GENERACIÓN DE LA NUBE DE PALABRAS (Wordcloud)
# ==============================================================================
# Abre una ventana limpia para el gráfico
dev.off() 

wordcloud(
  words = frecuencia$palabra, 
  freq = frecuencia$n, 
  min.freq = 2,           # Muestra palabras que aparezcan al menos 2 veces
  max.words = 60,          # Límite de palabras en la nube para que no se sature
  random.order = FALSE,    # Pone las más importantes y grandes al centro
  rot.per = 0.15,          # Porcentaje de palabras que se giran verticalmente
  colors = brewer.pal(8, "Dark2") # Paleta de colores profesional
)





































































