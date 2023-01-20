# PREPAREMOS EL ESPACIO DE TRABAJO
# ==============================================================================
# Instalamos el paquete tidyverse y lo cargamos para usarlo
library(tidyverse)

# Directorio de trabajo
setwd("C:/Alx/Documentos/School/Almacenes y Minería de Datos/Práctica8/SRC")
getwd()

#Cargamos el archivo en memoria
datos <- read.csv(file = "datos_groceries.csv") #Aqui pones la ruta de tu archivo .csv

# Mostrarmos los primeros productos comprados en formato de tabla larga.
head(datos)

# El dataset contiene la información de las compras realizadas por diferentes 
# clientes en una tienda de abarrotes, cada compra agrupa los productos y lo 
# mostramos en la siguiente sentencia donde se muestran el contenido de la
# compra con id=14
datos %>% filter(id_compra == 14) %>% pull(item)

# Instalamos el paquete arules y lo cargamos para usarlo
install.packages("arules")
library(arules)

# Los objetos de tipo transactions son almacenados en una matriz con valores
# 0/1, cada transacción o compra se almacena en una fila, y cada columna
# contiene cada item o producto vendido en las compras.
# En esta matriz nos dice si la i-esima transacción contiene el j-esimo item.
transacciones <- read.transactions(file = "datos_groceries.csv", #de nuevo pones la ruta absoluta de preferencia
                                   format = "single",
                                   sep = ",",
                                   header = TRUE,
                                   cols = c("id_compra", "item"),
                                   rm.duplicates = TRUE)
transacciones

# Obtener los primeros 5 nombres de las columnas en transacciones, que
# correspondena los productos vendidos
colnames(transacciones)[1:5]

#Obtener los primeros 5 nombres de los renglones en transacciones, que
# corresponden a los id's de las transacciones
rownames(transacciones)[1:5]

# CONVERSIÓN DE UN DATAFRAME A UN OBJETO TIPO TRANSACTION
# ==============================================================================
# Convertimos el dataframe en una lista en la que cada elemento contiene los
# items de su transacción
datos_split <- split(x = datos$item, f = datos$id_compra)
transacciones <- as(datos_split, Class = "transactions")
transacciones

# CONVERSIÓN DE UNA MATRIZ A UN OBJETO TIPO TRANSACTION
# ==============================================================================
datos_matriz <- datos %>%
  as.data.frame() %>%
  mutate(valor = 1) %>%
  spread(key = item, value = valor, fill = 0) %>%
  column_to_rownames(var = "id_compra") %>%
  as.matrix()
transacciones <- as(datos_matriz, Class = "transactions")
transacciones

# EXPLORANDO LOS ITEMS DEL DATAFRAME
# ==============================================================================
# Devuelve el itemset y el id de cada transacción, del renglón 1 al 5. 
inspect(transacciones[1:5])

# Convierte el tipo transactions a un data frame
df_transacciones <- as(transacciones, Class = "data.frame")

# Ajustamos el tamaño de la tabla en un tibble
as_tibble(df_transacciones) %>% head()

# Para extraer el tamaño de cada transacción se emplea la función size().
tams <- size(transacciones)
summary(tams)

# Calcula los quantiles establecidos por cada 10%
quantile(tams, probs = seq(0,1,0.1))

# Graficamos la distribución de transacciones
data.frame(tams) %>%
  ggplot(aes(x = tams)) +
  geom_histogram() +
  labs(title = "Distribución del tamaño de las transacciones",
       x = "Tamaño") +
  theme_bw()

# Guarda la frecuencia relativa de cada item en el conjunto de transacciones,
# ordenadas de mayor a menor
frecuencia_items <- itemFrequency(x = transacciones, type = "relative")
frecuencia_items %>% sort(decreasing = TRUE) %>% head(5)

# Guarda la frecuencia absoluta de cada item en el conjunto de transacciones,
# ordenadas de mayor a menor
frecuencia_items <- itemFrequency(x = transacciones, type = "absolute")
frecuencia_items %>% sort(decreasing = TRUE) %>% head(5)

# Establecemos el soporte mínimo y ejectuamos el algoritmo apriori sobre el
# conjunto de transacciones
soporte <- 30 / dim(transacciones)[1]
itemsets <- apriori(data = transacciones,
                    parameter = list(support = soporte,
                                     minlen = 1,
                                     maxlen = 20,
                                     target = "frequent itemset"))

# Mostramos el resumen
summary(itemsets)

# Mostramos el top 20 de itemsets ordenados descendentes según su soporte
top_20_itemsets <- sort(itemsets, by = "support", decreasing = TRUE)[1:20]
inspect(top_20_itemsets)

# Para representarlos con ggplot se convierte a dataframe 
as(top_20_itemsets, Class = "data.frame") %>%
  ggplot(aes(x = reorder(items, support), y = support)) +
  geom_col() +
  coord_flip() +
  labs(title = "Itemsets más frecuentes", x = "itemsets") +
  theme_bw()

# Se muestran los 20 itemsets más frecuentes formados por más de un item.
inspect(sort(itemsets[size(itemsets) > 1], decreasing = TRUE)[1:20])

# FILTREMOS ITEMSETS
# ==============================================================================
# Obtenemos los itemsets que contienen el item requerido; en este caso, 
# "newspapares"
itemsets_filtrado <- arules::subset(itemsets,
                                    subset = items %in% "newspapers")
itemsets_filtrado


# Mostramos los primeros 10 elementos
inspect(itemsets_filtrado[1:10])

# Obtenemos los itemsets que contienen el itemset requerido como subconjunto; 
# en este caso, {"newspapers", "whole milk"}
itemsets_filtrado <- arules::subset(itemsets,
                                    subset = items %ain% c("newspapers", "whole milk"))
itemsets_filtrado

# Se muestran 10 de ellos
inspect(itemsets_filtrado[1:10])

# Para encontrar los subsets dentro de un conjunto de itemsets, se compara el
# conjunto de itemsets con sigo mismo.
subsets <- is.subset(x = itemsets, y = itemsets, sparse = FALSE)

# La suma de una matriz lógica cuenta las incidencias de valores TRUE
sum(subsets)

# REGLAS DE ASOCIACIÓN
# ==============================================================================
# Establecemos el soporte y ejecutamos el algoritmo apriori sobre el conjunto
# de transacciones. Esta vez, se especifica una confianza y que se quiere
# obtener reglas de asociación
soporte <- 30 / dim(transacciones)[1]
reglas <- apriori(data = transacciones,
                  parameter = list(support = soporte,
                                   confidence = 0.70,
                                   # Se especifica que se creen reglas
                                   target = "rules"))

# Mostramos el resumen
summary(reglas)

# Mostramos las reglas ordenadas descendientemente según su confianza (4ta
# columna)
inspect(sort(x = reglas, decreasing = TRUE, by = "confidence"))

# EVALUACIÓN DE REGLAS
# ==============================================================================
# La función interestMeasure logra calcular más de 20 métricas distintas 
# generadas por la función apriori()
metricas <- interestMeasure(reglas, measure = c("coverage", "fishersExactTest"),
                            transactions = transacciones)
metricas

# Agregamos las nuevas métricas que establecimos anteriormente como atributos
# de la calidad de las reglas
quality(reglas) <- cbind(quality(reglas), metricas)

# Mostramos las reglas con estas nuevas métricas
inspect(sort(x = reglas, decreasing = TRUE, by = "confidence"))

# Convertimos el conjunto de reglas a un data frame
df_reglas <- as(reglas, Class = "data.frame") 

# Mostramos los primeros resultados del dataframe ordenado por confianza y en 
# forma de un tibble
df_reglas %>% as_tibble() %>% arrange(desc(confidence)) %>% head()

# Gracias al paquete arules nos es posible seleccionar varias formas de 
# seleccionar reglas dentro del conjunto
soporte <- 30 / dim(transacciones)[1]
reglas_vegetables <- apriori(data = transacciones,
                             parameter = list(support = soporte,
                                              confidence = 0.70,
                                              # Se especifica que se creen reglas
                                              target = "rules"),
                             appearance = list(rhs = "other vegetables")) # Se especifica que queremos
                                                                          # que el lado derecho de las reglas sea
                                                                          # {"other vegetables"}

# Mostramos el resumen de las reglas con rhs {"other vegetables"}
summary(reglas_vegetables)

# Mostramos las reglas encontradas antes. Vemos que solo hay 3 con los requerimientos
# establecidos
inspect(reglas_vegetables)

# También podemos filtrar las reglas bajo algún criterio antecedente.
filtrado_reglas <- subset(x = reglas,
                          subset = lhs %ain% c("other vegetables","citrus fruit"))
inspect(filtrado_reglas)

# Un itemSet es maximal si no hay algún otro que sea su superSet
# Entonces una regla maximal es generada a partir de un itemset maximal con la
# función is.maximal()
reglas_maximales <- reglas[is.maximal(reglas)]
reglas_maximales

# Mostramos las primeras 10 reglas maximales encontradas
inspect(reglas_maximales[1:10])

# Encontramos las reglas redundantes
reglas_redundantes <- reglas[is.redundant(x = reglas, measure = "confidence")]
reglas_redundantes

# Se identifica la regla con mayor confianza
as(reglas, "data.frame") %>%
  arrange(desc(confidence)) %>%
  head(1) %>%
  pull(rules)

# Obtenemos las transacciones que tengan el itemset requerido como subconunto;
# en este caso, {"citrus fruit", "root vegetables", "tropical fruit", "whole
# milk", "other vegetables"}
filtrado_transacciones <- subset(x = transacciones,
                                 subset = items %ain% c("citrus fruit",
                                                        "root vegetables",
                                                        "tropical fruit",
                                                        "whole milk",
                                                        "other vegetables"))
filtrado_transacciones
