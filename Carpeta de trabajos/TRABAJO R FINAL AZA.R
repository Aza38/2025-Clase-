# AzaharaGildelaPlaza_Trabajo2.R
# Trabajo final Bioinformática - Curso 25/26
# Análisis de parámetros biomédicos por tratamiento

# 1. Cargar librerías (si necesarias) y datos del archivo "datos_biomed.csv". (0.5 pts)
library(ggplot2)
library(dplyr)

datos <- read.csv("datos_biomed.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
#Comprobamos que esté bien
head(datos)

#Lo que estamos viendo aquí es pasando a R la información biomédica que voy a analizar. Mi archivo contiene la muestra de cada paciente.
#Este conjunto de datos que me da lo que quiere decir es que el Fármaco B provoca cambios medibles de glucosa, presión y colesterol en comparación al grupo placebo. 
#Busca ver si tienen efecto metabólico o cardiovascular frente a no recibir el tto. 


# 2. Exploración inicial con las funciones head(), summary(), dim() y str(). ¿Cuántas variables hay? ¿Cuántos tratamientos? (0.5 pts)

#voy a ver las primeras filas 
head(datos) #ya lo había hecho, muestra las primeras 6 filas del data frame.  
#ahora un resumen de cada variable 
summary(datos) #aquí el resultado lo que me dice son los datos de la glucosa, presión y colesterol.
#el valor del individuo con la glucosa más baja (min) el 1st Qu es el valor típico de los pacientes con glucosa más baja (el 25% de los pacientes tienen menos de ese valor)
#La mediana, la mitad de los pacientes tienen menos de 106.45 mg/dL y la otra mitad más. Representa el valor central típico. 
#La media que es el valor medio de la glucosa del grupo, el normal cercano al ideal. 
#El 3qr es el valor tipico entre los que tienen la glucosa más alta. El 75% de los pacientes tienen menos de 115.62 mg/dl.
#Y por último el max que es la glucosa más elevada que puede sugerir hiperglucemia leve. 


# 3. Una gráfica que incluya todos los boxplots por tratamiento. (1 pt)
ggplot(datos, aes(x = Tratamiento, y = Glucosa, fill = Tratamiento)) + #Crea un gráfico a partir del data frame datos donde definimos que variable va al eje X y cual al eje Y. Ponemos el tratamiento en el eje X y la glucosa en el eje Y. 
  #Fill=tratamiento para distinguir visualmente cada uno, coloreamos cada grupo de boxplot de un color distinto.
  geom_boxplot() + #aquí resumo la distribución de la glucosa en cada grupo, dibujo el boxplot propiamente dicho.
  labs(title = "Glucosa por tratamiento", #labs(...) para añadir los títulos y etiquetas a los ejes. 
       #hace el gráfico donde interpretar qué variable utilizo y porqué. 
       x = "Tratamiento",
       y = "Glucosa (mg/dL)") +
  theme_minimal() #aplico un estilo limpio y claro al gráfico (no necesario)
#GRÁFICA: el fármaco a tiene la mediana alta entorno a 110mg/dl eso podría elevar la glucosa en sangre,
#el fármaco B sobre 105mg/dl, glucosa más estable y el placebo tiene una mediana menor, cerca de 100mg/dl pero una dispersión más amplia. 

# 4. Realiza un violin plot (investiga qué es). (1 pt)
# 4. Realiza un violin plot (investiga qué es). (1 pt)

ggplot(datos, aes(x = Tratamiento, y = Glucosa, fill = Tratamiento)) +
  geom_violin(trim = FALSE) +   # forma de violín (distribución de los datos)
  #gem_violin() indica como se agrupan los valores de glucosa, dibuja el violín mostrando la distribución de frecuencia.
  #trim=false hace que el violín se dibuje entero, muestra toda la gama de glucosa observada en los pacientes. 
  geom_boxplot(width = 0.1,) +   # boxplot encima, para ver mediana y cuartiles
  #geom_boxplot añade un boxplot dentro del violín que permite ver la distribución de los valores centrales a la vez. 
  labs(title = "Distribución (violin) de glucosa por tratamiento",
       x = "Tratamiento",
       y = "Glucosa (mg/dL)") +
  theme_minimal() #otra vez no es necesario pero permite un fondo limpio.

#Explicación de la distribución del violín. 
#En el caso del Fármaco A (color rojo), la forma del violín es relativamente estrecha y simétrica,
#lo que indica que la mayoría de los pacientes presentan valores de glucosa muy similares entre sí. 
#Esto sugiere que el medicamento podría ayudar a mantener niveles estables de glucosa, reduciendo la variabilidad entre individuos.

#El Fármaco B (en verde) muestra una anchura moderada alrededor de los 100–120 mg/dL. Esto significa que existe una distribución más equilibrada: algunos pacientes tienen valores algo más altos y otros un poco más bajos. 
#En conjunto, el fármaco parece tener un efecto más variable, que puede depender de las características individuales de cada paciente.
#Por último, el grupo Placebo (azul) presenta el violín más ancho y alargado, sobre todo hacia los valores bajos de glucosa (alrededor de 70 mg/dL). 
#Esto refleja una mayor dispersión o variabilidad natural en los niveles de glucosa, típica de un grupo que no recibe tratamiento activo.

# 5. Realiza un gráfico de dispersión "Glucosa vs Presión". Emplea legend() para incluir una leyenda en la parte inferior derecha. (1 pt)
plot(datos$Glucosa, datos$Presion,
     xlab = "Glucosa (mg/dL)",
     ylab = "Presión arterial (mmHg)",
     pch = 19,
     col = as.factor(datos$Tratamiento),
     main = "Glucosa vs Presión por tratamiento")

legend("bottomright",
       legend = unique(datos$Tratamiento),
       col = 1:length(unique(datos$Tratamiento)),
       pch = 19,
       title = "Tratamiento")
#RESULTADOS: se representa cada fármaco, los paciententes que recibieron el fármaco A y B
#tienden a concentrarse en el medio eso significa que tienen parámetros dentro de lo normal
#pero el grupo placebo tiene una distribución más disperso donde se relfeja la variabilidad natural sin intervención farmacológica. 
#sugiere que los ttos no modifcan de forma evidente la relación entre glucosa y presión. 

# 6. Realiza un facet Grid (investiga qué es): Colesterol vs Presión por tratamiento. (1 pt)
ggplot(datos, aes(x = Presion, y = Colesterol)) +
  geom_point(color = "hotpink1") +
  facet_wrap(~ Tratamiento) +
  labs(
    title = "Colesterol vs Presión por tratamiento",
    x = "Presión arterial (mmHg)",
    y = "Colesterol (mg/dL)"
  ) +
  theme_minimal()

#Los puntos están dispersos los que puede indicar que no hay relación fuerte. Valores de los tres tratamientos normales. 
#ggplot(datos, aes(x = Presion, y = Colesterol)) para usar el conjunto de datos llamado datos 
#y colocar la expresión en el eje X y el colesterol en el eje Y 
#geom_point(color = "hotpink1") Dibuja puntos en el gráfico, todos del color rosa fuerte (“hotpink1”). Cada punto es una muestra o paciente
#facet_wrap(~ Tratamiento) Crea un panel separado para cada grupo de tratamiento.
#labs(title = ..., x = ..., y = ...) Añade el título del gráfico y los nombres de los ejes.

# 7. Realiza un histogramas para cada variable. (0.5 pts)
  vars_numericas <- names(datos)[sapply(datos, is.numeric)] #vars_numericas <- names(datos)[sapply(datos, is.numeric)] busca las columnas 
  #numéricas dentro del archivo datos y guarda sus nombres. 


for (v in vars_numericas) { #esto lo que hace es un bucle que repite las instrucciones para cada variable numérica. 
  hist(datos[[v]], #crea el histograma para la variable v 
       main = paste("Histograma de", v), #pone ese título
       xlab = v, #muestra lo que mido. Pone el nombre del eje X con el de la variable
       ylab = "Frecuencia", #nombre del eje Y con el de la variable (cuantos pacientes hay en cada rango)
       col = "lightblue3", #color elegido :))
       border = "white")} #el borde de las barras bonito en blanco 
  #La mayoría de los pacientes están entre 180 y 210mg/dl.
  #muy pocos por debajo de 160mg/dl o por encima de 240mg/dl.


# 8. Crea un factor a partir del tratamiento. Investifa factor(). (1 pt)
  datos$Tratamiento <- factor(datos$Tratamiento)
  str(datos$Tratamiento)
  
  #usamos factor() para converitr la columna de texto como fármacoa y farmaco b. Placebo es un factor es decir una varable que R va a 
  #entender como grupo o categoria
  #datos$tartamiento guarda la conversión dentro del conjunto de datos 
  #str(datos%trtamiento) lo que hace es comprobar que el cambio se hizo correctamente. 
  
  #a ver esto se hace porque el tratamiento no es un número sino un grupo experimental 
  #entonces lo tengo que convertir en factor y así R sabe que es una variable de clasificación 
  
  # Convertir la variable Tratamiento en factor (variable categórica)
  datos$Tratamiento <- factor(datos$Tratamiento)
  
  # Comprobar que la conversión se hizo correctamente
  str(datos$Tratamiento)
  
  # Ver los niveles del factor
  levels(datos$Tratamiento)
  #HE VUELTO AQUÍ DESDE EL 10 PORQUE LO HABÍA HECHO MAL 

# 9. Obtén la media y desviación estándar de los niveles de glucosa por tratamiento. Emplea aggregate() o apply(). (0.5 pts)
  aggregate(Glucosa ~ Tratamiento, data = datos,
            FUN = function(x) c(Media = mean(x), Desviacion = sd(x)))
 #aggregate() agrupa los datos por tratamiento.
  # ~ Tratamiento significa: “calcula algo sobre la variable Glucosa, separando por Tratamiento”.
  #FUN = function(x) ... indica qué queremos calcular.
  #En este caso, la media (promedio) y la desviación estándar (cuánto varían los valores). 
  datos$Tratamiento <- trimws(datos$Tratamiento)  # elimina espacios extra
  datos$Tratamiento <- as.factor(datos$Tratamiento)
  str(datos$Tratamiento)
  
  unique(datos$Tratamiento) #hago esto porque no me salen los datos que quiero, parece que la variable no se ha asociado
  datos$Tratamiento <- factor(datos$Tratamiento,
                              levels = c("FarmacoA", "FarmacoB", "Placebo"))
  str(datos$Tratamiento)
  #LO HE HECHO MAL ASÍ QUE VOY A CORREGIR AL 8 
  # Ya me salen bien los valores 
  
# 10. Extrae los datos para cada tratamiento y almacenalos en una variable. Ejemplo todos los datos de Placebo en una variable llamada placebo. (1 pt)
  farmacoA <- subset(datos, Tratamiento == "FarmacoA")
  farmacoB <- subset(datos, Tratamiento == "FarmacoB")
  placebo  <- subset(datos, Tratamiento == "Placebo")
  
#He hecho lo que dice el enunciado, estoy dividiendo mi tabla en tres pequeñas una por tratamiento. 
  #Coge solo las filas del conjunto datos donde el tratamiento sea FarmacoA, y las guarda en una nueva variable llamada farmacoA.
  
# 11. Evalúa si los datos siguen una distribución normal y realiza una comparativa de medias acorde. (1 pt)
  # Prueba de normalidad para cada tratamiento
  by(datos$Glucosa, datos$Tratamiento, shapiro.test) #aplico el test shapiro-wilk por cada grupo de tto. 
  #Lo que hace es comprobar los valores de glucosa por cada grupo, si siguen una distribución normal. 
  
  # Si los datos NO son normales → usar prueba no paramétrica (Kruskal-Wallis)
  #Lo que hace es que si los datos no son normales nos sirve para ver si hay diferencias en las medanas de la glucosa
  #entre los tratamientos.
  kruskal.test(Glucosa ~ Tratamiento, data = datos)
  
  # Si los datos SON normales → usar ANOVA (ya se hará en el punto 12)
  aov(Glucosa ~ Tratamiento, data = datos)
  #aov(Glucosa ~ Tratamiento, data = datos) compara si los niveles medios de glucosa son diferentes entre los 3 ttos. 
  
  
# 12. Realiza un ANOVA sobre la glucosa para cada tratamiento. (1 pt)

#me ha salido en el apartado anterior porque los datos eran normales y estaba ejecutando ya el código. 
#Los datos son normales y me permite saber si los tratamientos cambian significativamente los niveles de glucosa. 

  