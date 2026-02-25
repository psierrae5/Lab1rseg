
# 1. Cargamos el Dataset
library(tidyverse)
df <- read.csv("/Users/pablosierra/Downloads/Mall_Customers.csv.xls")
head(df)

# 2. Exploración y análisis de los datos

# 2.1. Exploramos el DataFrame
dim(df)
#Tiene 200 filas y 5 columnas
str(df)

cat("El número de clentes que tenemos es:", nrow(df))
summary(df)
# 2.2. Renombramos columnas, transformamos Gender a numérico (1 para Hombre y 0 para mujer) y normalizamos los campos
df <- df %>% 
  rename(
    CustomerID = CustomerID,
    Gender = Genre,
    Age = Age,
    Annual_income = Annual.Income..k..,
    Spending_score = Spending.Score..1.100.
  ) %>% 
  mutate(
    Gender = as.integer(Gender == "Male"),
    Annual_income = scale(Annual_income),
    Spending_score = scale(Spending_score)
  )

head(df)

# 2.3. Exploración de Variables

# Variable Gender
df_gender <- df %>% 
  group_by(Gender) %>% 
  select(CustomerID) %>% 
  count() %>%
  ungroup() %>% 
  mutate(percentage = n/sum(n)*100)
    #Tenemos 112 mujeres (56%) y 88 hombres (44%)

#Variable Age
df_age <- df %>% ggplot(aes(x= Age)) +
  geom_histogram(bins= 20, fill= "red") +
  ggtitle("Distribución Edad")

cat("El cliente de menor edad tiene: ", min(df$Age), "\n")
cat("El cliente de mayor edad tiene: ", max(df$Age), "\n")

#Variable Anual_income
df_income <- df %>% 
  ggplot(aes(x=Annual_income)) +
  geom_histogram(bins = 20, fill="green") +
  ggtitle("Distribucion de los Ingresos Anuales (k$)")

cat("El cliente con menos ingresos anuales tiene: ", min(df$Annual_income) * 1000, "$ \n")
cat("El cliente con más ingresos anuales tiene: ", max(df$Annual_income) * 1000, "$")

#Variable Spending_score
df_score <- df %>% 
  ggplot(aes(x=Spending_score)) +
  geom_histogram(bins = 20, fill="blue") +
  ggtitle("Distribucion de la Spending Score")

cat("El cliente con menos spending score tiene: ", min(df$Spending_score) * 1000, "$ \n")
cat("El cliente con más spending score tiene: ", max(df$Spending_score) * 1000, "$")

# 4. Entrenamiento de modelos de clustering

# 4.1. K-MEANS
set.seed(123)  # para reproducibilidad

tot_within <- c()  # vector vacío para guardar tot.withinss

rango <-2:6

# Probamos K de 2 a 6
for(k in rango){
  km <- kmeans(df, centers = k, nstart = 25)
  tot_within <- c(tot_within, km$tot.withinss)
}

# Graficamos las sumas de las varianzas:
codo_df <- data.frame(
  K = rango,
  TotWithinSS = tot_within
)

metodo_codo <- ggplot(codo_df, aes(x = K, y = TotWithinSS)) +
  geom_point(size = 3, color = "blue") +       # puntos
  geom_line(color = "blue") + # línea
  ggtitle("Método del Codo para K-Means") +
  xlab("Número de clusters K") +
  ylab("Tot.WithinSS") +
  theme_minimal() +
  geom_vline(xintercept = 4, linetype = "dotted", color = "red") +
  annotate("text", x = 4, y = max(tot_within), label = "Codo ≈ 4", color = "red", vjust = -1)

#El número óptimo de clusteres es 4 para K-MEANS, lo asignamos al dataframe

km_final <- kmeans(df, centers = 4, nstart = 25)
df$cluster_kmeans <- as.factor(km_final$cluster)

# 4.2. Clustering Aglomerativo

dendrograma <- df %>%
  dist(method = "euclidean") %>% 
  hclust(method="ward.D2")
plot(dendrograma, hang=-1)

#Cortamos el dendograma en k=2 
clusters_jer <- cutree(dendrograma, k = 2)
df$cluster_jerarquico <- as.factor(clusters_jer)

#5. Evaluación de Modelos

library(cluster)

# 5.1. Calculamos el coeficiente de silhouette para K-MEANS

all_silhouettes <- c()
best_silhouette <- -1
best_out <- NULL
ks <- 2:6
get_silhouette <- function(df,
                           clusters) {
  ss <- silhouette(clusters, dist(df))
  return(mean(ss[, 3]))
}

for (k in ks) {
  out <- df %>% kmeans(centers=k)
  silhouette <- get_silhouette(df,out$cluster)
  all_silhouettes <- c(all_silhouettes, silhouette)
  cat("La segmentación con", k, "segmentos tiene un silhouette de: ", 
      round(silhouette, 2), '\n')
  if (silhouette > best_silhouette) {
    best_silhouette <- silhouette
    best_out <- out
  }
}
best_kmeans <- best_out
#La segmentación con 2 segmentos es la segmentación óptima utilizando silhouette.
#con coeficiente de silhouette de 0.57.
#Como por el metodo del codo tenemos k=4, sabemos que existen dos grupos 
#cohesionados con dos subgrupos con coeficiente de silhouette de 0.45

#5.2. Calculamos el coeficiente de silhouette para el jerárquico aglomerativo
all_silhouettes <- c()
best_silhouette <- -1
ks <- 2:6
get_silhouette <- function(df,
                           clusters) {
  ss <- silhouette(clusters, dist(df))
  return(mean(ss[, 3]))
}
for (k in ks) {
  clusters <- cutree(dendrograma, k = k)
  silhouette <- get_silhouette(df,clusters)
  all_silhouettes <- c(all_silhouettes, silhouette)
  cat("La segmentación con", k, "segmentos tiene un silhouette de: ", 
      round(silhouette, 2), '\n')
  if (silhouette > best_silhouette) {
    best_silhouette <- silhouette
  }
}
best_aglomerative <- cutree(dendrograma, k = 2)

# La segmentación óptima usando un algoritmo de clustering jerarquico 
# aglomerativo es k = 2 con coeficiente de silhouette de 0.57 

#6. Análisis Descriptivo

df_kmeans <- aggregate(df[, c("Gender","Age", "Annual_income", "Spending_score")],
          by = list(Cluster = df$cluster_kmeans),
          FUN = mean)

#Con estos datos podemos ver con el K-MEANS 4 clusteres bien diferenciados:
# Cluster 1: Adultos de 37 con ingresos en torno a la media
# Cluster 2: Jovenes de 35 con un ingreso anual muy por debajo de la media
# Cluster 3: Adultos de 37 con ingresos anuales muy altos
# Cluster 4: Mayores de 45 con ingresos ligeramente por debajo de la media 

df_jerarquico <- aggregate(df[, c("Gender","Age", "Annual_income", "Spending_score")],
          by = list(Cluster = df$cluster_jerarquico),
          FUN = mean)

#Con estos datos podemos ver con el jeraquico 2 clusteres bien diferenciados:
# Cluster 1: Mayores de 41 con ingresos por debajo de la media
# Cluster 2: Jovenes de 36 con un ingreso anual por encima de la media

#7. Visualización

library(gridExtra)

p11 <- df %>%
  mutate(cluster=best_kmeans$cluster) %>%
  ggplot(aes(Annual_income, Spending_score, col=as.character(cluster))) + 
  geom_point(show.legend = FALSE)

p12 <- df %>%
  mutate(cluster=best_kmeans$cluster) %>%
  ggplot(aes(Age, Spending_score, col=as.character(cluster))) + 
  geom_point(show.legend = FALSE)

p13 <- df %>%
  mutate(cluster=best_kmeans$cluster) %>%
  ggplot(aes(Age, Annual_income, col=as.character(cluster))) + 
  geom_point(show.legend = FALSE)

p21 <- df %>%
  mutate(cluster=best_aglomerative) %>%
  ggplot(aes(Annual_income, Spending_score, col=as.character(cluster))) + 
  geom_point(show.legend = FALSE)

p22 <- df %>%
  mutate(cluster=best_aglomerative) %>%
  ggplot(aes(Age, Spending_score, col=as.character(cluster))) + 
  geom_point(show.legend = FALSE)

p23 <- df %>%
  mutate(cluster=best_aglomerative) %>%
  ggplot(aes(Age, Annual_income, col=as.character(cluster))) + 
  geom_point(show.legend = FALSE)

options(repr.plot.width=25, repr.plot.height=6)
grid.arrange(p11, p12, p13, 
             p21, p22, p23, 
             nrow = 2)