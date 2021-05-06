rm(list=ls())   # Limpiamos todos los objetos creados en R. la función ls() indica los nombres de todos los objetos y rm() los remueve.
graphics.off() # Limpiamos los gráficos en el ambiente Plots (esquina inferior derecha)
set.seed(12345) #Fijamos una semilla de aleatoriedad. Se debe realizar si se toman muestras aleatorias o se generan datos aleatorios. 


library(dplyr)
library(knitr) #llamamos a la librería que nos permite visualizar contenido en Rmarkdown
library(readr)
library(lubridate)
library(ggplot2)
library(lattice) 
library(caret) 
library(Hmisc) # todos los histogramas ??
supermarket_sales <- read_csv("./Tarea1/supermarket_sales.csv")
View(supermarket_sales)

#cambiarle el nombre a la base para que sea mas corto y mas facil de trabajar
df <- supermarket_sales

#transformar a categoricas
df$Branch = as.factor(df$Branch)
df$City =as.factor(df$City)
df$`Customer type`= as.factor(df$`Customer type`)
df$Gender =as.factor(df$Gender)
df$`Product line` = as.factor(df$`Product line`)
df$Payment = as.factor(df$Payment)


#Pasar Date a tipo fecha
df$Date = as.Date(df$Date, tryFormats = c("%m/%d/%Y"))
class(df$Date)

#Nivel de agregacion
#1. clasificamos por semana c/fecha
df$Semana <- cut(df$Date, breaks = "1 week", labels = FALSE)
df$Semana = as.factor(df$Semana)
df <- df[with(df, order(df$Date)), ] 




#3. Agregamos el día de la semana
View(df)
TablaA

#Divsion de los datos
# samples aleatorio
index <- sample(1:nrow(df), size= nrow(df)*0.7)
# entrenamiento 70%
train <- df[index, ]
# test 30%
test  <- df[-index, ]


# EDA ####
#0. Histograma de todas las variables
hist.data.frame(df[,-c(1,11)])



#1. Histograma variable de interes
ggplot(data=df)+ #Se define un gráfico con ggplot()
  aes(x=Rating)+ #Solo le ingresamos el eje "x" para un histograma
  geom_histogram(col="black", fill="green", alpha = 0.2) # Se define la forma del gráfico. "col" pinta el contorno, "fill" el entorno y "alpha" entrega transparencia 

#2. Histograma Unit price
ggplot(data=df)+
  aes(x=Total)+
  geom_histogram(col="black", fill="green", alpha=0.2)+
  xlab("Precio unitario")+ #Etiqueta para el eje x
  ylab("Frecuencia")+ #Etiqueta para el eje y
  ggtitle("Distribución Precio unitario")+ #Título del gráfico
  theme(plot.title = element_text(hjust = 0.5)) #centra el título en el gráfico. Lo ajusta en la posición horizontal (hjust = 0.5)

#2.2. Histograma Unit price
ggplot(data=df)+
  aes(x=log(Total))+
  geom_histogram(col="black", fill="green", alpha=0.2)+
  xlab("Precio unitario")+ #Etiqueta para el eje x
  ylab("Frecuencia")+ #Etiqueta para el eje y
  ggtitle("Distribución Precio unitario")+ #Título del gráfico
  theme(plot.title = element_text(hjust = 0.5)) #centra el título en el gráfico. Lo ajusta en la posición horizontal (hjust = 0.5)


#3. Scatterplot Branch
ggplot(df) +
  aes(x=Total, y=Rating, col=Branch)+ #Se agrega una dimensión de colores "col".
  geom_point(size=1, alpha=0.4) +
  geom_smooth(se=FALSE, method="lm") +
  xlab("Precio unitario") 

#3.2. Scatterplot Branch
ggplot(df) +
  aes(x=log(Total), y=Rating, col=Day)+ #Se agrega una dimensión de colores "col".
  geom_point(size=1, alpha=0.4) +
  geom_smooth(se=FALSE, method="lm") +
  xlab("Precio unitario") 

#4. Boxplot Branch
ggplot(df) +
  aes(x=Branch, y=Rating) +
  geom_boxplot(alpha=0.4, fill="black") #cambiamos el tipo de gráfico

#5. Boxplot Branch
ggplot(df) +
  aes(x=Branch, y=log(Total)) +
  geom_boxplot(alpha=0.4, fill="black") #cambiamos el tipo de gráfico

# Obs: Vemos que los precios son similares, pero que van subiendo un poco de Branch en Branch en cuando 
# la media. En cambio, en Rating, se ve una clara baja de este en la Branch B.

#6. Boxplot Branch
ggplot(df) +
  aes(x=Day, y=Rating) +
  geom_boxplot(alpha=0.4, fill="black") #cambiamos el tipo de gráfico

#7. Boxplot Branch
ggplot(df) +
  aes(x=Day, y=`Unit price`) +
  geom_boxplot(alpha=0.4, fill="black") #cambiamos el tipo de gráfico

#8. Boxplot Branch
ggplot(df) +
  aes(x=Semana, y=`Unit price`) +
  geom_boxplot(alpha=0.4, fill="black") #cambiamos el tipo de gráfico

#9. Boxplot Branch
ggplot(df) +
  aes(x=Semana, y=Rating) +
  geom_boxplot(alpha=0.4, fill="blue") #cambiamos el tipo de gráfico

# genero
#10. Boxplot Branch
ggplot(df) +
  aes(x=Gender, y=`Unit price`) +
  geom_boxplot(alpha=0.4, fill="black") #cambiamos el tipo de gráfico

#11. Boxplot Branch
ggplot(df) +
  aes(x=Gender, y=Rating) +
  geom_boxplot(alpha=0.4, fill="blue") #cambiamos el tipo de gráfico

# Payment
#12. Boxplot Branch
ggplot(df) +
  aes(x=Payment, y=`Unit price`) +
  geom_boxplot(alpha=0.4, fill="black") #cambiamos el tipo de gráfico

#13. Boxplot Branch
ggplot(df) +
  aes(x=Payment, y=Rating) +
  geom_boxplot(alpha=0.4, fill="blue") #cambiamos el tipo de gráfico

# Customer type
#14. Boxplot Branch
ggplot(df) +
  aes(x=`Customer type`, y=`Unit price`) +
  geom_boxplot(alpha=0.4, fill="black") #cambiamos el tipo de gráfico

#15. Boxplot Branch
ggplot(df) +
  aes(x=`Customer type`, y=Rating) +
  geom_boxplot(alpha=0.4, fill="blue") #cambiamos el tipo de gráfico

# Customer type
#16. Boxplot Branch
ggplot(df) +
  aes(x=`Customer type`, y=`Unit price`) +
  geom_boxplot(alpha=0.4, fill="black") #cambiamos el tipo de gráfico

#17. Boxplot Branch
ggplot(df) +
  aes(x=`Customer type`, y=Rating) +
  geom_boxplot(alpha=0.4, fill="blue") #cambiamos el tipo de gráfico





# REGRESIONES #################################################################
#0. Regresion lineal sin diferenciar por tienda
train.lm <- train(form = Rating ~ (Total + Branch + `Customer type` + Gender + `Product line` + Payment )^2, #Fórmula
                  data = train, #Datos
                  method = "lm", #Algoritmo 
                  trControl = trainControl(method = "cv", number = 5) #Method = cross validation, number=10 (k-fold) 
)
test.lm  <- predict(train.lm , newdata=test) #Vector de datos predichos. Recibe una base de datos (newdata) y un modelo entrenado (train.lm)
error.lm <- test$Rating-test.lm #Calcular los errores de predicción (dato real - dato estimado)
summary(train.lm)
print(paste('Error de predicción: ', mean(abs(error.lm))))



#1. Regresion lineal sin diferenciar por tienda (con menos variables)
train.lm <- train(form = Rating ~ `Unit price`+ Branch + Gender + Semana, #Fórmula
                  data = train, #Datos
                  method = "lm", #Algoritmo 
                  trControl = trainControl(method = "cv", number = 5) #Method = cross validation, number=10 (k-fold) 
)

test.lm  <- predict(train.lm , newdata=test) #Vector de datos predichos. Recibe una base de datos (newdata) y un modelo entrenado (train.lm)
error.lm1 <- test$Rating-test.lm #Calcular los errores de predicción (dato real - dato estimado)
summary(train.lm)


#2. Regresion lineal diferenciando por tienda
allstores = unique(train$Branch) # Valores únicos de cada tienda

nobs = nrow(train)  # filas del set de training
nbranches = length(allstores) # n de tiendas

atributos_interes <- c('Total','Gender')
#regcoeff = array(NA, dim=c(nbranches,length(atributos_interes))) # matriz auxiliar vacía para los coefs de las 3 tiendas
regcoeff = array(NA, dim=c(nbranches,11))
# Loop en las tiendas
for (i in 1:nbranches){
 
  sub_train <- train[(train$Branch==allstores[i]),]

  
  
  #regrmodel = lm(Rating2 ~ Uprice + Gender2 + Semana2)
  reg_model <- lm(Rating ~Total + `Customer type` + Gender + `Product line` + Payment, data = sub_train)
  
  #print(summary(reg_model))
 # print(reg_model$coeff)
  #regcoeff[i,] = reg_model$coeff
  
  sub_test <- test[test$Branch==allstores[i],]
  reg_model_test.lm  <- predict(reg_model , newdata=sub_test) #Vector de datos predichos. Recibe una base de datos (newdata) y un modelo entrenado (train.lm)
  error.lm2 <- (sub_test$Rating -  reg_model_test.lm) #Calcular los errores de predicción (dato real - dato estimado)
  summary(reg_model)
  print(paste('Branch:', allstores[i],'/tError:',mean(abs(error.lm2))))
}




# 3. LASSO
library(glmnet) #libreria para usar lasso
x <- as.matrix(train[,c(1:17)])
y <- as.matrix(train[,c(19)])
lassomodel = glmnet(x, y, alpha=1, lambda=0.05)

# 4. Ridge
ridgemodel = glmnet(x, y, alpha=2, lambda=0.05)

#5. ML --> KNN
train.knn <- train(Rating ~ `Unit price`+ Branch + Gender + Semana, 
                   data=train, method="knn",  
                   trControl = trainControl("cv", number=5),
                   preProcess = c("center","scale"),
                   tuneLength = 5 
)

print(train.knn)
ggplot(train.knn)
test.knn  <- predict(train.knn, newdata=test) 
error.knn <- test$Rating-test.knn

# 6. ML --> Random forest
train.randomf <- train(Rating ~ `Unit price`+ Branch + Gender + Semana, 
                       data=train, method="rf",  
                       trControl = trainControl("cv", number=5),
                       preProcess = c("center","scale"),
                       tuneLength = 5 
)
print(train.randomf)
ggplot(train.randomf)
test.randomf  <- predict(train.randomf, newdata=test) 
error.randomf <- test$Rating-test.randomf

# Comparacion modelos RL y ML
rating.test <- data.frame(lm1=test.lm, lm2=regrmodel_test.lm, knn=test.knn, rf=test.randomf, rating=test$Rating)
error.test <- data.frame(lm1=error.lm1, lm2=error.lm2, knn=error.knn, rf=error.randomf)

summary(abs(error.test))
summary(error.test)
boxplot(abs(subset(error.test))); title(main="ML models", sub="Forecasting Absolute Errors")
boxplot(subset(error.test)); title(main="ML models", sub="Forecasting Errors")


##ERRORES
#no funcionan los modelos automaticos
## para el boxplot de comparacion ML y RL hay un error en el numero de filas pq el modelo 2 no calza
