# Limpieza de entorno #### 
rm(list=ls())       # clear the list of objects
graphics.off()			# clear the list of graphs
options(digits = 3)	# number of digits to display

# Liberías ####
library(mlogit)			# load package to compare
library(mnlogit)

# Base de datos ####
dir = './Tarea2/Dataset/homogeneo.csv' # Nueva base actualizada
df <- read.table(file=dir, header=TRUE, sep=",")		#complete data file loaded in a matrix*/
df$Eleccion <- as.factor(df$Eleccion)

# Cambio de formato ####
df_formateada <- mlogit.data(df, shape="wide", varying=4:ncol(df), choice="Eleccion")		#format the data

#run logit model
# cat("mlogit - logit\n")


animelogit <- mlogit(Eleccion ~ Combat + Emotion + Qual + Duration + Rating | Edad + Mujer, data = df_formateada)		
#animelogit <- mlogit(Eleccion ~ 0 | Mujer, data = df_formateada)
summary(animelogit)


# Mixed Logit
mixed_animelogit <- mlogit(Eleccion ~ Combat + Emotion + Qual + Duration + Rating | Edad + Mujer, data = df_formateada,
                     rpar=c("(Intercept):hunterX"="n", "(Intercept):dragonball"="n", "(Intercept):naruto"="n", Combat="n", Emotion="n")
)
summary(mixed_animelogit)

