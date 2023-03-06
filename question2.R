# Limpieza
rm(list = ls())

#Librerias
#install.packages("mlbench")
library(mlbench)
library(data.table)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(xtable)

#data
data(Glass)
str(Glass)
summary(Glass)

#===============================================================================
#  Data Glass
#===============================================================================
#  Attribute Information:
#  1. Id number: 1 to 214
#  2. RI: refractive index
#  3. Na: Sodium (unit measurement: weight percent in corresponding oxide, as 
#                 are attributes 4-10)
#  4. Mg: Magnesium
#  5. Al: Aluminum
#  6. Si: Silicon
#  7. K: Potassium
#  8. Ca: Calcium
#  9. Ba: Barium
#  10. Fe: Iron
#  11. Type of glass: (class attribute)
#  -- 1 building_windows_float_processed
#  -- 2 building_windows_non_float_processed
#  -- 3 vehicle_windows_float_processed
#  -- 4 vehicle_windows_non_float_processed (none in this database)
#  -- 5 containers
#  -- 6 tableware
#  -- 7 headlamps
#  

#---------------------------------
# Detección usando la puntuación Z
#---------------------------------

is.outlier_z <- function(x, k=2) {
  return(abs(scale(x)) > k)           # scale: (x-media)/desv_est
}

# Registros atípicos para todas las variables (9 variables)
l_names <- c("RI","Na","Mg","Al","Si","K","Ca","Ba","Fe")
l_names

l_outliers <- list()
for(i in 1:length(l_names)){
  nombre <-  l_names[i]
  # Indices (T/F) de los valores atipicos 
  idx_outliers_z <- is.outlier_z(Glass[,i], k=3)
  # Registros asociados con los valores atípicos
  dt_outliers <- Glass[idx_outliers_z, ]
  l_outliers[[nombre]] <- dt_outliers
}

#print(paste("--> ", nombre))
#print(l_outliers[[nombre]])

xt_estad <- list()
xt_reg_outl <- list()
for(i in 1:length(l_names)){
  dt_Glass <- data.table(Glass)
  dt_aux <- dt_Glass[,c(l_names[i]),with = F]
  variable_continua <- l_names[i]
  var <- dt_Glass[, variable_continua, with = FALSE][[1]]
  dt_estadisticas <- data.table(data.frame(NCases = length(var)))
  dt_estadisticas[, ":="(Mean, mean(var, na.rm = TRUE))]
  dt_estadisticas[, ":="(Median, median(var,na.rm = TRUE))]
  dt_estadisticas[, ":="(StDev, sd(var, na.rm = TRUE))]
  dt_estadisticas[, ":="(Q25, quantile(var,0.25, na.rm = TRUE)[[1]])]
  dt_estadisticas[, ":="(Q75, quantile(var,0.75, na.rm = TRUE)[[1]])]
  dt_estadisticas[, ":="(Max, max(var, na.rm = TRUE))]
  dt_estadisticas[, ":="(Min, min(var, na.rm = TRUE))]
  n_outliers <-nrow(l_outliers[[i]])
  dt_estadisticas[,n_outliers:=n_outliers]
  
  xt_estad[[i]] <- xtable(dt_estadisticas,
                          label = "",
                          caption = paste0("Estadisticas de la variable: ",variable_continua),
                          align="l|l|r|r|r|r|r|r|r|r|",digits=2)
  
  # Imprimir todos los valores atípicos por variable
  dt_out <- data.table(l_outliers[[i]])
  xt_reg_outl[[i]] <- xtable(dt_out,
                             label = "",
                             caption = paste0("Registros atipicos de la variable: ",variable_continua),
                             align="l|l|r|r|r|r|r|r|r|r|r|",digits=2)
  
}

#Imprimir los outliers
for(i in 1:length(l_names)){
  print(xt_estad[[i]], include.rownames = FALSE)
}

#Imprimir los outliers
for(i in 1:length(l_names)){
  print(xt_reg_outl[[i]], include.rownames = FALSE)
}
#-----------------------------------------------------------------------------
# Función para detectar outliers usando la regla de Tukey
is.outlier <- function(x, k=1.5) {
  return(x < quantile(x,0.25)-k*IQR(x) | x > quantile(x,0.75)+k*IQR(x))
}

graf_boxplot <- function(data, variable){
  data = data
  var = variable
  g <- Glass %>%
    mutate(outlier = ifelse(is.outlier(get(var)), get(var), as.numeric(NA))) %>%
    ggplot(., aes(x = 1, y = get(var))) +
    geom_boxplot(fill="lightblue") +
    geom_text(aes(label = outlier), na.rm = TRUE, hjust = -0.3) +
    labs(y = var, x="")+
    theme_bw()
  return(list(g))
}

g <- list()
for(i in 1:length(l_names)){
  print(l_names[i])
  g[i]<- graf_boxplot(data = Glass, variable = l_names[i])
}

graf_boxplot(data = Glass, variable = "Ba")

# Generar una sola gráfica
 final_plot <- annotate_figure(
   ggarrange(g[1], g[2], g[3], g[4], g[5], g[6], g[7], g[8], g[9], ncol=3, nrow=3),
   top = text_grob("Análisis Univariado de Valores Extremos", size = 15))
 final_plot

# Boxplot mostrando los valores atípicos de cada variable
library(gridExtra)    
g_plot <- grid.arrange(grobs=c(g[1], g[2], g[3], g[4], g[5], g[6], g[7], g[8], g[9]), ncol=3, nrow=3,
                       top = "Análisis Univariado de Valores Extremos")
plot(g_plot)


# Resultados:
# Según el analisis Z y graficos de boxplot todas las variables tienen valores atípicos
# con excepción de la variable "Mg: Magnesium".


# Solo si se quiere guardar los objetos
save(xt_estad,xt_reg_outl,g_plot,l_names,
      g,
      file = "ws_tarea.Rdata")

