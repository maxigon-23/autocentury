
#################### PAR?METROS DE plot_cent() ########################

# var: Par?metro a graficar
# sch: Nombre del archivo sch a usado en la ejecuci?n de CENTURY
# var.out: Variable de salida a graficar.
# plot.out: TRUE por omisi?n. Si TRUE guarda el gr?fico en .tiff en carpeta de salida. Si FALSE no imprime y solo se muestra en el entorno de R.
# name.type: TRUE por omisi?n. Si TRUE cambia el c?digo de las variables en el gr?fico por los ingresados en match_var.csv. Si FALSE permanecen los c?digos de CENTURY en el gr?fico.



plot_cent <- function(var, sch, var.out, plot.out=TRUE, name.type=TRUE, size=8){
  
  if(!require(readxl)){install.packages("readxl")}
  if(!require(stringr)){install.packages("stringr")}
  if(!require(tidyverse)){install.packages("tidyverse")}
  
  param <- str_remove(var, pattern="\\)") 
  param <-  str_remove(param, pattern="\\(")
  param <-  str_remove(param, pattern="\\,")
  var.out <- str_remove(var.out, pattern="\\)") 
  var.out <-  str_remove(var.out, pattern="\\(")
  var.out <-  str_remove(var.out, pattern="\\,")
  
  
  salida <- read_excel(paste(paste("salida",sch, var, sep="_"), ".xlsx", sep=""), sheet = 1)
  
  
  match_var <- read.csv2(file="match_var.csv",header=T)
  match_var$var <-   str_remove(match_var$var, pattern="'")
  match_var$var <-   str_remove(match_var$var, pattern="'")#Hay que correrlo dos veces para que borre las dos '
  match_var$var <-   str_remove(match_var$var, pattern=("\\(")) #C?digo regexp para seleccionar "("
  match_var$var <-   str_remove(match_var$var, pattern="\\)")#C?digo regexp para seleccionar ")"
  match_var$var <-   str_remove(match_var$var, pattern="\\,")#C?digo regexp para seleccionar ")"
  
  
  colnames(salida) <- str_remove(colnames(salida), pattern=("\\("))
  colnames(salida) <- str_remove(colnames(salida), pattern=("\\)"))
  colnames(salida) <- str_remove(colnames(salida), pattern=("\\,"))
  colnames(salida) <- str_remove(colnames(salida), pattern=("\\."))
  colnames(salida) <- str_remove(colnames(salida), pattern=("\\."))
  
  salida[ , 3:43] <- sapply(salida[ , 3:43], as.numeric)
  
  index1 <- match_var$var == param
  index2 <- match_var$var == var.out
  
  name_param <- match_var[index1, 3]
  name_var_out <- match_var[index2, 3]
  
  theme_set(theme_bw())
  
      if(name.type==TRUE){
        grafico <<- ggplot(salida, aes_string(x=param, y= var.out))+
          geom_point() + labs(y= name_var_out , x= name_param )+ 
          theme(axis.text=element_text(size=8), axis.title=element_text(size=size))
      }else{
        grafico <<- ggplot(salida, aes_string(x=param, y= var.out))+
          geom_point() + labs(y= var.out , x= var )+ 
          theme(axis.text=element_text(size=8), axis.title=element_text(size=size))
      }
  
  
      if(plot.out==TRUE){
        ggsave(paste(var.out, " by " , var, ".tiff", sep=""), plot = grafico, units="in", width=8, height=4.6, dpi=350, compression = 'lzw')
        print("Salida gr?fica guardada en carpeta")
      }else{
        print("Salida gr?fica NO guardada en carpeta")
      }

   return(grafico)
}




