
#PAR?METROS DE autocen():
# ar100: archivo de par?metros .100 a modificar. Colocar "site" si se va a modificar el archivo de par?metros del sitio <site>.100
# site: s?lo si ar100= "site" informar aqu? el nombre que toma el archivo site <site>. Si ar100 != de <site> no poner nada, este par?metro tiene valor por omisi?n NA.
# var: variable - par?metro a modificar dentro del archivo .100 definido antes
# sch: nombre del archivo sch a usar en la ejecuci?n de CENTURY
# from: valor inicial para el par?metro definido en var
# to: valor final para el par?metro definido en var
# by: amplitud de los intervalor entre from y to para var

#INFORMACI?N EXTRA:
#Para aquellos archivos con m?s de una opci?n para el conjunto de par?metros (como tree.100), dejar
#SOLO la opci?n que se va a usar (el archivo original queda en una carpeta de respaldo)

autocen <- function(ar100, site=NA, var, sch, from, to, by, out.rm=TRUE, out.table=TRUE, par.cons=TRUE){
  
  if(!require(readtext)){install.packages("readtext")}
  if(!require(tidyverse)){install.packages("tidyverse")}
  if(!require(openxlsx)){install.packages("openxlsx")}
  if(!require(readtext)){install.packages("readtext")}
  

  if(ar100=="crop" | ar100=="fix" | ar100=="tree"){
    
    assign(paste("hora.inicial", ar100, var, sch, sep="_"), format(Sys.time(), "%a %b %d %X %Y %Z"), envir = globalenv())
    
    if(par.cons==TRUE){
      file.copy(from = paste(ar100, ".100", sep=""), to=paste(ar100, "1.100", sep=""))
      print("Par?metros respaldados")
    }else{
      print("Cuidado: Valor final del par?metro ha sido modificado")
    }
    
    tabla_par <- read.delim2(paste(ar100,".100", sep=""), sep="")
    tabla_par[,1] <- as.numeric(tabla_par[,1])
    
    #Creaci?n de data.frame vac?o para sacar los outputs:
    variables <- scan("variables.txt", character(), quote = "")
    df <- data.frame(matrix(ncol = length(variables)+2 , nrow = 0))
    colnames(df) <- c(var , "time", variables)
    
    #Identificaci?n de la variable a sustituir en el archivo de par?metros:
    index <- tabla_par[2]== paste("'",var,"'", sep="") #Vector l?gico para buscar a var en el archivo de par?metros
    
    for(i in seq(from=from, to=to, by=by)){
      tabla_par[index,1] <- i #Usa el vector l?gico index para sustituir el par?metro correspondiente por el valor del contador i
      write.table(tabla_par, file = paste(ar100,".100", sep=""), sep = "    ", quote = FALSE, row.names = FALSE)#Crea el archivo de par?metros en cada vuelta del for.
      
      #Creaci?n del batch file:
      a <- paste("century -s ", sch, " -n ",paste(sch ,var,i, sep="_"), sep="")
      int <- paste("salida",sch, var, i, sep = "_")
      b <- paste(paste(paste("list100 ",sch, sep=""), var, i, sep="_"), int, "variables.txt")
      c <- rbind(a,b)       
      write.table(c, file = "EJECUCION_CENTURY.bat", quote = FALSE, row.names = FALSE, col.names=FALSE)
      
      #Ejecuci?n CENTURY:
      system2("EJECUCION_CENTURY.bat")
      
      #Borrar archivos binarios en cada loop:
      remove <- paste(paste(sch, var, i, sep = "_"), ".bin", sep="")
      unlink(remove)
      
      #OUTPUTS:
      output_name <- paste(paste("salida", sch, var,  i, sep = "_"), ".lis", sep="")
      output_table <- read.delim2(output_name, sep="")
      last_row <- tail(output_table, n=1)
      variable <- c(i)
      last_row <-  cbind(variable, last_row)
      colnames(last_row)[which(names(last_row) == "variable")] <- var
      df <- rbind(df, last_row)
      
      if(out.rm==TRUE){
        unlink(output_name)
        print("Salidas CENTURY eliminadas de directorio de trabajo")
      }else{
        print("Salidas CENTURY NO eliminadas de directorio de trabajo")
      }
      
    }
    
    salida <<- df
    
  }else if(ar100=="site"){
    
    ar100 <- site
    
    assign(paste("hora.inicial", ar100, var, sch, sep="_"), format(Sys.time(), "%a %b %d %X %Y %Z"), envir = globalenv())
    
    if(par.cons==TRUE){
      file.copy(from = paste(ar100, ".100", sep=""), to=paste(ar100, "1.100", sep=""))
      print("Par?metros respaldados")
    }else{
      print("Cuidado: Valor final del par?metro ha sido modificado")
    }
    
    tabla_par <- read.delim2(paste(site,".100", sep=""), header = FALSE)
    
    tabla1 <- as_vector(tabla_par[,1]) #Trsnsformo a tabla_par en character vector
    #Porque es la ?nica forma en la que funciona grep() para construir el index.
    
    #Creaci?n de data.frame vac?o para sacar los outputs:
    variables <- scan("variables.txt", character(), quote = "")
    df <- data.frame(matrix(ncol = length(variables)+2 , nrow = 0))
    colnames(df) <- c(var , "time", variables)
    
    for(i in seq(from=from, to=to, by=by)){
      patr <- paste("'",var,"'", sep="")
      index <- grep(pattern = patr, x= tabla1, fixed = TRUE)
      
      sustitucion <- paste(i, "               ",patr, sep = "")
      tabla_par[index, ] <- sustitucion
      
      write.table(tabla_par, file = paste(ar100,".100", sep=""), sep = "    ", quote = FALSE, row.names = FALSE, col.names = FALSE)#Crea el archivo de par?metros en cada vuelta del for.
      
      #Creaci?n del batch file:
      a <- paste("century -s ", sch, " -n ",paste(sch ,var,i, sep="_"), sep="")
      int <- paste("salida",sch, var, i, sep = "_")
      b <- paste(paste(paste("list100 ",sch, sep=""), var, i, sep="_"), int, "variables.txt")
      c <- rbind(a,b)       
      write.table(c, file = "EJECUCION_CENTURY.bat", quote = FALSE, row.names = FALSE, col.names=FALSE)
      
      #Ejecuci?n CENTURY:
      system2("EJECUCION_CENTURY.bat")
      
      #Borrar archivos binarios en cada loop:
      remove <- paste(paste(sch, var, i, sep = "_"), ".bin", sep="")
      unlink(remove)
      
      #OUTPUTS:
      output_name <- paste(paste("salida", sch, var,  i, sep = "_"), ".lis", sep="")
      output_table <- read.delim2(output_name, sep="")
      last_row <- tail(output_table, n=1)
      variable <- c(i)
      last_row <-  cbind(variable, last_row)
      colnames(last_row)[which(names(last_row) == "variable")] <- var
      df <- rbind(df, last_row)
      
      if(out.rm==TRUE){
        unlink(output_name)
        print("Salidas CENTURY eliminadas de directorio de trabajo")
      }else{
        print("Salidas CENTURY NO eliminadas de directorio de trabajo")
      }
    }
    
    salida <<- df
    
  }else{
    
    print("error")
    
  }
  
      if(out.table==TRUE){
        export <- list("salida.century" = salida) #Crea el objeto "salida" (elegir cualquier nombre) en el que se listan las
        write.xlsx(export, paste(paste("salida",sch, var, sep="_"), ".xlsx", sep=""), colWidths = c(NA, "auto", "auto")) #Exporta en arhivo .xlsx la lista creada anteriormente.
        print("Tabla de salida exportada a carpeta")
        
      }else{
        print("Tabla de salida NO exportada a carpeta")
      }
  
  
      if(par.cons==TRUE){
        unlink(paste(ar100, ".100", sep=""))
        file.copy(from = paste(ar100, "1.100", sep=""), to=paste(ar100, ".100", sep=""))
        unlink(paste(ar100, "1.100", sep=""))
        print(paste(ar100, ".100 permanece igual al archivo input", sep=""))
      }else{
        print(paste(ar100, ".100 modificado al ?ltimo valor simulado para el par?metro problema", sep=""))
      }
  
  assign(paste("hora.final", ar100, var, sch, sep="_"), format(Sys.time(), "%a %b %d %X %Y %Z"), envir = globalenv()) 
  
  print("################# EJECUCI?N AUTOCEN FINALIZADA #######################")
  
  
  
}



