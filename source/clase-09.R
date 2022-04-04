#===========================================#
# author: Eduard Fernando Martínez González
# update: 11-11-2021
# R version 4.1.1 (2021-08-10)
#===========================================#

# initial configuration
rm(list=ls()) # limpiar entorno
Sys.setlocale("LC_CTYPE", "en_US.UTF-8") # Encoding UTF-8
set.seed(12345) # fijar semilla

# load packages
require(pacman)
p_load(tidyverse , rio , data.table , png , grid)

## Hoy veremos

### **1.** Funciones

### **2.** Familia apply

#======================#
#====== Funciones =====#
#======================#

browseURL(url = "https://fhernanb.github.io/Manual-de-R/creafun.html", browser = getOption("browser")) # partes de una función
browseURL(url = "https://es.r4ds.hadley.nz/funciones.html", browser = getOption("browser")) # cuando se debería escribir una función
browseURL(url = "https://www.r-bloggers.com/2016/02/functions-exercises/", browser = getOption("browser")) # ejercicios extras
browseURL(url = "https://adv-r.hadley.nz/conditions.html", browser = getOption("browser")) # errors, messages and warnings

#----------------------#
## Funciones
dev.off()
grid.raster(readPNG("input/pics/function_machine.png")) # source: https://fhernanb.github.io

#----------------------#
## Veamos un ejemplo (...)
cat("message, warnings & errors indican diferentes niveles de mensajes. 
     *message: informa resultados... etc. 
     *warnings: informa algun tipo de error o anuncio importante que no impide el funcionamiento del programa.
     *error: indicador sobre el fua = ncionamiento correcto de la funcion.")
  
unir = function(x, y){
       palabra = paste0(x," - ",y) %>% toupper()
       message(paste("message: las combinacion de las palabras producen:", palabra))
}
unir(x = "hola", y = "clase")

#----------------------#
## Veamos otro ejemplo (...)
remove_na = function(x) x = ifelse(is.na(x)==T,0,x)

vector = c(1:5,rep(NA,5),11:15)

vector
vector = remove_na(x = vector)
vector

storms %>% head()
df = storms %>% as_tibble() 
df$tropicalstorm_force_diameter
remove_na(x = df$tropicalstorm_force_diameter) %>% print()

df = df %>% mutate(tropicalstorm_force_diameter = remove_na(tropicalstorm_force_diameter))
df %>% head()

#----------------------#
## Veamos un ejemplo (...)
cat("funcion que regresa el producto de un numero por si mismo")
num_2 = function(x){
        c = x*x
return(c)
}
num_2(x = 4)
num_2(x = "A")

# incluir controles de flujo
num_2 = function(numero){
  
        # si es un numero
        if (is.numeric(numero)){
            c = numero*numero
        return(c)
        }
        
        # si no es un numero
        if (is.numeric(numero)==F){
            warning(paste0(numero," no es un número"))
        }
}
num_2(numero = 10)
num_2(numero = "hola")
num_2(numero = "10")

#======================#
#=== Familia apply ====#
#======================#

#### Apply, Lapply & Sapply 
dev.off()
grid.raster(readPNG("input/pics/familia_apply.png"))

#----------------------#
## Apply
dev.off()
grid.raster(readPNG("input/pics/apply.png"))

####  Operaciones por columnas
mtcars
apply(X = mtcars, MARGIN = 2, FUN = min)
apply(mtcars , 2 , function(columna) min(...=columna , na.rm=T)) 

####  Operaciones por filas
apply(X = mtcars, MARGIN = 1, function(x) sum(x))

#----------------------#
## Lapply
dev.off()
grid.raster(readPNG("input/pics/lapply.png"))

lapply(mtcars, function(x) summary(x))
lap = lapply(mtcars, function(x) summary(x))
lap

storms
table(is.na(storms$hu_diameter))
table(is.na(storms$ts_diameter))
lapply(storms ,function(x)  table(is.na(x)))

#----------------------#
## Sapply
dev.off()
grid.raster(readPNG("input/pics/sapply.png"))

sap = sapply(mtcars, summary)
sap

#=========================#
#==== Aplicación: chip ===#
#=========================#

# limpiar entorno
rm(list=ls()) 

#----------------------#
## Chip
browseURL("https://www.chip.gov.co/schip_rt/index.jsf")

ejemplo = import("input/chip/2019/11767600044K212410-1220191625694914330.xls", skip = 7) %>% as_tibble()
ejemplo

#----------------------#
## 1. Obtener rutas de los datos
list.files("input/chip",full.names=T, recursive = T) 

paths = list.files("input/chip",full.names=T, recursive = TRUE) %>% unlist()

#----------------------#
## 1. Obtener rutas de los datos
list.files("input/chip",full.names=T, recursive = T) 

paths = list.files("input/chip",full.names=T, recursive = TRUE) %>% unlist()

#----------------------#
## 2. Hacer ejemplo para una observacion

## 2.1. leer archivo
df = import("input/chip/2019/11767600044K212410-1220191625694914330.xls") %>%
     as_tibble()

## 2.2. obtener codigo-DANE 
code = colnames(df)[1]

## 2.3. obtener tipo de inversion
tipo="SALUD"
df$...2
## 2.4. obtener valor
valor = df %>% subset(`...2`==tipo) %>% .[,8]

## 2.5. consolidar informacion
db = tibble(cod_dane=code , tipo_gasto=tipo , valor_gatso=valor)

#----------------------#
## 3. Generalizar ejemplo en una función
f_extrac = function(ruta,name_rubro){
  
  ## 2.1. leer archivo
  df = import(ruta) %>%
    as_tibble()
  
  ## 2.2. obtener codigo-DANE 
  code = colnames(df)[1]
  
  ## 2.3. obtener tipo de inversion
  tipo=name_rubro
    
  ## 2.4. obtener valor
colnames(df)[2] = "var"
  valor = df %>% subset(var==tipo) %>% .[,8] %>% unlist()
  
  ## 2.5. consolidar informacion
  db = tibble(cod_dane=code , tipo_gasto=tipo , valor_gatso=valor)
           
## 3.6 Retornar output
return(db)
}
f_extrac(ruta = paths[35] , name_rubro = "SALUD")
data = lapply(paths, function(x) f_extrac(ruta = x , name_rubro = "SALUD"))
data = rbindlist(l = data , use.names = T)
