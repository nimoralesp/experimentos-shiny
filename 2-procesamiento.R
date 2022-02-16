# Generacion de BBDD agrupadas---------------------------
contador.unicos <- function(vector){length(unique(vector[!is.na(vector)]))}
Generar.BD <- function(objeto, elementos_group_by_at){
  output <- objeto %>% 
    filter(eval(parse(text = "Fecha_de_visita != '##NA##' & semana!=45"))) %>% 
    group_by_at(elementos_group_by_at) %>% 
    summarise(
      Visitas                 = n(),
      Dias                    = length(unique(Fecha_de_visita[!is.na(Fecha_de_visita)])),
      Censistas               = length(unique(Censista[!is.na(Censista)])),
      Vivienda                = length(unique(Codigo_entrevista[!is.na(Codigo_entrevista)])),
      Contacto                = sum(Contacto,na.rm = T),
      rph_completo_A          = sum(rph_completo_A,na.rm = T),
      Entrevista_completa_A   = sum(Entrevista_completa_A,na.rm = T),
      Rechazo                 = sum(Rechazo,na.rm = T),
      desocupada              = sum(desocupada,na.rm = T),
      morador_ausente         = sum(morador_ausente,na.rm = T)
    ) %>% as.data.frame()
  
  return(output)
}
Poner.Tasas <- function(objeto, cuales = c("1-8","1-9","18-19")){
  nombres_columnas <- c("Visitas", "Vivienda", "Contacto", "rph_completo_A", "Entrevista_completa_A", "Rechazo", "desocupada", "morador_ausente")
  output <- objeto
  
  if (cuales == "1-8" | cuales == "18-19"){
    nombres_tasas <- paste0("Tasa",1:8)
    for (k in 1:8){ output[,nombres_tasas[k]] = objeto[,nombres_columnas[k]]/objeto[,"Censistas"] }
  }
  nombres_tasas <- paste0("Tasa",1:9,"d")
  if (cuales == "1-9"){
    output[,nombres_tasas[1]] = NA
    for (k in 1:8){ output[,nombres_tasas[k+1]] = objeto[,nombres_columnas[k]]/objeto[,"Dias"] }
  }
  if (cuales == "18-19"){
    output[,nombres_tasas[1]] = NA
    for (k in 1:8){ output[,nombres_tasas[k+1]] = objeto[,nombres_columnas[k]]/objeto[,"Censistas"]/objeto[,"Dias"] }
  }
  
  return(output)
}

# Por dia de levantamiento
dia_grupo               <- datos %>% Generar.BD(., c("Fecha_de_visita", "Grupo_multimodal"))                       %>% Poner.Tasas(., cuales = "1-8")   # Tasas.18()
# Por dia de levantamiento y comuna
dia_grupo_comuna        <- datos %>% Generar.BD(., c("Fecha_de_visita", "Grupo_multimodal", "Codigo_Comuna_ALC"))  %>% Poner.Tasas(., cuales = "1-8")   # Tasas.18()
# Por semana
semana_grupo            <- datos %>% Generar.BD(., c("semana", "Grupo_multimodal"))                                %>% Poner.Tasas(., cuales = "18-19") # Tasas.18.19d()
# Por comuna
comuna_grupo            <- datos %>% Generar.BD(., c("Codigo_Comuna_ALC", "Grupo_multimodal"))                     %>% Poner.Tasas(., cuales = "18-19") # Tasas.18.19d()
# Por area
area_grupo              <- datos %>% Generar.BD(., c("Area_c", "Grupo_multimodal"))                                %>% Poner.Tasas(., cuales = "18-19") # Tasas.18.19d()
# Por censista
censista_grupo          <- datos %>% Generar.BD(., c("Censista", "Grupo_multimodal"))                              %>% Poner.Tasas(., cuales = "1-9")   # Tasas.19d()
# Por censista y comuna
censista_grupo_comuna   <- datos %>% Generar.BD(., c("Censista", "Grupo_multimodal", "Codigo_Comuna_ALC"))         %>% Poner.Tasas(., cuales = "1-9")   # Tasas.19d()
  
## Tabulados de productividad y esfuerzo semanal segun grupo-------------------------------------
Tabulado.Grupo <- function(objeto, atributo, agrup, vars1, vars2, max, cual, x = ""){
  t0 <- objeto %>%
    ungroup() %>% 
    filter(atributo %in% agrup) %>% 
    select_at(vars1) %>%
    as.matrix() %>% 
    t() %>% 
    as.data.frame() %>% 
    mutate_all(as.numeric)
  t1 <- objeto %>%
    ungroup() %>% 
    filter(atributo %in% agrup) %>% 
    mutate(!!paste0("Tasa0",x):=NA) %>% 
    select_at(c(vars2,paste0("Tasa",0:max,x))) %>%
    as.matrix() %>% 
    t() %>% 
    as.data.frame() %>% 
    mutate_all(as.numeric)
  output <- cbind(t0,t1)
  return(output)
}
Dos.Tablas <- function(atributo_grupo, atributo, nombres_agrup, numeros, vector, excel){
  auxiliar <- 2
  for ( contador in vector ){
    tabla_1 <- Tabulado.Grupo( objeto = atributo_grupo, atributo = atributo_grupo[,atributo], agrup = contador, vars1 = nombres_agrup,                  vars2 = c("Grupo_multimodal"),          max = 8, cual = "a" )
    tabla_2 <- Tabulado.Grupo( objeto = atributo_grupo, atributo = atributo_grupo[,atributo], agrup = contador, vars1 = append(nombres_agrup,"Dias",2), vars2 = c("Grupo_multimodal"), x = "d", max = 9, cual = "b" )
    
    writeData( wb = excel, sheet = "Censistas", x = tabla_1[-1,], xy = c(auxiliar, numeros[1]), colNames = F, rowNames = F)
    writeData( wb = excel, sheet = "Censistas", x = tabla_2[-1,], xy = c(auxiliar, numeros[2]), colNames = F, rowNames = F)
    
    auxiliar <- auxiliar + 4
  }
}

# Tabulados de productividad y esfuerzo semanal según grupo
variables = c("Grupo_multimodal", "Censistas", "Visitas", "Vivienda", "Contacto", "rph_completo_A", "Entrevista_completa_A", "Rechazo", "desocupada", "morador_ausente")
Dos.Tablas(atributo_grupo = semana_grupo, atributo = "semana", nombres_agrup = variables, numeros = c(8,24), vector = 46:51, excel = wb)

# Tabulados de productividad y esfuerzo por comuna según grupo
comuna <- c("2101","15101","5103","13106","13111","14108","14101","5101","13132")
Dos.Tablas(atributo_grupo = comuna_grupo, atributo = "Codigo_Comuna_ALC", nombres_agrup = variables, numeros = c(41,57), vector = comuna, excel = wb)


# Tabulados de productividad y esfuerzo por área según grupo
aux_vector <- c("Rural","Urbano")
Dos.Tablas(atributo_grupo = area_grupo, atributo = "Area_c", nombres_agrup = variables, numeros = c(74,90), vector = aux_vector, excel = wb)

## Test estadistico base por fecha (segun grupo y comuna)-------------------------------------

Tabla.Test <- function(data, lista_nombres, data.dia){
  output <- as.data.frame(matrix(,nrow=10,ncol=8))
  for(i in 1:8){
    output[1,i] <- t.test(data[,lista_nombres[i]] ~ Grupo_multimodal, data = data)$p.value
  }
  for (j in 1:9){
    a <- data.dia %>% filter(Codigo_Comuna_ALC==comuna[j]) 
    for (k in 1:8){
      output[j+1,k] <- t.test(a[,lista_nombres[k]] ~ Grupo_multimodal, data = a)$p.value
    }
  }
  return(output)
}

lista_1 <- c("Visitas", "Vivienda", "Contacto", "rph_completo_A" , "Entrevista_completa_A", "Rechazo", "desocupada", "morador_ausente")

tab1_dia  <- Tabla.Test(dia_grupo, lista_1, dia_grupo_comuna)

## Test estadistico para tasas de productividad por fecha (segun grupo y comuna)-------------------------------------

lista_2 <- c("Tasa1", "Tasa2", "Tasa3", "Tasa4", "Tasa5", "Tasa6", "Tasa7", "Tasa8")

tab2_dia  <- Tabla.Test(dia_grupo, lista_2, dia_grupo_comuna)

## Test estadistico base por censista (segun grupo y comuna)-------------------------------------

lista_3 <- c("Visitas", "Vivienda", "Contacto", "rph_completo_A" , "Entrevista_completa_A", "Rechazo", "desocupada", "morador_ausente")

tab1_cen  <- Tabla.Test(censista_grupo, lista_3, censista_grupo_comuna)

## Test estadistico tasa diaria por censista (segun grupo y comuna)-------------------------------------

lista_4 <- c( "Tasa2d", "Tasa3d", "Tasa4d", "Tasa5d", "Tasa6d", "Tasa7d", "Tasa8d", "Tasa9d" )

tab2_cen  <- Tabla.Test(censista_grupo, lista_4, censista_grupo_comuna)

### EXPORTAR RESULTADOS ---------------------

lista_tablas <- list(tab1_dia, tab1_cen, tab2_dia, tab2_cen)

aux <- c(8,23,40,55)
for (k in 1:4){   
  writeData( wb = wb, sheet = "Tests T", x = lista_tablas[[k]], xy = c(2, aux[k]), colNames = F )   
  }

showGridLines( wb = wb, sheet = "Tests T", showGridLines = F )


