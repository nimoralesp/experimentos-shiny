# Primera parte -----------------------

library("tidyverse")
library("dplyr")
library("magrittr")
library("vtable")
library("openxlsx")
 

# Importar los datos como tibble, un tipo de dataframe
# m?s moderno, reconoce n?meros o factores como tal y 
# es m?s ordenado para imprimirse en consola

# datos <- read_rds(args)
datos <- as_tibble(datos)

datos %<>% mutate(
  desocupada = (datos$Estado_de_ocupacion %in% 2) * 1,
  rph_completo_A <- ((Completitud_RPH %in% 1) & (Cooperacion_de_los_informantes %in% 1))* 1,
  # Por qu? el anterior tiene esas dos condiciones
  Entrevista_completa_A <- (Estado_de_cuestionario %in% 1) * 1,
  Rechazo = (Razones_de_no_entrevista %in% 1) * 1,
  Contacto = (Contacto_con_los_residentes %in% 1) * 1,
)

datos %<>% filter(!is.na(Estado_de_ocupacion))


# Tabulados generales visita -----------------------
Generar.Tablas <- function(datos, especificacion = TRUE, datos_originales = NULL){
  # funciones
  contador.unicos <- function(vector){length(unique(vector[!is.na(vector)]))}
  # filtro por especificacion
  datos <- datos[especificacion,]
  # tabla peque?a
  tabla_a  <- data.frame(
    Viviendas <- datos %>% nrow(),
    Semanas <- contador.unicos(datos_originales$semana), #Tengo mis dudas con esto
    Dias <- contador.unicos(datos_originales$Fecha_de_visita),
    Censistas <- contador.unicos(datos_originales$Censista)
  )
  colnames(tabla_a) <- c("Viviendas", "Semanas", "Dias", "Cencistas")
  # tabla grande
  variables_b = c("Contacto", "rph_completo_A", "Entrevista_completa_A",
                  "Rechazo", "desocupada", "morador_ausente")
  tabla_b <- datos[variables_b] %>%
    lapply(. %>% sum) %>% 
    as.matrix() %>% 
    as.data.frame()
  # retornar tablas como una lista
  tablas <- list(
    tabla_a <- tabla_a,
    tabla_b <- tabla_b
  )
  return(tablas)
}


tabla_0.1 <- Generar.Tablas(datos, datos_originales = datos)[[1]]
tabla_0.2 <- Generar.Tablas(datos, datos_originales = datos)[[2]]

# Grupo 1
pertenencia_G1 <- (datos$Grupo_multimodal == 1)
tabla_1.1 <- Generar.Tablas(datos, pertenencia_G1, datos_originales = datos)[[1]]
tabla_1.2 <- Generar.Tablas(datos, pertenencia_G1, datos_originales = datos)[[2]]

# Grupo 2
pertenencia_G2 <- (datos$Grupo_multimodal == 2)
tabla_2.1 <- Generar.Tablas(datos, pertenencia_G2, datos_originales = datos)[[1]]
tabla_2.2 <- Generar.Tablas(datos, pertenencia_G2, datos_originales = datos)[[2]]

# Sin semana 1 (SS1) -----------------------
ss1 <- if_else(datos$semana != 45,TRUE,FALSE,FALSE)
tabla_0.1.ss1 <- Generar.Tablas(datos, ss1, datos_originales = datos)[[1]]
tabla_0.2.ss1 <- Generar.Tablas(datos, ss1, datos_originales = datos)[[2]]

# Grupo 1
pertenencia_G1.ss1 <- (datos$Grupo_multimodal == 1) & (ss1)
tabla_1.1.ss1 <- Generar.Tablas(datos, pertenencia_G1.ss1, datos_originales = datos)[[1]]
tabla_1.2.ss1 <- Generar.Tablas(datos, pertenencia_G1.ss1, datos_originales = datos)[[2]]

# Grupo 2
pertenencia_G2.ss1 <- (datos$Grupo_multimodal == 2) & (ss1)
tabla_2.1.ss1 <- Generar.Tablas(datos, pertenencia_G2.ss1, datos_originales = datos)[[1]]
tabla_2.2.ss1 <- Generar.Tablas(datos, pertenencia_G2.ss1, datos_originales = datos)[[2]]

# Tabulados por visita -----------------------
Generar.st <- function(datos, grupo = NA){
  tabla <- st(datos, 
              vars= c("Contacto", "rph_completo_A","Entrevista_completa_A","Rechazo","desocupada","morador_ausente"),
              summ = c('notNA(x)','sum(x)','mean(x)'),
              group = grupo,
              summ.names = c('N','Sum','Mean'),
              out = "return",
              group.test = F
              )
  names(tabla) <- paste0("V",1:length(names(tabla)))
  
  return(tabla)
}

visitas <- Generar.st(datos)
visitas_grupo <- Generar.st(datos, "Grupo_multimodal")
visitas_comuna <- Generar.st(datos, "Nombre_Comuna")
visitas_area <- Generar.st(datos, "Area_c")
visitas_dia <- Generar.st(datos, "dia_s")
visitas_semana <- Generar.st(datos, "semana")



# Analisis por nivel de Vivienda -----------------------------
Sumariar <- function(objeto){
  output  <- objeto %>% summarise(
    Vivienda = n(),
    Contacto = max(Contacto),
    Contacto_prop = sum(Contacto)/Vivienda,
    rph_completo_A= max(rph_completo_A),
    rph_completo_A_prop= sum(rph_completo_A)/Vivienda, 
    Entrevista_completa_A = max(Entrevista_completa_A),
    Entrevista_completa_A_prop = sum(Entrevista_completa_A)/Vivienda,
    Rechazo = max(Rechazo),
    Rechazo_prop = sum(Rechazo)/Vivienda,
    desocupada = if_else(Contacto %in% 1, 0, max(desocupada), 0),
    desocupada_prop = sum(desocupada)/Vivienda,
    morador_ausente = if_else(Contacto %in% 1, 0, max(morador_ausente), 0),
    morador_ausente_prop = sum(morador_ausente)/Vivienda,
    Comuna = max(Codigo_Comuna_ALC),
    Area_c = max(Area_c),
    Grupo_multimodal = max(Grupo_multimodal),
    Nombre_Comuna = first(Nombre_Comuna),
    # semana = semana,
    # Fecha_de_visita = Fecha_de_visita,
    # Censista = Censista
  )
  return(output)
}


vivienda <- datos %>% 
  group_by(Codigo_entrevista) %>% 
  Sumariar()

vivienda_semana <- datos %>% 
  group_by(Codigo_entrevista, semana) %>% 
  Sumariar()

vivienda_dia <- datos %>% 
  group_by(Codigo_entrevista, dia_s) %>% 
  Sumariar()


# Tabulados generales vivienda -----------------------
tabla_0.1v <- Generar.Tablas(vivienda, datos_originales = datos)[[1]]
tabla_0.2v <- Generar.Tablas(vivienda, datos_originales = datos)[[2]]

# Grupo 1
pertenencia_G1v <- (vivienda$Grupo_multimodal == 1)
tabla_1.1v <- Generar.Tablas(vivienda, pertenencia_G1v, datos_originales = datos)[[1]]
tabla_1.2v <- Generar.Tablas(vivienda, pertenencia_G1v, datos_originales = datos)[[2]]

# Grupo 2
pertenencia_G2v <- (vivienda$Grupo_multimodal == 2)
tabla_2.1v <- Generar.Tablas(vivienda, pertenencia_G2v, datos_originales = datos)[[1]]
tabla_2.2v <- Generar.Tablas(vivienda, pertenencia_G2v, datos_originales = datos)[[2]]



# Tabulados generales sin semana 1 -----------------------

vivienda_s1 <- datos %>% 
  filter(semana!=45) %>% 
  group_by(Codigo_entrevista) %>% 
  Sumariar()


tabla_0.1v.ss1 <- Generar.Tablas(vivienda_s1, datos_originales = datos)[[1]]
tabla_0.2v.ss1 <- Generar.Tablas(vivienda_s1, datos_originales = datos)[[2]]

# Grupo 1
pertenencia_G1v.ss1 <- (vivienda_s1$Grupo_multimodal == 1)
tabla_1.1v.ss1 <- Generar.Tablas(vivienda_s1, pertenencia_G1v.ss1, datos_originales = datos)[[1]]
tabla_1.2v.ss1 <- Generar.Tablas(vivienda_s1, pertenencia_G1v.ss1, datos_originales = datos)[[2]] # .....

# Grupo 2
pertenencia_G2v.ss1 <- (vivienda_s1$Grupo_multimodal == 2)
tabla_2.1v.ss1 <- Generar.Tablas(vivienda_s1, pertenencia_G2v.ss1, datos_originales = datos)[[1]]
tabla_2.2v.ss1 <- Generar.Tablas(vivienda_s1, pertenencia_G2v.ss1, datos_originales = datos)[[2]]







# Tabulados por vivienda -----------------------
viviendas <- Generar.st(vivienda)
viviendas_grupo <- Generar.st(vivienda, "Grupo_multimodal")
viviendas_comuna <- Generar.st(vivienda, "Nombre_Comuna")
viviendas_area <- Generar.st(vivienda, "Area_c")
viviendas_dia <- Generar.st(vivienda_dia, "dia_s")
viviendas_semana <- Generar.st(vivienda_semana, "semana")

# Analisis a nivel de Vivienda ocupadas -----------------------------

vivienda_o <- datos %>% 
  filter(Estado_de_ocupacion==1) %>% 
  group_by(Codigo_entrevista) %>% 
  Sumariar()

vivienda_semana_o <- datos %>% 
  filter(Estado_de_ocupacion==1) %>% 
  group_by(Codigo_entrevista, semana)  %>% 
  Sumariar()

vivienda_dia_o <- datos %>% 
  filter(Estado_de_ocupacion==1) %>% 
  group_by(Codigo_entrevista, dia_s) %>% 
  Sumariar()


# Tabulados por visita (?ocupadas?) -----------------------
viviendas_o <- Generar.st(vivienda_o)
viviendas_grupo_o <- Generar.st(vivienda_o, "Grupo_multimodal")
viviendas_comuna_o <- Generar.st(vivienda_o, "Nombre_Comuna")
viviendas_area_o <- Generar.st(vivienda_o, "Area_c")
viviendas_dia_o <- Generar.st(vivienda_dia_o, "dia_s")
viviendas_semana_o <- Generar.st(vivienda_semana_o, "semana")


# Guardo resultados --------------------------
wb <- loadWorkbook(file = "output-version-nico.xlsx")

Llenar.Excel <- function(archivo, nombre_sheet, lista_tablas, lista_coordenadas){
  n <- length(lista_tablas)
  for (k in 1:n){
    writeData(wb = archivo, 
              sheet = nombre_sheet, 
              x = lista_tablas[[k]],  
              xy = lista_coordenadas[[k]],  
              colNames = F)
  }
}

# "General"
tablas_general <- list(
  tabla_0.1, tabla_0.2, tabla_1.1, tabla_1.2, tabla_2.1, tabla_2.2,
  tabla_0.1.ss1, tabla_0.2.ss1, tabla_1.1.ss1, tabla_1.2.ss1,
  tabla_2.1.ss1, tabla_2.2.ss1
  )
coord_general <- list(
  c(2,6), c(2,9), c(2,20), c(2,23), c(9,20), c(9,23), 
  c(2,34), c(2,37), c(2,48), c(2,51), c(9,48), c(9,51))
# "General (Viviendas)"
tablas_general_v <- list(
  tabla_0.1v, tabla_0.2v, tabla_1.1v, tabla_1.2v,
  tabla_2.1v, tabla_2.2v, tabla_0.1v.ss1, tabla_0.2v.ss1,
  tabla_1.1v.ss1, tabla_1.2v.ss1, tabla_2.1v.ss1,
  tabla_2.2v.ss1
  )
coord_general_v <- list(
  c(2,6),  c(2,9),  c(2,20), c(2,23), c(9,20), c(9,23), c(2,34),
  c(2,37), c(2,48), c(2,51), c(9,48), c(9,51))
# "Visitas"
tablas_visitas <- list(
  visitas[,-1], visitas_semana[-1,-1], 
  visitas_dia[-1,-1], visitas_grupo[-1,-1],
  visitas_area[-1,-1], visitas_comuna[-1,-1])

coord_visitas <- list(c(2, 6), c(5, 18), c(5, 28), c(2, 40), c(2, 52), c(5, 62))
# "Viviendas"
tablas_viviendas <- list(
  viviendas[,-1], viviendas_semana[-1,-1], 
  viviendas_dia[-1,-1], viviendas_grupo[-1,-1], 
  viviendas_area[-1,-1], viviendas_comuna[-1,-1]
  )
coord_viviendas <- list(c(2, 6), c(5, 18), c(5, 28), c(2, 40), c(2, 52), c(5, 62))
# "Viviendas Ocupadas"
tablas_viviendas_o <- list(
  viviendas_o[,-1],
  viviendas_semana_o[-1,-1],
  viviendas_dia_o[-1,-1],
  viviendas_grupo_o[-1,-1],
  viviendas_area_o[-1,-1],
  viviendas_comuna_o[-1,-1]
  )
coord_viviendas_o <- list(c(2, 6), c(5, 17), c(5, 26), c(2, 37), c(2, 48), c(5, 57))


Llenar.Excel(wb, "General", tablas_general, coord_general)
Llenar.Excel(wb, "General (Viviendas)", tablas_general_v, coord_general_v)
Llenar.Excel(wb, "Visitas", tablas_visitas, coord_visitas)
Llenar.Excel(wb, "Viviendas", tablas_viviendas, coord_viviendas)
Llenar.Excel(wb, "Viviendas Ocupadas", tablas_viviendas_o, coord_viviendas_o)

# saveWorkbook(wb        = wb,
#              file      ="output-version-nico - copia.xlsx",
#              overwrite = T)


