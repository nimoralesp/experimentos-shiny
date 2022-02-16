Graficar.1  <- function(objeto, mi_factor, mi_xlab, mi_ylab){
  output <- ggplot( objeto, aes(x = factor(mi_factor), y = value, fill=variable)) +
    geom_bar(stat='identity', position='dodge') + 
    xlab(mi_xlab) + ylab(mi_ylab) + 
    scale_fill_manual(values = c("darkslategrey", "firebrick4")) + 
    theme_bw() + 
    labs(title = "Contactos y entrevistas completas", subtitle = paste0("Por ", mi_factor), fill = 'Hito') 
}

Graficar.12 <- fuction(objeto, multimodal, ){
  output <- ggplot(objeto %>% filter( .$Grupo_multimodal == multimodal ),
                   aes(x = factor(.$semana), y = value, fill = variable)) +
    geom_bar(stat='identity', position='dodge') + xlab("Semana") + ylab("Casos") +
    theme_bw() + labs(fill = paste0("Grupo ", multimodal, sep = "")) + ylim(c(0,1100))
}

Graficar.34 <- fuction(objeto, multimodal, mi_y, mi_ylab){
  output <- ggplot(objeto,
                   aes(x = factor(.$semana), y = mi_y, fill = as.factor(.$Grupo_multimodal))) +
    geom_bar(stat='identity', position='dodge') + xlab("Semana") + ylab(mi_ylab) +
    theme_bw() + labs(fill = paste0("Grupo ", multimodal, sep = "")) + ylim(c(0,1100))
}


Para.Graficar <- function(objeto, agrupar_por){
  output <- objeto %>% 
    group_by_at(agrupar_por) %>% 
    summarise(
      Contactos                = sum(Contacto, na.rm = T),
      "Entrevistas completas"  = sum(Entrevista_completa_A, na.rm = T)
    ) %>% 
    tidyr::pivot_longer(
      .,
      cols         = c('Contactos', 'Entrevistas completas'), 
      names_to     = 'variable', 
      values_to    = "value"
    )
  return(output)
}

semanas_contactos_visitas <- Para.Graficar(datos, c("semana"))
dias_contactos_visitas <- Para.Graficar(datos, c("Fecha_de_visita"))
semanas_contactos_visitas_grupo <- Para.Graficar(datos, c("semana", "Grupo_multimodal"))


CyE_semana <-  ggplot(semanas_contactos_visitas, aes(x = factor(semana), y = value, fill=variable)) +
  geom_bar(stat='identity', position='dodge') + 
  xlab("Semana") + 
  ylab("Conteo") + 
  scale_fill_manual(values = c("darkslategrey", "firebrick4")) + 
  theme_bw() + 
  labs(title = "Contactos y entrevistas completas", subtitle = "Por semana",fill='Hito') 




# ggplot(
#   semanas_contactos_visitas, 
#   aes( fill = variable, y = value, x = semana )
#   ) + 
#   geom_bar(
#     position="dodge", 
#     stat="identity"
#     ) + 
#   theme_bw() + 
#   labs(title = "Contactos y entrevistas completas", 
#        subtitle = "Por semana",
#        fill='Hito'
#        ) +
#   ylab("Cantidad") + xlab("Semana")

