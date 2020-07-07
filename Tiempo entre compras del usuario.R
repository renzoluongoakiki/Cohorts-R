library(tidyverse)
library(data.table)

# FUNCION DIFERENCIA DE MESES
months_difference <- function(end_date, start_date) {ed <- as.POSIXlt(end_date)
                                                     sd <- as.POSIXlt(start_date)
                                                     12 * (ed$year - sd$year) + (ed$mon - sd$mon)}       

# Me quedo únicamente con id_usuario y la fecha de cada compra
compras_usuario <- compras_conv[,c("dat_anio_mes", "dat_fecha", "id_usuario")]

# Obtengo la Fecha de 1era Compra de cada usuario
fecha_1ra_compra <- aggregate(dat_fecha~id_usuario, compras_usuario, min, na.rm = TRUE)
colnames(fecha_1ra_compra)[2] <- "fecha_1ra_compra"

# A cada fila de compra le agrego cuándo fue la 1ra compra de ese usuario
compras_usuario <- merge(compras_usuario, fecha_1ra_compra, by.x = "id_usuario", by.y = "id_usuario", all.x = TRUE)
compras_usuario$cohort <- format(as.Date(compras_usuario$fecha_1ra_compra), "%Y-%m")

# Calculo la diferencia en días entre cada compra y la fecha de 1ra compra
compras_usuario$ant_dias <- as.numeric(difftime(compras_usuario$dat_fecha, compras_usuario$fecha_1ra_compra, units = c("days")))
# Calculo la diferencia en meses calendario
compras_usuario$ant_meses <- months_difference(compras_usuario$dat_fecha, compras_usuario$fecha_1ra_compra)

# A cada compra le pongo que Nª de compra de ese usuario es
compras_usuario <- compras_usuario %>% arrange(id_usuario, dat_fecha)
compras_usuario$num_compra_del_usuario <- ave(compras_usuario$id_usuario, compras_usuario$id_usuario, FUN=seq)

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# A LOS EFECTOS DE ESTE ANÁLISIS COMPRAS DE UN MISMO DÍA DE UN USUARIO CUENTAN COMO UNA ÚNICA COMPRA
duplicados <- which(duplicated(compras_usuario[,c("id_usuario", "dat_fecha", "fecha_1ra_compra")]))
compras_usuario_dist_dia <- compras_usuario[-duplicados,]
rm(duplicados)

#Agrego columna indicando qué número de compra del usuario es cada compra (fila)
compras_usuario_dist_dia <- compras_usuario_dist_dia %>% arrange(id_usuario, dat_fecha)
compras_usuario_dist_dia$num_compra_del_usuario <- ave(compras_usuario_dist_dia$id_usuario, compras_usuario_dist_dia$id_usuario, FUN=seq)
#Agrego columna indicando para cada compra cuántos días después de la compra anterior del usuario fue hecha
compras_usuario_dist_dia$dias_compra_anterior <- if_else(compras_usuario_dist_dia$id_usuario == lag(compras_usuario_dist_dia$id_usuario, default = first(compras_usuario_dist_dia$id_usuario)), 
                                                         compras_usuario_dist_dia$ant_dias - lag(compras_usuario_dist_dia$ant_dias, default = first(compras_usuario_dist_dia$ant_dias)),
                                                         0)

# # Tabla de cantidad de compras por usuario
# cant_compras_us <- compras_usuario_dist_dia %>% group_by(id_usuario) %>% summarise(cant_compras=max(num_compra_del_usuario))
# # Miro cuantiles para quitar el ~1% o similar de más compras por ser outliers
# quantile(cant_compras_us$cant_compras, probs = c(.25,.50,.75,.95,.98,0.99))
# # Lista de ese ~1% de usuarios con demasiadas compras
# usuarios_demasiadas_compras <- cant_compras_us %>% filter(cant_compras > 100)

# Veo tiempo entre compras de los usuarios
usuarios_APP_BR <- users %>% filter(id_user<10000000 & cat_iso_pais=="BR")
usuarios_APP_IN <- users %>% filter(id_user<10000000 & cat_iso_pais=="IN")
compras_usuario_dist_dia %>% filter(id_usuario %in% usuarios_APP_IN$id_user &
                                    fecha_1ra_compra>="2019-01-01" &
                                    num_compra_del_usuario<=10) %>%
                        group_by(num_compra_del_usuario) %>% summarise(Q25=quantile(dias_compra_anterior, probs=0.25),
                                                                       Q50=quantile(dias_compra_anterior, probs=0.5),
                                                                       Q75=quantile(dias_compra_anterior, probs=0.75),
                                                                       Q80=quantile(dias_compra_anterior, probs=0.8),
                                                                       Q95=quantile(dias_compra_anterior, probs=0.95),
                                                                       Media=mean(dias_compra_anterior))
remove(usuarios_APP_BR, usuarios_APP_IN)






# Gráfico para ver la distribución de días entre una compra y su compra anterior
ggplot(compras_usuario_dist_dia %>% filter(fecha_1ra_compra>="2019-01-01" & ((num_compra_del_usuario==2 & ant_dias<=45) | (num_compra_del_usuario==3 & ant_dias<=90)))) +
  aes(x = ant_dias) +
  geom_density(fill = "red") +
  theme_minimal() +
  labs(title = "Distribución de días entre 1ª compra y la compra nº2 y nº3", x="Días desde la 1ª Compra", y="")+
  #scale_x_continuous(breaks = c(0:45), labels=waiver())+
  facet_wrap(vars(num_compra_del_usuario), scales = "free_x")

# ANALISIS DE TIEMPO ENTRE COMPRAS SOLO PARA BRASIL
users_BR <- users %>% filter(cat_iso_pais=="BR")
compras_usuario_dist_dia_BR <- compras_usuario_dist_dia %>% filter(id_usuario %in% users_BR$id_user)
tiempo_entre_compras_BR <- compras_usuario_dist_dia_BR %>% filter(id_usuario %notin% usuarios_demasiadas_compras$id_usuario &
                                                                  fecha_1ra_compra>="2019-01-01" &
                                                                  (num_compra_del_usuario==2 & ant_dias<=45) | (num_compra_del_usuario==3 & ant_dias<=90)) %>%
                                                           group_by(num_compra_del_usuario) %>% summarise(dias_compra_ant_min=min(dias_compra_anterior),
                                                                                                          dias_compra_ant_25=quantile(dias_compra_anterior, probs=0.25),
                                                                                                          dias_compra_ant_mediana=quantile(dias_compra_anterior, probs=0.5),
                                                                                                          dias_compra_ant_75=quantile(dias_compra_anterior, probs=0.75),
                                                                                                          dias_compra_ant_80=quantile(dias_compra_anterior, probs=0.8),
                                                                                                          dias_compra_ant_max=max(dias_compra_anterior),
                                                                                                          dias_compra_ant_media=mean(dias_compra_anterior))

tiempo_entre_compras_BR %>% write_csv("~/Desktop/Tiempo_entre_compras_BR.csv")

ggplot(compras_usuario_dist_dia_BR %>% filter(fecha_1ra_compra>="2019-01-01" & ((num_compra_del_usuario==2 & ant_dias<=45) | (num_compra_del_usuario==3 & ant_dias<=90)))) +
  aes(x = ant_dias) +
  geom_density(fill = "red") +
  theme_minimal() +
  labs(title = "Días entre 1ª y 2ª compra  en BRASIL que demoraron más de 2 y menos de 45 días", x="Días desde la 1ª Compra", y="")+
  scale_x_continuous(breaks = c(7,14,21,28,35,42), labels=waiver())+
  geom_vline(xintercept=20,linetype="longdash", size=1)
  #facet_wrap(vars(num_compra_del_usuario), scales = "free")

remove(users_BR, compras_usuario_dist_dia_BR, tiempo_entre_compras_BR)
aux <- compras_usuario_dist_dia_BR %>% filter(fecha_1ra_compra>="2019-01-01" & (num_compra_del_usuario==2 & ant_dias<=45 & dias_compra_anterior>2))
