# ANÁLISIS COHORTES

library(tidyverse)
library(data.table)
# FUNCION DIFERENCIA DE MESES
months_difference <- function(end_date, start_date) {ed <- as.POSIXlt(end_date)
                                                     sd <- as.POSIXlt(start_date)
                                                     12 * (ed$year - sd$year) + (ed$mon - sd$mon)}                                                  

# TRAIGO INFO DE BASE DE DATOS ---------------------------------------------------------------------------------------
library(RMySQL)
dbapcreplica = dbConnect(MySQL(), user='dbuser', password='Astr0Pay2018', dbname='apc', host='10.120.41.68')

# USUARIO Y FECHA Y MONTO DE SUS COMPRAS
compras_web_conv <- dbSendQuery(dbapcreplica, "select i.id_user as id_usuario,
                                                      i.date_created as dat_fecha,
                                                      pc.usd_amount as trx_monto_usd
                                               from apc_purchases.purchase i
                                                    inner join apc_purchases.purchase_card pc on i.id_purchase = pc.id_purchase
                                               where i.status in ('COMPLETED','APPROVED');")
compras_web_conv <- fetch(compras_web_conv, n=-1)

compras_app_conv <- dbSendQuery(dbapcreplica, "select i.id_user as id_usuario,
                                                      i.date_created as dat_fecha,
                                                      i.usd_amount as trx_monto_usd
                                               from invoice_mobile i
                                               where i.status in ('COMPLETED','APPROVED');")
compras_app_conv <- fetch(compras_app_conv, n=-1)

compras_web_conv$id_usuario <-  compras_web_conv$id_usuario+10000000
compras_conv <- rbind(compras_app_conv, compras_web_conv)
remove(compras_app_conv, compras_web_conv)

compras_conv <- compras_conv %>% filter(dat_fecha>="2012-01-01" & trx_monto_usd>0)
compras_conv$dat_fecha <- format(as.Date(compras_conv$dat_fecha), "%Y-%m-%d")
compras_conv$dat_anio_mes <- format(as.Date(compras_conv$dat_fecha), "%Y-%m")


# USUARIO Y SU PAÍS
users_web_pais <- dbSendQuery(dbapcreplica, "select u.id_user as id_user,
                                                    u.country as cat_iso_pais
                                             from apc_users.user u;")
users_web_pais <- fetch(users_web_pais, n=-1)

users_app_pais <- dbSendQuery(dbapcreplica, "select u.id_user_mobile as id_user,
                                                    u.country as cat_iso_pais
                                             from user_mobile u;")
users_app_pais <- fetch(users_app_pais, n=-1)

users_web_pais$id_user <- users_web_pais$id_user + 10000000
users <- rbind(users_app_pais, users_web_pais)
remove(users_app_pais, users_web_pais)

# USUARIO APP Y LA PLATAFORMA QUE UTILIZA
users_app_platform <- dbSendQuery(dbapcreplica, "select id_user_mobile, platform from user_mobile;")
users_app_platform <- fetch(users_app_platform, n=-1)
# -----------------------------------------------------------------------------------------------------------



# Separo las variables requeridas
compras_usuario <- compras_conv[,c("dat_anio_mes", "dat_fecha", "id_usuario")]


# Obtengo la Fecha de 1era Compra de cada usuario
fecha_1ra_compra <- aggregate(dat_fecha~id_usuario, compras_usuario, min, na.rm = TRUE)
colnames(fecha_1ra_compra)[2] <- "fecha_1ra_compra"

# Unifico la fecha de 1ra compra al dataframe cohort
compras_usuario <- merge(compras_usuario, fecha_1ra_compra, by.x = "id_usuario", by.y = "id_usuario", all.x = TRUE)
compras_usuario$cohort <- format(as.Date(compras_usuario$fecha_1ra_compra), "%Y-%m")


# Calculo la diferencia en días entre cada compra y la fecha de 1ra compra
compras_usuario$ant_días <- as.numeric(difftime(compras_usuario$dat_fecha, compras_usuario$fecha_1ra_compra, units = c("days")))

# Calculo la diferencia en meses calendario
compras_usuario$ant_meses <- months_difference(compras_usuario$dat_fecha, compras_usuario$fecha_1ra_compra)

# A cada compra le pongo que Nª de compra de ese usuario es
compras_usuario <- compras_usuario %>% arrange(id_usuario, dat_fecha)
compras_usuario$num_compra_del_usuario <- ave(compras_usuario$id_usuario, compras_usuario$id_usuario, FUN=seq)


#------------------------------------------------------------------------------------------------------------------------------------
# COHORTS ------------------------------------------------------------------------------------------------------------------------------
# Identifico y elimino filas de usuarios repetidos en un mismo mes (varias compras de un mismo usuario en un mismo mes)
dupes <- which(duplicated(compras_usuario[,c("id_usuario", "ant_meses", "cohort")]))                    
cohorts <- compras_usuario[-dupes,]
rm(dupes)

# A cada compra de cohorts le pongo que Nª de compra de ese usuario es (al borrar dupes de un mismo mes, debo numerarlas de nuevo)
cohorts$num_compra_del_usuario <- NULL
cohorts <- cohorts %>% arrange(id_usuario, dat_fecha)
cohorts$num_compra_del_usuario <- ave(cohorts$id_usuario, cohorts$id_usuario, FUN=seq)

# Creo una fila para cada grupo cohort (mes de 1ra compra)
# Creo columna para cada mes de antiguedad
# The default aggregation setup for dcast is, fun.aggregate = length
cohorts.wide <- reshape2::dcast(cohorts, cohort~ant_meses,
                                value.var = "id_usuario",
                                fun.aggregate = length)

write.csv(cohorts.wide, "~/Documents/BI/Cohortes/exports/cohorts.csv")

#------------------------------------------------------------------------------------------------------------------------------------
# COHORTS POR PAÍS ------------------------------------------------------------------------------------------------------------------
cohorts_pais <- merge(x = cohorts, y = users[, c("id_user", "cat_iso_pais")], by.x = "id_usuario", by.y = "id_user", all.x = TRUE)
cohorts.wide_pais <- reshape2::dcast(cohorts_pais, cohort+cat_iso_pais~ant_meses,
                                     value.var = "id_usuario",
                                     fun.aggregate = length)

cohorts.wide_pais <- cohorts.wide_pais %>% mutate(cat_pais_group = case_when(cat_iso_pais == "TR" ~ "Turquia",
                                                                             cat_iso_pais == "CN" ~ "China",
                                                                             cat_iso_pais == "BR" ~ "Brasil",
                                                                             cat_iso_pais == "CL" ~ "Chile",
                                                                             cat_iso_pais == "IN" ~ "India",
                                                                             cat_iso_pais == "PE" ~ "Peru",
                                                                             cat_iso_pais == "CO" ~ "Colombia",
                                                                             cat_iso_pais == "AR" ~ "Argentina",
                                                                             cat_iso_pais == "MX" ~ "Mexico",
                                                                             cat_iso_pais == "UY" ~ "Uruguay",
                                                                             cat_iso_pais == "JP" ~ "Japon",
                                                                             TRUE ~ "Otros"))
# Traigo más hacia el principio la columna cat_pais_group
cohorts.wide_pais <- cohorts.wide_pais %>% select(cohort, cat_pais_group, everything())

write.csv(cohorts.wide_pais, "~/Documents/BI/Cohortes/exports/cohorts_pais.csv")


#------------------------------------------------------------------------------------------------------------------------------------
# COHORTS POR ANDROD/iOS para usuarios APP ------------------------------------------------------------------------------------------------------------------

cohorts_platform <- merge(x = cohorts %>% filter(id_usuario %in% users_app_platform$id_user_mobile), 
                          y = users_app_platform[, c("id_user_mobile", "platform")],
                          by.x = "id_usuario", by.y = "id_user_mobile")
cohorts.wide_platform <- reshape2::dcast(cohorts_platform, cohort+platform~ant_meses,
                                     value.var = "id_usuario",
                                     fun.aggregate = length)

write.csv(cohorts.wide_platform, "~/Documents/BI/Cohortes/exports/cohorts_platform.csv")


#------------------------------------------------------------------------------------------------------------------------------------
# MONTO EN 1 MES ----------------------------------------------------------------------------------------------------------
# Separo las variables requeridas
compras_usuario_con_monto <- compras_conv[,c("dat_anio_mes", "dat_fecha", "id_usuario", "trx_monto_usd")]

# Unifico la fecha de 1ra compra al dataframe cohort
compras_usuario_con_monto <- merge(compras_usuario_con_monto, fecha_1ra_compra, by.x = "id_usuario", by.y = "id_usuario", all.x = TRUE)
compras_usuario_con_monto$cohort <- format(as.Date(compras_usuario_con_monto$fecha_1ra_compra), "%Y-%m")

# Calculo la diferencia en días entre cada orden y la fecha de 1ra compra
compras_usuario_con_monto$ant_días <- as.numeric(difftime(compras_usuario_con_monto$dat_fecha, compras_usuario_con_monto$fecha_1ra_compra, units = c("days")))

# Calculo la diferencia en meses calendario
compras_usuario_con_monto$ant_meses <- months_difference(compras_usuario_con_monto$dat_fecha, compras_usuario_con_monto$fecha_1ra_compra)

# A cada compra con monto le pongo que Nª de compra de ese usuario es
compras_usuario_con_monto <- compras_usuario_con_monto %>% arrange(id_usuario, dat_fecha)
compras_usuario_con_monto$num_compra_del_usuario <- ave(compras_usuario_con_monto$id_usuario, compras_usuario_con_monto$id_usuario, FUN=seq)

cohorts_monto <- compras_usuario_con_monto %>% filter(ant_meses==0) %>% group_by(cohort) %>% summarise(Volumen_cohort=sum(trx_monto_usd))

cohorts_monto %>% write_csv("~/Documents/BI/Cohortes/exports/cohorts_monto_1mes.csv")

