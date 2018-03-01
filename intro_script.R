

library(tidyverse)

# poblacion
poblacion <- read_csv("data/poblacion_municipal.csv")

tab_pob <- poblacion %>% 
  mutate(cve_ent = factor(state_code, 1:32, c(paste0(0, 1:9), 10:32))) %>% 
  left_join(poblacion %>% 
              select(mun_code) %>% 
              mutate(len = str_length(mun_code), 
                     init = c("00", "0", "")[len], 
                     cve_mun = paste0(init, mun_code)) %>% 
              dplyr::select(mun_code, cve_mun)) %>% 
  dplyr::select(cve_ent, cve_mun, year, poblacion) %>% 
  filter(year %in% 2015:2016) %>% 
  unite(ent_mun, cve_ent, cve_mun, sep = "") %>% 
  gather(indicador, valor, poblacion) %>% 
  unique()
tab_pob



poblacion %>% 
  dplyr::select(state_code, state) %>% 
  unique %>% 
  data.frame()


# 2456 municipios
df_conapo <- readr::read_csv("data/conapo.csv")
df_conapo

df_conapo_ejercicio <- df_conapo %>% 
  gather(indicador, valor, analfabeta:salarios_minimos) %>% 
  unite(ent_mun, c(CVE_ENT, CVE_MUN), sep = "") %>% 
  dplyr::select(ent_mun, indicador, valor) %>% 
  arrange(indicador) %>% 
  mutate(year = 2015)
df_conapo_ejercicio

df_conapo_ejercicio %>% 
  bind_rows(tab_pob) %>% 
  spread(year, valor) %>% 
  arrange(indicador, `2015`) %>% 
  write.csv("data/datos_ejercicio.csv", row.names = F)




# Importar datos
df_ejercicio <- read_csv("data/datos_ejercicio.csv")
df_ejercicio

# Filtrar y ordenar
df_fo <- df_ejercicio %>% 
  filter(indicador %in% c("analfabeta", "sin_primaria", "poblacion")) %>% 
  arrange(ent_mun, indicador)%>% 
  mutate(est = str_sub(ent_mun, start = 1, end = 2),
         mun = str_sub(ent_mun, start = 3, end = 5)) %>% 
  select(-ent_mun)


df_gather <- df_fo %>% 
  gather(key = year, 
         value = valor, 
         c(`2015`, `2016`))
df_gather

df_tidy <- df_gather %>% 
  spread(key = indicador, 
         value = valor) 
df_tidy


df_resumen <- df_tidy  %>% 
  arrange(est, mun, year) %>% 
  mutate_at(.vars = c('analfabeta', 'poblacion', 'sin_primaria'), 
            .funs = parse_number) %>% 
  group_by(est, mun) %>% 
  mutate(analfabeta = mean(analfabeta, na.rm = T), 
         dif_poblacion = diff(poblacion), 
         difp_poblacion = dif_poblacion/poblacion,
         sin_primaria = mean(sin_primaria, na.rm = T)) %>% 
  ungroup %>% 
  filter(year == '2016')  
df_resumen


# Visualización
df_resumen %>% 
  filter(est %in% c("07", "09", "28", "30")) %>% 
  ggplot(aes(x = analfabeta, 
             y = sin_primaria, 
             color = est)) + 
  geom_point(alpha = .5, 
             size = 2)

df_resumen %>% 
  filter(est %in% c("07", "09", "28", "30")) %>% 
  ggplot(aes(x = analfabeta, 
             y = difp_poblacion, 
             color = est)) + 
  geom_point(alpha = .5)

df_resumen %>% 
  filter(est %in% c("07", "09", "28", "30")) %>% 
  mutate(est = fct_recode(est, 
                          chiapas = '07',
                          cdmx = '09',
                          tamaulipas = '28',
                          veracruz = '30')) %>% 
  ggplot(aes(x = sin_primaria, 
             y = difp_poblacion, 
             color = est)) + 
  geom_point(alpha = .5)


# Modelar
mod_1 <- lm(formula = difp_poblacion ~ sin_primaria, data = df_resumen)

broom::tidy(mod_1)

tab_results <- df_resumen %>% 
  modelr::add_predictions(model = mod_1) %>% 
  modelr::add_residuals(model = mod_1)
tab_results



# Un poco mas complicado
modelo_fun <- function(sub_tab){
  mod <- lm(formula = difp_poblacion ~ sin_primaria, data = sub_tab)
  broom::tidy(mod)
}

tbl_modelos <- df_resumen %>% 
  group_by(est) %>% 
  do(modelo_fun(sub_tab = .))
tbl_modelos


tbl_modelos %>% 
  dplyr::select(est:estimate) %>% 
  spread(term, estimate) %>% 
  ggplot(aes(x = `(Intercept)`, 
             y = sin_primaria)) + 
  geom_label(aes( label = est)) +
  geom_point()


library(ggrepel)
tbl_modelos %>% 
  dplyr::select(est:estimate) %>% 
  spread(term, estimate) %>% 
  ggplot(aes(x = `(Intercept)`, 
             y = sin_primaria)) + 
  geom_label_repel(aes( label = est)) +
  geom_point()






# Pruebas -----
library(nycflights13)
write_rds(flights, path = "data/flights.Rds")
write_csv(flights, path = "data/flights.csv")


# Ejemplo pequeño
df_ejem <- data_frame(
  mes = c("1_ene", "2_feb", "3_mar", "4_abr", "5_may"),
  `2005`= c(8.6, 9.1, 8.7, 8.4, 8.5),
  `2006`= c(8.5, 8.5, 9.1, 8.6, 7.9),
  `2007`= c(9.0, 10.7, 20.0, 21, 16.2))
df_ejem


df_ejem %>% 
  gather(year, value, c(`2005`, `2006`,`2007`))



# Import data
flights_csv <- readr::read_csv("data/flights.csv")
class(flights_csv)

flights_rds <- readr::read_rds("data/flights.Rds")
class(flights_rds)


df_flights <- flights_rds
df_flights %>% head()

library(lubridate)


tab_dplyr <- df_flights %>% 
  filter(!is.na(dep_delay)) %>% 
  mutate(week_day = weekdays(time_hour), 
         velocidad = distance / air_time * 60) %>% 
  dplyr::select(time_hour, week_day, hour, 
                dep_delay, velocidad, carrier)
tab_dplyr

tab_spl_app_comb <- tab_mutate %>% 
  group_by(hour, week_day) %>% 
  summarise(prom_delay = mean(dep_delay)) %>% 
  ungroup() 

tab_spl_app_comb %>% 
  ggplot(aes(x = hour, 
             y = prom_delay, 
             color = week_day)) + 
  geom_point() + 
  geom_line()

tab_reorder <- tab_spl_app_comb %>% 
  mutate(week_day = fct_reorder(week_day, x = prom_delay, fun = mean, .desc = T))
tab_reorder$week_day %>% levels

tab_reorder %>% 
  ggplot(aes(x = hour, 
             y = prom_delay, 
             color = week_day)) + 
  geom_point() + 
  geom_line()


mod_1 <- lm(formula = prom_delay ~ hour + week_day, data = tab_reorder)

tab_results <- tab_reorder %>% 
  modelr::add_predictions(model = mod_1) %>% 
  modelr::add_residuals(model = mod_1)
tab_results

tab_results %>% 
  ggplot(aes(x = prom_delay, 
             y = pred)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0)

tab_reorder %>% 
  modelr::add_predictions(model = mod_1) %>% 
  modelr::add_residuals(model = mod_1) %>% 
  ggplot(aes(x = hour, 
             y = pred, 
             color = week_day)) + 
  geom_point(aes(y = prom_delay)) +
  geom_line()


# Split apply combine generalizado
modelo_fun <- function(sub){
  mod <- lm(formula = prom_delay ~ hour + week_day, data = sub)
}

preds_fun <- function(sub, mod){
  sub %>% 
    modelr::add_predictions(mod) %>% 
    modelr::add_predictions(mod)
}

tbl_modelos <- tab_mutate %>% 
  group_by(hour, week_day, carrier) %>% 
  summarise(prom_delay = mean(dep_delay)) %>% 
  ungroup() %>% 
  group_by(carrier) %>% 
  do(modelo = modelo_fun(sub = .), 
     predicciones = preds_fun(sub = ., 
                              mod = modelo_fun(sub = .)),
     rmse = rmse(model = modelo_fun(sub = .),
                 data = .))

tbl_modelos$carrier
tbl_modelos$rmse 

tbl_modelos %>% 
  filter(carrier == "AA") %>% 
  .$mod


ex <- resample_partition(mtcars, c(test = 0.3, train = 0.7))
ex$train$idx
ex$test$idx 


boot <- bootstrap(mtcars, 100)
boot$strap
