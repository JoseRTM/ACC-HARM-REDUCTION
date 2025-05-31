library(tidyverse)
library(rio)
library(survival)
library(stringi)
install.packages("stringdist")
library(stringdist)
install.packages("geepack")
library(geepack)
rm(list = ls());gc()


centros <- import("acc_centros.xlsx") 
data <- import("CONS_C1_2010_22.rds") %>% 
  filter(region_del_centro == "METROPOLITANA") %>% 
  select(nombre_centro, tipo_de_plan, diasen_tratamiento,
         sexo, edad, etnia, estado_conyugal, condicion_ocupacional,
         sustancia_principal, frecuencia_de_consumo_sustancia_principal,
         edad_inicio_sustancia_principal, compromiso_biopsicosocial,
         fecha_ingresoa_tratamiento, fecha_egresode_tratamiento,
         motivode_egreso, id = HASH_KEY) %>% 
  inner_join(centros, by = "nombre_centro")

# DATA WRANGLING
table(data$tipo_de_plan)
table(data$sexo)
table(data$etnia)
table(data$estado_conyugal)
table(data$condicion_ocupacional)
table(data$sustancia_principal)
table(data$frecuencia_de_consumo_sustancia_principal)
table(data$compromiso_biopsicosocial)
table(data$motivode_egreso)
unique(data$frecuencia_de_consumo_sustancia_principal)
unique(data$sustancia_principal)

data <- data %>% 
  mutate(tipo_de_plan = ifelse(tipo_de_plan %in% c("M-PAI",
                                                   "M PAI (p)",
                                                   "PAI LV",
                                                   "PG-PAI",
                                                   "PG PAI 2",
                                                   "PG-PAB"), "Ambulatory","Residential"),
         etnia = ifelse(etnia == "No pertenece",etnia, "Pertenece"),
         sustancia_principal = case_when(sustancia_principal == "Alcohol" ~ "Alcohol",
                                         sustancia_principal == "Cocaí­na" ~ "Cocaína",
                                         sustancia_principal == "Marihuana" ~ "Marihuana",
                                         sustancia_principal == "Pasta Base" ~ "PBC",
                                         TRUE ~ "Otra"),
         frecuencia_de_consumo_sustancia_principal = na_if(frecuencia_de_consumo_sustancia_principal, "Desconocida"),
         frecuencia_de_consumo_sustancia_principal = case_when(frecuencia_de_consumo_sustancia_principal %in% 
                                                                 c("1 dí­as - semana","Menos de 1 dí­a - semana") ~ 1,
                                                               frecuencia_de_consumo_sustancia_principal == "2-3 dí­as - semana" ~ 2,
                                                               frecuencia_de_consumo_sustancia_principal == "4-6 dí­as - semana" ~ 3,
                                                               frecuencia_de_consumo_sustancia_principal == "No consumió" ~ 0,
                                                               frecuencia_de_consumo_sustancia_principal == "Todos los dí­as" ~ 4),
         estado_conyugal = na_if(estado_conyugal, "No contesta"),
         estado_conyugal = ifelse(estado_conyugal %in% c("Casado", "Conviviente", "Convivente civil"), "Casado/conviviente", "No casado"),
         motivode_egreso = ifelse(motivode_egreso == "Alta Terapéutica", 1, 0),
         centro_politom = factor(centro_politom, levels = 1:3, labels = c("Harm Reduction",
                                                                          "None", "Abstinence")),
         centro_politom = relevel(centro_politom, ref = "None")) %>%
  distinct() %>% 
  mutate(across(where(is.character), as.factor))%>%
  group_by(id) %>%
  mutate(readmision = if_else(n() > 1, 1, 0)) %>%
  ungroup() %>%
  mutate(
    fecha_ingreso = as.Date(fecha_ingresoa_tratamiento, format = "%d/%m/%Y"),
    fecha_egreso  = as.Date(fecha_egresode_tratamiento, format = "%d/%m/%Y")
  ) %>%
  arrange(id, fecha_ingreso) %>%
  group_by(id) %>%
  mutate(
    event = row_number(),
    tstart = 0,
    tstop = as.numeric(fecha_egreso - fecha_ingreso),
    status = if_else(row_number() < n(), 1, 0)
  ) %>%
  ungroup() %>%
  mutate(tstop = ifelse(tstop == tstart,tstop+1,tstop),
         event = ifelse(event >3,3,event)) %>%
  arrange(id,fecha_ingreso) 

# Autojoin del dataset para comparar cada episodio con todos los del mismo id
cruces <- data %>%
  select(id, fecha_ingreso, fecha_egreso, nombre_centro) %>%
  inner_join(
    data %>% select(id, fecha_ingreso, fecha_egreso, nombre_centro),
    by = "id",
    suffix = c("_a", "_b")
  ) %>%
  filter(
    fecha_ingreso_a < fecha_egreso_b,
    fecha_egreso_a > fecha_ingreso_b,
    fecha_ingreso_a != fecha_ingreso_b | fecha_egreso_a != fecha_egreso_b  # evitar comparar una fila consigo misma
  )

ids_solapados <- cruces %>%
  distinct(id) %>%
  pull(id)
data <- data %>%
  filter(!id %in% ids_solapados)
data_clean <- na.omit(data)

# MODELO 1. OUTCOME MOTIVO DE EGRESO (MULTILEVEL)

logit_egreso <- glm(motivode_egreso ~ 
               factor(centro_politom) +               # Variables del centro
               sustancia_principal +                          # Variables individuales
               frecuencia_de_consumo_sustancia_principal +
               estado_conyugal + etnia + sexo + edad, data = data_clean,
             family = "binomial")
summary(logit_egreso)
exp(coef(logit_egreso))




data %>%
  group_by(centro_politom) %>%
  summarise(n_individuos = n_distinct(id)) %>%
  arrange(desc(n_individuos))

data %>% count(id) %>% summarise(
  min = min(n),
  max = max(n),
  median = median(n),
  mean = mean(n),
  over1 = mean(n > 1)
)

# MODELOS GEE

gee_egreso <- geeglm(
  motivode_egreso ~ centro_politom + sustancia_principal + frecuencia_de_consumo_sustancia_principal +
    estado_conyugal + etnia + sexo + edad,
  id = id,
  family = binomial,
  data = data_clean,
  corstr = "exchangeable"
)
coefs_egreso <- summary(gee_egreso)$coefficients


OR_egreso<- data.frame(
  variable = rownames(coefs_egreso),
  OR = exp(coefs_egreso[, "Estimate"]),
  CI_lower = exp(coefs_egreso[, "Estimate"] - 1.96 * coefs_egreso[, "Std.err"]),
  CI_upper = exp(coefs_egreso[, "Estimate"] + 1.96 * coefs_egreso[, "Std.err"]),
  p_value = coefs_egreso[, "Pr(>|W|)"]
)

# Redondear para presentación
OR_egreso <- OR_egreso %>% 
  mutate(across(c(OR, CI_lower, CI_upper, p_value), ~ round(., 3)))

# Mostrar la tabla
OR_egreso
export(OR_egreso, "tabla_egreso.xlsx")

# PWP READMISSION
library(survival)

pwp_read <- coxph(Surv(tstart, tstop, status == 1) ~ 
                    centro_politom*strata(event)  + sustancia_principal + frecuencia_de_consumo_sustancia_principal +
                    estado_conyugal + etnia + sexo + edad +
                          cluster(id),
                       data = data_clean)
summary(pwp_read)
hr_data <- broom::tidy(pwp_read, exponentiate = TRUE, conf.int = TRUE)
export(hr_data, "tabla_pwp.xlsx")
# Filtrar solo efectos de centro e interacciones con strata(event)
hr_centros <- hr_data %>%
  filter(str_detect(term, "centro_politom")) %>%
  mutate(
    evento = case_when(
      str_detect(term, "event=2") ~ 2,
      str_detect(term, "event=3") ~ 3,
      TRUE ~ 1
    ),
    centro_politom = case_when(
      str_detect(term, "centro_politomHarm") ~ "Harm Reduction",
      str_detect(term, "centro_politomAbstinence") ~ "Abstinence"
    )
  ) %>%
  select(centro_politom, evento, estimate, conf.low, conf.high)

# Graficar
ggplot(hr_centros, aes(x = evento, y = estimate, color = centro_politom)) +
  geom_point(size = 3) +
  geom_line(aes(group = centro_politom), linewidth = 1) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray30") +
  scale_x_continuous(breaks = c(1, 2, 3)) +
  scale_color_brewer(palette = "Pastel1") +  # paleta pastel
  labs(
    title = "Readmission risk stratified by treatment episode 
    and treatment center approach",
    x = "Treatment episode",
    y = "Hazard Ratio (HR)",
    color = "Treatment center approach"
  ) +
  theme_minimal(base_size = 14)

# Tabla de descriptivos
library(janitor)
install.packages("gtsummary")
library(gtsummary)
data_clean <- data_clean %>%
  arrange(id, fecha_ingreso) %>%
  group_by(id) %>%
  mutate(dias_hasta_readmision = as.numeric(
    lead(fecha_ingreso) - fecha_egreso
  )) %>%
  ungroup()
data_clean <- data_clean %>% 
  mutate(
    frecuencia_de_consumo_sustancia_principal = factor(frecuencia_de_consumo_sustancia_principal,
                                                            levels = c(0, 1, 2, 3, 4),
                                                            labels = c("Non use", "1 day/week", "2-3 days/week", "4-6 days/week", "Daily")),
    sustancia_principal = recode(sustancia_principal,
                               "Alcohol" = "Alcohol",
                               "Cocaína" = "Cocaine",
                               "Marihuana" = "Marijuana",
                               "PBC" = "PBC",
                               .default = "Other"),
estado_conyugal = recode(estado_conyugal,
                         "Casado/conviviente" = "Married/cohabiting",
                         "No casado" = "Not married"),
etnia = recode(etnia,
               "Pertenece" = "Yes",
               "No pertenece" = "No"),
sexo = recode(sexo,
              "Hombre" = "Male",
              "Mujer" = "Female"),
motivode_egreso = recode(motivode_egreso,
                         `1` = "Therapeutic discharge",
                         `0` = "Discharge w/o clinical advice")
)

tabla_desc <- data_clean %>%
  select(
    `Days to readmission` = dias_hasta_readmision,
    `Reason for discharge` = motivode_egreso,
    `Treatment center type` = centro_politom,
    `Primary substance used` = sustancia_principal,
    `Frequency of use` = frecuencia_de_consumo_sustancia_principal,
    `Marital status` = estado_conyugal,
    `Indigenous identification` = etnia,
    `Sex` = sexo,
    `Age` = edad
  ) %>%
  mutate(across(where(is.character), as.factor)) %>%
  tbl_summary(
    statistic = list(all_continuous() ~ "{median} ({IQR})",
                     all_categorical() ~ "{n} ({p}%)"),
    digits = all_continuous() ~ 2,
    missing = "no"
  ) %>%
  modify_header(label = "**Variable**") %>%
  bold_labels()

# Exportar a Excel
tabla_df <- as_tibble(as.data.frame(tabla_desc$table_body))
export(tabla_df, "descriptivos.xlsx")
