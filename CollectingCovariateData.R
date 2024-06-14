## COVARIATE DATA below ----

library(tidyverse)

# Collect Electricity prices ----

#' monthly electricity prices for *entire* Spain from until now 
#'  does not include comunidades, only whole of spain
#' **data source:** FRED.stlouisfed.org  

fred_data <- read_csv("fredgraph.csv", 
                      col_types = cols(DATE = col_date(format = "%Y-%m-%d"), 
                                       CP0451ESM086NEST = col_double(), 
                                       CP0000ESM086NEST = col_double(), 
                                       ESPCPIENGMINMEI = col_double(), 
                                       ESPCPHPLA01IXOBM = col_double()))
summary(fred_data)

spain_electricity_index <- fred_data |> 
  select(DATE, CP0451ESM086NEST) |> 
  drop_na() |> 
  rename(price.index = CP0451ESM086NEST) |> 
  mutate(month = lubridate::month(DATE),
         year = lubridate::year(DATE))

rm(fred_data)

# Collect PIB ----

#' yearly GDP for each comunidad through 2022
#' does NOT include 2023 
#' **data source:** INE.es

PIB.raw.data <- read_delim("INE_PIB.csv", delim = ";", trim_ws = T, 
                           locale=readr::locale(encoding = "ISO-8859-1", 
                                                decimal_mark = ",", 
                                                grouping_mark = ".")
)

PIB.data <- PIB.raw.data |> 
  mutate(year = str_sub(periodo, 1, 4),
         # DATE = as.Date(year, "%Y" )
  ) |> 
  select(-c(`Totales Territoriales`, periodo)) 

ine_regions <- tribble(~CCAA, ~CCAAnombre,
                       "Andalucía", "Andalucía",
                       "Aragón", "Aragón",                 
                       "Asturias, Principado de", "Asturias",    
                       "Balears, Illes", "Islas Baleares",      
                       "Canarias", "Islas Canarias",                   
                       "Cantabria", "Cantabria",                 
                       "Castilla y León", "Castilla y León",
                       "Castilla - La Mancha", "Castilla la Mancha",
                       "Cataluña", "Cataluña",                    
                       "Comunitat Valenciana", "Valencia",       
                       "Extremadura", "Extremadura",                 
                       "Galicia", "Galicia",                     
                       "Madrid, Comunidad de", "Madrid",       
                       "Murcia, Región de", "Murcia",          
                       "Navarra, Comunidad Foral de", "Navarra",
                       "País Vasco", "País Vasco",                 
                       "Rioja, La", "La Rioja",                  
                       "Ceuta", "Ceuta",                       
                       "Melilla", "Melilla" 
)

CCAA_PIB_yearly <- PIB.data |> 
  filter(`Ramas de actividad` == "PRODUCTO INTERIOR BRUTO A PRECIOS DE MERCADO") |> 
  select(-`Ramas de actividad`) |> 
  rename(PIB = Total, 
         CCAA = "Comunidades y Ciudades Autonomas",
  ) |> 
  separate_wider_delim(cols = CCAA, delim = " ", 
                       names = c("id", "CCAA"), too_many = "merge" ) |> 
  left_join(ine_regions, by = "CCAA") |> 
  select(-CCAA) |> 
  mutate(year = as.numeric(year))

rm(list = c("PIB.data", "PIB.raw.data"))

# Collect population -----

pop_ccaa.raw <- read_delim("./ine_pop.csv", delim = ";", trim_ws = T, 
                           locale=readr::locale(encoding = "ISO-8859-1", 
                                                decimal_mark = ",", 
                                                grouping_mark = ".")
)

pop_ccaa_data <- pop_ccaa.raw |> 
  rename(ccaa = `Comunidades y Ciudades Autónomas`, 
         pop = Total, year = Año) |> 
  select(ccaa, year, pop) |> 
  separate_wider_delim(cols = ccaa, delim = " ", 
                       names = c("id", "CCAA"), too_many = "merge" ) |> 
  left_join(ine_regions, by = "CCAA") |> 
  select(-CCAA) |> 
  mutate(year = as.numeric(year))

rm(pop_ccaa.raw)

## combine PIB & POP : ---- 

combined_data_pop_pib <- left_join(CCAA_PIB_yearly, pop_ccaa_data, 
                                   by = c("CCAAnombre", "year"))


# Collect climate data ----

#' averages from 1981-2010 for each community for each month
#' **data source:** AEMET.es
#' 

PREC.raw <- read_csv2("./AEMET_clima/PREC.csv")
TMIN.raw <- read_csv2("./AEMET_clima/TMIN.csv")
TMAX.raw <- read_csv2("./AEMET_clima/TMAX.csv")
TMED.raw <- read_csv2("./AEMET_clima/TMED.csv")

clima.data <- rbind(PREC.raw, TMIN.raw, TMAX.raw, TMED.raw) |> 
  mutate(across(enero:anual, as.numeric)) |> 
  pivot_longer(cols = enero:anual, names_to = "mes", values_to = "value") |> 
  ## replace values to make them shorter 
  mutate(
    parametro = case_match(Parametro,
                           "Temperatura media de las máximas" ~ "tmax",
                           "Temperatura media de las minimas" ~ "tmin",
                           "Temperatura media" ~ "tmed",
                           "Precipitacion" ~ "prec",
                           .default = "tmax"
    )
  ) |> 
  mutate(mes = as.factor(mes),
         CCAA = as.factor(region),
         #parametro = as.factor(Parametro) # not needed though 
  ) |> 
  select(-c(Parametro, `periodo de referencia`)) 

AEMET_regions <- tribble(~CCAA, ~CCAAnombre,
                         "PRINCIPADO DE ASTURIAS", "Asturias",
                         "COMUNIDAD AUToNOMA DE BALEARES", "Islas Baleares",  
                         "COMUNIDAD AUToNOMA DE CANTABRIA", "Cantabria",
                         "CIUDAD AUToNOMA DE CEUTA", "Ceuta",
                         "COMUNIDAD AUToNOMA DE LA RIOJA", "La Rioja",
                         "COMUNIDAD DE MADRID", "Madrid",
                         "CIUDAD AUToNOMA DE MELILLA", "Melilla",
                         "REGIoN DE MURCIA", "Murcia",
                         "COMUNIDAD FORAL DE NAVARRA", "Navarra",  
                         "COMUNIDAD AUToNOMA DE CANARIAS", "Islas Canarias",  
                         "COMUNIDAD AUToNOMA DE ANDALUCiA", "Andalucía",      
                         "COMUNIDAD AUToNOMA DE ARAGoN", "Aragón",
                         "COMUNIDAD AUToNOMA DE CASTILLA-LA MANCHA", "Castilla la Mancha",
                         "COMUNIDAD AUToNOMA DE CASTILLA Y LEoN", "Castilla y León",
                         "COMUNIDAD AUToNOMA DE CATALUnA", "Cataluña",
                         "COMUNIDAD AUToNOMA DE EXTREMADURA", "Extremadura",
                         "COMUNIDAD AUToNOMA DE GALICIA", "Galicia",
                         "COMUNIDAD AUToNOMA DEL PAiS VASCO", "País Vasco",
                         "COMUNITAT VALENCIANA", "Valencia")

# Create a vector of month names in Spanish
meses <- tribble(
  ~mes, ~mes_num,
  "enero", 1,
  "febrero", 2,
  "marzo", 3,
  "abril", 4,
  "mayo", 5,
  "junio", 6,
  "julio", 7,
  "agosto", 8,
  "septiembre", 9,
  "octubre", 10,
  "noviembre", 11,
  "diciembre", 12
)

climate_data <- full_join(AEMET_regions, clima.data, by = "CCAA") |> 
  left_join(meses, by = "mes") |> 
  select(-c(region, CCAA))

rm(list = c("clima.data", "AEMET_regions", "meses", 
            "PREC.raw", "TMAX.raw", "TMED.raw", "TMIN.raw"))

mes_clima <- mes_clima |> 
  mutate(month = factor(mes_num) )

# Collect survey ----

#' survey results for all ccaa from 2023 
#' **data source:** CIS.es

CIS.raw <-  read_csv2("./CIS_survey/3424_num.csv", 
                      trim_ws = TRUE)

responses_to_investigate <- c(c(11, 21, 31, 32))

CIS.data1 <- CIS.raw |> 
  ## replace N.C.(no answer) with *NA* 
  mutate(
    medio_ambiente = replace(IDEASPROS4_1, IDEASPROS4_1 == 9, NA),
    problema.1 = replace(PROBLMUN1, PROBLMUN1 == 99, NA),
    problema.2 = replace(PROBLMUN2, PROBLMUN2 == 99, NA),
    problema.3 = replace(PROBLMUN3, PROBLMUN3 == 99, NA),
  )

CIS.data2 <- CIS.data1 |> 
  ## select only the questions I want to investigate 
  select(CCAA, medio_ambiente, problema.1, problema.2, problema.3) |> 
  mutate( ## convert them to factors, but why? for less confusion since they are represented with numbers. 
    CCAA = as.factor(CCAA), 
    medio_ambiente = as.factor(medio_ambiente), # all factors for less confusion 
    
    problema.1 = as.factor(problema.1), # all factors for less confusion 
    problema.2 = as.factor(problema.2), # all factors for less confusion 
    problema.3 = as.factor(problema.3), # all factors for less confusion 
    
    response_flag = case_when(
      problema.1 %in% responses_to_investigate |
        problema.2 %in% responses_to_investigate |
        problema.3 %in% responses_to_investigate 
      ~ 1,
      .default = 0
    )
  ) |> 
  select(CCAA, medio_ambiente, response_flag)

cont.table <- with(CIS.data2,  table(CCAA, medio_ambiente) )
chisq.test(cont.table, simulate.p.value = T)
#' #### Chi-squared test shows evidence of relationship between CCAA and opinion on how the environment will get better/same/worse in the next 10 yrs. 

CIS.masOmenos <- cont.table |> as_tibble() |> 
  pivot_wider(names_from = medio_ambiente, values_from = "n", names_prefix = "masOmenos" ) |> 
  mutate(
    #   CCAA = as.factor(CCAA), # convert to factor
    total = masOmenos1 + masOmenos2 + masOmenos3 + masOmenos8,
    mas =  masOmenos1 / total,
    menos =  masOmenos2 / total,
    igual = masOmenos3 / total,
    ns = masOmenos8 / total
  ) |> 
  select(CCAA, mas, menos, igual, ns, total)

CIS.problemas <- CIS.data2 |>
  select(-medio_ambiente) |> 
  group_by(CCAA) |> 
  summarise(
    response_p  = mean(response_flag) ,
    total = n(),
    response_sd = sqrt(response_p * (1 - response_p) / total ),
  ) |> ungroup()

cis_regions <- tribble(~CCAA, ~CCAAnombre, 
                       1,  "Andalucía",
                       2,  "Aragón",
                       3,  "Asturias",
                       4,  "Islas Baleares",
                       5,  "Islas Canarias",
                       6, "Cantabria",
                       7, "Castilla la Mancha",
                       8, "Castilla y León",
                       9, "Cataluña",
                       10, "Valencia",
                       11, "Extremadura",
                       12, "Galicia",
                       13, "Madrid",
                       14, "Murcia",
                       15, "Navarra",
                       16, "País Vasco",
                       17, "La Rioja", 
                       18, "Ceuta", 
                       19, "Melilla"
) |> mutate(CCAA = as.factor(CCAA)) 

survey_data <- full_join(CIS.problemas, CIS.masOmenos, by = "CCAA") |> 
  right_join(cis_regions, by = "CCAA")

rm(list = c("CIS.data1", "CIS.masOmenos",
            "CIS.problemas", "CIS.raw", "cont.table"))

## COMBINE data -----

### by date ----
#' (year, month, regardless of ccaa)

data.master <- full_join(spain_electricity_index, climate_data,
                         by = join_by(month == mes_num),
                         relationship = "many-to-many")|> 
  pivot_wider(names_from = parametro, values_from = value )

data.master <- data.master |> 
  left_join(CCAA_PIB_yearly, by = c("year", "CCAAnombre")) |> 
  left_join(survey_data, by = "CCAAnombre")


write_csv(data.master, file = "covariate_data.csv")

# more data goes here ----

readr::read_delim("./AEMET_clima/PREC_1981_2010_Provincias.csv", delim=";", locale=readr::locale(encoding="ISO-8859-1"))

# Collect population -----

pop_ccaa.raw <- read_delim("./ine_pop.csv", delim = ";", trim_ws = T, 
                           locale=readr::locale(encoding = "ISO-8859-1", 
                                                decimal_mark = ",", 
                                                grouping_mark = ".")
                            )

pop_ccaa_data <- pop_ccaa.raw |> 
  rename(ccaa = `Comunidades y Ciudades Autónomas`, 
         pop = Total, year = Año) |> 
  select(ccaa, year, pop) |> 
  separate_wider_delim(cols = ccaa, delim = " ", 
                       names = c("id", "CCAA"), too_many = "merge" ) |> 
  left_join(ine_regions, by = "CCAA") |> 
  select(-CCAA) |> 
  mutate(year = as.numeric(year))


# Collect land area ----

# data source: <https://www.ign.es/web/ane-datos-geograficos/-/datos-geograficos/datosPoblacion?tipoBusqueda=CCAA>

land_area <- tribble(~CCAAnombre, ~Población, ~Superf,
 "Andalucía", 8424102, 87597,
 "Aragón", 1346293, 47720,
 "Asturias", 1081487, 10602,
 "Cantabria", 593121, 5321, 
 "Castilla la Mancha", 2115334, 79462, 
 "Castilla y León", 2558463, 94227,
 "Cataluña", 7539618, 32091,
  "Valencia", 5117190, 23254,
  "Extremadura", 1109367, 41635,
  "Galicia", 2795422, 29575,
  "Madrid",	6489680, 8028,
  "Murcia", 1470069, 11314,
  "Navarra", 642051, 10390, 
  "País Vasco",	2184606, 7230, 
  "La Rioja", 322955, 5045,
  "Islas Baleares", 1113114, 4992,
  "Islas Canarias",	2126769,	7447,
  "Ceuta", 82376, 19,
  "Melilla", 78476, 13
)


save.image(file = "data.RData")

