## Procesamiento de datos de jefe de hogar segun atributos que clasifiquen como vulnerable

datos_processing <- datos_full %>%
  mutate(
    titulo_none = case_when(
      A_titulo_jefe_hogar == "Ninguno" ~ 1,
      TRUE ~ 0
    ),
    homolog_titulo_none = case_when(
      A_homolog_titulo_jefe %in% c("No", "No Sabe", "Esta en trámite") ~ 1,
      TRUE ~ 0
    ),
    certificado_none = case_when(
      A_certificado_comp_jefe %in% c("No", "No Sabe") ~ 1,
      TRUE ~ 0
    ),
    lgbti = case_when(
      B_lgbtiq %in% c("Si", "Prefiere no responder") ~ 1,
      TRUE ~ 0
    ),
    B_fecha_llegada_meses = as.numeric(B_fecha_llegada_meses),
    menos_12_meses = case_when(
      B_fecha_llegada_meses <= 12 ~ 1,
      TRUE ~ 0
    ),
    forma_ingreso = case_when(
      B_forma_ingreso == "A través de pasos no oficiales, como trochas (Irregular)" ~ 1,
      TRUE ~ 0
    ),
    
    medios_transporte_a = rowSums(across(c(B_medio_transporte_caminando,
                                           B_medio_transporte_canoa,
                                           B_medio_transporte_camion)), na.rm = TRUE),
    medios_transporte = case_when(
      medios_transporte_a >= 1 ~ 1,
      TRUE ~ 0
    )
  ) %>%
  
  mutate(across(c(B_discapacidad_ver,
                  B_discapacidad_oir,
                  B_discapacidad_subir,
                  B_discapacidad_recordar,
                  B_discapacidad_bañar,
                  B_discapacidad_comunic),
                ~ case_when(
                  . %in% c("Tiene mucha dificultad", "Le resulta imposible") ~ 1,
                  TRUE ~ 0
                ))) %>%
  
  mutate(
    discapacidad_total = rowSums(across(c(B_discapacidad_ver,
                                          B_discapacidad_oir,
                                          B_discapacidad_subir,
                                          B_discapacidad_recordar,
                                          B_discapacidad_bañar,
                                          B_discapacidad_comunic)), na.rm = TRUE),
    discapacidad = case_when(
      discapacidad_total >= 1 ~ 1,
      TRUE ~ 0
    )
  )