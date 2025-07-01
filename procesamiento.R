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
  )|>
  mutate(
    ppt_total = rowSums(across(c(
      B_noaplica_ppt,
      B_porque_no_ppt_noconocenna,
      B_porque_no_ppt_noconoceproc,
      B_porque_no_ppt_nosabereg,
      B_porque_no_ppt_noaccinter,
      B_porque_no_ppt_noquiere,
      B_porque_no_ppt_notiempo,
      B_porque_no_ppt_nocriteriores,
      B_porque_no_ppt_nocriteriodoc,
      B_porque_no_ppt_soliasilo,
      B_porque_no_ppt_otrasbarreras,
      B_porque_no_ppt_miedo,
      B_porque_no_ppt_nnanohijos,
      B_porque_no_ppt_nosabe,
      B_porque_no_ppt_noresponde,
      B_porque_no_ppt_otro)), na.rm = TRUE),
    ppt = case_when(
      ppt_total >= 1 ~ 1,
      TRUE ~ 0
    )
  )|>
  mutate(
    titulo_none = case_when(
      C_leer_escribir >= 1 ~ 1,
      TRUE ~ 0
    ),
    medios_restringidos = case_when(
      D_ingreso_sostiene_gasto %in% c("Hasta un día", "Hasta una semana") ~ 1,
      TRUE ~ 0
    ))|>
  mutate(
    fuente_ingreso_total = rowSums(across(c(
      D_principal_fuente_ingreso_prestamo,
      D_principal_fuente_ingreso_ahorros,
      D_principal_fuente_ingreso_asistencia,
      D_principal_fuente_ingreso_comunidad,
      D_principal_fuente_ingreso_venbien,
      D_principal_fuente_ingreso_donacion,
      D_principal_fuente_ingreso_notiene,
      D_principal_fuente_ingreso_otra,
      D_principal_fuente_ingreso_nosabe,
      D_principal_fuente_ingreso_noresponde)), na.rm = TRUE),
    fuente_ingreso = case_when(
      fuente_ingreso_total >= 1 ~ 1,
      TRUE ~ 0),
    producto_financiero = case_when(
      D_producto_financiero_ninguno >= 1 ~ 1,
      TRUE ~ 0)
    )|>
  mutate(
    fuente_alimento_total = rowSums(across(c(
      E_fuente_alimento_intercambio,
      E_fuente_alimento_regalo,
      E_fuente_alimento_asistencia,
      E_fuente_alimento_noresponde)), na.rm = TRUE),
    fuente_alimento = case_when(
      fuente_alimento_total >= 1 ~ 1,
      TRUE ~ 0)
  )|>
  mutate(
    salud = case_when(
      F_estado_atencion == "No"~ 1,
      TRUE ~ 0),
    alojamiento = case_when(
      G_tipo_alojamiento %in% c("Apartamento compartido (con otros hogares)",
                                "Casa compartida (con otros hogares)",
                                "Vivienda improvisada (cambuche)",
                                "Pagadiario",
                                "Hotel/Hospedaje",
                                "Albergues y/o Alojamientos Colectivos Temporales") ~ 1,
      TRUE ~ 0
    )
    )|>
  mutate(
    problemas_alojamiento_total = rowSums(
      across(
        c(
          G_problema_alojamiento_noseguri,
          G_problema_alojamiento_noluz,
          G_problema_alojamiento_nopriv,
          G_problema_alojamiento_noprot,
          G_problema_alojamiento_ninguna,
          G_problema_alojamiento_otro,
          G_problema_alojamiento_nosabe,
          G_problema_alojamiento_noresponde
        )
      ), na.rm = TRUE
    ),
    problemas_alojamiento = case_when(
      problemas_alojamiento_total >= 1 ~ 1,
      TRUE ~ 0),
    agua_consumo = case_when(
      H_agua_consumo %in% c("De pozo con bomba",                        
                            "Carrotanque",                                
                            "Aguatero",                                   
                            "No tiene acceso",                            
                            "De pozo sin bomba, aljibe, jagüey o barreno",               
                            "De pila pública",                           
                            "Aguas lluvias",                             
                            "Río, quebrada, nacimiento o manantial") ~ 1,
      TRUE ~ 0
    ),
    acceso_agua = case_when(
      str_detect(H_problema_acceso_agua, fixed("Sí", ignore_case = FALSE)) ~ 1,
      TRUE ~ 0
    ),
    enfermedad_agua = case_when(
      H_enfermedad_agua == "Sí"~ 1,
                                TRUE ~ 0
    )
    
  )|>
  mutate(
    restriccion_total = rowSums(
      across(
        c(
          I_razon_restriccion_amenaza,
          I_razon_restriccion_minas,
          I_razon_restriccion_actividad,
          I_razon_restriccion_otra
        )
      ), na.rm = TRUE
    ),
    restriccion = case_when(
      restriccion_total >= 1 ~ 1,
      TRUE ~ 0),
    discriminacion = case_when(
      I_discriminacion == "Sí"~ 1,
      TRUE ~ 0
    ),
    doc_col = case_when( 
      M2_documento_col_ninguno==1 ~ 1,
      TRUE ~ 0),
    doc_ven = case_when( 
      M2_documento_ven_ninguno==1 ~ 1,
      TRUE ~ 0),
    seguro_salud = case_when(
      M4_afiliado_salud == "No"~ 1,
      TRUE ~ 0
    ),
    problemas_salud = case_when(
      M4_tuvo_problema_salud == "Sí"~ 1,
      TRUE ~ 0
    )
  )|>
  mutate(
    barreras_salud_total = rowSums(
      across(
        c(
          M4_barreras_acceso_salud_geog,
          M4_barreras_acceso_salud_finan,
          M4_barreras_acceso_salud_transp,
          M4_barreras_acceso_salud_dispon,
          M4_barreras_acceso_salud_fisica,
          M4_barreras_acceso_salud_cultur,
          M4_barreras_acceso_salud_noafil,
          M4_barreras_acceso_salud_ninguna,
          M4_barreras_acceso_salud_otro,
          M4_barreras_acceso_salud_nosabe,
          M4_barreras_acceso_salud_noresponde
        )
      ), na.rm = TRUE
    ),
    barreras_salud = case_when(
      barreras_salud_total >= 1 ~ 1,
      TRUE ~ 0),
    TOTAL_SCORE = titulo_none+
      homolog_titulo_none+
      certificado_none+
      lgbti+
      menos_12_meses+
      forma_ingreso+
      medios_transporte_a+
      discapacidad+
      ppt+
      titulo_none+
      medios_restringidos+
      fuente_ingreso+
      producto_financiero+
      fuente_alimento_total+
      salud+
      alojamiento+
      problemas_alojamiento+
      agua_consumo+
      acceso_agua+
      enfermedad_agua+
      restriccion+
      discriminacion+
      doc_col+
      doc_ven+
      seguro_salud+
      problemas_salud+
      barreras_salud
  )|>
  mutate(
    indice_normalizado = (TOTAL_SCORE - min(TOTAL_SCORE, na.rm = TRUE)) /
      (max(TOTAL_SCORE, na.rm = TRUE) - min(TOTAL_SCORE, na.rm = TRUE)),
    
    clasificacion_indice = case_when(
      indice_normalizado <= 0.33 ~ "Leve",
      indice_normalizado <= 0.66 ~ "Moderado",
      indice_normalizado <= 1 ~ "Severo",
      TRUE ~ NA_character_
    )
  )

  