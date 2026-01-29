source("analisisprincipal4.R", local = TRUE)

#==================================================
# app.R
#==================================================

library(shiny)
library(bslib)
library(dplyr)
library(DT)
library(broom)
library(emmeans)

#--------------------------------------------------
# FUNCIONES AUXILIARES
#--------------------------------------------------
p_stars <- function(p) {
  case_when(
    p < 0.001 ~ "***",
    p < 0.01  ~ "**",
    p < 0.05  ~ "*",
    TRUE      ~ ""
  )
}

fmt4 <- function(x) formatC(x, format = "f", digits = 4)

#--------------------------------------------------
# TEMA VISUAL
#--------------------------------------------------
theme_app <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  base_font = "sans-serif"
)

#==================================================
# UI
#==================================================
ui <- page_navbar(
  title = "Factores asociados al éxito en pinturas del museo del Prado",
  theme = theme_app,
  fillable = TRUE,
  
  #--------------------------------
  # INTRODUCCIÓN
  #--------------------------------
  nav_panel(
    "Introducción",
    card(
      card_body(
        markdown("
### Objetivo del estudio

El objetivo de este trabajo es analizar **qué características están asociadas al éxito
de una pintura**, entendido como un resultado binario.
")
      )
    )
  ),
  
  #--------------------------------
  # DATOS
  #--------------------------------
  nav_panel(
    "Datos",
    card(
      card_header("Datos utilizados"),
      card_body(
        DTOutput("tabla_datos", height = "600px")
      )
    )
  ),
  
  #--------------------------------
  # MODELO PRINCIPAL
  #--------------------------------
  nav_panel(
    "Modelo principal",
    layout_columns(
      col_widths = 12, # Ambas ocupan todo el ancho
      row_heights = c("auto", "1fr"), # La 1ª se ajusta al texto, la 2ª crece
      card(
        border = "primary",
        card_body(
          div(class = "texto-secundario", 
              markdown("El modelo principal es el modelo que con las variables obtenidas mejor explica el éxito en las obras del museo del prado"))
        )
      ),
      card(
        border = "primary",
        full_screen = TRUE,
        card_header("Resultados del modelo principal"),
        card_body(DTOutput("tabla_principal", height = "100%")),
        card_footer(
          HTML("<em>*** p &lt; 0.001 &nbsp;&nbsp; ** p &lt; 0.01 &nbsp;&nbsp; * p &lt; 0.05</em>")
        )
      )
    )
  ),
  #--------------------------------
  # MODELOS ALTERNATIVOS
  #--------------------------------
  nav_panel(
    "Modelos alternativos",
    div(
      style = "display:flex; justify-content:flex-end; margin-bottom:1rem;",
      popover(
        actionButton("btn_modelo", "Seleccionar modelo alternativo",
                     icon = icon("sliders-h")),
        title = "Modelos alternativos",
        selectInput(
          "modelo_alt",
          NULL,
          choices = c(
            "Modelo alternativo A1" = "mA1",
            "Modelo alternativo A2" = "mA2",
            "Modelo alternativo A3" = "mA3"
          )
        )
      )
    ),
    card(
      border = "secondary",
      card_body(
        div(
          class = "texto-secundario",
          markdown("
Se han plateado 3 modelos alternativos para explicar el éxito. \n 
**mA1**: trabaja con las mismas variables que el modelo principal pero eliminando la interacción \n 
**mA2**: amplía el modelo principal incorporando la variable tema, elimina el término de interacción e introduce un control flexible por la fecha de ejecución. \n 
**mA3**: amplía el modelo principal incorporando la variable tema, elimina el término de interacción y las variables referentes al soporte (tipo de soporte y montaje) e introduce un control flexible por la fecha de ejecución. \n
")
        )
      )
    ),
    card(
      border = "secondary",
      full_screen = TRUE,
      card_header("Resultados del modelo alternativo"),
      card_body(DTOutput("tabla_alt", height = "100%")),
      card_footer(
        HTML("<em>*** p &lt; 0.001 &nbsp;&nbsp; ** p &lt; 0.01 &nbsp;&nbsp; * p &lt; 0.05</em>")
      )
    )
  ),
  
  #--------------------------------
  # CSS
  #--------------------------------
  tags$style(HTML("
    .texto-secundario {
      font-size: 0.85rem;
      color: #555;
    }
  "))
)

#==================================================
# SERVER
#==================================================
server <- function(input, output, session) {
  
  #------------------------------
  # DATOS
  #------------------------------
  output$tabla_datos <- renderDT({
    df %>%
      mutate(
        log_ancho = fmt4(log_ancho),
        log_area  = fmt4(log_area)
      ) %>%
      datatable(
        options = list(pageLength = 10, scrollY = "450px"),
        rownames = FALSE
      )
  })
  
  #------------------------------
  # TABLA MODELO PRINCIPAL
  #------------------------------
  output$tabla_principal <- renderDT({
    tidy(m_principal, exponentiate = TRUE, conf.int = TRUE) %>%
      filter(term != "(Intercept)") %>%
      mutate(
        estimate   = fmt4(estimate),
        std.error  = fmt4(std.error),
        statistic  = fmt4(statistic),
        conf.low   = fmt4(conf.low),
        conf.high  = fmt4(conf.high),
        p.value    = paste0(fmt4(p.value), " ", p_stars(p.value))
      ) %>%
      datatable(
        options = list(pageLength = -1, dom = "t"),
        rownames = FALSE
      )
  })
  
  #------------------------------
  # TABLA MODELOS ALTERNATIVOS
  #------------------------------
  output$tabla_alt <- renderDT({
    modelo <- get(input$modelo_alt, envir = .GlobalEnv)
    
    tidy(modelo, exponentiate = TRUE, conf.int = TRUE) %>%
      filter(term != "(Intercept)") %>%
      mutate(
        estimate   = fmt4(estimate),
        std.error  = fmt4(std.error),
        statistic  = fmt4(statistic),
        conf.low   = fmt4(conf.low),
        conf.high  = fmt4(conf.high),
        p.value    = paste0(fmt4(p.value), " ", p_stars(p.value))
      ) %>%
      datatable(
        options = list(pageLength = -1, dom = "t"),
        rownames = FALSE
      )
  })
}

#==================================================
# RUN APP
#==================================================
shinyApp(ui, server)