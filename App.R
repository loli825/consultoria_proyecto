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

El objetivo general de este trabajo es construir un modelo explicativo que nos ayude a entender qué factores se asocian con que una obra cumpla o no la proporción áurea. No buscamos hacer predicción “para acertar”, sino explicar con qué características de las obras (como la época, el tamaño, la orientación, la técnica o el soporte) es más probable encontrar la proporción áurea y cómo estas características se relacionan entre sí.

Como objetivo específico, queremos describir si el cumplimiento de la proporción áurea es un fenómeno frecuente dentro del conjunto de obras analizadas en el museo del prado y si dicho cumplimiento se concentra en determinados contextos (por ejemplo, en ciertos periodos históricos o en ciertos formatos). 

Aunque nuestro conjunto de datos no mide directamente la “belleza” ni la “calidad artística”, sí nos permite plantear la pregunta de manera crítica: si la proporción áurea fuera una regla compositiva muy general en el arte, deberíamos observar patrones claros y consistentes en las obras analizadas; y si aparece de forma irregular o depende fuertemente del contexto, ello refuerza la idea de que su presencia no es universal y que conviene interpretarla como una herramienta posible, pero no como un criterio determinante.

Queremos describir como son las obras del prado

### Hipotesis

Consideramos las siguientes hipótesis acerca de variables que podrían relacionarse con la probabilidad de cumplimiento de la razón aurea en las pinturas:

1) Posiblemente la fecha de creación de la obra de relacione con el evento de interés debido a corrientes artísticas ir al innegable cambio es los conceptos de belleza.

2) Presuponemos que posiblemente el tamaño de la obra mantenga relación con la decisión de sus medidas y su relación (proporción aurea) debido a la perspectiva con que esta se mira. Es decir, cuando una obra es más pequeña podemos verla en su totalidad más rápidamente, mientras que si esta es de dimensiones más grandes nos vemos forzados a recorrerla con la mirada. Por esta razón queremos estudiar si la proporción aurea pudiera estar relacionada con estos aspectos y quizás marcar la semilla de una futura investigación.

3) Muy relacionado con el punto anterior, se nos planteó la pregunta de si quizás las dimensiones de la obra por si solas no fueran el punto clave. Consideramos que el material de soporte utilizado es un factor influyente en sus dimensiones y quizás más limitante en unos tamaños que en otros, por lo tanto relacionado con la proporción aurea en función del tamaño de la obra

4) Relacionado con el apartado anterior, también se considera estudiar si la iconografia de la pintura podría estar relacionada debido a aspectos de composición en su interior que acaben repercutiendo en las dimensiones totales.

5) Por último y como aspecto fundamental de cualquier composición tenemos su técnica o técnicas utilizadas. Cada una ofrece posibilidades y sobre todo flexibilidad diferentes en los trazos, por lo que son un buen indicador de la planificación previa de la obra, planificación que es necesaria si se considera imponer la proporción aurea.
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
      card_body(DTOutput("tabla_alt")),
      card_footer(
        HTML("<em>*** p &lt; 0.001 &nbsp;&nbsp; ** p &lt; 0.01 &nbsp;&nbsp; * p &lt; 0.05</em>")
      )
    )
  ),
  #--------------------------------
  # BONDAD DE AJUSTE
  #--------------------------------
  nav_panel(
    "Bondad de Ajuste",
    layout_columns(
      col_widths = c(4, 8), 
      card(
        card_header("Comparativa de Modelos"),
        tableOutput("tabla_comparativa"),
        hr(),
        card_title("Métricas AUC"),
        tableOutput("tabla_auc")
      ),
      card(
        card_header("Capacidad de Discriminación (Curva ROC)"),
        checkboxGroupInput(
          "modelos_roc", 
          "Modelos a visualizar:",
          choices = c("Principal" = "m_principal", "A1" = "mA1", "A2" = "mA2", "A3" = "mA3"),
          selected = c("m_principal", "mA1"),
          inline = TRUE
        ),
        plotOutput("plot_roc", height = "400px")
      )
    ),
    layout_columns(
      col_widths = 12,
      card(
        card_header("Diagnóstico: Análisis de Residuos (Modelo Principal)"),
        plotOutput("plot_residuos", height = "300px")
      )
    )
  ),
  #--------------------------------
  # EFECTOS MARGINALES 
  #--------------------------------
  nav_panel(
    "Efectos Marginales",
    layout_sidebar(
      sidebar = sidebar(
        title = "Configuración del Análisis",
        # Selector de Modelo
        selectInput(
          "modelo_emmeans",
          "Selecciona el modelo a explorar:",
          choices = c(
            "Modelo Principal" = "m_principal",
            "Modelo A1 (Sin interacción)" = "mA1",
            "Modelo A2 (Con Tema)" = "mA2",
            "Modelo A3 (Reducido)" = "mA3"
          )
        ),
        # Selector de Variable
        selectInput(
          "var_emmeans",
          "Selecciona variable para explicar:",
          choices = c(
            "Soporte" = "soporte_grp",
            "Tamaño" = "tam_cat",
            "Técnica" = "tecnica",
            "Serie" = "serie",
            "Montaje" = "sop_montaje",
            "Orientación" = "orientacion"
          )
        ),
        hr()
      ),
      card(
        full_screen = TRUE,
        card_header(textOutput("titulo_plot_emmeans")),
        plotOutput("plot_emmeans"),
        card_footer("Las barras de error representan intervalos de confianza del 95% (escala de probabilidad).")
      )
    )
  ),
  
  #--------------------------------
  # INTERACCIONES
  #--------------------------------
  nav_panel(
    "Interacciones",
    layout_sidebar(
      sidebar = sidebar(
        title = "Análisis de Interacción",
        markdown("Las interacciones permiten ver cómo el efecto de una variable **depende** del valor de otra."),
        selectInput(
          "inter_var1",
          "Variable principal (Eje X):",
          choices = c("Soporte" = "soporte_grp")
        ),
        selectInput(
          "inter_var2",
          "Variable condicionante (Color):",
          choices = c("Tamaño" = "tam_cat")
        ),
        hr(),
        markdown("Solo disponible para el **Modelo Principal**.")
      ),
      card(
        full_screen = TRUE,
        card_header("Visualización de la Interacción"),
        plotOutput("plot_interaccion")
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
  
  #------------------------------
  # GRÁFICO EMMEANS
  #------------------------------
  output$titulo_plot_emmeans <- renderText({
    paste("Probabilidades estimadas según", input$var_emmeans, 
          "en el", names(which(c("Modelo Principal" = "m_principal", "Modelo A1" = "mA1", "Modelo A2" = "mA2", "Modelo A3" = "mA3") == input$modelo_emmeans)))
  })
  
  # Gráfico EMMEANS Dinámico
  output$plot_emmeans <- renderPlot({
    # 1. Recuperar el modelo seleccionado
    mod <- get(input$modelo_emmeans, envir = .GlobalEnv)
    
    # 2. Validar si la variable existe en ese modelo específico
    # (Evita que la app se cuelgue si eliges A3 y pides 'soporte')
    if (!(input$var_emmeans %in% names(model.frame(mod)))) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "Esta variable no está incluida en el modelo seleccionado.") + 
               theme_void())
    }
    
    # 3. Calcular emmeans
    # Usamos type = "response" para obtener probabilidades (0-1)
    emm <- emmeans(mod, specs = input$var_emmeans, type = "response")
    df_emm <- as.data.frame(emm)
    
    # 4. Gráfico
    ggplot(df_emm, aes(x = reorder(!!sym(input$var_emmeans), prob), y = prob)) +
      geom_point(size = 4, color = "#2c3e50") +
      geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2, color = "#2c3e50") +
      scale_y_continuous(labels = scales::percent, limits = c(0, max(df_emm$asymp.UCL) + 0.05)) +
      coord_flip() +
      labs(
        x = "Categoría",
        y = "Probabilidad de Éxito estimada"
      ) +
      theme_minimal(base_size = 15) +
      theme(panel.grid.minor = element_blank())
  })
  
  #--- Tabla AIC/BIC ---
  output$tabla_comparativa <- renderTable({
    modelos <- list("Principal" = m_principal, "A1" = mA1, "A2" = mA2, "A3" = mA3)
    lapply(modelos, glance) %>%
      bind_rows(.id = "Modelo") %>%
      select(Modelo, AIC, BIC, logLik) %>%
      mutate(across(where(is.numeric), round, 2))
  }, striped = TRUE)
  
  #--- Gráfico ROC ---
  output$plot_roc <- renderPlot({
    req(input$modelos_roc)
    library(pROC)
    
    colores <- c("m_principal" = "#2c3e50", "mA1" = "#e74c3c", "mA2" = "#27ae60", "mA3" = "#f39c12")
    
    # Crear el lienzo del gráfico
    plot(NULL, xlim=c(1,0), ylim=c(0,1), xlab="Especificidad", ylab="Sensibilidad")
    abline(a=1, b=-1, lty=2, col="grey")
    
    for(m_name in input$modelos_roc){
      mod <- get(m_name, envir = .GlobalEnv)
      curva <- roc(mod$y, predict(mod, type="response"), quiet = TRUE)
      plot(curva, add=TRUE, col=colores[m_name], lwd=3)
    }
    legend("bottomright", legend=input$modelos_roc, col=colores[input$modelos_roc], lwd=3)
  })
  
  #--- Tabla AUC ---
  output$tabla_auc <- renderTable({
    req(input$modelos_roc)
    data.frame(
      Modelo = input$modelos_roc,
      AUC = sapply(input$modelos_roc, function(m) {
        mod <- get(m, envir = .GlobalEnv)
        as.numeric(auc(mod$y, predict(mod, type="response")))
      })
    ) %>% arrange(desc(AUC))
  }, digits = 3)
  
  #--- Gráfico de Residuos ---
  output$plot_residuos <- renderPlot({
    df_res <- data.frame(
      ajustados = predict(m_principal, type = "response"),
      residuos = residuals(m_principal, type = "pearson")
    )
    ggplot(df_res, aes(x = ajustados, y = residuos)) +
      geom_point(alpha = 0.3) +
      geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
      geom_smooth(method = "loess", se = FALSE, color = "blue") +
      theme_minimal() +
      labs(x = "Probabilidad predicha", y = "Residuos de Pearson")
  })
  #------------------------------
  # GRÁFICO DE INTERACCIÓN
  #------------------------------
  output$plot_interaccion <- renderPlot({
    formula_inter <- as.formula(paste("~", input$inter_var1, "|", input$inter_var2))
    
    emmip(m_principal, formula_inter, type = "response") +
      geom_line(linewidth = 1) +
      geom_point(size = 3) +
      labs(
        title = paste("Interacción entre", input$inter_var1, "y", input$inter_var2),
        y = "Probabilidad de Éxito",
        x = input$inter_var1,
        color = input$inter_var2
      ) +
      scale_y_continuous(labels = scales::percent) +
      theme_minimal(base_size = 15) +
      theme(legend.position = "bottom")
  })
}
#==================================================
# RUN APP
#==================================================
shinyApp(ui, server)

