# app.R
# Aplicación Shiny para Planeación Agregada con Proveedores

# Cargar bibliotecas necesarias
library(shiny)
library(shinydashboard)
library(DT)
library(lpSolve)
library(tidyverse)
library(plotly)

# Definir la interfaz de usuario
ui <- dashboardPage(
  dashboardHeader(title = "Planeación Agregada con Proveedores"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Parámetros", tabName = "parameters", icon = icon("cogs")),
      menuItem("Solución", tabName = "solution", icon = icon("table")),
      menuItem("Gráficos", tabName = "graphs", icon = icon("chart-bar"))
    )
  ),
  dashboardBody(
    tabItems(
      # Pestaña de parámetros
      tabItem(tabName = "parameters",
              fluidRow(
                box(
                  title = "Parámetros de Producción", status = "primary", solidHeader = TRUE,
                  width = 6,
                  numericInput("dias_laborables", "Días laborables por mes:", 23),
                  numericInput("horas_por_dia", "Horas por día:", 8),
                  numericInput("tiempo_produccion", "Tiempo de producción por unidad (minutos):", 8),
                  numericInput("salario_normal", "Salario por hora en tiempo normal ($):", 25),
                  numericInput("prima_extra", "Prima por tiempo extra (%):", 50),
                  numericInput("costo_materiales", "Costo de materiales por unidad ($):", 24),
                  numericInput("costo_inventario", "Costo de mantener inventario por unidad por mes ($):", 3),
                  numericInput("max_horas_extra", "Máximo de horas extra por trabajador por mes:", 18)
                ),
                box(
                  title = "Parámetros de Fuerza Laboral e Inventario", status = "primary", solidHeader = TRUE,
                  width = 6,
                  numericInput("trabajadores_iniciales", "Número inicial de trabajadores:", 1200),
                  numericInput("inventario_inicial", "Inventario inicial (miles de unidades):", 60)
                )
              ),
              fluidRow(
                box(
                  title = "Parámetros de Proveedores", status = "primary", solidHeader = TRUE,
                  width = 12,
                  fluidRow(
                    column(4,
                           h4("Proveedor A"),
                           numericInput("costo_A", "Costo por unidad ($):", 39),
                           numericInput("capacidad_A", "Capacidad máxima (miles de unidades):", 150)
                    ),
                    column(4,
                           h4("Proveedor B"),
                           numericInput("costo_B", "Costo por unidad ($):", 37.5),
                           numericInput("capacidad_B", "Capacidad máxima (miles de unidades):", 150)
                    ),
                    column(4,
                           h4("Proveedor C"),
                           numericInput("costo_C", "Costo por unidad ($):", 38),
                           numericInput("capacidad_C", "Capacidad máxima (miles de unidades):", 90)
                    )
                  )
                )
              ),
              fluidRow(
                box(
                  title = "Pronóstico de Demanda", status = "primary", solidHeader = TRUE,
                  width = 12,
                  fluidRow(
                    column(2, numericInput("demanda_1", "Mes 1:", 950)),
                    column(2, numericInput("demanda_2", "Mes 2:", 1050)),
                    column(2, numericInput("demanda_3", "Mes 3:", 1200)),
                    column(2, numericInput("demanda_4", "Mes 4:", 1300)),
                    column(2, numericInput("demanda_5", "Mes 5:", 1450)),
                    column(2, numericInput("demanda_6", "Mes 6:", 1600))
                  ),
                  fluidRow(
                    column(2, numericInput("demanda_7", "Mes 7:", 1500)),
                    column(2, numericInput("demanda_8", "Mes 8:", 1100)),
                    column(2, numericInput("demanda_9", "Mes 9:", 950)),
                    column(2, numericInput("demanda_10", "Mes 10:", 900)),
                    column(2, numericInput("demanda_11", "Mes 11:", 1200)),
                    column(2, numericInput("demanda_12", "Mes 12:", 1350))
                  )
                )
              ),
              fluidRow(
                box(
                  width = 12,
                  actionButton("solve", "Resolver Modelo", icon = icon("calculator"), 
                               style = "color: #fff; background-color: #337ab7; border-color: #2e6da4; padding: 10px 15px;")
                )
              )
      ),
      
      # Pestaña de solución
      tabItem(tabName = "solution",
              fluidRow(
                box(
                  title = "Resultados del Modelo", status = "success", solidHeader = TRUE,
                  width = 12,
                  verbatimTextOutput("costo_total"),
                  DTOutput("resultados_tabla"),
                  downloadButton("download_results", "Descargar Resultados")
                )
              )
      ),
      
      # Pestaña de gráficos
      tabItem(tabName = "graphs",
              fluidRow(
                box(
                  title = "Demanda vs. Producción", status = "info", solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("grafico_demanda_produccion", height = 350)
                ),
                box(
                  title = "Nivel de Inventario", status = "info", solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("grafico_inventario", height = 350)
                )
              ),
              fluidRow(
                box(
                  title = "Distribución de la Producción", status = "info", solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("grafico_distribucion", height = 350)
                ),
                box(
                  title = "Utilización de Proveedores", status = "info", solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("grafico_proveedores", height = 350)
                )
              )
      )
    )
  )
)

# Definir la lógica del servidor
server <- function(input, output, session) {
  
  # Almacenar los resultados del modelo
  resultados <- reactiveVal(NULL)
  
  # Función para resolver el modelo de programación lineal
  observeEvent(input$solve, {
    # Crear un vector de demanda a partir de los inputs
    demanda <- c(
      input$demanda_1, input$demanda_2, input$demanda_3, input$demanda_4, 
      input$demanda_5, input$demanda_6, input$demanda_7, input$demanda_8, 
      input$demanda_9, input$demanda_10, input$demanda_11, input$demanda_12
    )
    
    # Parámetros del modelo
    n_periodos <- 12
    dias_laborables <- input$dias_laborables
    horas_por_dia <- input$horas_por_dia
    tiempo_produccion <- input$tiempo_produccion / 60  # Convertir a horas
    salario_normal <- input$salario_normal
    salario_extra <- input$salario_normal * (1 + input$prima_extra/100)
    costo_materiales <- input$costo_materiales
    costo_inventario <- input$costo_inventario
    max_horas_extra <- input$max_horas_extra
    trabajadores_iniciales <- input$trabajadores_iniciales
    inventario_inicial <- input$inventario_inicial
    
    # Parámetros de los proveedores
    costo_A <- input$costo_A
    capacidad_A <- input$capacidad_A
    costo_B <- input$costo_B
    capacidad_B <- input$capacidad_B
    costo_C <- input$costo_C
    capacidad_C <- input$capacidad_C
    
    # Calcular el número de variables por período
    # Orden: W, H, L, P, O, PO, I, PA, PB, PC (10 variables por período)
    n_variables_por_periodo <- 10
    total_variables <- n_variables_por_periodo * n_periodos
    
    # Calcular el número de restricciones
    # Por período: 1 balance de inventario, 1 capacidad tiempo normal, 1 límite horas extra,
    # 3 capacidades proveedores, 1 balance de trabajadores, 1 producción en tiempo extra
    # Total: 8 restricciones por período + 2 restricciones de condiciones finales
    n_restricciones_por_periodo <- 8
    total_restricciones <- n_restricciones_por_periodo * n_periodos + 2
    
    # Crear la matriz de coeficientes para la función objetivo
    obj <- numeric(total_variables)
    
    # Calcular capacidad de producción por trabajador (en miles de unidades)
    capacidad_trabajador <- (dias_laborables * horas_por_dia / tiempo_produccion) / 1000
    
    # Llenar la función objetivo
    for (t in 1:n_periodos) {
      indice_base <- (t - 1) * n_variables_por_periodo
      
      # Costo de mano de obra regular (W)
      obj[indice_base + 1] <- salario_normal * dias_laborables * horas_por_dia
      
      # Costo de contratación (H) - Consideramos 0 para este modelo
      obj[indice_base + 2] <- 0
      
      # Costo de despido (L) - Consideramos 0 para este modelo
      obj[indice_base + 3] <- 0
      
      # Costo de materiales para producción normal (P)
      obj[indice_base + 4] <- costo_materiales
      
      # Costo de tiempo extra (O)
      obj[indice_base + 5] <- salario_extra
      
      # Costo de producción en tiempo extra (PO) - Solo materiales
      obj[indice_base + 6] <- 0  # Costo de mano de obra ya incluido en O
      
      # Costo de inventario (I)
      obj[indice_base + 7] <- costo_inventario
      
      # Costo de proveedores
      obj[indice_base + 8] <- costo_A  # Proveedor A
      obj[indice_base + 9] <- costo_B  # Proveedor B
      obj[indice_base + 10] <- costo_C  # Proveedor C
    }
    
    # Crear la matriz de restricciones
    mat <- matrix(0, nrow = total_restricciones, ncol = total_variables)
    
    # Crear vector de dirección de las restricciones y vector de términos independientes
    dir <- character(total_restricciones)
    rhs <- numeric(total_restricciones)
    
    # Contador para llevar registro de las restricciones
    contador_restricciones <- 1
    
    # Llenar las restricciones
    for (t in 1:n_periodos) {
      indice_base <- (t - 1) * n_variables_por_periodo
      
      # 1. Restricción de producción en tiempo extra (PO = O / (tiempo_produccion * 1000))
      mat[contador_restricciones, indice_base + 5] <- -1 / (tiempo_produccion * 1000)  # O
      mat[contador_restricciones, indice_base + 6] <- 1  # PO
      dir[contador_restricciones] <- "="
      rhs[contador_restricciones] <- 0
      contador_restricciones <- contador_restricciones + 1
      
      # 2. Restricción de balance de inventario
      if (t == 1) {
        # Para el primer período, usamos el inventario inicial
        mat[contador_restricciones, indice_base + 4] <- 1  # P
        mat[contador_restricciones, indice_base + 6] <- 1  # PO
        mat[contador_restricciones, indice_base + 7] <- 1  # I
        mat[contador_restricciones, indice_base + 8] <- 1  # PA
        mat[contador_restricciones, indice_base + 9] <- 1  # PB
        mat[contador_restricciones, indice_base + 10] <- 1  # PC
        dir[contador_restricciones] <- "="
        rhs[contador_restricciones] <- demanda[t] + inventario_inicial
      } else {
        # Para los demás períodos
        mat[contador_restricciones, indice_base - 3] <- -1  # I del período anterior
        mat[contador_restricciones, indice_base + 4] <- 1  # P
        mat[contador_restricciones, indice_base + 6] <- 1  # PO
        mat[contador_restricciones, indice_base + 7] <- 1  # I
        mat[contador_restricciones, indice_base + 8] <- 1  # PA
        mat[contador_restricciones, indice_base + 9] <- 1  # PB
        mat[contador_restricciones, indice_base + 10] <- 1  # PC
        dir[contador_restricciones] <- "="
        rhs[contador_restricciones] <- demanda[t]
      }
      contador_restricciones <- contador_restricciones + 1
      
      # 3. Restricción de capacidad de producción normal
      mat[contador_restricciones, indice_base + 1] <- -capacidad_trabajador  # W
      mat[contador_restricciones, indice_base + 4] <- 1  # P
      dir[contador_restricciones] <- "<="
      rhs[contador_restricciones] <- 0
      contador_restricciones <- contador_restricciones + 1
      
      # 4. Restricción de horas extra
      mat[contador_restricciones, indice_base + 1] <- -max_horas_extra  # W
      mat[contador_restricciones, indice_base + 5] <- 1  # O
      dir[contador_restricciones] <- "<="
      rhs[contador_restricciones] <- 0
      contador_restricciones <- contador_restricciones + 1
      
      # 5-7. Restricciones de capacidad de los proveedores
      # Proveedor A
      mat[contador_restricciones, indice_base + 8] <- 1  # PA
      dir[contador_restricciones] <- "<="
      rhs[contador_restricciones] <- capacidad_A
      contador_restricciones <- contador_restricciones + 1
      
      # Proveedor B
      mat[contador_restricciones, indice_base + 9] <- 1  # PB
      dir[contador_restricciones] <- "<="
      rhs[contador_restricciones] <- capacidad_B
      contador_restricciones <- contador_restricciones + 1
      
      # Proveedor C
      mat[contador_restricciones, indice_base + 10] <- 1  # PC
      dir[contador_restricciones] <- "<="
      rhs[contador_restricciones] <- capacidad_C
      contador_restricciones <- contador_restricciones + 1
      
      # 8. Restricción de balance de trabajadores
      if (t == 1) {
        # Para el primer período
        mat[contador_restricciones, indice_base + 1] <- 1  # W
        mat[contador_restricciones, indice_base + 2] <- -1  # H
        mat[contador_restricciones, indice_base + 3] <- 1  # L
        dir[contador_restricciones] <- "="
        rhs[contador_restricciones] <- trabajadores_iniciales
      } else {
        # Para los demás períodos
        mat[contador_restricciones, indice_base - 9] <- -1  # W del período anterior
        mat[contador_restricciones, indice_base + 1] <- 1  # W
        mat[contador_restricciones, indice_base + 2] <- -1  # H
        mat[contador_restricciones, indice_base + 3] <- 1  # L
        dir[contador_restricciones] <- "="
        rhs[contador_restricciones] <- 0
      }
      contador_restricciones <- contador_restricciones + 1
    }
    
    # Restricción de número final de trabajadores
    mat[contador_restricciones, total_variables - 9] <- 1  # W del último período
    dir[contador_restricciones] <- "="
    rhs[contador_restricciones] <- trabajadores_iniciales
    contador_restricciones <- contador_restricciones + 1
    
    # Restricción de inventario final
    mat[contador_restricciones, total_variables - 3] <- 1  # I del último período
    dir[contador_restricciones] <- "="
    rhs[contador_restricciones] <- inventario_inicial
    
    # Resolver el modelo
    resultado_lp <- lp("min", obj, mat, dir, rhs, all.bin = FALSE)
    
    # Verificar si se encontró una solución
    if (resultado_lp$status == 0) {
      # Extraer los valores de las variables
      valores <- resultado_lp$solution
      
      # Crear un data frame con los resultados
      resultados_df <- data.frame(
        Mes = 1:n_periodos,
        W = numeric(n_periodos),  # Trabajadores
        H = numeric(n_periodos),  # Contrataciones
        L = numeric(n_periodos),  # Despidos
        P = numeric(n_periodos),  # Producción normal
        O = numeric(n_periodos),  # Horas extra
        PO = numeric(n_periodos), # Producción en tiempo extra
        I = numeric(n_periodos),  # Inventario
        PA = numeric(n_periodos), # Proveedor A
        PB = numeric(n_periodos), # Proveedor B
        PC = numeric(n_periodos), # Proveedor C
        D = demanda             # Demanda
      )
      
      # Llenar el data frame con los valores de las variables
      for (t in 1:n_periodos) {
        indice_base <- (t - 1) * n_variables_por_periodo
        resultados_df$W[t] <- round(valores[indice_base + 1])
        resultados_df$H[t] <- round(valores[indice_base + 2])
        resultados_df$L[t] <- round(valores[indice_base + 3])
        resultados_df$P[t] <- round(valores[indice_base + 4], 2)
        resultados_df$O[t] <- round(valores[indice_base + 5], 2)
        resultados_df$PO[t] <- round(valores[indice_base + 6], 2)
        resultados_df$I[t] <- round(valores[indice_base + 7], 2)
        resultados_df$PA[t] <- round(valores[indice_base + 8], 2)
        resultados_df$PB[t] <- round(valores[indice_base + 9], 2)
        resultados_df$PC[t] <- round(valores[indice_base + 10], 2)
      }
      
      # Calcular la producción total para cada mes
      resultados_df$Produccion_Total <- resultados_df$P + resultados_df$PO + resultados_df$PA + resultados_df$PB + resultados_df$PC
      
      # Calcular el costo total
      costo_total <- resultado_lp$objval
      
      # Almacenar los resultados
      resultados(list(
        df = resultados_df,
        costo_total = costo_total
      ))
    } else {
      # Si no se encontró una solución, mostrar un mensaje de error
      showNotification("No se pudo encontrar una solución óptima. Verifique los parámetros.", type = "error")
    }
  })
  
  # Mostrar la tabla de resultados
  output$resultados_tabla <- renderDT({
    req(resultados())
    
    # Crear una tabla con los resultados
    tabla <- resultados()$df %>%
      select(Mes, W, P, O, PO, PA, PB, PC, I, D, Produccion_Total) %>%
      rename(
        "Trabajadores" = W,
        "Prod. Normal" = P,
        "Horas Extra" = O,
        "Prod. Extra" = PO,
        "Prov. A" = PA,
        "Prov. B" = PB,
        "Prov. C" = PC,
        "Inventario" = I,
        "Demanda" = D,
        "Prod. Total" = Produccion_Total
      )
    
    # Agregar una fila con los totales
    totales <- tabla %>%
      summarise(
        Mes = "Total",
        Trabajadores = NA,
        `Prod. Normal` = sum(`Prod. Normal`),
        `Horas Extra` = sum(`Horas Extra`),
        `Prod. Extra` = sum(`Prod. Extra`),
        `Prov. A` = sum(`Prov. A`),
        `Prov. B` = sum(`Prov. B`),
        `Prov. C` = sum(`Prov. C`),
        Inventario = NA,
        Demanda = sum(Demanda),
        `Prod. Total` = sum(`Prod. Total`)
      )
    
    tabla_final <- rbind(tabla, totales)
    
    datatable(tabla_final, options = list(
      pageLength = 13,
      dom = 'Bfrtip',
      buttons = c('copy', 'csv', 'excel', 'pdf')
    ), rownames = FALSE) %>%
      formatRound(c('Prod. Normal', 'Horas Extra', 'Prod. Extra', 'Prov. A', 'Prov. B', 'Prov. C', 'Inventario', 'Prod. Total'), 2)
  })
  
  # Mostrar el costo total
  output$costo_total <- renderText({
    req(resultados())
    paste("Costo Total: $", formatC(resultados()$costo_total, format = "f", big.mark = ",", digits = 2))
  })
  
  # Gráfico de demanda vs producción
  output$grafico_demanda_produccion <- renderPlotly({
    req(resultados())
    
    df <- resultados()$df
    
    plot_ly() %>%
      add_trace(x = ~Mes, y = ~D, data = df, name = 'Demanda', type = 'scatter', mode = 'lines+markers', line = list(color = 'blue')) %>%
      add_trace(x = ~Mes, y = ~Produccion_Total, data = df, name = 'Producción Total', type = 'scatter', mode = 'lines+markers', line = list(color = 'green')) %>%
      layout(title = 'Demanda vs. Producción Total',
             xaxis = list(title = 'Mes'),
             yaxis = list(title = 'Miles de unidades'))
  })
  
  # Gráfico de nivel de inventario
  output$grafico_inventario <- renderPlotly({
    req(resultados())
    
    df <- resultados()$df
    
    plot_ly() %>%
      add_trace(x = ~Mes, y = ~I, data = df, name = 'Inventario', type = 'scatter', mode = 'lines+markers', line = list(color = 'orange')) %>%
      layout(title = 'Nivel de Inventario',
             xaxis = list(title = 'Mes'),
             yaxis = list(title = 'Miles de unidades'))
  })
  
  # Gráfico de distribución de producción
  output$grafico_distribucion <- renderPlotly({
    req(resultados())
    
    df <- resultados()$df
    
    plot_ly() %>%
      add_trace(x = ~Mes, y = ~P, data = df, name = 'Prod. Normal', type = 'bar', marker = list(color = '#1f77b4')) %>%
      add_trace(x = ~Mes, y = ~PO, data = df, name = 'Prod. Extra', type = 'bar', marker = list(color = '#ff7f0e')) %>%
      add_trace(x = ~Mes, y = ~PA, data = df, name = 'Prov. A', type = 'bar', marker = list(color = '#2ca02c')) %>%
      add_trace(x = ~Mes, y = ~PB, data = df, name = 'Prov. B', type = 'bar', marker = list(color = '#d62728')) %>%
      add_trace(x = ~Mes, y = ~PC, data = df, name = 'Prov. C', type = 'bar', marker = list(color = '#9467bd')) %>%
      layout(title = 'Distribución de la Producción',
             xaxis = list(title = 'Mes'),
             yaxis = list(title = 'Miles de unidades'),
             barmode = 'stack')
  })
  
  # Gráfico de utilización de proveedores
  output$grafico_proveedores <- renderPlotly({
    req(resultados())
    
    df <- resultados()$df
    
    plot_ly() %>%
      add_trace(x = ~Mes, y = ~PA, data = df, name = 'Proveedor A', type = 'scatter', mode = 'lines+markers', line = list(color = '#2ca02c')) %>%
      add_trace(x = ~Mes, y = ~PB, data = df, name = 'Proveedor B', type = 'scatter', mode = 'lines+markers', line = list(color = '#d62728')) %>%
      add_trace(x = ~Mes, y = ~PC, data = df, name = 'Proveedor C', type = 'scatter', mode = 'lines+markers', line = list(color = '#9467bd')) %>%
      layout(title = 'Utilización de Proveedores',
             xaxis = list(title = 'Mes'),
             yaxis = list(title = 'Miles de unidades'))
  })
  
  # Descargar resultados
  output$download_results <- downloadHandler(
    filename = function() {
      paste("resultados_planeacion_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(resultados()$df, file, row.names = FALSE)
    }
  )
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)

