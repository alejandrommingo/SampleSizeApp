# Instalamos y cargamos los paquetes necesarios
if (!require("shiny")) install.packages("shiny")
library(shiny)
if (!require("lme4")) install.packages("lme4")
library(lme4)
if (!require("pwr")) install.packages("pwr")
library(pwr)
if (!require("shinythemes")) install.packages("shinythemes")
library(shinythemes)

# Definimos la interfaz de usuario
ui <- fluidPage(
  theme = shinythemes::shinytheme("cerulean"),
  titlePanel("Calculadora de tamaño de muestra"),
  tags$style(
    "
    table.shiny-table td {
      padding: 10px;
      border: 1px solid #ddd;
    }
    "
  ),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("method", "Método",
                  choices = c("Comparar proporciones",
                              "Comparar medias - grupos de igual tamaño",
                              "Comparar medias - grupos de distinto tamaño",
                              "ANOVA de un Factor con N grupos independientes",
                              "Regresión logística simple",
                              "Medidas repetidas",
                              "Regresión logística mixta (Monte Carlo) - efecto fijo y medidas repetidas")),
      helpText("Seleccione el método para calcular el tamaño de muestra."),
      
      conditionalPanel(
        condition = "input.method == 'Comparar proporciones'",
        numericInput("p1", "Proporción grupo 1", value = 0.45, min = 0, max = 1),
        helpText("Proporción esperada de eventos en el grupo 1 (de 0 a 1)"),
        numericInput("p2", "Proporción grupo 2", value = 0.55, min = 0, max = 1),
        helpText("Proporción esperada de eventos en el grupo 2 (de 0 a 1)"),
        numericInput("alpha", "Nivel de significación", value = 0.05, min = 0, max = 1),
        helpText("Nivel de significación usado para la prueba estadística"),
        numericInput("power", "Potencia estadística", value = 0.8, min = 0, max = 1),
        helpText("La potencia deseada para la prueba estadística")
      ),
      
      conditionalPanel(
        condition = "input.method == 'Comparar medias - grupos de igual tamaño'",
        numericInput("delta", "Diferencia mínima en las medias", value = 5),
        helpText("La diferencia mínima en las medias que se desea detectar entre los dos grupos"),
        numericInput("sigma", "Desviación estándar común estimada", value = 10),
        helpText("La desviación estándar común estimada para ambos grupos"),
        numericInput("alpha_cm", "Nivel de significación", value = 0.05, min = 0, max = 1),
        helpText("Nivel de significación usado para la prueba estadística"),
        numericInput("power_cm", "Potencia estadística", value = 0.8, min = 0, max = 1),
        helpText("La potencia deseada para la prueba estadística")
      ),
      
      conditionalPanel(
        condition = "input.method == 'Comparar medias - grupos de distinto tamaño'",
        numericInput("delta_unequal", "Diferencia mínima en las medias", value = 5),
        helpText("La diferencia mínima en las medias que se desea detectar entre los dos grupos"),
        numericInput("sigma_unequal", "Desviación estándar común estimada", value = 10),
        helpText("La desviación estándar común estimada para ambos grupos"),
        numericInput("proporcion_casos", "Proporción de casos en el grupo objetivo", value = 0.5, min = 0, max = 1),
        helpText("Proporción de casos en el grupo objetivo"),
        numericInput("alpha_cmd", "Nivel de significación", value = 0.05, min = 0, max = 1),
        helpText("Nivel de significación usado para la prueba estadística"),
        numericInput("power_cmd", "Potencia estadística", value = 0.8, min = 0, max = 1),
        helpText("La potencia deseada para la prueba estadística")
      ),
      
      conditionalPanel(
        condition = "input.method == 'ANOVA de un Factor con N grupos independientes'",
        numericInput("k", "Número de grupos", value = 3),
        helpText("Número de grupos independientes sobre los que se va a comparar una diferencia de medias"),
        numericInput("f", "Tamaño del efecto (Cohen's f)", value = 0.25),
        helpText("Tamaño del efecto (Cohen's f), que se puede calcular como la raíz cuadrada de (suma de cuadrados entre grupos / suma de cuadrados total)."),
        numericInput("alpha_anova", "Nivel de significación", value = 0.05, min = 0, max = 1),
        helpText("Nivel de significación usado para la prueba estadística"),
        numericInput("power_anova", "Potencia estadística", value = 0.8, min = 0, max = 1),
        helpText("La potencia deseada para la prueba estadística")
      ),
      
      conditionalPanel(
        condition = "input.method == 'Regresión logística simple'",
        numericInput("k_rl", "Número de predictores independientes", value = 1, min = 1),
        helpText("Número de variables independientes en la regresión"),
        numericInput("p_rl", "Proporción de eventos en la muestra", value = 0.5, min = 0, max = 1),
        helpText("Proporción de casos en el grupo objetivo")
      ),
      
      conditionalPanel(
        condition = "input.method == 'Medidas repetidas'",
        numericInput("n_groups", "Número de grupos", value = 2, min = 2),
        helpText("El número de grupos en el diseño del estudio"),
        numericInput("n_measurements", "Número de mediciones por grupo", value = 2, min = 1),
        helpText("El número de mediciones por grupo en el diseño del estudio"),
        numericInput("effect_size", "Tamaño del efecto", value = 0.2, min = 0),
        helpText("El tamaño del efecto que se desea detectar en el análisis de medidas repetidas"),
        numericInput("alpha_mr", "Nivel de significación", value = 0.05, min = 0, max = 1),
        helpText("El nivel de significancia alfa para la prueba estadística en el análisis de medidas repetidas (0 a 1)"),
        numericInput("power_mr", "Potencia estadística", value = 0.8, min = 0, max = 1),
        helpText("La potencia deseada para la prueba estadística en el análisis de medidas repetidas (0 a 1)"),
        numericInput("correlation", "Correlación entre mediciones", value = 0.5, min = -1, max = 1),
        helpText("La correlación estimada entre las mediciones en el diseño de medidas repetidas (-1 a 1)")
      ),
      
      conditionalPanel(
        condition = "input.method == 'Regresión logística mixta (Monte Carlo) - efecto fijo y medidas repetidas'",
        numericInput("initial_n", "Valor inicial de muestra para la simulación", value = 10, min = 1),
        helpText("El valor inicial de la muestra para la simulación Monte Carlo"),
        numericInput("k_mixed", "Número de mediciones repetidas", value = 2, min = 1),
        helpText("El número de mediciones repetidas en el diseño del estudio"),
        numericInput("p_mixed", "Proporción de eventos en la muestra", value = 0.5, min = 0, max = 1),
        helpText("La proporción de eventos en la muestra"),
        numericInput("rho", "Correlación intra-sujeto estimada", value = 0.5, min = 0, max = 1),
        helpText("La correlación intra-sujeto estimada entre las mediciones (-1 a 1)"),
        numericInput("alpha_mixed", "Nivel de significación", value = 0.05, min = 0, max = 1),
        helpText("El nivel de significancia alfa para la prueba estadística en el análisis de modelos mixtos (0 a 1)"),
        numericInput("beta_mixed", "Coeficiente para el efecto fijo", value = 0.5, min = 0, max = 1),
        helpText("El coeficiente para el efecto fijo en el modelo mixto"),
        numericInput("power_mixed", "Potencia estadística", value = 0.8, min = 0, max = 1),
        helpText("La potencia deseada para la prueba estadística en el análisis de modelos mixtos (0 a 1)"),
        numericInput("n_simulations", "Número de simulaciones de Monte Carlo", value = 100, min = 10),
        helpText("El número de simulaciones de Monte Carlo que se realizarán para estimar el tamaño de muestra"),
        numericInput("step_size", "Incremento en el tamaño de la muestra en cada iteración", value = 5, min = 1),
        helpText("El incremento en el tamaño de la muestra en cada iteración de la simulación Monte Carlo")
      ),
      actionButton("calculate", "Calcular")
    ),
    
    mainPanel(
      uiOutput("result")
    )
  )
)


# Definimos la lógica del servidor
server <- function(input, output) {
  observeEvent(input$calculate, {
    n <- NULL
    reference <- NULL
    
    if (input$method == "Comparar proporciones") {
      z_alpha <- qnorm(1 - input$alpha / 2)
      z_beta <- qnorm(input$power)
      p_bar <- (input$p1 + input$p2) / 2
      n <- round(((z_alpha * sqrt(2 * p_bar * (1 - p_bar)) + z_beta * sqrt(input$p1 * (1 - input$p1) + input$p2 * (1 - input$p2))) / (input$p1 - input$p2))^2, digits = 0)
      reference <- "Rosner, B. (2015). Fundamentals of biostatistics. Cengage learning."
    } else if (input$method == "Comparar medias - grupos de igual tamaño") {
      delta <- input$delta
      sigma <- input$sigma
      alpha_cm <- input$alpha_cm
      power_cm <- input$power_cm
      
      result <- pwr.t.test(d = delta / sigma, sig.level = alpha_cm, power = power_cm, type = "two.sample", alternative = "two.sided", n = NULL)
      n <- round(result$n* 2, digits = 0)
      
      reference <- "Chow, S. C., Shao, J., Wang, H., & Lokhnygina, Y. (2017). Sample size calculations in clinical research. CRC press. \n Lenth, R. V. (2001). Some practical guidelines for effective sample size determination. The American Statistician, 55(3), 187-193."
      
    } else if (input$method == "Comparar medias - grupos de distinto tamaño") {
      delta_unequal <- input$delta_unequal
      sigma_unequal <- input$sigma_unequal
      proporcion_casos <- input$proporcion_casos
      alpha_cmd <- input$alpha_cmd
      power_cmd <- input$power_cmd
      
      result <- pwr.t.test(n = NULL, d = delta_unequal / sigma_unequal, sig.level = alpha_cmd, power = power_cmd, type = "two.sample", alternative = "two.sided")
      n_total <- ceiling(as.numeric(result$n)*2)
      n1 <- ceiling(n_total * proporcion_casos)
      n2 <- n_total - n1
      n_plot = n_total
      n <- paste("Grupo 1:", n1, "// Grupo 2:", n2)
      reference <- "Chow, S. C., Shao, J., Wang, H., & Lokhnygina, Y. (2017). Sample size calculations in clinical research. CRC press. \n Lenth, R. V. (2001). Some practical guidelines for effective sample size determination. The American Statistician, 55(3), 187-193."
      
    } else if (input$method == "ANOVA de un Factor con N grupos independientes") {
      # Parámetros para el cálculo del tamaño de muestra
      k <- input$k  # Número de grupos
      f <- input$f  # Tamaño del efecto (Cohen's f)
      sig.level <- input$alpha_anova  # Nivel de significación (alfa)
      power <- input$power_anova  # Potencia estadística (1 - beta)
      
      # Calcular el tamaño de muestra
      result <- pwr.anova.test(k = k, f = f, sig.level = sig.level, power = power)
      
      n <- round(result$n* k, digits = 0)
      
      reference <- "Chow, S. C., Shao, J., Wang, H., & Lokhnygina, Y. (2017). Sample size calculations in clinical research. CRC press. \n Lenth, R. V. (2001). Some practical guidelines for effective sample size determination. The American Statistician, 55(3), 187-193."
      
    } else if (input$method == "Regresión logística simple") {
      n <- ceiling((10 * input$k_rl) / input$p_rl)
      reference <- "Peduzzi, P., Concato, J., Kemper, E., Holford, T. R., & Feinstein, A. R. (1996). A simulation study of the number of events per variable in logistic regression analysis. Journal of clinical epidemiology, 49(12), 1373-1379. \n Vittinghoff, E., & McCulloch, C. E. (2007). Relaxing the rule of ten events per variable in logistic and Cox regression. American journal of epidemiology, 165(6), 710-718."
    } else if (input$method == "Medidas repetidas") {
      alpha_mr <- input$alpha_mr
      power_mr <- input$power_mr
      n_groups <- input$n_groups
      n_measurements <- input$n_measurements
      effect_size <- input$effect_size
      correlation <- input$correlation
      
      n_total <- n_groups * n_measurements
      
      # Función para calcular el tamaño de muestra para un diseño de medidas repetidas
      library(pwr)
      result <- pwr.t.test(d = effect_size, sig.level = alpha_mr, power = power_mr,
                           type = "two.sample", alternative = "two.sided", n = NULL)
      
      n <- round(result$n * n_total)
      
      reference <- "Cohen, J. (1977). Statistical power analysis for the behavioral sciences (revised ed.). \n Maxwell, S. E., Delaney, H. D., & Kelley, K. (2017). Designing experiments and analyzing data: A model comparison perspective. Routledge."
    } else if (input$method == "Regresión logística mixta (Monte Carlo) - efecto fijo y medidas repetidas") {
      
      # Función para generar la simulacion
      monte_carlo_simulation <- function(n, k, p, rho, beta, n_simulations = 1000, alpha = 0.05) {
        significant_results <- 0
        
        for (i in 1:n_simulations) {
          # Simula los datos
          random_intercept <- rnorm(n, mean = 0, sd = sqrt(rho))
          fixed_effect <- rnorm(n * k, mean = 0, sd = 1)
          linear_predictor <- beta * fixed_effect + rep(random_intercept, each = k)
          probability <- 1 / (1 + exp(-linear_predictor))
          response <- rbinom(n * k, size = 1, prob = probability)
          
          # Ajusta el modelo de regresión logística mixta
          data <- data.frame(subject = rep(1:n, each = k),
                             time = rep(1:k, n),
                             fixed_effect = fixed_effect,
                             response = response)
          model <- glmer(response ~ fixed_effect + (1 | subject), data = data, family = binomial, nAGQ = 1)
          
          # Evalúa si el efecto fijo es significativo
          p_value <- coef(summary(model))["fixed_effect", "Pr(>|z|)"]
          if (p_value < alpha) {
            significant_results <- significant_results + 1
          }
        }
        
        # Calcula el poder estadístico
        power <- significant_results / n_simulations
        return(power)
      }
      
      n = input$initial_n
      k_mixed <- input$k_mixed
      p_mixed <- input$p_mixed
      rho <- input$rho
      beta_mixed = input$beta_mixed
      alpha_mixed <- input$alpha_mixed
      target_power_mixed <- input$power_mixed
      n_simulations <- input$n_simulations
      step_size = input$step_size
      
      # Calculamos el tamaño de muestra necesario utilizando simulación de Monte Carlo
      power <- 0
      withProgress(message = "Calculando el tamaño de muestra...", {
        while (power < target_power_mixed) {
          power <- monte_carlo_simulation(n, k_mixed, p_mixed, rho, beta_mixed, n_simulations, alpha_mixed)
          cat("Tamaño de muestra:", n, " Poder estadístico:", power, "\n")
          
          if (power < target_power_mixed) {
            n <- n + step_size
          }
          setProgress(value = power / target_power_mixed)
        }
      })
      
      reference <- "Hsieh, F. Y., Bloch, D. A., & Larsen, M. D. (1998). A simple method of sample size calculation for linear and logistic regression. Statistics in medicine, 17(14), 1623-1634. \n Bosker, R., & Snijders, T. A. (2011). Multilevel analysis: An introduction to basic and advanced multilevel modeling. Multilevel analysis, 1-368."
    }
    
    output$result <- renderUI({
      if (!is.null(n)) {
        tags$table(
          class = "table shiny-table",
          tags$tr(
            tags$td(style = "font-weight: bold;", "Tamaño de muestra necesario:"),
            tags$td(n)
          ),
          tags$tr(
            tags$td(style = "font-weight: bold;", "Referencia(s):"),
            tags$td(HTML(paste("<ul><li>", gsub("\n", "</li><li>", reference), "</li></ul>")))
          )
        )
        
        
      } else {
        tags$div("Error: no se pudo calcular el tamaño de muestra.")
      }
    })
    
  })
}

# Ejecutamos la aplicación
shinyApp(ui = ui, server = server)