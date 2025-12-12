library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("An interactive look at hypothesis testing"),
  
  sidebarLayout(
    sidebarPanel(
      p(strong("Sample Size (n): 1000")),
      
      sliderInput("se", 
                  "Standard Error:", 
                  min = 0.5, 
                  max = 5, 
                  value = 2, 
                  step = 0.1),
      
      sliderInput("estimate", 
                  "Observed Estimate:", 
                  min = -10, 
                  max = 10, 
                  value = 3, 
                  step = 0.5),
      
      sliderInput("conf_level", 
                  "Confidence Level:", 
                  min = 0.80, 
                  max = 0.99, 
                  value = 0.95, 
                  step = 0.01),
      
      hr(),
      
      h4("Statistics:"),
      textOutput("t_stat"),
      textOutput("ci_text"),
      textOutput("p_value")
    ),
    
    mainPanel(
      plotOutput("distPlot", height = "600px"),
      br(),
      h4("Interpretation:"),
      htmlOutput("interpretation")
    )
  )
)

server <- function(input, output) {
  
  calc_stats <- reactive({
    n <- 1000
    df <- n - 1
    t_stat <- input$estimate / input$se
    alpha <- 1 - input$conf_level
    t_crit <- qt(1 - alpha/2, df)
    ci_lower <- input$estimate - t_crit * input$se
    ci_upper <- input$estimate + t_crit * input$se
    p_val <- 2 * pt(-abs(t_stat), df)
    
    list(
      n = n,
      df = df,
      t_stat = t_stat,
      t_crit = t_crit,
      ci_lower = ci_lower,
      ci_upper = ci_upper,
      p_val = p_val,
      alpha = alpha
    )
  })
  
  output$distPlot <- renderPlot({
    stats <- calc_stats()
    
    x_min <- -15
    x_max <- 15
    x_range <- seq(x_min, x_max, length.out = 500)
    t_vals <- x_range / input$se
    y_vals <- dt(t_vals, df = stats$df) / input$se
    df_plot <- data.frame(x = x_range, y = y_vals)
    
    p <- ggplot(df_plot, aes(x = x, y = y)) +
      geom_line(size = 1.2, color = "black") +
      theme_minimal(base_size = 14) +
      labs(
        title = paste0("Null Distribution (estimate = ", input$estimate, ", SE = ", input$se, ", t = ", round(stats$t_stat, 2), ")"),
        x = "Estimate",
        y = ""
      ) +
      theme(
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        panel.grid.minor = element_blank()
      ) +
      coord_cartesian(xlim = c(x_min, x_max))
    
    crit_lower <- -stats$t_crit * input$se
    crit_upper <- stats$t_crit * input$se
    
    p <- p +
      geom_area(data = subset(df_plot, x <= crit_lower),
                aes(x = x, y = y),
                fill = "red", alpha = 0.3) +
      geom_area(data = subset(df_plot, x >= crit_upper),
                aes(x = x, y = y),
                fill = "red", alpha = 0.3)
    
    y_ci <- -max(y_vals) * 0.05
    p <- p +
      geom_segment(aes(x = stats$ci_lower, xend = stats$ci_upper, 
                       y = y_ci, yend = y_ci),
                   color = "blue", size = 3, linewidth = 3) +
      geom_point(aes(x = stats$ci_lower, y = y_ci), 
                 color = "blue", size = 5) +
      geom_point(aes(x = stats$ci_upper, y = y_ci), 
                 color = "blue", size = 5) +
      annotate("text", x = input$estimate, y = y_ci * 1.8, 
               label = paste0(input$conf_level * 100, "% CI"),
               color = "blue", size = 4, fontface = "bold")
    
    p <- p +
      geom_vline(xintercept = input$estimate, 
                 color = "darkgreen", size = 1.5, linetype = "dashed") +
      annotate("text", 
               x = input$estimate, 
               y = max(y_vals) * 0.85,
               label = paste0("Estimate = ", round(input$estimate, 2)),
               color = "darkgreen", 
               size = 5,
               angle = 90,
               vjust = -0.5,
               fontface = "bold")
    
    p <- p +
      geom_vline(xintercept = 0, 
                 color = "black", size = 1, linetype = "solid", alpha = 0.5) +
      annotate("text", x = 0, y = max(y_vals) * 0.5,
               label = "Null (H₀ = 0)",
               color = "black", size = 4, angle = 90, vjust = 1.2)
    
    p <- p +
      geom_vline(xintercept = c(crit_lower, crit_upper),
                 color = "red", linetype = "dotted", size = 1) +
      annotate("text", x = crit_upper, y = max(y_vals) * 0.7,
               label = paste0("Critical values:\n ±", round(stats$t_crit, 2), " SE"),
               color = "red", size = 3.5, hjust = -0.1)
    
    p
  })
  
  output$t_stat <- renderText({
    stats <- calc_stats()
    paste0("T-statistic: ", round(stats$t_stat, 3))
  })
  
  output$ci_text <- renderText({
    stats <- calc_stats()
    paste0(input$conf_level * 100, "% Confidence Interval: [", 
           round(stats$ci_lower, 2), ", ", 
           round(stats$ci_upper, 2), "]")
  })
  
  output$p_value <- renderText({
    stats <- calc_stats()
    if(stats$p_val < 0.001) {
      "P-value: <0.001"
    } else {
      paste0("P-value: ", round(stats$p_val, 4))
    }
  })
  
  output$interpretation <- renderText({
    stats <- calc_stats()
    
    is_significant <- stats$p_val < stats$alpha
    sig_text <- if(is_significant) {
      "<span style='color: purple; font-weight: bold;'>SIGNIFICANT</span>"
    } else {
      "<span style='color: black; font-weight: bold;'>NOT SIGNIFICANT</span>"
    }
    
    null_in_ci <- stats$ci_lower <= 0 && stats$ci_upper >= 0
    ci_text <- if(null_in_ci) {
      "<span style='color: blue; font-weight: bold;'>is</span> contained in the confidence interval"
    } else {
      "<span style='color: blue; font-weight: bold;'>is NOT</span> contained in the confidence interval"
    }
    
    p_comparison <- if(is_significant) {
      paste0("<span style='font-weight: bold;'>less than</span> ", round(stats$alpha, 3))
    } else {
      paste0("<span style='font-weight: bold;'>greater than</span> ", round(stats$alpha, 3))
    }
    
    t_comparison <- if(abs(stats$t_stat) > stats$t_crit) {
      paste0("<span style='color: red; font-weight: bold;'>greater than</span> ", round(stats$t_crit, 2))
    } else {
      paste0("<span style='color: red; font-weight: bold;'>less than</span> ", round(stats$t_crit, 2))
    }
    
    paste0(
      "<p><strong>Context:</strong> Remember that the black line shows us the \"null distribution,\" or the results that we would expect to get if we re-ran the analysis over and over and over again in a world where the true underlying difference was 0.</p>",
      
      "<p><strong style='color: red;'>t-statistic:</strong> Remember that the <span style='color: red;'>t-statistic</span> tells us how big our <span style='color: darkgreen;'>estimate</span> is, in terms of the standard error. Colloquially, we could think of it as answering the question 'How many standard errors away from zero is my estimate?' Mathematically, it is defined as <span style='color: darkgreen;'>estimate</span> / SE. For a test with ", input$conf_level * 100, " percent confidence, we know that the \"<span style='color: red;'>critical value</span>\" is ", round(stats$t_crit, 2), "; that is, any <span style='color: red;'>t-statistic</span> whose absolute value is greater than ", round(stats$t_crit, 2), " will be statistically significant.</p>",
      
      "<p><strong>Effect of Standard Error:</strong> A larger SE makes the distribution wider. This is because, holding everything else constant, we expect more noise / less precision in our <span style='color: darkgreen;'>estimate</span>. If you adjust this, you will see the distribution literally spread out! This also means that the range of potential true differences (our <span style='color: blue;'><strong>confidence interval</strong></span>) will increase as SE increases and vice versa.</p>",
      
      "<p><strong>Current Result:</strong> The observed <span style='color: darkgreen;'>estimate</span> is ", sig_text, " at the ", input$conf_level * 100, "% confidence level. We know this because:</p>",
      "<ul>",
      "<li>The null hypothesis value (0) ", ci_text, "</li>",
      "<li>The p-value is ", p_comparison, "</li>",
      "<li>The <span style='color: red;'>t-statistic</span> (<span style='color: red;'><strong>", round(stats$t_stat, 2), "</strong></span>) has an absolute value ", t_comparison, "</li>",
      "</ul>"
    )
  })
}

shinyApp(ui = ui, server = server)