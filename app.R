library(shiny)
library(tidyverse)
library(ggplot2)
library(patchwork)


ui <- fluidPage(
    tags$head(
        tags$style(HTML("
            .main-container {
                display: flex;
                flex-direction: row;
                height: 800px;
            }
            .sidebar-column {
                width: 25%;
                display: flex;
                align-items: center;
                justify-content: center;
                padding: 10px;
            }
            .sidebar-panel {
                width: 100%;
                padding: 15px;
                background-color: #f8f9fa;
                border-radius: 5px;
            }
            .plot-column {
                width: 75%;
                display: flex;
                flex-direction: column;
            }
            .plot1 {
                width: 90%;
                height: 400px;
            }
            .plot2 {
                flex: 1;
                width: 90%;
            }
        "))
    ),
    
    tags$h1("Dice Roll Simulation", style = "text-align: center;"),
    
    div(class = "main-container",
        div(class = "sidebar-column",
            div(class = "sidebar-panel",
                p("This simulation shows that as the number of dice rolls (Trials) 
                  increases, the average outcome (Mean Value) converges toward the expected 
                  value of 3.5. This result illustrates the law of large numbers, 
                  which predicts that with a sufficiently large sample size, the 
                  empirical mean will approach the theoretical mean."),
                numericInput("min_trials", "Minimum Trials:", value = 100, min = 1),
                numericInput("max_trials", "Maximum Trials:", value = 1000, min = 1),
                numericInput("step_trials", "Step Size:", value = 100, min = 1),
                sliderInput("num_trials",
                            "Number of Trials:",
                            min = 100,
                            max = 1000,
                            value = 500,
                            step = 100),
                actionButton("run", "Run Simulation")
            )
        ),
        div(class = "plot-column",
            div(class = "plot1", plotOutput("plot1", width = "100%", height = "100%")),
            div(class = "plot2", plotOutput("plot2", width = "100%", height = "70%"))
        )
    )
)


server <- function(input, output, session) {
    sliderParams <- reactiveValues(min = 100, max = 1000, step = 100, value = 500)
    observe({
        newMin <- max(1, input$min_trials)
        newMax <- max(newMin, input$max_trials)
        newStep <- max(1, min(input$step_trials, newMax - newMin))
        
        if (newMin != sliderParams$min || newMax != sliderParams$max || newStep != sliderParams$step) {
            sliderParams$min <- newMin
            sliderParams$max <- newMax
            sliderParams$step <- newStep
            sliderParams$value <- min(max(sliderParams$value, newMin), newMax)
            
            updateSliderInput(
                session, "num_trials",
                min = newMin,
                max = newMax,
                step = newStep,
                value = sliderParams$value
                )
        }
    })
    
    sim_data <- eventReactive(input$run, {
        trials <- tibble(
            roll = sample(1:6, input$num_trials, replace = TRUE),
            trial_num = 1:input$num_trials) |>
            mutate(cumulative_avg = cumsum(roll) / trial_num)
        })
    
    expected_val <- (1/6) * sum(c(1:6))
    
    output$plot1 <- renderPlot({
        trials <- sim_data()
        
        trials |> 
            ggplot(aes(x = trial_num, y = cumulative_avg)) +
            geom_hline(yintercept = 3.5, color = "darkgreen") +
            geom_ribbon(aes(ymin = 3.5, ymax = cumulative_avg), fill = "red", alpha = 0.3) +
            geom_line(color = "red") +
            labs(
                title = "Average Dice Value Against Number of Rolls",
                x = "Trials",
                y = "Mean Value") +
            scale_x_continuous(limits = c(0, max(trials$trial_num))) +
            scale_y_continuous(limits = c(1, 6)) +
            theme_light(base_size = 16) +
            theme(plot.title = element_text(hjust = 0.5, margin = margin(b = 20))) +
            annotate(
                "text", 
                x = max(trials$trial_num), 
                y = 3.5, 
                label = "Expected value = 3.5", 
                vjust = -0.5, 
                hjust = 1,
                size = 5,
                color = "darkgreen"
            )
        })
    
    output$plot2 <- renderPlot({
        trials <- sim_data()
        
        # Distribution plot
        dist_plot <- trials |>
            count(roll) |>
            mutate(proportion = n / sum(n)) |>
            ggplot(aes(x = factor(roll), y = proportion)) +
            geom_col(fill = "skyblue", color = "black") +
            geom_text(aes(label = scales::percent(proportion, accuracy = 0.1)), vjust = -0.5) +
            labs(
                title = paste("Distribution of Dice Rolls:", input$num_trials, "Trials"),
                x = "Roll Value",
                y = "Proportion") +
            theme_light(base_size = 14) +
            theme(plot.title = element_text(hjust = 0.5, margin = margin(0, 0, 20, 0))) +
            scale_y_continuous(labels = scales::percent, limits = c(0, 0.3))
        
        # Gaussian-like distribution plot of running averages
        gaussian_plot <- trials |>
            ggplot(aes(x = cumulative_avg)) +
            geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", color = "black") +
            geom_density(color = "red", fill = "red", size = 1, alpha = 2/10) +
            labs(
                title = "Distribution of Running Averages",
                x = "Running Average",
                y = "Density") +
            theme_light(base_size = 14) +
            theme(plot.title = element_text(hjust = 0.5, margin = margin(0, 0, 20, 0))) +
            geom_vline(xintercept = expected_val, color = "darkgreen", linetype = "dotdash", size = 1) +
            annotate(
                "text", 
                x = expected_val, 
                y = Inf, 
                label = "Expected Value", 
                vjust = 1.5, 
                hjust = 1.5,
                size = 4,
                angle = 90,
                color = "darkgreen")
        
        dist_plot + gaussian_plot +
            plot_layout(ncol = 2) +
            plot_annotation(
                #title = "Dice Roll Distribution Analysis",
                theme = theme(plot.title = element_text(hjust = 0.5, size = 18))
            )
        })
}

shinyApp(ui = ui, server = server)







