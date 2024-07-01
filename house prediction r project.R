library(shiny)

# Define the UI layout
ui <- fluidPage(
  headerPanel("Rocknar"),
  sidebarPanel(
    textInput("user", "Username"),
    passwordInput("password", "Password"),
    actionButton("login", "Login"),
    uiOutput("inputs")
  ),
  mainPanel(
    sidebarPanel(width = 25,
                 headerPanel("THE PRICE OF A HOUSE IS:- "),
                 textOutput("value"),
                 textOutput("r_squared"),
                 textOutput("mse"),
                 textOutput("rmse"),
                 plotOutput("pricePlot")
    )
  )
)

# Define the server function
server <- function(input, output, session) {
  # Define credentials
  valid_credentials <- reactiveValues(
    Tejas = "sonar",
    user2 = "password2"
  )
  
  # Authentication
  observeEvent(input$login, {
    if (input$user %in% names(valid_credentials) && input$password == valid_credentials[[input$user]]) {
      output$inputs <- renderUI({
        fluidRow(
          selectInput("area", "Choose the area", list("Built-up Area 1", "Super Built-up Area 4", "Plot Area 3", "Carpet Area 2")),
          textInput("area_id", "Enter the area ID", ""),
          textInput("location_id", "Enter the location pin-code", ""),
          textInput("bhk", "How many BHK flat do you want?", ""),
          textInput("sqft", "Enter the total square feet", ""),
          textInput("bath", "How many bathrooms do you want?", ""),
          textInput("balcony", "How many balconies do you want?", ""),
          actionButton('go', "Predict")
        )
      })
    } else {
      showModal(
        modalDialog(
          title = "Authentication failed",
          "Invalid username or password. Please try again.",
          easyClose = TRUE
        )
      )
    }
  })
  
  data2 <- reactiveValues()
  observeEvent(input$go, {
    data <- read.csv("C:/Users/dell/Desktop/data.csv")
    
    # Data preprocessing
    data$area_type <- as.factor(data$area_type)
    data$location <- as.factor(data$location)
    
    use_data <- data[, c("area_type", "location", "size", "total_sqft", "bath", "balcony", "price")]
    na_clean_data <- na.omit(use_data)
    
    area.type <- as.numeric(as.factor(na_clean_data$area_type))
    data.location <- as.numeric(as.factor(na_clean_data$location))
    
    second_final <- cbind(na_clean_data, area_id = area.type)
    second_main_dataset <- cbind(second_final, location_id = data.location)
    
    input_data <- second_main_dataset[, c("area_id", "location_id", "size", "total_sqft", "bath", "balcony", "price")]
    
    # Removing outliers
    threshold <- 2  # Define a z-score threshold
    input_data <- input_data[abs(scale(input_data$price)) < threshold, ]
    
    # Capture user inputs
    data2$myarea_id <- as.numeric(input$area_id)
    data2$mylocation_id <- as.numeric(input$location_id)
    data2$mybhk <- as.numeric(input$bhk)
    data2$mysqft <- as.numeric(input$sqft)
    data2$mybath <- as.numeric(input$bath)
    data2$mybalcony <- as.numeric(input$balcony)
    
    # Calculate mean and mode values based on user inputs
    mean_val <- mean(input_data$total_sqft, na.rm = TRUE)
    mode_val <- as.numeric(names(sort(table(input_data$size), decreasing = TRUE)[1]))
    
    # Prepare new data for prediction
    new_predict <- data.frame(area_id = data2$myarea_id, location_id = data2$mylocation_id,
                              size = data2$mybhk, total_sqft = data2$mysqft,
                              bath = data2$mybath, balcony = data2$mybalcony,
                              mean_val = mean_val, mode_val = mode_val)
    
    # Train the linear regression model
    input_data$mean_val <- mean(input_data$total_sqft, na.rm = TRUE)
    input_data$mode_val <- as.numeric(names(sort(table(input_data$size), decreasing = TRUE)[1]))
    
    model <- lm(price ~ area_id + location_id + size + total_sqft + bath + balcony + mean_val + mode_val,
                data = input_data, weights = 1 / input_data$price ^ 1.9)
    
    # Make predictions and store the result
    data2$op <- predict(model, new_predict)
    
    # Calculate performance metrics
    predictions <- predict(model, input_data)
    actuals <- input_data$price
    data2$r_squared <- summary(model)$r.squared
    data2$mse <- mean((actuals - predictions)^2)
    data2$rmse <- sqrt(data2$mse)
    
    # Render the predicted price
    output$value <- renderPrint({ data2$op })
    
    # Render the performance metrics
    output$r_squared <- renderText({ paste("R-squared:", round(data2$r_squared, 4)) })
    output$mse <- renderText({ paste("Mean Squared Error (MSE):", round(data2$mse, 4)) })
    output$rmse <- renderText({ paste("Root Mean Squared Error (RMSE):", round(data2$rmse, 4)) })
    
    # Render the scatter plot
    output$pricePlot <- renderPlot({
      plot(input_data$price, predictions, 
           xlab = "Actual Price", ylab = "Predicted Price",
           main = "Actual vs Predicted Prices",
           col = "blue", pch = 19)
      abline(0, 1, col = "red")  # Add a diagonal line for reference
    })
  })
}

# Run the Shiny app


shinyApp(ui, server)
