zipcode_data = read.table(file = "zipcode.txt", header = TRUE, sep = "");
zipcode_set = sort(zipcode_data$zipcode);

shinyUI(bootstrapPage(

  headerPanel("", windowTitle = "PM2.5 Prediction"),
  
  sidebarPanel(
  selectInput(inputId = "n_zipcode",
      label = "Beijing zipcode:",
      #choices = c(100101, 100042, 100046, 100076, 101406, 101205),
      choices = zipcode_set,
      selected = zipcode_set[1]),

  checkboxInput(inputId = "individual_obs",
      label = strong("Show observed data"),
      value = FALSE),

  checkboxInput(inputId = "whole_area",
      label = strong("Show whole-area prediction"),
      value = FALSE),

  checkboxInput(inputId = "map_area",
      label = strong("Show prediction on map"),
      value = FALSE)
  ), 

  mainPanel(
   plotOutput(outputId = "main_plot", width = "600px", height = "600px")
  ),

  # Display this only if the whole-area is shown
  conditionalPanel(
     condition = "input.whole_area == true",
     sidebarPanel(
         sliderInput(inputId = "rs_adjust",
                     label = "Resolution adjustment:",
                     min = 10, max = 30, value = 20, step = 2)
                  )
                 )

))

