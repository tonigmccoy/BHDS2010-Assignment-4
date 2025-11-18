# app.R

# load in libraries
library(shiny)
library(ggplot2)
library(DT)
library(bslib)


# Load the HERS Dataset
# Use the first line for local RStudio (requires CSV saved locally)
# Use the second line for RStudio Cloud
hers <- read.csv("~/BHDS 2010/FinalProject/hersdatamod.csv", check.names = FALSE)
# hers <- read.csv("hersdatamod.csv", check.names = FALSE)

# Define variable groups used in the app
# These lists control what variables to show & convert

# Numeric Variables
numeric_vars <- c("Age", 
                  "1 Year LDL Change (mg/ dL)",
                  "1 Year BMI Change (kg/ m²)",
                  "1 Year Weight Change (kg)",
                  "1 Year Glucose Change (mg/ dL)",
                  "1 Year Waist Circumference Change (cm)")

# Categorical Variables
categ_vars <- c("Treatment", "Smoker (Yes/ No)")


# Combined set (used for table display)
all_app_vars <- c(numeric_vars, categ_vars)


# Drop the first row of the dataset (because row 1 = labels)
dataset <- hers[-1, ]

# Convert all variables listed as numeric into numeric type
# Only convert if the name actually exists in the dataset
for (v in numeric_vars) {
  if (v %in% names(dataset)) {
    dataset[[v]] <- as.numeric(dataset[[v]])
  }
}

# Define UI theme using Bootswatch "flatly"
my_theme <- bs_theme(bootswatch = "flatly")

# User Interface
ui <- page_fillable(
  theme = my_theme,
  
  navset_card_tab(
    id = "tabset",
    
    # Tab 1: Data description + preview
    nav_panel(
      "Data description",
      h3("HERS dataset"),
      verbatimTextOutput("data_description"),
      br(),
      h4("Data preview"),
      DTOutput("data_table")
    ),
    
    
    # Tab 2: Biological graphs
    # User selects predefined plots
    nav_panel(
      "Graphs",
      layout_sidebar(
        
        # Sidebar allows user to pick which plot to view
        sidebar = sidebar(
          title = "Example plots",
          
          
          radioButtons(
            "plot_choice",
            "Choose a plot to explore:",
            choices = c(
              "Distribution of Age" = "age_dist",
              "Distribution of LDL change" = "ldl_dist",
              "BMI change vs Weight change by Treatment" = "bmi_weight_scatter",
              "Glucose change vs Waist change by Treatment" = "glucose_waist_scatter",
              "LDL change by Treatment group" = "ldl_treat_box",
              "Glucose change by Smoking status" = "glucose_smoke_box"
            ),
            selected = "age_dist"
          )
        ),
        # The main plot area
        plotOutput("main_plot", height = "450px")
      )
    )
  )
)

# Server Logic 
server <- function(input, output, session) {
  
  # Test Description of the dataset 
  output$data_description <- renderText({
    paste(
      "HERS is a randomized, blinded, placebo-controlled trial",
      "of estrogen plus progestin for secondary prevention of CHD",
      "in postmenopausal women with existing heart disease.",
      "",
      "In this app we focus on a subset of 10 numeric and 10 categorical variables",
      "related to cardiometabolic risk factors and health behaviors.",
      sep = "\n"
    )
  })
 
# Data preview (first 20 rows of selected variables) 
  output$data_table <- renderDT({
    # only display variables that exist within the dataset
    keep_vars <- intersect(all_app_vars, names(dataset))
    
    datatable(
      dataset[, keep_vars, drop = FALSE],  # show all rows
      options = list(pageLength = 10)      # but only 10 per page with pagination
    )
  })
  
  # output$summ_title <- renderText({
  #   paste("Summary for:", selected_var())
  # })
  # 
  # output$summ_note <- renderUI({
  #   if (input$summ_type == "num" && selected_var() %in% numeric_vars) {
  #     tags$p("Numeric summary (N, mean, SD, quartiles).")
  #   } else if (input$summ_type == "num") {
  #     tags$p("This variable is not numeric, so a categorical table is shown instead.")
  #   } else {
  #     tags$p("Categorical summary (counts and percentages).")
  #   }
  # })
  # 
  # output$summ_table <- renderDT({
  #   var_name <- selected_var()
  #   
  #   if (!var_name %in% names(dataset)) {
  #     out <- data.frame(Message = "Variable not found in dataset.")
  #     return(datatable(out, options = list(dom = "t")))
  #   }
  #   
  #   x <- dataset[[var_name]]
  #   
  #   if (input$summ_type == "num" && var_name %in% numeric_vars) {
  #     x <- as.numeric(x)
  #     x <- x[!is.na(x)]
  #     
  #     if (length(x) == 0) {
  #       out <- data.frame(Message = "No non-missing numeric values.")
  #     } else {
  #       out <- data.frame(
  #         Statistic = c("N", "Mean", "SD", "Min", "Q1", "Median", "Q3", "Max"),
  #         Value = round(c(
  #           length(x),
  #           mean(x),
  #           sd(x),
  #           min(x),
  #           quantile(x, 0.25),
  #           median(x),
  #           quantile(x, 0.75),
  #           max(x)
  #         ), 2)
  #       )
  #     }
  #     
  #   } else {
  #     tab <- table(x, useNA = "ifany")
  #     if (length(tab) == 0) {
  #       out <- data.frame(Message = "No values to tabulate.")
  #     } else {
  #       prop <- round(100 * prop.table(tab), 1)
  #       out <- data.frame(
  #         Level = names(tab),
  #         Count = as.integer(tab),
  #         Percent = prop
  #       )
  #     }
  #   }
  #   
  #   datatable(
  #     out,
  #     rownames = FALSE,
  #     options = list(
  #       pageLength = 8,
  #       searching = FALSE,
  #       lengthChange = FALSE,
  #       dom = "t" # just the table
  #     )
  #   )
  # })
  
  # MAIN GRAPH OUTPUT
  # Preconfigured biological plots 
  output$main_plot <- renderPlot({
    
    choice <- input$plot_choice
    
    # sometimes names might not exist
    has <- function(v) v %in% names(dataset)
    
    # Histogram of Age 
    if (choice == "age_dist" && has("Age")) {
      
      ggplot(dataset, aes(x = Age)) +
        geom_histogram(aes(y = ..density..), bins = 30, na.rm = TRUE) +
        geom_density(na.rm = TRUE) +
        xlab("Age (years)") +
        ylab("Density") +
        ggtitle("Distribution of Age") +
        # Increase the size of the axis labels as well as the key
        # bold and center the title of teh graph 
        theme(
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 16),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          plot.title = element_text(size = 18, face = "bold", hjust = 0.5), # bold & centered
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12)
        )
      
      # Histogram of LDL Change 
    } else if (choice == "ldl_dist" && has("1 Year LDL Change (mg/ dL)")) {
      
      ggplot(dataset, aes(x = `1 Year LDL Change (mg/ dL)`)) +
        geom_histogram(aes(y = ..density..), bins = 30, na.rm = TRUE) +
        geom_density(na.rm = TRUE) +
        xlab("1 Year LDL Change (mg/dL)") +
        ylab("Density") +
        ggtitle("Distribution of 1 Year LDL Change") +
        theme(
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 16),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          plot.title = element_text(size = 18, face = "bold", hjust = 0.5), 
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12)
        )
      
      
    # BMI Change versus Weight Change, colored by Treatment 
    } else if (choice == "bmi_weight_scatter" &&
               has("1 Year BMI Change (kg/ m²)") &&
               has("1 Year Weight Change (kg)") &&
               has("Treatment")) {
      
      ggplot(dataset,
             aes(x = `1 Year BMI Change (kg/ m²)`,
                 y = `1 Year Weight Change (kg)`,
                 color = Treatment)) +
        geom_point(na.rm = TRUE, alpha = 0.7) +
        xlab("1 Year BMI Change (kg/m²)") +
        ylab("1 Year Weight Change (kg)") +
        ggtitle("BMI Change vs Weight Change by Treatment") +
        theme(
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 16),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          plot.title = element_text(size = 18, face = "bold", hjust = 0.5), # bold & centered
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12)
        )
      
    # Glucose Change vs Waist Change, colored by Treatment
    } else if (choice == "glucose_waist_scatter" &&
               has("1 Year Glucose Change (mg/ dL)") &&
               has("1 Year Waist Circumference Change (cm)") &&
               has("Treatment")) {
      
      ggplot(dataset,
             aes(x = `1 Year Glucose Change (mg/ dL)`,
                 y = `1 Year Waist Circumference Change (cm)`,
                 color = Treatment)) +
        geom_point(na.rm = TRUE, alpha = 0.7) +
        xlab("1 Year Glucose Change (mg/dL)") +
        ylab("1 Year Waist Circumference Change (cm)") +
        ggtitle("Glucose Change vs Waist Circumference Change by Treatment") +
        theme(
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 16),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          plot.title = element_text(size = 18, face = "bold", hjust = 0.5), 
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12)
        )
    
    # LDL Change by Treatment Group (boxplot)    
    } else if (choice == "ldl_treat_box" &&
               has("1 Year LDL Change (mg/ dL)") &&
               has("Treatment")) {
      
      ggplot(dataset,
             aes(x = Treatment,
                 y = `1 Year LDL Change (mg/ dL)`)) +
        geom_boxplot(na.rm = TRUE) +
        xlab("Treatment group") +
        ylab("1 Year LDL Change (mg/dL)") +
        ggtitle("LDL Change by Treatment Group") +
        theme(
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 16),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          plot.title = element_text(size = 18, face = "bold", hjust = 0.5), 
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12)
        )
    
    # Glucose Change by Smoking Status (boxplot)    
    } else if (choice == "glucose_smoke_box" &&
               has("1 Year Glucose Change (mg/ dL)") &&
               has("Smoker (Yes/ No)")) {
      
      ggplot(dataset,
             aes(x = `Smoker (Yes/ No)`,
                 y = `1 Year Glucose Change (mg/ dL)`)) +
        geom_boxplot(na.rm = TRUE) +
        xlab("Smoker (Yes/No)") +
        ylab("1 Year Glucose Change (mg/dL)") +
        ggtitle("Glucose Change by Smoking Status") +
        theme(
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 16),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          plot.title = element_text(size = 18, face = "bold", hjust = 0.5), 
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12)
        )
     
    # Fallback if variable names don't exist   
    } else {
      ggplot() +
        ggtitle("One or more variables for this example plot were not found in 
                the dataset.")
    }
  })
}

# Run the Shiny App 
shinyApp(ui = ui, server = server)
