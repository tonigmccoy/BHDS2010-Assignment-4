# HERS Data Exploration Shiny App

**Project Overview**

This Shiny app allows users to explore the Heart and Estrogen/Progestin Replacement Study (HERS) dataset. HERS was a randomized, blinded, placebo-controlled clinical trial that studied the effect of estrogen plus progestin on cardiovascular outcomes in postmenopausal women with existing heart disease.

The app provides an interactive interface for examining numeric and categorical variables, visualizing relationships between variables, and gaining insights from the clinical trial data.

The purpose of the app is to provide a hands-on tool for exploratory data analysis, visualization, and understanding treatment effects in a clinical trial dataset.

**Dataset**

The dataset includes:
 * **Numeric variables:** Age, 1 Year LDL Change, 1 Year BMI Change, 1 Year Weight Change, 1 Year Glucose Change, 1 Year Waist Circumference Change.
 * **Categorical variables:** Treatment group, Smoker (Yes/No).

This dataset is relevant for practicing data visualization, exploratory analysis, and understanding relationships between clinical variables, which aligns with key concepts in data analytics and applied statistics.

**Key Features**
<u> User Inputs </u>
 * **Plot Selection**: Users select from six pre-defined plots using radio buttons:
    * Distribution of Age
    * Distribution of LDL Change
    * BMI Change vs Weight Change by Treatment
    * Glucose Change vs Waist Circumference Change by Treatment
    * LDL Change by Treatment Group
    * Glucose Change by Smoking Status
<u> Outputs </u>
 * **Data Description Tab:**
    * Text description of the HERS study.
    * Table preview of selected numeric and categorical variables.
 * **Graphs Tab:**
    * Displays the selected plot.
    * Includes histograms, scatter plots, and boxplots.
    * Plots have larger axis labels, legend text, and centered bold titles for readability.

**Code**

The app is written entirely in R using the following packages:
 * shiny – For building the interactive web application.
 * ggplot2 – For creating all plots and visualizations.
 * DT – For rendering interactive tables.
 * bslib – For customizing the app theme.

The code is structured into three main sections:
1. Data Loading and Preprocessing
    * Loads the HERS dataset from a CSV file.
    * Converts numeric columns to numeric type.
    * Stores numeric and categorical variables in separate lists for use throughout the app.
2.  User Interface (UI)
    * Built with page_fillable() and navset_card_tab().
    * Contains two main tabs: Data Description and Graphs.
    * Uses the “flatly” Bootswatch theme.
3. Server Logic
    * Handles all reactive outputs for tables and plots.
    * Dynamically generates plots based on the user’s selection.
    * Checks for variable existence before plotting.
    * Enhances plots with larger labels, legends, and bold, centered titles.

**Server Logic Details**
 * **Reactive Tables:**  Shows selected variables with a preview of the first 20 rows.
 * **Reactive Plots:** Generates histograms, density plots, scatter plots, and boxplots based on user input.
 * **Data Validation:** Ensures selected variables exist in the dataset before plotting to prevent errors.

**Examples of Insights**
Using this app, users can:
 * Examine distributions of age, LDL change, and other numeric variables.
 * Compare changes in BMI, weight, glucose, or LDL between treatment groups.
 * Explore relationships between numeric variables such as BMI change vs Weight change, or Glucose change vs Waist circumference change.
 * Compare outcomes for subgroups, such as smokers vs non-smokers.
 * Identify missing or unusual values in the dataset.
