# shinyAPP_HemaProtRisk
Hematologic Malignancy Proteomic Risk Stratification

## Overview
HemaProtRisk is a computational R Shiny platform integrating proteomics, genomics, and customized risk scoring. Through this app, you can:
- View the effects of 2920 proteins on hematologic malignancies and their subtypes (proteomics & genomics levels)
- Upload your own proteomics data to compute risk scores for Tier 1, 2, and 3 proteins

### Quick Access
You can directly use the platform online:  
👉 https://hemaprotrisk.shinyapps.io/shinyapp

## Tutorial for HemaProtRisk Shiny App
Follow the steps below to use the app locally on your computer.

# Step 1. Download the R shiny app to your local PC
Users can download the HemaProtRisk app to their local computer for use via the following code.
git clone https://github.com/QiuLab1991/shinyAPP_HemaProtRisk
Note: Please remember the path of your working directory when you download. For example, I am currently executing the download command in C:/Users/QiuLab1991/Desktop, so the downloaded app are located in C:/Users/QiuLab1991/Desktop/shinyAPP_HemaProtRisk.

# Step 2. Run HemaProtRisk app on your local PC
Open your local Rstudio software, and run the following R codes.
install.packages("shiny")
library(shiny)
your_HemaProtRisk_path <- "path/to/your/downloaded/shinyAPP_HemaProtRisk"  # Setting to your own directory path as the above noted.
runApp(your_HemaProtRisk_path)
Note: If prompted to install certain required R packages while running this app, simply download and install them according to the respective package's installation instructions.

Now you can view or calculate risk score.

