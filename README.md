# shinyAPP_HemaProtRisk
Hematologic Malignancy Proteomic Risk Stratification

Tutorial for HemaProtRisk shiny app
HemaProtRisk is a computational R shiny platform integrating proteomics, genomics, and customized risk scoring. Through this app, you can view the effects of 2920 proteins on hematologic malignancies and their subtypes in proteomics and genomics levels. Additionally, you can upload your own proteomics data to compute risk scores of Tier 1, 2, and 3 proteins. You can directly access https://hemaprotrisk.shinyapps.io/shinyapp to use this platform. Alternatively, you can refer to the tutorial below for instructions on how to use this app.

Step 1. Download the R shiny app to your local PC
Users can download the HemaProtRisk app to their local computer for use via the following code.

git clone https://github.com/QiuLab1991/shinyAPP_HemaProtRisk
Note: Please remember the path of your working directory when you download. For example, I am currently executing the download command in C:/Users/QiuLab1991/Desktop, so the downloaded app are located in C:/Users/QiuLab1991/Desktop/shinyAPP_HemaProtRisk.

Step 2. Run HemaProtRisk app on your local PC
Open your local Rstudio software, and run the following R codes.

# install.packages("shiny")
library(shiny)
your_HemaProtRisk_path <- "path/to/your/downloaded/shinyAPP_HemaProtRisk"  # Setting to your own directory path as the above noted.
runApp(your_HemaProtRisk_path)
Note: If prompted to install certain required R packages while running this app, simply download and install them according to the respective package's installation instructions.

Now you can view or calculate risk score.
