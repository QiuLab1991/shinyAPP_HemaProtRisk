#rsconnect::deployApp(appDir = "~/wcz111/bloodcancer/shinyAPP.HemaProtRisk",appFiles=c("app.R", "data/","www/"))
library(shiny)
library(data.table)
library(dplyr)
library(DT)
library(ggplot2)
library(forestplot)
library(grid)
ui <- fluidPage(

  ## ================= CSS =================
  tags$head(
    tags$style(HTML("
      body {
        background-color: #f8f9fa;
        font-family: 'Helvetica Neue', Arial, sans-serif;
      }

      /* ===== Top Header (NOT navbarPage) ===== */
      .top-header {
        background-color: #003366;
        height: 90px;
        padding: 0 25px;
        display: flex;
        align-items: center;
        box-shadow: 0 2px 6px rgba(0,0,0,0.15);
      }

      .top-header-title {
        color: #ffffff;
        font-size: 20px;
        font-weight: 700;
      }

      /* ===== Sidebar ===== */
      .sidebar-box {
        background-color: #ffffff;
        border-radius: 12px;
        border: none;
        padding: 20px 15px;
        box-shadow: 0 2px 6px rgba(0,0,0,0.08);
        height: calc(100vh - 130px);
      }

      /* ===== Sidebar Buttons ===== */
      .sidebar-btn {
        width: 100%;
        text-align: left !important;
        padding: 18px 16px;
        margin-bottom: 12px;
        background-color: #f1f4f8;
        color: #003366;
        border: none;
        font-size: 16px;
        font-weight: 600;
        border-radius: 8px;
      }

      .sidebar-btn:hover {
        background-color: #003366;
        color: #ffffff;
      }

      .sidebar-btn:focus {
        outline: none;
        box-shadow: none;
      }

      /* ===== Card ===== */
      .card {
        background: #ffffff;
        border-radius: 12px;
        box-shadow: 0 2px 6px rgba(0,0,0,0.1);
        padding: 20px;
        margin-bottom: 20px;
      }

      /* ===== Tabs ===== */
      .nav-tabs > li > a {
        color: #003366;
        font-weight: 600;
      }

      .nav-tabs > li.active > a {
        border-bottom: 3px solid #00b894;
        color: #00b894;
        background-color: #ffffff;
      }

      /* ===== Buttons ===== */
      .btn-primary {
        background-color: #0066cc !important;
        border-color: #0066cc !important;
      }

      .btn-success {
        background-color: #00b894 !important;
        border-color: #00b894 !important;
      }

      .btn-success:hover {
        background-color: #019870 !important;
      }

      /* ===== DataTable ===== */
      table.dataTable thead {
        background-color: #eef2f6;
        color: #003366;
      }
    "))
  ),

  ## ================= Top Header =================
  div(
    class = "top-header",
    div(
      class = "top-header-title",
      "Hematologic Malignancy Proteomic Risk Stratification"
    )
  ),

  ## ================= Main Layout =================
  fluidRow(

    ## ---------- Sidebar ----------
    column(
      width = 2,
      div(
        class = "sidebar-box",

      actionButton("nav_home",  "🏠 Home", class = "btn-default sidebar-btn"),
      actionButton("nav_prot",  "🧪 Proteomics Research", class = "btn-default sidebar-btn"),
      actionButton("nav_geno",  "🧬 Genomics Research", class = "btn-default sidebar-btn"),
      actionButton("nav_score", "📊 Customized Risk Score", class = "btn-default sidebar-btn")
      )
    ),

    ## ---------- Main Panel ----------
    column(
      width = 10,

      ## ===== Home =====
      conditionalPanel(
        condition = "output.current_module == 'home'",
        div(
          class = "card",
          h3("Welcome"),
          p("This platform integrates proteomics, genomics, and customized risk scoring.")
        )
      ),

      ## ===== Proteomics =====
      conditionalPanel(
        condition = "output.current_module == 'prot'",
        div(
          class = "card",
          h3("Proteomics Research"),

          sidebarLayout(
            sidebarPanel(
              width = 3,

              selectizeInput(
                "selected_Protein",
                "Select Protein(s):",
                choices = NULL,
                multiple = TRUE
              ),

              selectizeInput(
                "selected_model",
                "Select Model(s):",
                choices = NULL,
                multiple = TRUE
              ),

              actionButton("update", "Apply", class = "btn-primary"),
              br(), br(),

              downloadButton("download_forest", "Download Forest Plot")
            ),

            mainPanel(
              tabsetPanel(
                tabPanel("Table", DT::DTOutput("result_table")),
                tabPanel("Forest Plot", plotOutput("forest_plot", height = "700px"))
              )
            )
          )
        )
      ),

      ## ===== Genomics =====
      conditionalPanel(
        condition = "output.current_module == 'geno'",
        div(
          class = "card",
          h3("Genomics Research"),

          sidebarLayout(
            sidebarPanel(
              width = 3,

              selectizeInput(
                "geno_selected_protein",
                "Select Protein(s):",
                choices = NULL,
                multiple = TRUE
              ),

              selectizeInput(
                "geno_selected_pQTL_source",
                "Select pQTL_source(s):",
                choices = NULL,
                multiple = TRUE
              ),

              actionButton("geno_update", "Apply", class = "btn-primary"),
              br(), br(),

              downloadButton("geno_download_forest", "Download Forest Plot")
            ),

            mainPanel(
              tabsetPanel(
                tabPanel("Table", DT::DTOutput("geno_result_table")),
                tabPanel("Forest Plot", plotOutput("geno_forest_plot", height = "700px"))
              )
            )
          )
        )
      ),

      ## ===== Risk Score =====
      conditionalPanel(
        condition = "output.current_module == 'score'",
        div(
          class = "card",
          h3("Customized Risk Score"),

          strong("Reference: Hematologic Malignancy Risk Score Distribution in UK Biobank"),
          br(), br(),
          pre(
"Tier 1:
Min.  1st   Median   Mean   3rd   Max.
-1.68 -0.10  0.33    0.48   0.90   4.98

Tier 1+2:
-1.98 -0.09  0.42    0.55   1.01   5.13

Tier 1+2+3:
-1.63  0.00  0.59    0.76   1.38   6.15"
          ),
          br(),

          fileInput(
            "expr_file",
            "Upload expression matrix (rows = proteins, columns = samples)",
            accept = c(".csv", ".txt")
          ),

          actionButton("calc_score", "Calculate Risk Score", class = "btn-success"),
          br(), br(),

          DT::DTOutput("score_table"),
          br(),

          downloadButton("download_score", "Download Score Matrix")
        )
      )
    )
  )
)



server <- function(input, output, session) {

  current_module <- reactiveVal("home")

  observeEvent(input$nav_home,  current_module("home"))
  observeEvent(input$nav_prot,  current_module("prot"))
  observeEvent(input$nav_geno,  current_module("geno"))
  observeEvent(input$nav_score, current_module("score"))

  output$current_module <- reactive({
    current_module()
  })
  outputOptions(output, "current_module", suspendWhenHidden = FALSE)


  dfm <- reactive({
    path <- "./data/prot_result.csv"
    validate(need(file.exists(path), "Data file not found"))
    fread(path)
  })
  
  observe({
    updateSelectizeInput(
      session, "selected_Protein",
      choices = sort(unique(dfm()$Protein))
    )
    updateSelectizeInput(
      session, "selected_model",
      choices = sort(unique(dfm()$Model))
    )
  })

  selected_result <- eventReactive(input$update, {
    df <- dfm()
    
    req(length(input$selected_Protein) > 0)

df <- df %>% filter(Protein %in% input$selected_Protein)

    
    if (length(input$selected_model) > 0) {
      df <- df %>% filter(Model %in% input$selected_model)
    }
    
    df %>% arrange(desc(FDR))
  })


  output$result_table <- renderDT({
    datatable(
      selected_result() %>%
        transmute(
          Protein = Protein,
          Model = Model,
          `HR_95CI` = `HR_95CI`,
		  'P Value'=P,
          FDR =FDR
        ),
      rownames = FALSE,
      options = list(pageLength = 10, autoWidth = TRUE)
    )
  })

forest_plot_fun <- reactive({
  df <- selected_result()
  forest_data <- data.frame(
    Protein = df$Protein,
    Model   = df$Model,
    HR      = df$HR,
    lower   = df$lower,
    upper   = df$upper,
	`HR_95CI`=df$`HR_95CI`,
    P=df$P,
	FDR=df$FDR,
    stringsAsFactors = FALSE
  )
  
    a_df <- as.data.frame(matrix(colnames(forest_data),   nrow = 1))
    colnames(a_df) <- colnames(forest_data)
    forest_data <- rbind(a_df, forest_data)
    forest_data[1, 3:5] <- ""
    forest_data[, 3:5] <- lapply(forest_data[, 3:5], as.numeric)
    
    n_rows <- nrow(forest_data)
    hrzl_lines <- list(
      "1" = gpar(lty = 1, lwd = 2),
      "2" = gpar(lty = 1, lwd = 2)
    )
    hrzl_lines[[as.character(n_rows+1)]] <- gpar(lty = 1, lwd = 2)
    if(n_rows > 3) {
      for(i in 3:(n_rows)) {
        hrzl_lines[[as.character(i)]] <- gpar(lty = 2, col = "#00000040")
      }
    }
    valid_lower <- forest_data[-1, "HR"]
    valid_upper <- forest_data[-1, "HR"]
    min_tick <- floor(min(valid_lower, na.rm = TRUE) * 2) / 2
    max_tick <- ceiling(max(valid_upper, na.rm = TRUE) * 2) / 2
    xticks <- seq(from = min_tick, to = max_tick, by = 0.5)
    
    fp <- forestplot(
      forest_data[, c(1, 2, 6:8)],
      mean = forest_data[, 3],
      lower = forest_data[, 4],
      upper = forest_data[, 5],
      zero = 1,
      boxsize = 0.1,
      graph.pos   = 3,
      graphwidth = unit(.1, "npc"),
      xlab = "Hazard ratio and 95% CI",
      title = paste("Forest Plot"),
      xticks = xticks,
      clip = c(min_tick - 0.3, max_tick + 0.3),
      fn.ci_norm   = fpDrawNormalCI,
      lty.ci   = 1,
      lwd.ci   = 2,
      vertices = FALSE,
      is.summary   = c(TRUE, replicate(n = nrow(forest_data) - 1, FALSE)),
      hrzl_lines = hrzl_lines,
      col = fpColors(
        box = "orange",
        lines = "orange"
      ),
      txt_gp = fpTxtGp(
        label = gpar(
          fontfamily = "serif",
          fontface = "plain",
          cex = 1 
        ),
        ticks = gpar(
          fontfamily = "serif",
          fontface = "plain",
          cex = 0.8 
        ),
        xlab = gpar(
          fontfamily = "serif",
          fontface = "plain",
          cex = 1.1 
        ),
        title = gpar(
          fontfamily = "serif",
          fontface = "bold",
          cex = 1.5 
        )
      )
    )
    
    return(fp)
  })
  
  output$forest_plot <- renderPlot({
    forest_plot_fun()
  })
  
  output$download_forest <- downloadHandler(
    filename = function() {
      paste0("Forest_", input$selected_Protein, ".pdf")
    },
    content = function(file) {
      ggsave(file, forest_plot_fun(), width = 8, height = 6)
    }
  )

# =========================
# Genomics data
# =========================

geno_dfm <- reactive({
  path <- "./data/geno_result.csv"
  validate(need(file.exists(path), "Genomics data file not found"))
  data.table::fread(path)
})

# =========================
# Update select inputs
# =========================
observe({
  df <- geno_dfm()

  updateSelectizeInput(
    session,
    "geno_selected_protein",
    choices = sort(unique(df$Protein)),
    server = TRUE
  )

  updateSelectizeInput(
    session,
    "geno_selected_pQTL_source",
    choices = sort(unique(df$pQTL_source)),
    server = TRUE
  )
})

# =========================
# Filtered results
# =========================
geno_selected_result <- eventReactive(input$geno_update, {

  df <- geno_dfm()
  req(length(input$geno_selected_protein) > 0)

  df <- df %>%
    dplyr::filter(Protein %in% input$geno_selected_protein)

  if (length(input$geno_selected_pQTL_source) > 0) {
    df <- df %>%
      dplyr::filter(pQTL_source %in% input$geno_selected_pQTL_source)
  }

  df %>% dplyr::arrange(P)
})

# =========================
# Result table
# =========================
output$geno_result_table <- DT::renderDT({

  DT::datatable(
    geno_selected_result() %>%
      dplyr::transmute(
        Protein = Protein,
	  Disease=Disease,
	  GWAS_soucre=GWAS_soucre,
        `pQTL_source` = `pQTL_source`,
        `OR_95CI` = `OR_95CI`,
        `P value` = P
      ),
    rownames = FALSE,
    options = list(pageLength = 10, autoWidth = TRUE)
  )
})

# =========================
# Forest plot (grid object)
# =========================
geno_forestplot_obj <- reactive({

  df <- geno_selected_result()
  req(nrow(df) > 0)

  forest_data <- data.frame(
    Protein = df$Protein,
	  Disease=df$Disease,
	  GWAS_soucre=df$GWAS_soucre,
    `pQTL_source`  = df$`pQTL_source`,
    OR    = df$OR,
    lower = df$lower,
    upper = df$upper,
    `OR_95CI` = df$`OR_95CI`,
    P   = df$P,
    stringsAsFactors = FALSE
  )
      a_df <- as.data.frame(matrix(colnames(forest_data),   nrow = 1))
    colnames(a_df) <- colnames(forest_data)
    forest_data <- rbind(a_df, forest_data)
    forest_data[1, 3:5] <- ""
    forest_data[, 5:6] <- lapply(forest_data[, 5:6], as.numeric)

    n_rows <- nrow(forest_data)
    hrzl_lines <- list(
      "1" = gpar(lty = 1, lwd = 2),
      "2" = gpar(lty = 1, lwd = 2)
    )
    hrzl_lines[[as.character(n_rows+1)]] <- gpar(lty = 1, lwd = 2)
    if(n_rows > 3) {
      for(i in 3:(n_rows)) {
        hrzl_lines[[as.character(i)]] <- gpar(lty = 2, col = "#00000040")
      }
    }
    valid_lower <- forest_data[-1, "OR"]
    valid_upper <- forest_data[-1, "OR"]
    min_tick <- floor(min(valid_lower, na.rm = TRUE) * 2) / 2
    max_tick <- ceiling(max(valid_upper, na.rm = TRUE) * 2) / 2
    xticks <- seq(from = min_tick, to = max_tick, by = 0.5)
    print(forest_data)
    fp <- forestplot(
      forest_data[, c(1:4, 8:9)],
      mean = forest_data[, 5],
      lower = forest_data[, 6],
      upper = forest_data[, 7],
      zero = 1,
      boxsize = 0.1,
      graph.pos   = 3,
      graphwidth = unit(.1, "npc"),
      xlab = "Odd ratio and 95% CI",
      title = paste("Forest Plot"),
      xticks = xticks,
      clip = c(min_tick - 0.3, max_tick + 0.3),
      fn.ci_norm   = fpDrawNormalCI,
      lty.ci   = 1,
      lwd.ci   = 2,
      vertices = FALSE,
      is.summary   = c(TRUE, replicate(n = nrow(forest_data) - 1, FALSE)),
      hrzl_lines = hrzl_lines,
      col = fpColors(
        box = "orange",
        lines = "orange"
      ),
      txt_gp = fpTxtGp(
        label = gpar(
          fontfamily = "serif",
          fontface = "plain",
          cex = 1 
        ),
        ticks = gpar(
          fontfamily = "serif",
          fontface = "plain",
          cex = 0.8 
        ),
        xlab = gpar(
          fontfamily = "serif",
          fontface = "plain",
          cex = 1.1 
        ),
        title = gpar(
          fontfamily = "serif",
          fontface = "bold",
          cex = 1.5 
        )
      )
    )
    
    return(fp)
})

# =========================
# Render forest plot
# =========================
output$geno_forest_plot <- renderPlot({
  geno_forestplot_obj()
})

# =========================
# Download forest plot
# =========================
output$geno_download_forest <- downloadHandler(

  filename = function() {
    paste0(
      "Genomics_Forest_",
      paste(input$geno_selected_protein, collapse = "_"),
      ".pdf"
    )
  },

  content = function(file) {
    pdf(file, width = 9, height = 6)
    geno_forestplot_obj()
    dev.off()
  }
)


coef_tier1 <- c(
  CD7 = 0.10743, FAS = 0.17682, ICAM1 = 0.24812,
  LTBR = 0.54029, PDCD1 = 0.85740, TCL1A = 0.05656
)

coef_tier2 <- c(
  ADM = 0.558898, CD7 = 0.005976, CTSZ = -0.174698,
  FAS = 0.065984, ICAM1 = 0.119139, LTBR = 0.115596,
  PDCD1 = 0.832049, RNASET2 = 0.821152, TCL1A = 0.037388
)

coef_tier3 <- c(
  CD7 = -0.31248, FAS = 0.03571, ICAM1 = -0.19071,
  LTBR = 0.21420, PDCD1 = 0.48702, TCL1A = 0.08097,
  ADM = 0.68028, CTSZ = -0.17544, RNASET2 = 0.67670,
  ABL1 = -0.14398, BTN2A1 = -0.43470, CST7 = 0.02902,
  CXCL13 = 0.18588, FCGR2A = 0.05245, GP1BA = 0.38873,
  IGF1R = -0.27777, KLRB1 = -0.06402, KRT19 = 0.16109,
  KYNU = -0.03346, LAIR2 = -0.05429, LRP1 = 0.53700,
  LY9 = 1.00702, MANSC1 = -0.28046, MILR1 = 0.14775,
  PLXNB2 = -0.07108, SUSD5 = 0.32787, TNFRSF1B = 0.16438
)

risk_score <- eventReactive(input$calc_score, {

  req(input$expr_file)

  # 1. ??????
  expr <- fread(input$expr_file$datapath, data.table = FALSE)

  rownames(expr) <- expr[, 1]
  expr <- expr[, -1, drop = FALSE]

  # 2. ?? numeric(????)
  expr[] <- lapply(expr, as.numeric)

  # 3. ??????
  score_fun <- function(coef, expr) {
    genes <- intersect(names(coef), rownames(expr))
    if (length(genes) == 0)
      return(rep(NA_real_, ncol(expr)))

    coef_vec <- coef[genes]
    expr_mat <- as.matrix(expr[genes, , drop = FALSE])

    as.numeric(t(coef_vec) %*% expr_mat)
  }

  # 4. ???? tier
  tier1   <- score_fun(coef_tier1, expr)
  tier12  <- score_fun(c(coef_tier1, coef_tier2), expr)
  tier123 <- score_fun(c(coef_tier1, coef_tier2, coef_tier3), expr)

  # 5. ????
  data.frame(
    Sample = colnames(expr),
    Tier1 = tier1,
    Tier1_2 = tier12,
    Tier1_2_3 = tier123,
    stringsAsFactors = FALSE
  )
})

# ===== ???? =====
output$score_table <- renderDT({
  req(risk_score())

  datatable(
    risk_score(),
    rownames = FALSE,
    options = list(
      pageLength = 10,
      scrollX = TRUE,
      autoWidth = FALSE,   # ? ??
      columnDefs = list(
        list(className = "dt-left",  targets = 0),
        list(className = "dt-center", targets = "_all")
      )
    ),
    class = "compact stripe"
  )
})

# ===== ?? =====
output$download_score <- downloadHandler(
  filename = function() {
    "Customized_Risk_Score.csv"
  },
  content = function(file) {
    req(risk_score())
    fwrite(risk_score(), file)
  }
)

}
shinyApp(ui, server)
