library(shiny)
library(bs4Dash)
library(meta)
library(dmetar)
library(metafor)
library(ggplot2)
library(ggbeeswarm)
library(fontawesome)
library(shinyjs)  # Add shinyjs for improved UI interactions

# UI using bs4DashPage with improved organization
ui <- bs4DashPage(
  dark = FALSE,
  help = FALSE,
  fullscreen = TRUE,
  scrollToTop = TRUE,
  title = "786-MIII Risk Ratio/Odds Ratio Meta-Analysis Dashboard",
  
  header = bs4DashNavbar(
    title = "Meta-Analysis Dashboard",
    status = "primary"
  ),
  
  sidebar = bs4DashSidebar(
    skin = "light",
    status = "primary",
    elevation = 3,
    div(
      class = "user-panel",
      div(
        class = "image",
        img(src = "https://github.com/ahmadiyya-muslim-research-association/MetaAnalysisGPT/raw/main/logo.jpeg", 
            class = "img-circle elevation-2")
      ),
      div(
        class = "info",
        h5("Meta-Analysis Tool")
      )
    ),
    bs4SidebarMenu(
      id = "sidebar",
      bs4SidebarMenuItem(
        text = "Introduction & Data Import", 
        tabName = "dataImport", 
        icon = icon("file-import")
      ),
      bs4SidebarMenuItem(
        text = "Meta-Analysis Results", 
        tabName = "results", 
        icon = icon("calculator")
      ),
      bs4SidebarMenuItem(
        text = "Forest Plots", 
        tabName = "forestPlots", 
        icon = icon("tree")
      ),
      bs4SidebarMenuItem(
        text = "Publication Bias", 
        tabName = "pubBias", 
        icon = icon("search"), 
        bs4SidebarMenuSubItem(
          text = "Funnel Plots", 
          tabName = "funnelPlots",
          icon = icon("funnel-dollar")
        ),
        bs4SidebarMenuSubItem(
          text = "Bias Tests", 
          tabName = "biasMethods",
          icon = icon("vial")
        )
      ),
      bs4SidebarMenuItem(
        text = "Heterogeneity", 
        tabName = "heterogeneity", 
        icon = icon("random")
      ),
      bs4SidebarMenuItem(
        text = "Meta-Regression", 
        tabName = "metaReg", 
        icon = icon("chart-line")
      ),
      bs4SidebarMenuItem(
        text = "Bayesian Meta-Analysis", 
        tabName = "bayesian", 
        icon = icon("dice")
      ),
      bs4SidebarMenuItem(
        text = "Advanced Analysis", 
        tabName = "advanced", 
        icon = icon("brain"),
        bs4SidebarMenuSubItem(
          text = "Cumulative Meta-Analysis", 
          tabName = "cumulative",
          icon = icon("layer-group")
        ),
        bs4SidebarMenuSubItem(
          text = "Subgroup Analysis", 
          tabName = "subgroup",
          icon = icon("object-group")
        ),
        bs4SidebarMenuSubItem(
          text = "Influence Analysis", 
          tabName = "influence",
          icon = icon("microscope")
        )
      ),
      bs4SidebarMenuItem(
        text = "About", 
        tabName = "about", 
        icon = icon("info-circle")
      )
    )
  ),
  
  controlbar = NULL,
  
  body = bs4DashBody(
    useShinyjs(),  # Enable shinyjs for dynamic UI features
    bs4TabItems(
      # Introduction & Data Import Tab
      bs4TabItem(
        tabName = "dataImport",
        fluidRow(
          bs4Card(
            title = "Upload Data",
            status = "primary",
            width = 12,
            solidHeader = TRUE,
            collapsible = TRUE,
            fileInput("fileInput", "Upload CSV File",
                      accept = c("text/csv", ".csv")),
            checkboxInput("colorFunnel", "Create color-enhanced funnel plot", FALSE),
            p("Your CSV file must contain the following columns:"),
            tags$ul(
              tags$li(strong("eventintervention:"), "Number of events in intervention group"),
              tags$li(strong("totalintervention:"), "Total number of participants in intervention group"),
              tags$li(strong("eventcontrol:"), "Number of events in control group"),
              tags$li(strong("totalcontrol:"), "Total number of participants in control group"),
              tags$li(strong("author:"), "Study identifier (first author's name)"),
              tags$li(strong("year:"), "(Optional) Year of publication for cumulative analysis"),
              tags$li(strong("Reg, Reg2, Reg3:"), "(Optional) Moderator variables for meta-regression"),
              tags$li(strong("subgroup:"), "(Optional) Subgroup identifier for subgroup analysis")
            )
          )
        ),
        fluidRow(
          bs4Card(
            title = "Analysis Settings",
            status = "info",
            width = 12,
            solidHeader = TRUE,
            collapsible = TRUE,
            fluidRow(
              column(
                width = 4,
                radioButtons("effectMeasure", "Effect Measure:", 
                             choices = c("Risk Ratio" = "RR", "Odds Ratio" = "OR"), 
                             selected = "RR")
              ),
              column(
                width = 4,
                selectInput("method", "Combination Method:",
                            choices = c("Inverse (Default for Random effects)" = "Inverse",
                                        "Mantel-Haenszel (Recommended for fixed effects)" = "MH",
                                        "Peto (only for Odds Ratio and events are rare)" = "Peto",
                                        "GLMM (Only with Odds Ratio and if event rate really low)" = "GLMM",
                                        "Bakbergenuly-Sample Size Method (Just as Good as MH but only for Odds Ratio)" = "SSW"),
                            selected = "Inverse")
              ),
              column(
                width = 4,
                selectInput("method.tau", "Heterogeneity Method:",
                            choices = c("Paule-Mandel (default)" = "PM",
                                        "DerSimonian-Laird" = "DL",
                                        "Restricted Maximum Likelihood (REML)" = "REML",
                                        "Empirical Bayes (EB)" = "EB",
                                        "Sidik-Jonkman (SJ)" = "SJ"),
                            selected = "PM")
              )
            ),
            fluidRow(
              column(
                width = 4,
                selectInput("effectModel", "Effect Model:",
                            choices = c("Random" = "RE", "Fixed" = "FE"),
                            selected = "RE")
              ),
              column(
                width = 4,
                selectInput("selectedModerator", "Select Moderator for Bubble Plot:",
                            choices = c("Reg", "Reg2", "Reg3"), 
                            selected = "Reg")
              )
            )
          )
        ),
        fluidRow(
          bs4Card(
            title = "Understanding Meta-Analysis",
            status = "warning",
            width = 12,
            solidHeader = TRUE,
            collapsible = TRUE,
            tabsetPanel(
              tabPanel("Introduction", uiOutput("text")),
              tabPanel("Understanding Forest Plots", uiOutput("forest")),
              tabPanel("Choosing Methods", uiOutput("model")),
              tabPanel("Meta-Regression", uiOutput("mtext")),
              tabPanel("Publication Bias", uiOutput("pubtext")),
              tabPanel("Heterogeneity", uiOutput("htext"))
            )
          )
        )
      ),
      
      # Meta-Analysis Results Tab
      bs4TabItem(
        tabName = "results",
        fluidRow(
          bs4Card(
            title = "Meta-Analysis Summary",
            status = "primary",
            width = 12,
            solidHeader = TRUE,
            collapsible = TRUE,
            maximizable = TRUE,
            verbatimTextOutput("resultText"),
            downloadButton("downloadResults", "Download Results", class = "btn-primary")
          )
        ),
        fluidRow(
          bs4Card(
            title = "RMA Output",
            status = "info",
            width = 6,
            solidHeader = TRUE,
            collapsible = TRUE,
            verbatimTextOutput("RMA"),
            downloadButton("downloadRMA", "Download RMA Results", class = "btn-info")
          ),
          bs4Card(
            title = "Odds Ratio RMA Output",
            status = "info",
            width = 6,
            solidHeader = TRUE,
            collapsible = TRUE,
            verbatimTextOutput("RMAodd"),
            downloadButton("downloadRMAodd", "Download OR Results", class = "btn-info")
          )
        )
      ),
      
      # Forest Plots Tab
      bs4TabItem(
        tabName = "forestPlots",
        fluidRow(
          bs4Card(
            title = "Forest Plot Customization",
            status = "success",
            width = 12,
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,  # Start collapsed to improve initial load time
            fluidRow(
              column(
                width = 6,
                h4("Color Settings"),
                fluidRow(
                  column(
                    width = 4,
                    selectInput("col.diamond", "Diamond Color:",
                                choices = c("Red", "Blue", "Green", "Purple", "Orange",
                                            "Brown", "Pink", "Yellow", "Black"),
                                selected = "Black")
                  ),
                  column(
                    width = 4,
                    selectInput("col.diamond.lines", "Diamond Line Color:",
                                choices = c("Red", "Blue", "Green", "Orange",
                                            "Brown", "Pink", "Yellow", "Black"),
                                selected = "Yellow")
                  ),
                  column(
                    width = 4,
                    selectInput("col.square", "Square Color:",
                                choices = c("Red", "Blue", "Green", "Purple", "Orange",
                                            "Brown", "Pink", "Grey", "Yellow", "Black"),
                                selected = "Blue")
                  )
                ),
                fluidRow(
                  column(
                    width = 4,
                    selectInput("col.square.lines", "Square Line Color:",
                                choices = c("Red", "Blue", "Green", "Purple", "Orange",
                                            "Brown", "Pink", "Grey", "Yellow", "Black"),
                                selected = "Black")
                  ),
                  column(
                    width = 4,
                    selectInput("col.study", "Study Label Color:",
                                choices = c("Red", "Blue", "Green", "Purple", "Orange",
                                            "Brown", "Pink", "Grey", "Yellow", "Black"),
                                selected = "Green")
                  ),
                  column(
                    width = 4,
                    selectInput("col.circle", "Circle Color:",
                                choices = c("Red", "Blue", "Green", "Purple", "Orange",
                                            "Brown", "Pink", "Grey", "Yellow", "Black"),
                                selected = "Yellow")
                  )
                ),
                fluidRow(
                  column(
                    width = 4,
                    selectInput("col.circle.lines", "Circle Line Color:",
                                choices = c("Red", "Blue", "Green", "Purple", "Orange",
                                            "Brown", "Pink", "Grey", "Yellow", "Black"),
                                selected = "Black")
                  )
                )
              ),
              column(
                width = 6,
                h4("Plot Settings"),
                fluidRow(
                  column(
                    width = 6,
                    checkboxInput("labstudies", "Show Study Labels", value = TRUE),
                    sliderInput("textsize", "Text Size:", min = 0.5, max = 2, value = 1, step = 0.1),
                    sliderInput("linewidth", "Line Width:", min = 0.5, max = 5, value = 1, step = 0.1)
                  ),
                  column(
                    width = 6,
                    textInput("label", "Left Label:"),
                    textInput("labelr", "Right Label:"),
                    sliderInput("xRange", "X-axis Range:", min = 0.1, max = 90, value = c(0.1, 10)),
                    sliderInput("plotHeight", "Plot Height:", min = 300, max = 1000, value = 600),
                    sliderInput("plotWidth", "Plot Width:", min = 300, max = 1000, value = 800),
                    sliderInput("xAxisRange", "X-Axis Funnel Plot Range:", min = 0.1, max = 90, value = c(0.1, 10))
                    
                  )
                )
              )
            )
          )
        ),
        fluidRow(
          bs4TabCard(
            title = "Forest Plots",
            id = "forestPlotTabs",
            status = "primary",
            width = 12,
            side = "left",
            solidHeader = TRUE,
            elevation = 3,
            tabPanel(
              title = "Standard",
              icon = icon("chart-bar"),
              plotOutput("forestPlot", height = "500px"),
              downloadButton("downloadPlotmeta", "Download Plot", class = "btn-primary")
            ),
            tabPanel(
              title = "JAMA Style",
              icon = icon("certificate"),
              plotOutput("forestPlotJAMA", height = "500px"),
              downloadButton("downloadPlotJAMA", "Download JAMA Plot", class = "btn-info")
            ),
            tabPanel(
              title = "RevMan5 Style",
              icon = icon("leaf"),
              plotOutput("forestPlotRevman5", height = "500px"),
              downloadButton("downloadPlotRevman5", "Download RevMan Plot", class = "btn-warning")
            )
          )
        ),
        fluidRow(
          bs4TabCard(
            title = "Alternative Visualizations",
            id = "altVisualizationTabs",
            status = "success",
            width = 12,
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,  # Start collapsed to improve load time
            tabPanel("Drapery Plot", 
                     plotOutput("drapery", height = "400px"),
                     downloadButton("downloadDrapery", "Download Drapery Plot", class = "btn-success")),
            tabPanel("Radial Plot", 
                     plotOutput("radial", height = "400px"),
                     downloadButton("downloadRadial", "Download Radial Plot", class = "btn-success")),
            tabPanel("Bee Swarm Plot", 
                     plotOutput("beeswarm", height = "400px"),
                     downloadButton("downloadBeeswarm", "Download Bee Swarm Plot", class = "btn-success")),
            tabPanel("L'Abbé Plot", 
                     plotOutput("labbe", height = "400px"),
                     downloadButton("downloadLabbe", "Download L'Abbé Plot", class = "btn-success"))
          )
        )
      ),
      
      # Funnel Plots Tab (Publication Bias)
      bs4TabItem(
        tabName = "funnelPlots",
        fluidRow(
          bs4TabCard(
            title = "Funnel Plots",
            id = "funnelPlotTabs",
            status = "primary",
            width = 12,
            side = "left",
            solidHeader = TRUE,
            tabPanel(
              title = "Standard Funnel",
              icon = icon("filter"),
              plotOutput("funnelPlot", height = "500px"),
              p("A funnel plot is a scatter plot of treatment effect against study precision. In the absence of bias, it should resemble an inverted funnel."),
              downloadButton("downloadFunnel", "Download Funnel Plot", class = "btn-primary")
            ),
            tabPanel(
              title = "Trim & Fill Method",
              icon = icon("scissors"),
              plotOutput("Trimfill", height = "500px"),
              p("The trim and fill method estimates and adjusts for the number of missing studies due to publication bias."),
              downloadButton("downloadTrimfill", "Download Trim & Fill Plot", class = "btn-info")
            ),
            tabPanel(
              title = "Limit Meta-Analysis",
              icon = icon("border-style"),
              plotOutput("lm", height = "400px"),
              p("Standard limit meta-analysis helps identify and adjust for small-study effects."),
              downloadButton("downloadLM", "Download Limit Meta Plot", class = "btn-info")
            ),
            tabPanel(
              title = "Shrunken LMA",
              icon = icon("compress-arrows-alt"),
              plotOutput("lm2", height = "400px"),
              p("Shrunken limit meta-analysis applies corrections to reduce the influence of small studies."),
              downloadButton("downloadLM2", "Download Shrunken LMA Plot", class = "btn-info")
            )
          )
        )
      ),
      
      # Bias Tests Tab (Publication Bias)
      bs4TabItem(
        tabName = "biasMethods",
        fluidRow(
          bs4Card(
            title = "Publication Bias Tests",
            status = "primary",
            width = 12,
            solidHeader = TRUE,
            collapsible = TRUE,
            maximizable = TRUE,
            verbatimTextOutput("pubbias"),
            downloadButton("downloadPubbias", "Download Bias Analysis", class = "btn-primary")
          )
        ),
        fluidRow(
          bs4Card(
            title = "P-Curve Analysis",
            status = "info",
            width = 6,
            solidHeader = TRUE,
            collapsible = TRUE,
            plotOutput("pcurve", height = "500px"),
            p("P-curve analysis examines the distribution of statistically significant p-values to detect p-hacking or selective reporting."),
            downloadButton("downloadPcurve", "Download P-Curve Plot", class = "btn-info")
          ),
          bs4Card(
            title = "Egger's Test",
            status = "info",
            width = 6,
            solidHeader = TRUE,
            collapsible = TRUE,
            verbatimTextOutput("eggerText"),
            p("Egger's test assesses funnel plot asymmetry as an indicator of publication bias.")
          )
        ),
        fluidRow(
          bs4Card(
            title = "Comparison Plot",
            status = "warning",
            width = 12,
            solidHeader = TRUE,
            collapsible = TRUE,
            plotOutput("cforest", height = "500px"),
            p("This plot compares study sample sizes with their statistical significance to identify potential bias patterns."),
            downloadButton("downloadCforest", "Download Comparison Plot", class = "btn-warning")
          )
        )
      ),
      
      # Heterogeneity Tab
      bs4TabItem(
        tabName = "heterogeneity",
        fluidRow(
          bs4Card(
            title = "Heterogeneity Analysis",
            status = "primary",
            width = 12,
            solidHeader = TRUE,
            collapsible = TRUE,
            verbatimTextOutput("hetro"),
            downloadButton("downloadHetero", "Download Heterogeneity Analysis", class = "btn-primary")
          )
        ),
        fluidRow(
          bs4TabCard(
            title = "Influence Diagnostics",
            id = "influenceDiagnosticsTabs",
            status = "info",
            width = 12,
            solidHeader = TRUE,
            tabPanel("Baujat Plot", 
                     plotOutput("Baujatplot", height = "400px"),
                     downloadButton("downloadBaujat", "Download Baujat Plot", class = "btn-info")),
            tabPanel("Influence Plot", 
                     plotOutput("Influence", height = "400px"),
                     downloadButton("downloadInfluence", "Download Influence Plot", class = "btn-info")),
            tabPanel("Effect Size", 
                     plotOutput("effectsize", height = "400px"),
                     downloadButton("downloadEffectsize", "Download Effect Size Plot", class = "btn-info")),
            tabPanel("I² Plot", 
                     plotOutput("I2", height = "400px"),
                     downloadButton("downloadI2", "Download I² Plot", class = "btn-info"))
          )
        )
      ),
      
      # Meta-Regression Tab
      bs4TabItem(
        tabName = "metaReg",
        fluidRow(
          bs4Card(
            title = "Meta-Regression Analysis",
            status = "primary",
            width = 12,
            solidHeader = TRUE,
            collapsible = TRUE,
            maximizable = TRUE,
            verbatimTextOutput("metaRegressionResults"),
            downloadButton("downloadMetaReg", "Download Meta-Regression Results", class = "btn-primary")
          )
        ),
        fluidRow(
          bs4Card(
            title = "Bubble Plot",
            status = "info",
            width = 6,
            solidHeader = TRUE,
            collapsible = TRUE,
            plotOutput("selectedBubblePlot", height = "500px"),
            p("The bubble plot visualizes the relationship between the moderator variable and effect sizes, with bubble size representing study precision."),
            downloadButton("downloadBubblePlot", "Download Bubble Plot", class = "btn-info")
          ),
          bs4Card(
            title = "Correlation Analysis",
            status = "info",
            width = 6,
            solidHeader = TRUE,
            collapsible = TRUE,
            plotOutput("mmreg", height = "500px"),
            p("This plot shows correlations between moderator variables and effect sizes.")
          )
        ),
        fluidRow(
          bs4Card(
            title = "Meta-Regression Diagnostics",
            status = "warning",
            width = 12,
            solidHeader = TRUE,
            collapsible = TRUE,
            tabsetPanel(
              tabPanel("Residual Plot", 
                       plotOutput("residualPlot", height = "400px"),
                       downloadButton("downloadResidual", "Download Residual Plot", class = "btn-warning")),
              tabPanel("Correlation Matrix", verbatimTextOutput("correlationMatrix")),
              tabPanel("Model Selection", verbatimTextOutput("modelSelection"))
            ),
            p("These diagnostics help assess the fit and assumptions of the meta-regression model.")
          )
        ),
        
        # Add detailed meta-regression analyses
        fluidRow(
          bs4TabCard(
            title = "Detailed Meta-Regression Analyses",
            id = "detailedMetaRegTabs",
            status = "success",
            width = 12,
            solidHeader = TRUE,
            tabPanel(
              title = "One Moderator Analysis",
              icon = icon("chart-line"),
              verbatimTextOutput("metaRegOneModResult"),
              downloadButton("downloadMetaRegOne", "Download One-Moderator Analysis", class = "btn-success")
            ),
            tabPanel(
              title = "Two Moderator Analysis",
              icon = icon("chart-pie"),
              verbatimTextOutput("metaRegTwoModResult"),
              downloadButton("downloadMetaRegTwo", "Download Two-Moderator Analysis", class = "btn-success")
            ),
            tabPanel(
              title = "Three Moderator Analysis",
              icon = icon("sitemap"),
              verbatimTextOutput("metaRegThreeModResult"),
              downloadButton("downloadMetaRegThree", "Download Three-Moderator Analysis", class = "btn-success")
            )
          )
        )
      ),
      
      # Bayesian Meta-Analysis Tab
      bs4TabItem(
        tabName = "bayesian",
        fluidRow(
          bs4Card(
            title = "About Bayesian Meta-Analysis",
            status = "info",
            width = 12,
            solidHeader = TRUE,
            collapsible = TRUE,
            p("Bayesian meta-analysis represents an alternative approach to traditional frequentist meta-analysis. 
              Instead of providing p-values and confidence intervals, it offers posterior distributions, credible intervals, 
              and probabilities of effects. This approach incorporates prior information and updates it with the observed 
              data to provide a more nuanced understanding of the evidence."),
            p("Key advantages of Bayesian meta-analysis include:"),
            tags$ul(
              tags$li("The ability to make direct probability statements about parameters of interest"),
              tags$li("More intuitive interpretation of results through credible intervals"),
              tags$li("Better handling of uncertainty in heterogeneity parameters"),
              tags$li("The ability to incorporate prior knowledge when appropriate")
            ),
            p("Note: Bayesian analysis requires the 'bayesmeta' package. If you encounter errors, please install it by running: install.packages('bayesmeta')")
          )
        ),
        fluidRow(
          bs4Card(
            title = "Bayesian Meta-Analysis Results",
            status = "primary",
            width = 12,
            solidHeader = TRUE,
            collapsible = TRUE,
            verbatimTextOutput("bayesianMetaResult"),
            downloadButton("downloadBayesianMeta", "Download Bayesian Results", class = "btn-primary")
          )
        )
      ),
      
      # Cumulative Meta-Analysis Tab
      bs4TabItem(
        tabName = "cumulative",
        fluidRow(
          bs4Card(
            title = "Cumulative Meta-Analysis",
            status = "primary",
            width = 12,
            solidHeader = TRUE,
            collapsible = TRUE,
            maximizable = TRUE,
            plotOutput("cumForestPlot", height = "600px"),
            p("Cumulative meta-analysis shows how evidence has accumulated over time."),
            downloadButton("downloadCumMeta", "Download Cumulative Forest Plot", class = "btn-primary")
          )
        ),
        fluidRow(
          bs4Card(
            title = "Cumulative Meta-Analysis Results",
            status = "info",
            width = 12,
            solidHeader = TRUE,
            collapsible = TRUE,
            verbatimTextOutput("cumMetaText"),
            downloadButton("downloadCumText", "Download Cumulative Results", class = "btn-info")
          )
        )
      ),
      
      # Subgroup Analysis Tab
      bs4TabItem(
        tabName = "subgroup",
        fluidRow(
          bs4Card(
            title = "Subgroup Analysis",
            status = "primary",
            width = 12,
            solidHeader = TRUE,
            collapsible = TRUE,
            maximizable = TRUE,
            plotOutput("subgroupForest", height = "600px"),
            p("Subgroup analysis examines whether effect sizes differ across predefined subgroups."),
            downloadButton("downloadSubgroupPlot", "Download Subgroup Forest Plot", class = "btn-primary")
          )
        ),
        fluidRow(
          bs4Card(
            title = "Subgroup Analysis Results",
            status = "info",
            width = 12,
            solidHeader = TRUE,
            collapsible = TRUE,
            verbatimTextOutput("subgroupText"),
            downloadButton("downloadSubgroupText", "Download Subgroup Results", class = "btn-info")
          )
        )
      ),
      
      # Influence Analysis Tab
      bs4TabItem(
        tabName = "influence",
        fluidRow(
          bs4Card(
            title = "Leave-One-Out Analysis",
            status = "primary",
            width = 12,
            solidHeader = TRUE,
            collapsible = TRUE,
            maximizable = TRUE,
            verbatimTextOutput("leaveOneOutText"),
            downloadButton("downloadLeaveOneOut", "Download Leave-One-Out Results", class = "btn-primary")
          )
        )
      ),
      
      # About Tab
      bs4TabItem(
        tabName = "about",
        fluidRow(
          bs4Card(
            title = "About This Tool",
            status = "primary",
            width = 12,
            solidHeader = TRUE,
            collapsible = TRUE,
            uiOutput("why")
          )
        )
      )
    )
  ),
  
  footer = bs4DashFooter(
    fixed = FALSE,
    left = a(
      href = "https://github.com/ahmadiyya-muslim-research-association",
      target = "_blank",
      "Ackowldgement for the Ahmadiyya Muslim Research Association"
    ),
    right = "Meta-Analysis Dashboard © 2025"
  )
)

# Server function using organized sections
server <- function(input, output, session) {
  
  ### 1. Data Input ----------------------------------------------------------
  dataInput <- reactive({
    req(input$fileInput)
    read.csv(input$fileInput$datapath)
  })
  
  output$why <- renderUI({
    HTML("  
      <p>'He has encouraged us to use our brains and to ponder upon His creation...'</p>
      <p>Acknowledgments: Ahmadiyya Muslim Research Association, Luciano Candilio, etc.</p>
      <h2>Authors</h2>
      <p>List of authors...</p>
    ")
  })
  
  output$forest <- renderUI({
    HTML("
      <h2>Introduction to Forest Plots</h2>
      <p>Forest plots provide a visual summary of individual study results alongside a pooled estimate.</p>
      <img src='forestplot.png' alt='Forest Plot Example' style='max-width:600px;'>
      <p>Learn more below:</p>
      <iframe width='560' height='315' src='https://www.youtube.com/embed/s-yrMJEOGTY' frameborder='0' allowfullscreen></iframe>
    ")
  })
  
  output$model <- renderUI({
    HTML("
      <h2>Choosing the Right Tools for Meta-Analysis</h2>
      <p>Select the effect measure, combination method, heterogeneity method and model type based on your data.</p>
      <p>For more details, see our documentation.</p>
      <iframe width='560' height='315' src='https://www.youtube.com/embed/gZTAAuNZWy4' frameborder='0' allowfullscreen></iframe>
    ")
  })
  
  output$resultText <- renderPrint({
    req(dataInput())
    dat <- dataInput()
    m.bin <- metabin(
      event.e = dat$eventintervention,
      n.e = dat$totalintervention,
      event.c = dat$eventcontrol,
      n.c = dat$totalcontrol,
      studlab = dat$author,
      data = dat,
      sm = input$effectMeasure,
      method = input$method,
      method.tau = input$method.tau,
      comb.fixed = (input$effectModel == "FE"),
      comb.random = (input$effectModel == "RE")
    )
    print(summary(m.bin, digits = 2, text.tau2 = TRUE, text.I2 = TRUE, text.Q = TRUE))
  })
  
  ### 2. Forest Plots ----------------------------------------------------------
  output$forestPlot <- renderPlot({
    req(dataInput())
    dat <- dataInput()
    m.bin <- metabin(
      event.e = dat$eventintervention,
      n.e = dat$totalintervention,
      event.c = dat$eventcontrol,
      n.c = dat$totalcontrol,
      studlab = dat$author,
      data = dat,
      sm = input$effectMeasure,
      method = input$method,
      method.tau = input$method.tau,
      comb.fixed = (input$effectModel == "FE"),
      comb.random = (input$effectModel == "RE")
    )
    forest(m.bin,
           col.diamond = input$col.diamond,
           col.diamond.lines = input$col.diamond.lines,
           col.square = input$col.square,
           col.square.lines = input$col.square.lines,
           col.study = input$col.study,
           col.circle = input$col.circle,
           col.circle.lines = input$col.circle.lines,
           labstudies = input$labstudies,
           cex = input$textsize,
           lwd = input$linewidth,
           label.left = input$label,
           label.right = input$labelr,
           xlim = input$xRange,
           prediction = TRUE)
  })
  
  output$forestPlotJAMA <- renderPlot({
    req(dataInput())
    dat <- dataInput()
    m.bin <- metabin(
      event.e = dat$eventintervention,
      n.e = dat$totalintervention,
      event.c = dat$eventcontrol,
      n.c = dat$totalcontrol,
      studlab = dat$author,
      data = dat,
      sm = input$effectMeasure,
      method = input$method,
      method.tau = input$method.tau,
      comb.fixed = (input$effectModel == "FE"),
      comb.random = (input$effectModel == "RE")
    )
    forest(m.bin,
           col.diamond = input$col.diamond,
           col.diamond.lines = input$col.diamond.lines,
           col.square = input$col.square,
           col.square.lines = input$col.square.lines,
           col.study = input$col.study,
           col.circle = input$col.circle,
           col.circle.lines = input$col.circle.lines,
           labstudies = input$labstudies,
           cex = input$textsize,
           lwd = input$linewidth,
           prediction = FALSE,
           layout = "JAMA")
  })
  
  output$forestPlotRevman5 <- renderPlot({
    req(dataInput())
    dat <- dataInput()
    m.bin <- metabin(
      event.e = dat$eventintervention,
      n.e = dat$totalintervention,
      event.c = dat$eventcontrol,
      n.c = dat$totalcontrol,
      studlab = dat$author,
      data = dat,
      sm = input$effectMeasure,
      method = input$method,
      method.tau = input$method.tau,
      comb.fixed = (input$effectModel == "FE"),
      comb.random = (input$effectModel == "RE")
    )
    forest(m.bin,
           col.diamond = input$col.diamond,
           col.diamond.lines = input$col.diamond.lines,
           col.square = input$col.square,
           col.square.lines = input$col.square.lines,
           col.study = input$col.study,
           col.circle = input$col.circle,
           col.circle.lines = input$col.circle.lines,
           labstudies = input$labstudies,
           cex = input$textsize,
           lwd = input$linewidth,
           prediction = FALSE,
           layout = "RevMan5")
  }, height = reactive(input$plotHeight), width = reactive(input$plotWidth))
  
  output$drapery <- renderPlot({
    req(dataInput())
    dat <- dataInput()
    m.bin <- metabin(
      event.e = dat$eventintervention,
      n.e = dat$totalintervention,
      event.c = dat$eventcontrol,
      n.c = dat$totalcontrol,
      studlab = dat$author,
      data = dat,
      sm = input$effectMeasure,
      method = input$method,
      method.tau = input$method.tau,
      comb.fixed = (input$effectModel == "FE"),
      comb.random = (input$effectModel == "RE")
    )
    drapery(m.bin, labels = "studlab", type = "pval", legend = FALSE)
  })
  
  ### 3. Publication Bias & Diagnostics ------------------------------------
  output$funnelPlot <- renderPlot({
    req(dataInput())
    dat <- dataInput()
    m.bin <- metabin(
      event.e = dat$eventintervention,
      n.e = dat$totalintervention,
      event.c = dat$eventcontrol,
      n.c = dat$totalcontrol,
      studlab = dat$author,
      data = dat,
      sm = input$effectMeasure,
      method = input$method,
      method.tau = input$method.tau,
      comb.fixed = (input$effectModel == "FE"),
      comb.random = (input$effectModel == "RE")
    )
    col.contour <- c("gray75", "gray85", "gray95")
    if (input$colorFunnel) {
      funnel(m.bin, xlim = c(0.1, 5), contour = c(0.9, 0.95, 0.99), col.contour = col.contour)
      legend("topright", inset = 0.05,
             legend = c("p < 0.1", "p < 0.05", "p < 0.01"),
             fill = col.contour)
      title(main = "Contour-Enhanced Funnel Plot")
    } else {
      funnel(m.bin, xlim = input$xAxisRange)
      
    }
  })
  
  output$pubbias <- renderPrint({
    req(dataInput())
    dat <- dataInput()
    m.bin <- metabin(
      event.e = dat$eventintervention,
      n.e = dat$totalintervention,
      event.c = dat$eventcontrol,
      n.c = dat$totalcontrol,
      studlab = dat$author,
      data = dat,
      sm = input$effectMeasure,
      method = input$method,
      method.tau = input$method.tau,
      comb.fixed = (input$effectModel == "FE"),
      comb.random = (input$effectModel == "RE")
    )
    m.limitmeta <- limitmeta(m.bin)
    p.curve <- tryCatch({
      pcurve(m.bin, effect.estimation = FALSE, dmin = 0, dmax = 1)
    }, error = function(e) { NULL })
    print(summary(m.limitmeta))
    if (!is.null(p.curve)) {
      print(summary(p.curve))
    } else {
      cat("p-curve analysis could not be conducted.\n")
    }
  })
  
  ### 4. Meta-Regression -----------------------------------------------------
  # Meta-Regression with one, two, and three moderators
  output$metaRegressionResults <- renderPrint({
    req(dataInput())
    dat <- dataInput()
    m.bin <- metabin(
      event.e = dat$eventintervention,
      n.e = dat$totalintervention,
      event.c = dat$eventcontrol,
      n.c = dat$totalcontrol,
      studlab = dat$author,
      data = dat,
      sm = input$effectMeasure,
      method = input$method,
      method.tau = input$method.tau,
      comb.fixed = (input$effectModel == "FE"),
      comb.random = (input$effectModel == "RE")
    )
    results <- list()
    if ("Reg" %in% names(dat)) {
      m.reg1 <- metareg(m.bin, ~ Reg)
      results$OneModerator <- summary(m.reg1)
    } else {
      results$OneModerator <- "Moderator 'Reg' not found in data."
    }
    if (all(c("Reg", "Reg2") %in% names(dat))) {
      m.reg2 <- metareg(m.bin, ~ Reg + Reg2)
      results$TwoModerators <- summary(m.reg2)
    } else {
      results$TwoModerators <- "Moderators 'Reg' and/or 'Reg2' not found in data."
    }
    if (all(c("Reg", "Reg2", "Reg3") %in% names(dat))) {
      m.reg3 <- metareg(m.bin, ~ Reg + Reg2 + Reg3)
      results$ThreeModerators <- summary(m.reg3)
    } else {
      results$ThreeModerators <- "One or more of the moderators 'Reg', 'Reg2', 'Reg3' not found in data."
    }
    print(results)
  })
  
  # Bubble Plot for Meta-Regression with a choice of moderator
  output$selectedBubblePlot <- renderPlot({
    req(dataInput())
    dat <- dataInput()
    chosenMod <- input$selectedModerator
    if (!(chosenMod %in% names(dat))) {
      plot.new()
      text(0.5, 0.5, paste("Moderator", chosenMod, "not found in data."), cex = 1.5)
      return()
    }
    m.bin <- metabin(
      event.e = dat$eventintervention,
      n.e = dat$totalintervention,
      event.c = dat$eventcontrol,
      n.c = dat$totalcontrol,
      studlab = dat$author,
      data = dat,
      sm = input$effectMeasure,
      method = input$method,
      method.tau = input$method.tau,
      comb.fixed = (input$effectModel == "FE"),
      comb.random = (input$effectModel == "RE")
    )
    mod_formula <- as.formula(paste("~", chosenMod))
    m.reg <- metareg(m.bin, mod_formula)
    bubble(m.reg, studlab = TRUE, main = paste("Bubble Plot for Moderator:", chosenMod))
  })
  
  ### 5. Diagnostics, Cumulative, & Subgroup ---------------------------------
  output$cumMetaText <- renderPrint({
    req(dataInput())
    dat <- dataInput()
    m.bin <- metabin(
      event.e = dat$eventintervention,
      n.e = dat$totalintervention,
      event.c = dat$eventcontrol,
      n.c = dat$totalcontrol,
      studlab = dat$author,
      data = dat,
      sm = input$effectMeasure,
      method = input$method,
      method.tau = input$method.tau,
      comb.fixed = (input$effectModel == "FE"),
      comb.random = (input$effectModel == "RE")
    )
    if("year" %in% names(dat)){
      m.cum <- metacum(m.bin, sortvar = dat$year)
    } else {
      m.cum <- metacum(m.bin)
    }
    print(summary(m.cum))
  })
  
  output$cumForestPlot <- renderPlot({
    req(dataInput())
    dat <- dataInput()
    m.bin <- metabin(
      event.e = dat$eventintervention,
      n.e = dat$totalintervention,
      event.c = dat$eventcontrol,
      n.c = dat$totalcontrol,
      studlab = dat$author,
      data = dat,
      sm = input$effectMeasure,
      method = input$method,
      method.tau = input$method.tau,
      comb.fixed = (input$effectModel == "FE"),
      comb.random = (input$effectModel == "RE")
    )
    if("year" %in% names(dat)){
      m.cum <- metacum(m.bin, sortvar = dat$year)
    } else {
      m.cum <- metacum(m.bin)
    }
    forest(m.cum,
           col.diamond = input$col.diamond,
           col.diamond.lines = input$col.diamond.lines,
           col.square = input$col.square,
           col.square.lines = input$col.square.lines,
           col.study = input$col.study,
           col.circle = input$col.circle,
           col.circle.lines = input$col.circle.lines,
           labstudies = input$labstudies,
           cex = input$textsize,
           lwd = input$linewidth,
           label.left = input$label,
           label.right = input$labelr,
           xlim = input$xRange,
           prediction = TRUE)
  })
  
  output$subgroupForest <- renderPlot({
    req(dataInput())
    dat <- dataInput()
    if(!("subgroup" %in% names(dat))){
      plot.new()
      text(0.5, 0.5, "No subgroup column found in the data.", cex = 1.5)
    } else {
      m.subgroup <- metabin(
        event.e = dat$eventintervention,
        n.e = dat$totalintervention,
        event.c = dat$eventcontrol,
        n.c = dat$totalcontrol,
        studlab = dat$author,
        data = dat,
        sm = input$effectMeasure,
        byvar = dat$subgroup,
        comb.fixed = (input$effectModel == "FE"),
        comb.random = (input$effectModel == "RE")
      )
      forest(m.subgroup, xlab = "Log Effect", prediction = TRUE)
    }
  })
  
  output$subgroupText <- renderPrint({
    req(dataInput())
    dat <- dataInput()
    if(!("subgroup" %in% names(dat))){
      cat("No subgroup column found in the data.")
    } else {
      m.subgroup <- metabin(
        event.e = dat$eventintervention,
        n.e = dat$totalintervention,
        event.c = dat$eventcontrol,
        n.c = dat$totalcontrol,
        studlab = dat$author,
        data = dat,
        sm = input$effectMeasure,
        byvar = dat$subgroup,
        comb.fixed = (input$effectModel == "FE"),
        comb.random = (input$effectModel == "RE")
      )
      print(summary(m.subgroup))
    }
  })
  
  output$correlationMatrix <- renderPrint({
    req(dataInput())
    dat <- dataInput()
    if(!all(c("Reg", "Reg2", "Reg3") %in% names(dat))){
      cat("One or more moderator variables (Reg, Reg2, Reg3) not found.")
    } else {
      corr <- cor(dat[, c("Reg", "Reg2", "Reg3")])
      print(corr)
    }
  })
  
  output$modelSelection <- renderPrint({
    req(dataInput())
    dat <- dataInput()
    if(!("Reg" %in% names(dat))){
      cat("Moderator 'Reg' not found in the data.")
    } else {
      m.bin <- metabin(
        event.e = dat$eventintervention,
        n.e = dat$totalintervention,
        event.c = dat$eventcontrol,
        n.c = dat$totalcontrol,
        studlab = dat$author,
        data = dat,
        sm = input$effectMeasure,
        method = input$method,
        comb.fixed = (input$effectModel == "FE"),
        comb.random = (input$effectModel == "RE")
      )
      m.reg <- metareg(m.bin, ~ Reg)
      cat("AIC for meta-regression model:", AIC(m.reg), "\n")
      cat("BIC for meta-regression model:", BIC(m.reg), "\n")
    }
  })
  
  output$cooksDistance <- renderPlot({
    req(dataInput())
    dat <- dataInput()
    m.bin <- metabin(
      event.e = dat$eventintervention,
      n.e = dat$totalintervention,
      event.c = dat$eventcontrol,
      n.c = dat$totalcontrol,
      studlab = dat$author,
      data = dat,
      sm = input$effectMeasure,
      method = input$method,
      comb.fixed = (input$effectModel == "FE"),
      comb.random = (input$effectModel == "RE")
    )
    inf <- influence(m.bin)
    plot(inf, "cook", main = "Cook's Distance Plot")
  })
  
  output$Baujatplot <- renderPlot({
    req(dataInput())
    dat <- dataInput()
    m.bin <- metabin(
      event.e = dat$eventintervention,
      n.e = dat$totalintervention,
      event.c = dat$eventcontrol,
      n.c = dat$totalcontrol,
      studlab = dat$author,
      data = dat,
      sm = input$effectMeasure,
      method = input$method,
      comb.fixed = (input$effectModel == "FE"),
      comb.random = (input$effectModel == "RE")
    )
    m.gen.inf <- InfluenceAnalysis(m.bin, random = TRUE)
    plot(m.gen.inf, "baujat")
  })
  
  output$Influence <- renderPlot({
    req(dataInput())
    dat <- dataInput()
    m.bin <- metabin(
      event.e = dat$eventintervention,
      n.e = dat$totalintervention,
      event.c = dat$eventcontrol,
      n.c = dat$totalcontrol,
      studlab = dat$author,
      data = dat,
      sm = input$effectMeasure,
      method = input$method,
      comb.fixed = (input$effectModel == "FE"),
      comb.random = (input$effectModel == "RE")
    )
    m.gen.inf <- InfluenceAnalysis(m.bin, random = TRUE)
    plot(m.gen.inf, "influence")
  })
  
  output$effectsize <- renderPlot({
    req(dataInput())
    dat <- dataInput()
    m.bin <- metabin(
      event.e = dat$eventintervention,
      n.e = dat$totalintervention,
      event.c = dat$eventcontrol,
      n.c = dat$totalcontrol,
      studlab = dat$author,
      data = dat,
      sm = input$effectMeasure,
      method = input$method,
      comb.fixed = (input$effectModel == "FE"),
      comb.random = (input$effectModel == "RE")
    )
    m.gen.inf <- InfluenceAnalysis(m.bin, random = TRUE)
    plot(m.gen.inf, "es")
  })
  
  output$I2 <- renderPlot({
    req(dataInput())
    dat <- dataInput()
    m.bin <- metabin(
      event.e = dat$eventintervention,
      n.e = dat$totalintervention,
      event.c = dat$eventcontrol,
      n.c = dat$totalcontrol,
      studlab = dat$author,
      data = dat,
      sm = input$effectMeasure,
      method = input$method,
      comb.fixed = (input$effectModel == "FE"),
      comb.random = (input$effectModel == "RE")
    )
    m.gen.inf <- InfluenceAnalysis(m.bin, random = TRUE)
    plot(m.gen.inf, "i2")
  })
  
  output$lm <- renderPlot({
    req(dataInput())
    dat <- dataInput()
    m.bin <- metabin(
      event.e = dat$eventintervention,
      n.e = dat$totalintervention,
      event.c = dat$eventcontrol,
      n.c = dat$totalcontrol,
      studlab = dat$author,
      data = dat,
      sm = input$effectMeasure,
      method = input$method,
      comb.fixed = (input$effectModel == "FE"),
      comb.random = (input$effectModel == "RE")
    )
    lm_obj <- limitmeta(m.bin)
    funnel.limitmeta(lm_obj)
  })
  
  output$lm2 <- renderPlot({
    req(dataInput())
    dat <- dataInput()
    m.bin <- metabin(
      event.e = dat$eventintervention,
      n.e = dat$totalintervention,
      event.c = dat$eventcontrol,
      n.c = dat$totalcontrol,
      studlab = dat$author,
      data = dat,
      sm = input$effectMeasure,
      method = input$method,
      comb.fixed = (input$effectModel == "FE"),
      comb.random = (input$effectModel == "RE")
    )
    lm_obj <- limitmeta(m.bin)
    funnel.limitmeta(lm_obj, shrunken = TRUE)
  })
  
  output$pcurve <- renderPlot({
    req(dataInput())
    dat <- dataInput()
    m.bin <- metabin(
      event.e = dat$eventintervention,
      n.e = dat$totalintervention,
      event.c = dat$eventcontrol,
      n.c = dat$totalcontrol,
      studlab = dat$author,
      data = dat,
      sm = input$effectMeasure,
      method = input$method,
      comb.fixed = (input$effectModel == "FE"),
      comb.random = (input$effectModel == "RE")
    )
    pcurve(m.bin, effect.estimation = FALSE, dmin = 0, dmax = 1)
  })
  
  output$beeswarm <- renderPlot({
    req(dataInput())
    dat <- dataInput()
    m.bin <- metabin(
      event.e = dat$eventintervention,
      n.e = dat$totalintervention,
      event.c = dat$eventcontrol,
      n.c = dat$totalcontrol,
      studlab = dat$author,
      data = dat,
      sm = input$effectMeasure,
      method = input$method,
      comb.fixed = (input$effectModel == "FE"),
      comb.random = (input$effectModel == "RE")
    )
    summary_data <- summary(m.bin)
    plot_data <- data.frame(
      Study = dat$author,
      TE = summary_data$TE,
      SE = summary_data$seTE,
      Weight = summary_data$w.fixed + summary_data$w.random
    )
    ggplot(plot_data, aes(x = factor(1), y = TE, size = Weight)) +
      geom_beeswarm(cex = 1.5) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      labs(x = "", y = "Effect Size (e.g., Odds Ratio)", title = "Bee Swarm Plot") +
      theme_minimal() +
      theme(axis.text.x = element_blank(), axis.title.x = element_blank())
  })
  
  output$cforest <- renderPlot({
    req(dataInput())
    dat <- dataInput()
    m.bin <- metabin(
      event.e = dat$eventintervention,
      n.e = dat$totalintervention,
      event.c = dat$eventcontrol,
      n.c = dat$totalcontrol,
      studlab = dat$author,
      data = dat,
      sm = input$effectMeasure,
      method = input$method,
      comb.fixed = (input$effectModel == "FE"),
      comb.random = (input$effectModel == "RE")
    )
    z_values <- summary(m.bin)$TE / summary(m.bin)$seTE
    p_values <- 2 * (1 - pnorm(abs(z_values)))
    n_treatment <- dat$totalintervention
    n_control <- dat$totalcontrol
    plot_data <- data.frame(
      SampleSize = n_treatment + n_control,
      PValue = p_values
    )
    ggplot(plot_data, aes(x = log(SampleSize), y = -log10(PValue))) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, color = "blue") +
      labs(x = "Log of Sample Size", y = "-Log10 of P-value", title = "Albatross Plot") +
      theme_minimal()
  })
}



server <- function(input, output, session) {
  
  dataInput <- reactive({
    req(input$fileInput)
    read.csv(input$fileInput$datapath)
  })
  
  # ...[All your existing output$... definitions remain the same]...
  # Make sure you replaced studlab = author with studlab = dat$author
  # and removed the “N” argument from pcurve() calls.
  
  # Example fix for forestPlot:
  output$forestPlot <- renderPlot({
    req(dataInput())
    dat <- dataInput()
    m.bin <- metabin(
      event.e = dat$eventintervention,
      n.e = dat$totalintervention,
      event.c = dat$eventcontrol,
      n.c = dat$totalcontrol,
      studlab = dat$author,  # <<--- changed here
      data = dat,
      sm = input$effectMeasure,
      method = input$method,
      method.tau = input$method.tau,
      comb.fixed = input$effectModel == "FE",
      comb.random = input$effectModel == "RE"
    )
    forest(m.bin,
           col.diamond = input$col.diamond,
           col.diamond.lines = input$col.diamond.lines,
           col.square = input$col.square,
           col.square.lines = input$col.square.lines,
           col.study = input$col.study,
           col.circle = input$col.circle,
           col.circle.lines = input$col.circle.lines,
           labstudies = input$labstudies,
           cex = input$textsize,
           lwd = input$linewidth,
           label.left = input$label,
           label.right = input$labelr,
           xlim = input$xRange,
           prediction = TRUE
    )
  })
  
  # Similarly for forestPlotJAMA, forestPlotRevman5, labbe, drapery, etc.:
  # change studlab = author to studlab = dat$author.
  
  # Also remove “N” from pcurve calls, e.g.:
  #   pcurve(m.bin, effect.estimation = FALSE, dmin = 0, dmax = 1)
}

shinyApp(ui = ui, server = server)


# Server logic
server <- function(input, output, session) {
  dataInput <- reactive({
    req(input$fileInput)
    read.csv(input$fileInput$datapath)
    
  })
  
  output$why <- renderUI({
    HTML("
     <p>'He has encouraged us to use our brains and to ponder upon His creation and to search for new roads of human progress and innovation through research and reflection` (Mirza Masroor Ahmad) </p>
    <p> 
    I would like to acknowledge the Ahmadiyya Muslim Research Association, Luciano Candilio, Malik Takreem Ahmad, Niraj Kumar, Jonathan Bray,Reubeen Ahmad and Prof Rui Providencia</p>
    <h2>Authors</h2>
   I would like to acknowledge the Ahmadiyya Muslim Research Association, Luciano Candilio, Malik Takreem Ahmad, Niraj Kumar, Jonathan Bray and Prof Rui Providencia

Najia Ahmad, Student 
Tamar Schreiber, BSc, UCL
Malik Takreem Ahmad, MBBS, King’s College London
Reubeen Ahmad, MBBS, Brighton and Sussex medical school
Olivia Frost, BSc, UCL, St George’s Medical School
Lily Snell MBBS UCL Medical School 
Nishika Bhatt, MBBS , UCL Medical School
Bilaal Dar, MBBS, King’s College London
Zahid Khan, MBBS, MRCP, MSc Cardiology, MSc Medical and Health Education, Bart’s Heart Centre, London & University of South Wales, UK
Honey Panchal, MBBS BSc, University College London
Tayyibah Patel, MBBS BSc, UCL.
Shayan Shaikh, MBBS BSc, UCL
Sophie E Thompson, MBChB, University Hospitals Birmingham NHS Foundation Trust 
Gurkiran Kaur Sandhar, MBBS, UCL Medical School
Girishkumar Sivakumar, MBBS BSc, UCL Medical School
Shubkhez Azeem Bajwa MBBS BSc, St George’s Medical School
Hannah Glatzel, MBChB, BSc, Hutt Hospital, Wellington, NZ
Mahmood Ahmad, MBBS, Royal Free Hospital
YuZhi Phuah, MBBS, University College London 
Niraj S Kumar, PhD, Department of Cardiovascular Sciences, University of Leicester + National Medical Research Association, London, UK
Hamaad Muin Ahmad, MUDr. Croydon University Hospital 
Zain M Ahmad, Student
Zaki Ahmad, Student
Gaayen Ravii Sahgal, MBBS, King’s College London
Cynthia Ng MBBS, BSc, UCL 
Safa Kindawi, MBBS, iBSc, UCL Medical School
Kieran Williams, MBBS, iBSc, UCL Medical School
Osarumwense Ogbeide, MBBS, UCL Medical School
Shahzad Hashmi, MBBS iBSC, Imperial College London 
Max Aboutorabi, MBBS iBSC, UCL Medical School
Lakshmi Kowdley Hemanth, MBBS iBSc Student, UCL Medical School 
Kirshu Suvendiran MBBS UCL Medical School 
Amelia Snook, MBBS BSc, UCL Medical School 
Eesha Dev, MBBS, iBSc, UCL Medical School
Ahmed Salih, MBBS BSc, Imperial College London
Ameera Milhan, MBBS, iBSc, UCL Medical School
Maryam Imran, MBBS, iBSc, UCL Medical School
Buvani Punniyakotty, MBBCh BAO, Trinity College Dublin 
Erica Sugita, MBChB, Warwick Medical School
Majed Sheikh, MD, University of Pecs Medical School 
Talha Raza, MBBS, iBSc, UCL Medical School
Ruhi Satia, MBBS iBSc, UCL Medical School
Michelle Husna Esmati, MBBS iBSc, St George’s Medical School, Imperial College London 
Alvin jaison MBBS, UCL Medical school
Sanjali Anil Chu Ahuja, MBBS, MSc, Barts & the London School of Medicine & Dentistry, Queen Mary University of London
Rauhaan Tahir, MBBS, St George’s University
Malak Al Qaimari,  M.D, university of Szeged.
Jonathan Kow, MBBS BSc Student, Imperial College London
Lobna Ragab, MBBS, Ainshams University

    ")
  })
  
  output$mtext <- renderUI({
    HTML("
    <h2>Diving into Meta-Regression in Meta-Analysis</h2>
    <p>Meta-regression is like the detective work of meta-analysis. Instead of just summarizing study results, you explore what factors might influence those results. It’s your chance to dig deeper and uncover the mysteries behind the data. Let’s dive into the tools and plots that help us solve these puzzles.</p>

    <h3>Meta-Regression Text Results</h3>
    <p>The meta-regression text results provide detailed statistics and insights about how different variables (called moderators) impact the overall effect size. Think of it as your case file, revealing how each clue fits into the bigger picture.</p>
    <p>Here are some of the key components you might encounter:</p>
    <ul>
      <li><strong>Regression Coefficients:</strong> These numbers show how much the outcome changes with each unit change in the moderator variable. It’s like finding out that for every extra hour of studying, your test scores improve by a certain number of points. For example, a coefficient of 0.5 suggests that each additional study hour increases your test score by half a point.</li>
      <li><strong>Confidence Intervals:</strong> These intervals give you a range where the true effect of the moderator likely falls. If they don’t include zero, your moderator is likely having a significant impact. It's like knowing that there's a 95% chance your increased study hours will boost your scores within a specific range. For instance, a confidence interval of [0.2, 0.8] for the study hours means you can be reasonably sure the effect lies somewhere between those values.</li>
      <li><strong>p-Values:</strong> These tell you whether the effect of your moderator is statistically significant. A low p-value (typically less than 0.05) suggests your moderator is more likely to be a real influencer rather than a random fluke. Think of it as the evidence that makes your case stronger. If your p-value is 0.03, it's like having a solid piece of evidence in your detective case.</li>
      <li><strong>R² (R-squared):</strong> This statistic shows how much of the variation in study results can be explained by the moderators. Higher R² values mean your moderators are doing a good job explaining differences. It’s like saying, 'This study habit explains 80% of why students get different scores.' For example, an R² of 0.75 means that 75% of the variability in test scores can be accounted for by your study habits.</li>
    </ul>
    <p>These text results are crucial as they help you understand the role of different moderators in influencing the effect size. It’s your first step in uncovering the underlying patterns in your meta-analysis data.</p>

    <h3>Meta-Regression Bubble Plot</h3>
    <p>A bubble plot is a visual way to show the relationship between the effect sizes and a moderator variable. Imagine a scatter plot where the size of each bubble represents the weight of the study. It’s like plotting your friends’ movie ratings against their time spent watching trailers, with bigger bubbles for the more vocal friends.</p>
    <p>Here’s how to read a bubble plot:</p>
    <ul>
      <li><strong>Effect Sizes:</strong> The position of each bubble on the y-axis represents the effect size from each study. Higher bubbles indicate larger effect sizes, suggesting stronger effects.</li>
      <li><strong>Moderator Variable:</strong> The x-axis shows the values of your moderator variable. For instance, if you're examining study hours, the x-axis would show the range of hours spent studying.</li>
      <li><strong>Bubble Size:</strong> The size of each bubble reflects the weight or importance of the study, usually based on sample size. Larger bubbles mean the study has more influence on the overall results.</li>
      <li><strong>Trends and Patterns:</strong> Look for patterns or trends in the bubbles. For example, if the bubbles tend to rise as you move right along the x-axis, it suggests a positive relationship between the moderator and effect size.</li>
    </ul>
    <p>The bubble plot helps you see if there’s a trend or pattern. For example, you might notice that as the number of trailers watched increases, the movie ratings tend to get higher. This visual tool is handy for spotting trends that might not be obvious from the numbers alone.</p>

    <h3>Multiple Meta-Regression Text Results (Risk Ratio)</h3>
    <p>When you’re dealing with multiple moderators, things can get a bit more complex. The text results for multiple meta-regression provide insights into how several variables together influence the effect size. It’s like juggling multiple clues in your detective case, each adding a piece to the puzzle.</p>
    <p>Key components include:</p>
    <ul>
      <li><strong>Interaction Terms:</strong> These terms show how the effect of one moderator might depend on the level of another moderator. It's like discovering that your study time’s impact on scores also depends on how much sleep you get. For instance, the effect of study hours on test scores might be stronger for students who get 8 hours of sleep compared to those who get only 5 hours.</li>
      <li><strong>Overall Model Fit:</strong> This tells you how well your combined moderators explain the variation in the effect sizes. A good fit means you’re on the right track in solving the mystery. You’ll look at statistics like the F-test to determine if your model significantly improves the prediction of effect sizes.</li>
      <li><strong>Adjusted R²:</strong> Similar to R² but adjusted for the number of moderators in your model. It’s a more accurate reflection of how well your model explains the data, considering you’ve added multiple clues. An adjusted R² of 0.70, for example, suggests that 70% of the variability is explained by your combined moderators, accounting for the complexity of the model.</li>
      <li><strong>Coefficients for Each Moderator:</strong> Each moderator gets its own coefficient, confidence interval, and p-value, helping you understand their individual contributions. It's like having detailed evidence for each suspect in your case, showing who contributes what to the overall story.</li>
    </ul>
    <p>These results are crucial for understanding the combined impact of multiple factors on the effect size, helping you build a more comprehensive picture of the research landscape.</p>

    <h3>Multiple Meta-Regression Performance Analytic</h3>
    <p>This involves using performance analytics to evaluate and visualize the effectiveness of your multiple meta-regression model. It’s like getting a performance review for your detective skills, showing you how well you’ve pieced together the clues.</p>
    <p>Key tools include:</p>
    <ul>
      <li><strong>Chart.Correlation:</strong> This chart shows the correlation between your moderators and the effect sizes, helping you see which variables are closely related. It's like figuring out which study habits are most strongly linked to exam success. You’ll see a matrix of correlation coefficients that indicate the strength and direction of relationships.</li>
      <li><strong>Model Selection Criteria:</strong> Tools like AIC (Akaike Information Criterion) help you compare different models to see which one fits the data best. It’s like comparing different detective strategies to see which one solves the case most effectively. Lower AIC values suggest a better-fitting model. You'll often use criteria like AICc (corrected AIC) for smaller sample sizes.</li>
      <li><strong>Residual Plots:</strong> These plots show the residuals (differences between observed and predicted values) to check for patterns. It’s like checking your work for any inconsistencies or errors. A good model will have residuals randomly scattered around zero, indicating no obvious pattern or bias.</li>
      <li><strong>Goodness-of-Fit Tests:</strong> These tests, like the Chi-square goodness-of-fit test, help determine how well your model matches the observed data. It’s another way to ensure your detective work is accurate and reliable.</li>
    </ul>
    <p>Using these performance analytics, you can refine your meta-regression model to ensure it provides the most accurate and insightful conclusions. It’s your final check to make sure your detective work is spot on.</p>

    <p>By using these tools and methods, you can uncover the underlying factors that influence study results, ensuring a deeper and more nuanced understanding of the research data. Just like a good detective, you'll be able to see beyond the surface and understand the full story.</p>
  ")
  })
  
  output$pubtext <- renderUI({
    HTML("
    <h2>Exploring Publication Bias in Meta-Analysis</h2>
    <p>When conducting a meta-analysis, understanding publication bias is crucial. It’s like trying to get an accurate picture of your friend’s favorite movies but only hearing about the blockbuster hits. Let's dive into some tools and plots that help uncover the hidden gems (or biases) in the research world.</p>

    <h3>Publication Bias Text Results</h3>
    <p>The publication bias text results provide a comprehensive summary of key statistics and indicators that reveal the presence and extent of publication bias in your meta-analysis. Think of it as the critical review section where all the underlying biases and potential issues are laid bare.</p>
    <p>Here are some of the key components you might encounter in the text results:</p>
    <ul>
      <li><strong>Egger’s Test:</strong> This statistical test checks for asymmetry in the funnel plot. A significant result suggests that smaller studies may be missing, likely due to publication bias. It's like finding out that the only movies your friends didn’t mention are the ones they didn’t like much.</li>
      <li><strong>Begg’s Test:</strong> Another test for funnel plot asymmetry, Begg’s test is less sensitive than Egger’s but still useful. It’s like getting a second opinion on whether there's a skew in the studies being reported.</li>
      <li><strong>Trim and Fill Analysis:</strong> This method not only identifies but also corrects for funnel plot asymmetry by 'trimming' the asymmetric studies and 'filling' in the gaps with estimated missing studies. The adjusted results give a more accurate picture, akin to getting a more balanced list of movies, including those not initially mentioned.</li>
      <li><strong>Fail-safe N:</strong> This statistic tells you how many non-significant studies would be needed to nullify the meta-analysis results. If the number is high, your findings are robust. It’s like knowing how many bad reviews would be needed to make you doubt that a blockbuster is actually good.</li>
    </ul>
    <p>These text results are crucial as they help you understand whether the conclusions drawn from your meta-analysis are likely to be influenced by the selective publication of studies. It’s your first step in ensuring that you have a complete and unbiased picture of the research landscape.</p>

    <h3>Visualization Tools for Publication Bias</h3>
    <p>Visual tools provide a more intuitive understanding of publication bias. Let’s explore some of these plots that help you see the bigger picture:</p>

    <h3>Publication Bias Funnel Plot</h3>
    <p>Imagine a funnel plot as a scatterplot where each study is a dot. If there’s no bias, the dots will form a symmetrical funnel shape. If the plot looks lopsided, it might be hinting that some studies are missing—like your friends only sharing their favorite blockbuster movies and skipping the indie flicks. This plot is your first visual check for bias, helping you spot any obvious imbalances at a glance.</p>
    <p>The idea is simple: more precise studies (usually larger ones) should cluster around the true effect size, while smaller, less precise studies should scatter more widely at the bottom of the funnel. If studies are missing—often those with non-significant results—you’ll see an asymmetry, like a funnel missing one side. This asymmetry is a red flag for publication bias.</p>

    <h3>Publication Bias Trim and Fill</h3>
    <p>The trim and fill method is like adding missing pieces to a jigsaw puzzle. It identifies and estimates the number of missing studies to adjust the overall effect size. This way, you get a more complete picture, accounting for the studies that didn’t make it to publication. Imagine your friend tells you only about the exciting scenes from a movie, and you have to guess what happened in the dull parts they skipped. Trim and fill helps by estimating what those skipped parts might look like and adding them back in to give a fuller story.</p>
    <p>In practical terms, the method trims the asymmetric studies that are causing the funnel to be uneven and then fills in the gap with hypothetical studies that balance the funnel. The adjusted meta-analysis provides a more honest estimate of the effect size, considering the bias. It’s like getting a sneak peek at the deleted scenes that didn’t make it to the final cut.</p>

    <h3>Publication Bias Limit Meta-Analysis Curve</h3>
    <p>This technique plots the effect sizes and their standard errors to assess bias. The curve helps to identify any systematic bias in the studies. It's like plotting all your friends’ movie reviews to see if there’s a trend in their preferences. You might notice that they always rate action movies higher, suggesting a bias in their ratings.</p>
    <p>The limit meta-analysis curve provides a visual way to understand how the effect sizes change with varying levels of precision. If the curve indicates that more precise studies show a different effect than less precise ones, it’s a clue that publication bias or other small-study effects might be at play. It's akin to seeing if all the high-budget blockbusters get rave reviews while the low-budget films are overlooked, revealing a bias in the preferences or availability of reviews.</p>

    <h3>Advanced Methods for Detecting Publication Bias</h3>
    <p>For those who want to delve even deeper into the analysis of publication bias, here are some advanced methods that offer a more nuanced look:</p>

    <h3>Publication Bias Limit Meta-Analysis Curve and Shrunken</h3>
    <p>Similar to the regular limit meta-analysis curve but with a twist: it adjusts or 'shrinks' the effect sizes to correct for the bias. Think of it as getting your friends to reassess their movie ratings after considering the hidden gems they initially overlooked. This method fine-tunes the analysis to account for the missing studies, giving a more balanced view of the data.</p>
    <p>By shrinking the effect sizes, this method reduces the inflation caused by publication bias. It’s like asking your friends to rate movies not just based on the initial hype but also considering those underrated films they might have ignored. The adjusted curve then provides a more accurate reflection of the true effect size, free from the skew caused by missing studies.</p>

    <h3>Publication Bias P-Curve</h3>
    <p>The P-curve analyzes the distribution of p-values from the studies to determine if there’s evidence of p-hacking or selective reporting. It’s like checking if your friends are only telling you about movies they rated highly, skewing your perception of what’s actually good.</p>
    <p>By looking at the distribution of statistically significant p-values, the P-curve helps detect if there’s a suspiciously high number of just-significant results, suggesting that studies with non-significant results might be underreported. It’s like realizing that all your friends’ favorite movies have just the right amount of action or drama to barely make it interesting, making you wonder if they’re only remembering the highlights.</p>
    <p>The P-curve provides insights into the credibility of the findings by showing if the distribution of significant p-values matches what would be expected if the true effect is real and not just a result of selective reporting. It’s a tool to ensure that what you see is genuinely reflective of the entire spectrum of research, not just the cherry-picked highlights.</p>
    
    <p>Each of these tools and plots helps you uncover the hidden biases in your meta-analysis, ensuring you get a balanced view of all the evidence. They’re your statistical magnifying glass, helping you spot where the research picture might be a bit distorted. Just like making sure you hear about all the movies—blockbusters and indie films alike—these methods help ensure your meta-analysis captures the full story, warts and all.</p>
  ")
  })
  
  output$htext <- renderUI({
    HTML("
    <h2>Exploring Heterogeneity in Meta-Analysis</h2>
    <p>When conducting a meta-analysis, understanding heterogeneity is crucial. It's like figuring out why your group of friends all have different tastes in movies. Let's dive into some tools and plots that help us get to the bottom of this variability.</p>

    <h3>Heterogeneity Text Results</h3>
    <p>This is your basic report card. It gives you key stats like I² and tau², telling you how much the study results vary and how confident you can be in the overall findings. Think of it as the vital signs check before you dive deeper.</p>

    <h3>Heterogeneity Baujat Plot</h3>
    <p>Imagine a Baujat plot as a spotlight on the troublemakers. It shows which studies contribute most to the heterogeneity and influence the meta-analysis result. If one study is causing all the drama, this plot will point it out.</p>

    <h3>Heterogeneity Influence Diagnostics</h3>
    <p>These diagnostics are like your group therapy sessions, revealing how each study affects the overall meta-analysis. You can see which studies are steady contributors and which ones might be skewing the results. It's all about understanding individual impacts.</p>

    <h3>Heterogeneity Effect Size Forest Plot</h3>
    <p>This classic plot shows the effect sizes of all studies in a meta-analysis. It's like lining up all your friends and seeing how their movie preferences vary. Each line represents a study, showing its effect size and confidence interval, with the overall effect at the bottom.</p>

    <h3>Heterogeneity I² Forest Plot</h3>
    <p>This plot is similar to the effect size forest plot but with a twist. It includes I² values, indicating the percentage of variation across studies that's due to heterogeneity rather than chance. Think of it as adding a layer of analysis to understand how different everyone's tastes really are.</p>

    <h3>Heterogeneity L'Abbé Plot</h3>
    <p>The L'Abbé plot is like a scatterplot where you compare the control group's event rate to the treatment group's event rate for each study. It helps visualize the consistency of treatment effects across studies. It's a great way to spot patterns or outliers.</p>

    <h3>Heterogeneity Radial Plot</h3>
    <p>A radial plot is your meta-analysis compass. It plots the standardized effect sizes against precision, helping to identify outliers and influential studies. It's particularly useful for spotting those studies that might be throwing off your overall results.</p>

    <p>Each of these tools and plots helps you dissect the variability in your meta-analysis, ensuring you understand why studies differ and how that affects your overall findings. Think of them as the detective tools in your statistical toolkit, each one helping to piece together the puzzle of heterogeneity.</p>
    ")
  })
  
  
  output$text <- renderUI({
    HTML("
    <h2>A Glimpse Into the World of Meta-Analysis</h2>
    <p>
    You can use the text output to write our results using either of these GPT
    https://chatgpt.com/g/g-k7FryNboI-786miii-meta-results-writer
    https://chatgpt.com/g/g-GBxPpwHBY-risk-ratio-results-writer
    
    Also you can do risk of bias with these tools
    
    https://chatgpt.com/g/g-Fcw4nVGwI-786miii-newcastle-ottawa-risk-of-bias?utm_source=gptshunter.com
    
    
    Imagine each study in a meta-analysis is like a guest at a dinner party; they all bring something to the table, but not all of their contributions weigh equally. Let's decode what they're serving up and why some of them might get the bigger slice of cake (or, in our case, a bigger say in the results).</p>

    <h3>Study-Specific Data:</h3>
    <ul>
      <li><strong>Risk Ratios (RR) and Confidence Intervals:</strong> Each study provides a Risk Ratio, telling us how much the odds of an event increase (or decrease) with a particular intervention. The confidence intervals wrap this ratio in a cozy blanket of 'we're pretty sure it's around this value,' giving a range that says, 'Look here, but with a grain of salt.'</li>
      <li><strong>Weights:</strong> Not all studies are created equal. Those with more precise estimates (tight confidence intervals) often carry more weight in the analysis. It’s a bit like in group projects; the person who does the most work often has the most say.</li>
    </ul>

    <h3>Aggregated Insights:</h3>
    <p>After all studies chime in, we get the big picture:</p>
    <ul>
      <li><strong>Total Number of Studies and Observations:</strong> More studies and observations typically mean more robust conclusions. Think of it as gathering a larger consensus from a crowd—more voices can provide a clearer answer.</li>
      <li><strong>Overall Effect:</strong> This is where we see what all the studies, when considered together, suggest about the intervention. Is it effective? Is it not? This pooled estimate, represented by a snazzy diamond in many plots, gives us the consensus of the collected research.</li>
    </ul>

    <h3>Dealing with Differences - Heterogeneity:</h3>
    <p>Sometimes, studies don’t all sing the same tune, and this is where heterogeneity metrics come into play:</p>
    <ul>
      <li><strong>Tau² and I²:</strong> These are the gossipmongers of meta-analysis. Tau² whispers about the actual differences in effect sizes behind the scenes, while I² tells us what percentage of these differences is due to genuine disagreement rather than just random noise.</li>
      <li><strong>Q-test:</strong> This is like asking, 'Are you all really disagreeing that much, or is it just a misunderstanding?' A significant result means, yes, they truly don’t see eye to eye.</li>
    </ul>

    <h3>Methodological Fine Print:</h3>
    <p>The methods used can affect the final story told by the meta-analysis. Whether it’s the inverse variance method or another statistical approach, each has its own way of handling the data—kind of like different chefs might treat the same ingredients differently to make a dish uniquely their own.</p>

    <p>In essence, a meta-analysis tries to put together a coherent narrative from diverse research studies, assessing both their individual contributions and how they combine to answer a broader question. It’s a bit like creating a mosaic where every piece matters; only here, the pieces are studies, and the picture they form helps guide medical decisions.</p>
    ")
  })
  
  
  output$model <- renderUI({
    HTML("
    <h2>Choosing the Right Tools for Meta-Analysis</h2>
    <p>Just like picking the right instrument in surgery, choosing the right statistical tool in meta-analysis can significantly affect the outcome. Here’s a breakdown of some key choices you might face:</p>

    <h3>Effect Measure</h3>
    <p>Think of this as choosing whether you want to measure temperature in Celsius or Fahrenheit. It’s about picking the metric that best communicates the effect of an intervention.</p>
    <ul>
      <li><strong>Risk Ratio (RR) :</strong> Default. This tells you how many times more likely an event is to occur in the treatment group compared to the control. If RR is above 1, it’s like saying, 'Heads up! This might actually do something.'</li>
      <li><strong>Odds Ratio (OR):</strong> Similar to RR but slightly more complicated. It’s used especially when events are rare. An OR above 1 suggests that the event is more likely in the treatment group, like finding a four-leaf clover in a field of threes.</li>
    </ul>
<iframe width='560' height='315' src='https://www.youtube.com/embed/gZTAAuNZWy4' frameborder='0' allowfullscreen></iframe>
    <h3>Combination Method</h3>
    <p>Different strokes for different folks—or in this case, different methods for different datasets:</p>
    <ul>
      <li><strong>Mantel-Haenszel (MH) :</strong> Default for Risk Ratio. A steady and reliable method, great for when you have odds ratios and want to keep things straightforward.</li>
      <li><strong>Inverse Variance (Inverse):</strong> This method likes precision and gives more weight to studies that are more certain about their estimates. It’s the perfectionist of the group.</li>
      <li><strong>Peto's Method (Peto)  :</strong> Use for Odds Ratio. Optimal for rare events, because who said you can’t find patterns in rarity?</li>
      <li><strong>General Linear Mixed Models (GLMM):</strong> For the times when your data is as complex as a season finale of your favorite medical drama. It handles both fixed and random effects with aplomb.</li>
      <li><strong>Sidik-Jonkman (SSW):</strong> The choice for handling inconsistency like a champ. When your studies are as varied as patient symptoms, SSW steps in to make sense of the chaos.</li>
    </ul>

    <h3>Heterogeneity Method</h3>
    <p>Because not all studies are the perfect match, and sometimes you have to measure how much they really agree with each other:</p>
    <ul>
      <li><strong>DerSimonian-Laird (DL):</strong> A classic approach that says, 'Let’s try and get along despite our differences.' Suitable for most scenarios and doesn’t get too fussy about small sample sizes.</li>
      <li><strong>Restricted Maximum Likelihood (REML):</strong> This method takes a more refined approach to estimating variability, ideal when you suspect there’s more beneath the surface.</li>
      <li><strong>Paule-Mandel (PM)  :</strong> Default. A newer kid on the block, trying to make a name by offering an alternative way to look at heterogeneity.</li>
      <li><strong>Empirical Bayes (EB):</strong> For those who like to mix a bit of historical wisdom with contemporary data. It uses past data to inform the current analysis, a wise old sage in statistical form.</li>
      <li><strong>Sidik-Jonkman (SJ):</strong> When the going gets tough with heterogeneity, SJ offers a robust way to say, 'We can handle the truth!'</li>
    </ul>

    <h3>Effect Model</h3>
    <p>Choosing between these models is like deciding whether to view a medical issue through a microscope or a telescope:</p>
    <ul>
      <li><strong>Random Effects (RE):</strong> When you anticipate that not all studies are cut from the same cloth, RE allows each study its own unique effect size, recognizing that variety is the spice of life—and research.</li>
      <li><strong>Fixed Effects (FE):</strong> If you believe in uniformity across studies, that they all measure the same underlying effect, then FE is your go-to. It’s like saying, 'We’re all in this together, and we agree on the basics.'</li>
    </ul>
    <p>Each choice you make in setting up your meta-analysis could change how you interpret the vast sea of research. It’s akin to picking your gear before a dive—you need the right tools to properly explore the depths of data and come back with the treasure of true insights.</p>
    
    ")
  })
  
  
  output$forest <- renderUI({
    HTML("
    <h2>Introduction to Forest Plots</h2>
    <p>In the creation of the heavens and the earth and in the alternation of the night and the day there are indeed Signs for men of understanding(The Holy Quran)</p>
    <p><img src='forestplot.png' alt='Forest Plot Example' style='width:100%;max-width:600px;'></p>
    <p>Forest plots, also humorously dubbed 'blobbograms', are a staple in meta-analysis, essentially serving as a high-level overview of multiple studies on a single question. These plots provide a clear visual representation of individual study results alongside a collective summary, making them crucial in medical research where detail and precision are paramount.</p>
    <ul>
      <li><strong>Study Labels:</strong> Displayed on the left, each study is listed by the primary author's name and the year of publication, organizing the research lineage neatly.</li>
      <li><strong>Effect Sizes:</strong> Central to the plot, these figures (like odds ratios or risk ratios) quantify the impact of interventions. For instance, an odds ratio above 1 might quietly hint at a beneficial effect of a treatment.</li>
      <li><strong>Confidence Intervals:</strong> These horizontal lines suggest the reliability of effect sizes. If they shy away from the line of no effect, the results are statistically significant, whispering 'there's something here worth considering.'</li>
      <li><strong>Weights:</strong> Not all studies are created equal in a forest plot. Those with larger sample sizes and precise estimates carry more weight, their influence visibly larger on the plot.</li>
      <li><strong>Diamond:</strong> This figure represents the pooled estimate of effect sizes from all included studies. Its position and width offer a quick visual cue about the overall results, akin to reading the room in a glance.</li>
    </ul>
    <h2>How to Read a Forest Plot</h2>
    
  
    <iframe width='560' height='315' src='https://www.youtube.com/embed/s-yrMJEOGTY' frameborder='0' allowfullscreen></iframe>
    
    <p>Interpreting a forest plot involves a few steps akin to reading a complex watch:</p>
    <ul>
      <li><strong>Locate the Line of No Effect:</strong> This line serves as a benchmark for assessing the effectiveness of an intervention. It's a sober reminder of what neutrality looks like in numerical terms.</li>
      <li><strong>Assess Individual Study Results:</strong> Examine where the confidence intervals land in relation to this line. Those that don’t cross it are subtly asserting their statistical significance.</li>
      <li><strong>Evaluate the Diamond:</strong> The position and spread of the diamond synthesize the overall data, providing a broad perspective at a glance. If it avoids the line of no effect, the combined data suggests a clear direction.</li>
      <li><strong>Consider the Weights:</strong> The size of the squares reflects the influence of each study, a gentle nod to the idea that more data typically provides clearer answers.</li>
    </ul>
        <h2>Forest Plot with diamond to the right of the line indicating a positive effect</h2>
      <p><img src='p2.png' alt='Forest Plot Example' style='width:100%;max-width:600px;'></p>
       <h2>Forest Plot with diamond to the left of the line indicating a negative effect</h2>
      <p><img src='P3.png' alt='Forest Plot Example' style='width:100%;max-width:800px;'></p>
       <iframe width='560' height='315' src='https://www.youtube.com/embed/3-lkZTGBE38' frameborder='0' allowfullscreen></iframe>
  
    <h2>Statistical Considerations in Forest Plots</h2>
    <p>Delving into the statistics of forest plots unveils further depth:</p>
    <ul>
      <li><strong>Heterogeneity:</strong> This metric measures the variability among study outcomes, highlighting whether it’s prudent to combine these studies at all. High heterogeneity can be a quiet murmur for caution, suggesting differences that might need further exploration.</li>
     <h2>Heterogeneity</h2>
      <p><img src='P4.png' alt='Forest Plot Example' style='width:100%;max-width:1000px;'></p>
      
      <li><strong> Do the following exercise to generate your first forest plot </li>
    </ul>
  <iframe width='560' height='315' src='https://www.youtube.com/embed/r3plpy2YSGI' frameborder='0' allowfullscreen></iframe>
   
    ")
    
  })
  
  
  
  
  library(metafor)  # For meta-analysis via rma and forest plots
  library(dmetar)   # For multimodel inference
  library(dplyr)    # For data manipulation
  library(ggplot2)  # For advanced plotting
  
  output$MA <- renderPlot({
    req(dataInput())
    dat <- dataInput()
    
    # Convert counts to log relative risks and their variances
    dat <- dat %>%
      mutate(
        logRR = log((eventintervention + 0.5) / (totalintervention - eventintervention + 0.5) /
                      ((eventcontrol + 0.5) / (totalcontrol - eventcontrol + 0.5))),
        varLogRR = 1/(eventintervention + 0.5) + 1/(totalintervention - eventintervention + 0.5) +
          1/(eventcontrol + 0.5) + 1/(totalcontrol - eventcontrol + 0.5)
      )
    
    # Fit the random-effects model using rma
    rma.model <- rma(yi = logRR, vi = varLogRR, data = dat, method = "REML")
    
    # Check if the moderators are present in the dataset
    if(all(c("Reg", "Reg2", "Reg3") %in% names(dat))) {
      # Fit the random-effects model with moderators
      rma.model.with.mods <- rma(yi = logRR, vi = varLogRR, mods = ~ Reg + Reg2 + Reg3, data = dat, method = "REML")
      
      # Plot the results from the random-effects model with moderators
      forest(rma.model.with.mods, main="Forest Plot of RMA Model with Moderators", 
             xlab="Log Relative Risk", slab=paste(dat$author))
      
    } else {
      # If moderators are not available, use the basic model for plotting
      forest(rma.model, main="Forest Plot", 
             xlab="Log Relative Risk", slab=paste(dat$author))
    }
  })
  
  library(metafor)  # For meta-analysis via rma and forest plots
  library(dmetar)   # For multimodel inference
  library(dplyr)    # For data manipulation
  library(ggplot2)  # For advanced plotting
  
  output$metaregmultiple <- renderPlot({
    req(dataInput())
    dat <- dataInput()
    
    # Convert counts to log relative risks and their variances
    dat <- dat %>%
      mutate(
        logRR = log((eventintervention + 0.5) / (totalintervention - eventintervention + 0.5) /
                      ((eventcontrol + 0.5) / (totalcontrol - eventcontrol + 0.5))),
        varLogRR = 1/(eventintervention + 0.5) + 1/(totalintervention - eventintervention + 0.5) +
          1/(eventcontrol + 0.5) + 1/(totalcontrol - eventcontrol + 0.5)
      )
    
    # Fit the random-effects model using rma
    rma.model <- rma(yi = logRR, vi = varLogRR, data = dat, method = "REML")
    
    # Check if the moderators are present in the dataset
    if(all(c("Reg", "Reg2", "Reg3") %in% names(dat))) {
      # Fit the random-effects model with moderators
      rma.model.with.mods <- rma(yi = logRR, vi = varLogRR, mods = ~ Reg + Reg2 + Reg3, data = dat, method = "REML")
      
      # Plot the results from the random-effects model with moderators
      forest(rma.model.with.mods, main="Forest Plot of RMA Model with Moderators", 
             xlab="Log Relative Risk", slab=paste(dat$author))
      
    } else {
      # If moderators are not available, use the basic model for plotting
      forest(rma.model, main="Forest Plot of Basic RMA Model", 
             xlab="Log Relative Risk", slab=paste(dat$author))
    }
  })
  
  output$resultText <- renderPrint({
    req(dataInput())
    dat <- dataInput()
    m.bin <- metabin(event.e = dat$eventintervention,
                     n.e = dat$totalintervention,
                     event.c = dat$eventcontrol,
                     n.c = dat$totalcontrol,
                     studlab = dat$author,
                     data = dat,
                     sm = input$effectMeasure,
                     method = input$method,
                     method.tau = input$method.tau,
                     comb.fixed = input$effectModel == "FE",
                     comb.random = input$effectModel == "RE")
    
    
    
    
    print(summary(m.bin, 
                  digits = 2,
                  text.tau2 = TRUE,
                  text.I2 = TRUE,
                  text.Q = TRUE))
  })
  
  
  library(magrittr)       # For the pipe operator
  library(PerformanceAnalytics)  # For chart.Correlation
  library(metafor)        # For rma (random-effects model)
  
  output$mmreg <- renderPlot({
    req(dataInput())
    dat <- dataInput()
    # Convert counts to log relative risks and their variances
    dat <- dat %>%
      dplyr::mutate(
        logRR = log((eventintervention + 0.5) / (totalintervention - eventintervention + 0.5) /
                      ((eventcontrol + 0.5) / (totalcontrol - eventcontrol + 0.5))),
        varLogRR = 1/(eventintervention + 0.5) + 1/(totalintervention - eventintervention + 0.5) +
          1/(eventcontrol + 0.5) + 1/(totalcontrol - eventcontrol + 0.5)
      )
    
    # Fit the random-effects model using rma
    rma.model <- rma(yi = dat$logRR, vi = dat$varLogRR, data = dat, method = "REML")
    
    # Check if moderators are present
    if(all(c("Reg", "Reg2", "Reg3") %in% names(dat))) {
      # Fit the random-effects model with moderators
      rma.model.with.mods <- rma(yi = dat$logRR, vi = dat$varLogRR, mods = ~ Reg + Reg2 + Reg3, data = dat, method = "REML")
      
      # Extracting the fitted model data for correlation chart
      fit.data <- cbind(dat[ ,c("Reg", "Reg2", "Reg3")], Residuals = resid(rma.model.with.mods))
      
      # Generate correlation plot for the moderators and the residuals
      fit.data %>% 
        PerformanceAnalytics::chart.Correlation(histogram=TRUE, pch=19)
      
    } else {
      # If moderators are not available, use basic data for correlation chart
      base.data <- dat %>%
        dplyr::select(logRR, varLogRR)
      
      # Generate correlation plot for log relative risks and their variances
      base.data %>% 
        PerformanceAnalytics::chart.Correlation(histogram=TRUE, pch=19)
    }
  })
  
  
  library(metafor)  # For meta-analysis via rma
  library(dmetar)   # For multimodel inference
  library(dplyr)    # For data manipulation
  
  output$metaregmultiple <- renderPrint({
    req(dataInput())
    dat <- dataInput()
    
    # Convert counts to log relative risks and their variances
    dat <- dat %>%
      mutate(
        logRR = log((eventintervention + 0.5) / (totalintervention - eventintervention + 0.5) /
                      ((eventcontrol + 0.5) / (totalcontrol - eventcontrol + 0.5))),
        varLogRR = 1/(eventintervention + 0.5) + 1/(totalintervention - eventintervention + 0.5) +
          1/(eventcontrol + 0.5) + 1/(totalcontrol - eventcontrol + 0.5)
      )
    
    # Fit the random-effects model using rma
    rma.model <- rma(yi = logRR, vi = varLogRR, data = dat, method = "REML")
    
    # Check if the moderators are present in the dataset
    if(all(c("Reg", "Reg2", "Reg3") %in% names(dat))) {
      cat("\nModerator Analysis:\n")
      # Fit the random-effects model with moderators
      rma.model.with.mods <- rma(yi = logRR, vi = varLogRR, mods = ~ Reg + Reg2 + Reg3, data = dat, method = "REML")
      print(summary(rma.model.with.mods))
      
      # Compute the correlation matrix for the moderators
      cat("\nCorrelation Matrix for Moderators:\n")
      correlation_matrix <- cor(dat[,c("Reg", "Reg2", "Reg3")])
      print(correlation_matrix)
      
      # Perform multimodel inference using dmetar
      cat("\nMultimodel Inference from dmetar:\n")
      multi.inference.results <- multimodel.inference(rma.model.with.mods)
      print(multi.inference.results)
      
    } else {
      cat("The specified moderators are not present in the dataset.")
    }
    
    # Perform a permutation test for the overall effect size
    cat("\nPermutation Test for the Overall Effect Size:\n")
    perm.test.results <- permutest(rma.model, nperm = 999)
    print(perm.test.results)
  })
  
  
  
  
  output$RMA <- renderPrint({
    req(dataInput())
    dat <- dataInput()
    
    # Assuming 'eventintervention', 'totalintervention', 'eventcontrol', 'totalcontrol' are columns in your dataset
    # Convert counts to log odds ratios and their variances
    dat <- dat %>%
      mutate(
        logRR = log((eventintervention + 0.5) / (totalintervention - eventintervention + 0.5) /
                      ((eventcontrol + 0.5) / (totalcontrol - eventcontrol + 0.5))),
        varLogRR = 1/(eventintervention + 0.5) + 1/(totalintervention - eventintervention + 0.5) +
          1/(eventcontrol + 0.5) + 1/(totalcontrol - eventcontrol + 0.5)
      )
    
    # Fit the random-effects model using rma
    rma.model <- rma(yi = logRR, vi = varLogRR, data = dat, method = "REML")
    
    # Print a detailed summary of the fitted model
    cat("Detailed Summary of Random-Effects Meta-Analysis:\n")
    cat("Model Results:\n")
    print(summary(rma.model))
    
    # Additional details can be included such as confidence intervals, prediction intervals, etc.
    cat("\nConfidence Interval for the Overall Effect Size:\n")
    confint(rma.model)
    
    
    
    cat("\nTest for Residual Heterogeneity:\n")
    print(anova(rma.model))
    
    cat("\nTest for Overall Effect:\n")
    print(rma.model$test)
    
    # If you have moderators in your model, you can also include their results
    # For example:
    # cat("\nModerator Analysis:\n")
    # print(rma.model$mods)
    
    # Include diagnostics for influential studies
    cat("\nInfluential Studies Diagnostics:\n")
    inf <- influence(rma.model)
    print(inf)
    
    
  })
  output$RMAodd <- renderPrint({
    req(dataInput())
    dat <- dataInput()
    
    # Assuming 'eventintervention', 'totalintervention', 'eventcontrol', 'totalcontrol' are columns in your dataset
    # Convert counts to log odds ratios and their variances
    dat <- dat %>%
      mutate(
        logOR = log((eventintervention + 0.5) / (totalintervention - eventintervention + 0.5) /
                      ((eventcontrol + 0.5) / (totalcontrol - eventcontrol + 0.5))),
        varLogOR = 1/(eventintervention + 0.5) + 1/(totalintervention - eventintervention + 0.5) +
          1/(eventcontrol + 0.5) + 1/(totalcontrol - eventcontrol + 0.5)
      )
    
    # Fit the random-effects model using rma
    rma.model <- rma(yi = logOR, vi = varLogOR, data = dat, method = "REML")
    
    # Print a detailed summary of the fitted model
    cat("Detailed Summary of Random-Effects Meta-Analysis:\n")
    cat("Model Results:\n")
    print(summary(rma.model))
    
    # Additional details can be included such as confidence intervals, prediction intervals, etc.
    cat("\nConfidence Interval for the Overall Effect Size:\n")
    confint(rma.model)
    
    
    
    cat("\nTest for Residual Heterogeneity:\n")
    print(anova(rma.model))
    
    cat("\nTest for Overall Effect:\n")
    print(rma.model$test)
    
    # If you have moderators in your model, you can also include their results
    # For example:
    # cat("\nModerator Analysis:\n")
    # print(rma.model$mods)
    
    # Include diagnostics for influential studies
    cat("\nInfluential Studies Diagnostics:\n")
    inf <- influence(rma.model)
    print(inf)
    
    
  })
  
  output$hetro <- renderPrint({
    req(dataInput())
    dat <- dataInput()
    m.bin <- metabin(event.e = dat$eventintervention,
                     n.e = dat$totalintervention,
                     event.c = dat$eventcontrol,
                     n.c = dat$totalcontrol,
                     studlab = dat$author,
                     data = dat,
                     sm = input$effectMeasure,
                     method = input$method,
                     method.tau = input$method.tau,
                     comb.fixed = input$effectModel == "FE",
                     comb.random = input$effectModel == "RE")
    
    # Eggers test for funnel plot asymmetry
    m.genp <- eggers.test(m.bin)
    
    # Find outliers in the meta-analysis
    m.gen.out <- find.outliers(m.bin)
    
    # Influence analysis
    m.gen.inf <- InfluenceAnalysis(m.bin, random = TRUE)
    
    # Print Eggers test summary
    print(summary(m.genp))
    
    # Print outlier analysis summary
    print(summary(m.gen.out))
    
    # Print influence analysis results including DFFITS and Cook's Distance
    print(summary(m.gen.inf))
    
    # Extract heterogeneity measures from meta-analysis results
    heterogeneity_stats <- list(
      "Cochrane's Q" = m.bin$Q,
      "I2" = m.bin$I2,
      "H2" = m.bin$H2,
      "Tau^2" = m.bin$tau^2,
      "Tau" = sqrt(m.bin$tau^2)
    )
    
    # Print heterogeneity statistics
    print(heterogeneity_stats)
  })
  
  
  output$pubbias <- renderPrint({
    req(dataInput())
    dat <- dataInput()
    m.bin <- metabin(event.e = dat$eventintervention,
                     n.e = dat$totalintervention,
                     event.c = dat$eventcontrol,
                     n.c = dat$totalcontrol,
                     studlab = dat$author,
                     data = dat,
                     sm = input$effectMeasure,
                     method = input$method,
                     method.tau = input$method.tau,
                     comb.fixed = input$effectModel == "FE",
                     comb.random = input$effectModel == "RE")
    
    m.limitmeta <-limitmeta(m.bin)
    # Use tryCatch to handle errors in pcurve
    p.curve <- tryCatch({
      pcurve(m.bin)
    }, error = function(e) {
      NULL  # Return NULL or a custom message if pcurve fails
    })
    
    print(summary( m.limitmeta))
    # Only print the p.curve summary if it was successful
    if (!is.null(p.curve)) {
      print(summary(p.curve))
    } else {
      cat("p-curve analysis could not be conducted.\n")
    }
  })
  
  output$metareg <- renderPrint({
    req(dataInput())
    dat <- dataInput()
    m.bin <- metabin(event.e = dat$eventintervention,
                     n.e = dat$totalintervention,
                     event.c = dat$eventcontrol,
                     n.c = dat$totalcontrol,
                     studlab = dat$author,
                     data = dat,
                     sm = input$effectMeasure,
                     method = input$method,
                     method.tau = input$method.tau,
                     comb.fixed = input$effectModel == "FE",
                     comb.random = input$effectModel == "RE")
    
    m.gen.reg <- metareg(m.bin, ~Reg)
    
    print(summary( m.gen.reg))
    
  })
  
  
  
  output$forestPlot <- renderPlot({
    req(dataInput())
    dat <- dataInput()
    m.bin <- metabin(event.e = dat$eventintervention,
                     n.e = dat$totalintervention,
                     event.c = dat$eventcontrol,
                     n.c = dat$totalcontrol,
                     studlab = author,
                     data = dat,
                     sm = input$effectMeasure,
                     method = input$method,
                     method.tau = input$method.tau,
                     comb.fixed = input$effectModel == "FE",
                     comb.random = input$effectModel == "RE")
    forest(m.bin, 
           col.diamond = input$col.diamond,
           col.diamond.lines = input$col.diamond.lines,
           col.square = input$col.square,
           col.square.lines = input$col.square.lines,
           col.study = input$col.study,
           col.circle = input$col.circle,
           col.circle.lines = input$col.circle.lines,
           labstudies = input$labstudies,
           cex = input$textsize,
           lwd = input$linewidth,
           label.left = input$label,
           label.right = input$labelr,
           xlim = input$xRange,
           prediction = TRUE )
  })
  
  output$forestPlotJAMA <- renderPlot({
    req(dataInput())
    dat <- dataInput()
    m.bin <- metabin(event.e = dat$eventintervention,
                     n.e = dat$totalintervention,
                     event.c = dat$eventcontrol,
                     n.c = dat$totalcontrol,
                     studlab = author,
                     data = dat,
                     sm = input$effectMeasure,
                     method = input$method,
                     method.tau = input$method.tau,
                     comb.fixed = input$effectModel == "FE",
                     comb.random = input$effectModel == "RE")
    forest(m.bin, 
           col.diamond = input$col.diamond,
           col.diamond.lines = input$col.diamond.lines,
           col.square = input$col.square,
           col.square.lines = input$col.square.lines,
           col.study = input$col.study,
           col.circle = input$col.circle,
           col.circle.lines = input$col.circle.lines,
           labstudies = input$labstudies,
           cex = input$textsize,
           lwd = input$linewidth,
           prediction = FALSE,
           layout = "JAMA")
    
  })
  
  output$forestPlotRevman5 <- renderPlot({
    req(dataInput())
    dat <- dataInput()
    m.bin <- metabin(event.e = dat$eventintervention,
                     n.e = dat$totalintervention,
                     event.c = dat$eventcontrol,
                     n.c = dat$totalcontrol,
                     studlab = dat$author,
                     data = dat,
                     sm = input$effectMeasure,
                     method = input$method,
                     method.tau = input$method.tau,
                     comb.fixed = input$effectModel == "FE",
                     comb.random = input$effectModel == "RE")
    forest(m.bin, 
           col.diamond = input$col.diamond,
           col.diamond.lines = input$col.diamond.lines,
           col.square = input$col.square,
           col.square.lines = input$col.square.lines,
           col.study = input$col.study,
           col.circle = input$col.circle,
           col.circle.lines = input$col.circle.lines,
           labstudies = input$labstudies,
           cex = input$textsize,
           lwd = input$linewidth,
           prediction = FALSE,
           layout = "RevMan5")
  }, height = reactive(input$plotHeight), width = reactive(input$plotWidth))
  
  output$downloadPlotRevman5 <- downloadHandler(
    filename = function() {
      paste("forest_plot_revman5", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file, width = input$plotWidth, height = input$plotHeight)
      dat <- dataInput()
      m.bin <- metabin(event.e = dat$eventintervention,
                       n.e = dat$totalintervention,
                       event.c = dat$eventcontrol,
                       n.c = dat$totalcontrol,
                       studlab = dat$author,
                       data = dat,
                       sm = input$effectMeasure,
                       method = input$method,
                       method.tau = input$method.tau,
                       comb.fixed = input$effectModel == "FE",
                       comb.random = input$effectModel == "RE")
      forest(m.bin, 
             col.diamond = input$col.diamond,
             col.diamond.lines = input$col.diamond.lines,
             col.square = input$col.square,
             col.square.lines = input$col.square.lines,
             col.study = input$col.study,
             col.circle = input$col.circle,
             col.circle.lines = input$col.circle.lines,
             labstudies = input$labstudies,
             cex = input$textsize,
             lwd = input$linewidth,
             prediction = FALSE,
             layout = "RevMan5")
      dev.off()
    }
  )
  
  output$forestPlotRevman5 <- renderPlot({
    req(dataInput())
    dat <- dataInput()
    m.bin <- metabin(event.e = dat$eventintervention,
                     n.e = dat$totalintervention,
                     event.c = dat$eventcontrol,
                     n.c = dat$totalcontrol,
                     studlab = dat$author,
                     data = dat,
                     sm = input$effectMeasure,
                     method = input$method,
                     method.tau = input$method.tau,
                     comb.fixed = input$effectModel == "FE",
                     comb.random = input$effectModel == "RE")
    forest(m.bin, 
           col.diamond = input$col.diamond,
           col.diamond.lines = input$col.diamond.lines,
           col.square = input$col.square,
           col.square.lines = input$col.square.lines,
           col.study = input$col.study,
           col.circle = input$col.circle,
           col.circle.lines = input$col.circle.lines,
           labstudies = input$labstudies,
           cex = input$textsize,
           lwd = input$linewidth,
           prediction = FALSE,
           layout = "RevMan5")
  }, height = reactive(input$plotHeight), width = reactive(input$plotWidth))
  
  output$downloadPlotJAMA <- downloadHandler(
    filename = function() {
      paste("forest_plot_JAMA", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file, width = input$plotWidth, height = input$plotHeight)
      dat <- dataInput()
      m.bin <- metabin(event.e = dat$eventintervention,
                       n.e = dat$totalintervention,
                       event.c = dat$eventcontrol,
                       n.c = dat$totalcontrol,
                       studlab = dat$author,
                       data = dat,
                       sm = input$effectMeasure,
                       method = input$method,
                       method.tau = input$method.tau,
                       comb.fixed = input$effectModel == "FE",
                       comb.random = input$effectModel == "RE")
      forest(m.bin, 
             col.diamond = input$col.diamond,
             col.diamond.lines = input$col.diamond.lines,
             col.square = input$col.square,
             col.square.lines = input$col.square.lines,
             col.study = input$col.study,
             col.circle = input$col.circle,
             col.circle.lines = input$col.circle.lines,
             labstudies = input$labstudies,
             cex = input$textsize,
             lwd = input$linewidth,
             prediction = FALSE,
             layout = "JAMA")
      dev.off()
    }
  )
  
  output$forestPlotRevman5 <- renderPlot({
    req(dataInput())
    dat <- dataInput()
    m.bin <- metabin(event.e = dat$eventintervention,
                     n.e = dat$totalintervention,
                     event.c = dat$eventcontrol,
                     n.c = dat$totalcontrol,
                     studlab = dat$author,
                     data = dat,
                     sm = input$effectMeasure,
                     method = input$method,
                     method.tau = input$method.tau,
                     comb.fixed = input$effectModel == "FE",
                     comb.random = input$effectModel == "RE")
    forest(m.bin, 
           col.diamond = input$col.diamond,
           col.diamond.lines = input$col.diamond.lines,
           col.square = input$col.square,
           col.square.lines = input$col.square.lines,
           col.study = input$col.study,
           col.circle = input$col.circle,
           col.circle.lines = input$col.circle.lines,
           labstudies = input$labstudies,
           cex = input$textsize,
           lwd = input$linewidth,
           prediction = FALSE,
           layout = "RevMan5")
  }, height = reactive(input$plotHeight), width = reactive(input$plotWidth))
  
  output$downloadPlotmeta <- downloadHandler(
    filename = function() {
      paste("forest_plot_meta", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file, width = input$plotWidth, height = input$plotHeight)
      dat <- dataInput()
      m.bin <- metabin(event.e = dat$eventintervention,
                       n.e = dat$totalintervention,
                       event.c = dat$eventcontrol,
                       n.c = dat$totalcontrol,
                       studlab = dat$author,
                       data = dat,
                       sm = input$effectMeasure,
                       method = input$method,
                       method.tau = input$method.tau,
                       comb.fixed = input$effectModel == "FE",
                       comb.random = input$effectModel == "RE")
      forest(m.bin, 
             col.diamond = input$col.diamond,
             col.diamond.lines = input$col.diamond.lines,
             col.square = input$col.square,
             col.square.lines = input$col.square.lines,
             col.study = input$col.study,
             col.circle = input$col.circle,
             col.circle.lines = input$col.circle.lines,
             labstudies = input$labstudies,
             cex = input$textsize,
             lwd = input$linewidth,
             prediction = FALSE)
      dev.off()
    }
  )
  
  
  output$beeswarm <- renderPlot({
    req(dataInput())
    dat <- dataInput()
    
    # Assuming 'author' is a column in 'dat'
    author <- dat$author
    
    # Run the meta-analysis
    m.bin <- metabin(event.e = dat$eventintervention,
                     n.e = dat$totalintervention,
                     event.c = dat$eventcontrol,
                     n.c = dat$totalcontrol,
                     studlab = author,
                     data = dat,
                     sm = input$effectMeasure,  # Effect measure from user input (e.g., "OR" for Odds Ratio)
                     method = input$method,  # Meta-analysis method from user input
                     method.tau = input$method.tau,
                     comb.fixed = input$effectModel == "FE",  # Fixed effect model
                     comb.random = input$effectModel == "RE")  # Random effects model
    
    # Prepare data for bee swarm plot
    summary_data <- summary(m.bin)
    plot_data <- data.frame(
      Study = dat$author,  # Use author data directly from input data
      TE = summary_data$TE,
      SE = summary_data$seTE,
      Weight = summary_data$w.fixed + summary_data$w.random  # Total weight, fixed + random
    )
    
    # Using ggbeeswarm to create a Bee Swarm Plot
    ggplot(plot_data, aes(x = factor(1), y = TE, size = Weight)) +  # 'factor(1)' to align all points vertically
      geom_beeswarm(cex = 1.5) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Add a reference line at no effect
      labs(x = "", y = "Effect Size (e.g., Odds Ratio)", title = "Bee Swarm Plot of Study Effects") +
      theme_minimal() +
      theme(axis.text.x = element_blank(), axis.title.x = element_blank())
  })
  output$BubblePlot <- renderPlot({
    req(dataInput())
    dat <- dataInput()
    m.bin <- metabin(event.e = dat$eventintervention,
                     n.e = dat$totalintervention,
                     event.c = dat$eventcontrol,
                     n.c = dat$totalcontrol,
                     studlab = author,
                     data = dat,
                     sm = input$effectMeasure,
                     method = input$method,
                     method.tau = input$method.tau,
                     comb.fixed = input$effectModel == "FE",
                     comb.random = input$effectModel == "RE")
    
    m.gen.reg <- metareg(m.bin, ~Reg)
    bubble(m.gen.reg, studlab = TRUE)
    
  })
  output$labbe <- renderPlot({
    req(dataInput())
    dat <- dataInput()
    m.bin <- metabin(event.e = dat$eventintervention,
                     n.e = dat$totalintervention,
                     event.c = dat$eventcontrol,
                     n.c = dat$totalcontrol,
                     studlab = author,
                     data = dat,
                     sm = input$effectMeasure,
                     method = input$method,
                     method.tau = input$method.tau,
                     comb.fixed = input$effectModel == "FE",
                     comb.random = input$effectModel == "RE")
    
    
    labbe(m.bin)
    
  })
  output$radial <- renderPlot({
    req(dataInput())
    dat <- dataInput()
    m.bin <- metabin(event.e = dat$eventintervention,
                     n.e = dat$totalintervention,
                     event.c = dat$eventcontrol,
                     n.c = dat$totalcontrol,
                     studlab = author,
                     data = dat,
                     sm = input$effectMeasure,
                     method = input$method,
                     method.tau = input$method.tau,
                     comb.fixed = input$effectModel == "FE",
                     comb.random = input$effectModel == "RE")
    
    
    radial(m.bin)
  })
  output$drapery <- renderPlot({
    req(dataInput())
    dat <- dataInput()
    m.bin <- metabin(event.e = dat$eventintervention,
                     n.e = dat$totalintervention,
                     event.c = dat$eventcontrol,
                     n.c = dat$totalcontrol,
                     studlab = author,
                     data = dat,
                     sm = input$effectMeasure,
                     method = input$method,
                     method.tau = input$method.tau,
                     comb.fixed = input$effectModel == "FE",
                     comb.random = input$effectModel == "RE")
    
    drapery(m.bin, 
            labels = "studlab",
            type = "pval", 
            legend = FALSE)
  })
  
  output$cforest <- renderPlot({
    req(dataInput())
    dat <- dataInput()
    
    # Run the meta-analysis
    m.bin <- metabin(event.e = dat$eventintervention,
                     n.e = dat$totalintervention,
                     event.c = dat$eventcontrol,
                     n.c = dat$totalcontrol,
                     studlab = author,
                     data = dat,
                     sm = input$effectMeasure,  # Effect measure from user input (e.g., "OR" for Odds Ratio)
                     method = input$method,  # Meta-analysis method from user input
                     method.tau = input$method.tau,
                     comb.fixed = input$effectModel == "FE",  # Fixed effect model
                     comb.random = input$effectModel == "RE")  # Random effects model
    
    # Calculate z-values
    z_values <- summary(m.bin)$TE / summary(m.bin)$seTE
    
    # Calculate p-values
    p_values <- 2 * (1 - pnorm(abs(z_values)))
    
    # Prepare data for plotting
    # Ensure n_treatment and n_control are correctly defined or calculated from 'dat'
    n_treatment <- dat$totalintervention
    n_control <- dat$totalcontrol
    plot_data <- data.frame(
      SampleSize = n_treatment + n_control,
      PValue = p_values
    )
    
    # Create the Albatross Plot
    ggplot(plot_data, aes(x = log(SampleSize), y = -log10(PValue))) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, color = "blue") +
      labs(x = "Log of Sample Size", y = "-Log10 of P-value", title = "Albatross Plot") +
      theme_minimal()
  })
  
  output$Baujatplot <- renderPlot({
    req(dataInput())
    dat <- dataInput()
    m.bin <- metabin(event.e = dat$eventintervention,
                     n.e = dat$totalintervention,
                     event.c = dat$eventcontrol,
                     n.c = dat$totalcontrol,
                     studlab = author,
                     data = dat,
                     sm = input$effectMeasure,
                     method = input$method,
                     method.tau = input$method.tau,
                     comb.fixed = input$effectModel == "FE",
                     comb.random = input$effectModel == "RE")
    m.gen.inf <- InfluenceAnalysis(m.bin, random = TRUE)
    plot(m.gen.inf, "baujat")
    
  })
  
  output$Influence <- renderPlot({
    req(dataInput())
    dat <- dataInput()
    m.bin <- metabin(event.e = dat$eventintervention,
                     n.e = dat$totalintervention,
                     event.c = dat$eventcontrol,
                     n.c = dat$totalcontrol,
                     studlab = author,
                     data = dat,
                     sm = input$effectMeasure,
                     method = input$method,
                     method.tau = input$method.tau,
                     comb.fixed = input$effectModel == "FE",
                     comb.random = input$effectModel == "RE")
    m.gen.inf <- InfluenceAnalysis(m.bin, random = TRUE)
    plot(m.gen.inf, "influence")
    
  })
  output$effectsize <- renderPlot({
    req(dataInput())
    dat <- dataInput()
    m.bin <- metabin(event.e = dat$eventintervention,
                     n.e = dat$totalintervention,
                     event.c = dat$eventcontrol,
                     n.c = dat$totalcontrol,
                     studlab = author,
                     data = dat,
                     sm = input$effectMeasure,
                     method = input$method,
                     method.tau = input$method.tau,
                     comb.fixed = input$effectModel == "FE",
                     comb.random = input$effectModel == "RE")
    m.gen.inf <- InfluenceAnalysis(m.bin, random = TRUE)
    plot(m.gen.inf, "es")
    
  })
  output$I2 <- renderPlot({
    req(dataInput())
    dat <- dataInput()
    m.bin <- metabin(event.e = dat$eventintervention,
                     n.e = dat$totalintervention,
                     event.c = dat$eventcontrol,
                     n.c = dat$totalcontrol,
                     studlab = author,
                     data = dat,
                     sm = input$effectMeasure,
                     method = input$method,
                     method.tau = input$method.tau,
                     comb.fixed = input$effectModel == "FE",
                     comb.random = input$effectModel == "RE")
    m.gen.inf <- InfluenceAnalysis(m.bin, random = TRUE)
    plot(m.gen.inf, "i2")
    
  })
  output$lm2 <- renderPlot({
    req(dataInput())
    dat <- dataInput()
    m.bin <- metabin(event.e = dat$eventintervention,
                     n.e = dat$totalintervention,
                     event.c = dat$eventcontrol,
                     n.c = dat$totalcontrol,
                     studlab = author,
                     data = dat,
                     sm = input$effectMeasure,
                     method = input$method,
                     method.tau = input$method.tau,
                     comb.fixed = input$effectModel == "FE",
                     comb.random = input$effectModel == "RE")
    lm2 <-limitmeta(m.bin)
    funnel.limitmeta(lm2,shrunken = TRUE)
    
  })
  output$lm <- renderPlot({
    req(dataInput())
    dat <- dataInput()
    m.bin <- metabin(event.e = dat$eventintervention,
                     n.e = dat$totalintervention,
                     event.c = dat$eventcontrol,
                     n.c = dat$totalcontrol,
                     studlab = author,
                     data = dat,
                     sm = input$effectMeasure,
                     method = input$method,
                     method.tau = input$method.tau,
                     comb.fixed = input$effectModel == "FE",
                     comb.random = input$effectModel == "RE")
    lm <-limitmeta(m.bin)
    funnel.limitmeta(lm)
    
  })
  
  
  output$pcurve <- renderPlot({
    req(dataInput())
    dat <- dataInput()
    m.bin <- metabin(event.e = dat$eventintervention,
                     n.e = dat$totalintervention,
                     event.c = dat$eventcontrol,
                     n.c = dat$totalcontrol,
                     studlab = author,
                     data = dat,
                     sm = input$effectMeasure,
                     method = input$method,
                     method.tau = input$method.tau,
                     comb.fixed = input$effectModel == "FE",
                     comb.random = input$effectModel == "RE")
    lm <- pcurve(m.bin, effect.estimation = FALSE, N, dmin = 0, dmax = 1)
    
    
  })
  output$ggbee <- renderPlot({
    req(dataInput())
    dat <- dataInput()
    
    # Assuming 'author' is a column in 'dat'
    author <- dat$author
    
    # Run the meta-analysis
    m.bin <- metabin(event.e = dat$eventintervention,
                     n.e = dat$totalintervention,
                     event.c = dat$eventcontrol,
                     n.c = dat$totalcontrol,
                     studlab = author,
                     data = dat,
                     sm = input$effectMeasure,  # Effect measure from user input (e.g., "OR" for Odds Ratio)
                     method = input$method,  # Meta-analysis method from user input
                     method.tau = input$method.tau,
                     comb.fixed = input$effectModel == "FE",  # Fixed effect model
                     comb.random = input$effectModel == "RE")  # Random effects model
    
    # Prepare data for bee swarm plot
    summary_data <- summary(m.bin)
    plot_data <- data.frame(
      Study = dat$author,  # Use author data directly from input data
      TE = summary_data$TE,
      SE = summary_data$seTE,
      Weight = summary_data$w.fixed + summary_data$w.random  # Total weight, fixed + random
    )
    
    # Using ggbeeswarm to create a Bee Swarm Plot
    ggplot(plot_data, aes(x = factor(1), y = TE, size = Weight)) +  # 'factor(1)' to align all points vertically
      geom_beeswarm(cex = 1.5) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Add a reference line at no effect
      labs(x = "", y = "Effect Size (e.g., Odds Ratio)", title = "Bee Swarm Plot of Study Effects") +
      theme_minimal() +
      theme(axis.text.x = element_blank(), axis.title.x = element_blank())
  })
  
  output$Trimfill <- renderPlot({
    req(dataInput())
    dat <- dataInput()
    m.bin <- metabin(event.e = dat$eventintervention,
                     n.e = dat$totalintervention,
                     event.c = dat$eventcontrol,
                     n.c = dat$totalcontrol,
                     studlab = author,
                     data = dat,
                     sm = input$effectMeasure,
                     method = input$method,
                     method.tau = input$method.tau,
                     comb.fixed = input$effectModel == "FE",
                     comb.random = input$effectModel == "RE")
    tf <- trimfill(m.bin)
    # Define fill colors for contour
    contour <- c(0.9, 0.95, 0.99)
    col.contour <- c("gray75", "gray85", "gray95")
    ld <- c("p < 0.1", "p < 0.05", "p < 0.01")
    
    
    # Contour-enhanced funnel plot (full data)
    meta::funnel(tf, 
                 contour = contour,
                 col.contour = col.contour)
    legend(x = 1.1, y = 0.01, 
           legend = ld, fill = col.contour)
    title("Funnel Plot (Trim & Fill Method)")
    
    
  })
  
  
  
  output$funnelPlot <- renderPlot({
    req(dataInput())
    dat <- dataInput()
    m.bin <- metabin(event.e = dat$eventintervention,
                     n.e = dat$totalintervention,
                     event.c = dat$eventcontrol,
                     n.c = dat$totalcontrol,
                     studlab = dat$author,
                     data = dat,
                     sm = input$effectMeasure,
                     method = input$method,
                     method.tau = input$method.tau,
                     comb.fixed = input$effectModel == "FE",
                     comb.random = input$effectModel == "RE")
    
    # Define fill colors for contour
    col.contour <- c("gray75", "gray85", "gray95")
    
    if (input$colorFunnel) {
      # Generate color-enhanced funnel plot
      funnel(m.bin, xlim = c(0.1, 5), contour = c(0.9, 0.95, 0.99), col.contour = col.contour)
      
      # Add a legend
      legend("topright", inset = 0.05,
             legend = c("p < 0.1", "p < 0.05", "p < 0.01"),
             fill = col.contour)
      
      # Add a title
      title(main = "Contour-Enhanced Funnel Plot")
    } else {
      # Generate standard funnel plot
      funnel(m.bin, xlim = input$xAxisRange)
    }
  })
  
  
  output$modelSelection <- renderPrint({
    req(dataInput())
    dat <- dataInput()
    if(!("Reg" %in% names(dat))){
      cat("Moderator 'Reg' not found in the data.")
    } else {
      m.bin <- metabin(
        event.e = dat$eventintervention,
        n.e = dat$totalintervention,
        event.c = dat$eventcontrol,
        n.c = dat$totalcontrol,
        studlab = dat$author,
        data = dat,
        sm = input$effectMeasure,
        method = input$method,
        comb.fixed = input$effectModel == "FE",
        comb.random = input$effectModel == "RE"
      )
      m.reg <- metareg(m.bin, ~ Reg)
      cat("AIC for meta-regression model:", AIC(m.reg), "\n")
      cat("BIC for meta-regression model:", BIC(m.reg), "\n")
    }
  })
 
  output$correlationMatrix <- renderPrint({
    req(dataInput())
    dat <- dataInput()
    if(!all(c("Reg", "Reg2", "Reg3") %in% names(dat))){
      cat("One or more moderator variables (Reg, Reg2, Reg3) not found.")
    } else {
      corr <- cor(dat[, c("Reg", "Reg2", "Reg3")])
      print(corr)
    }
  })
  output$residualPlot <- renderPlot({
    req(dataInput())
    dat <- dataInput()
    if(!("Reg" %in% names(dat))){
      plot.new()
      text(0.5, 0.5, "Moderator 'Reg' not found.", cex = 1.5)
    } else {
      m.bin <- metabin(
        event.e = dat$eventintervention,
        n.e = dat$totalintervention,
        event.c = dat$eventcontrol,
        n.c = dat$totalcontrol,
        studlab = dat$author,
        data = dat,
        sm = input$effectMeasure,
        method = input$method,
        comb.fixed = input$effectModel == "FE",
        comb.random = input$effectModel == "RE"
      )
      m.reg <- metareg(m.bin, ~ Reg)
      plot(m.reg$fitted.values, resid(m.reg),
           xlab = "Fitted Values",
           ylab = "Residuals",
           main = "Residual Plot for Meta-Regression")
      abline(h = 0, col = "red", lty = 2)
    }
  })
  
  output$eggerText <- renderPrint({
    req(dataInput())
    dat <- dataInput()
    m.bin <- metabin(
      event.e = dat$eventintervention,
      n.e = dat$totalintervention,
      event.c = dat$eventcontrol,
      n.c = dat$totalcontrol,
      studlab = dat$author,
      data = dat,
      sm = input$effectMeasure,
      method = input$method,
      comb.fixed = input$effectModel == "FE",
      comb.random = input$effectModel == "RE"
    )
    egger <- eggers.test(m.bin)
    print(summary(egger))
  })
  output$subgroupText <- renderPrint({
    req(dataInput())
    dat <- dataInput()
    if(!("subgroup" %in% names(dat))){
      cat("No subgroup column found in the data.")
    } else {
      m.subgroup <- metabin(
        event.e = dat$eventintervention,
        n.e = dat$totalintervention,
        event.c = dat$eventcontrol,
        n.c = dat$totalcontrol,
        studlab = dat$author,
        data = dat,
        sm = input$effectMeasure,
        byvar = dat$subgroup,
        comb.fixed = input$effectModel == "FE",
        comb.random = input$effectModel == "RE"
      )
      print(summary(m.subgroup))
    }
  })
  output$subgroupForest <- renderPlot({
    req(dataInput())
    dat <- dataInput()
    if(!("subgroup" %in% names(dat))){
      plot.new()
      text(0.5, 0.5, "No subgroup column found in the data.", cex = 1.5)
    } else {
      m.subgroup <- metabin(
        event.e = dat$eventintervention,
        n.e = dat$totalintervention,
        event.c = dat$eventcontrol,
        n.c = dat$totalcontrol,
        studlab = dat$author,
        data = dat,
        sm = input$effectMeasure,
        byvar = dat$subgroup,
        comb.fixed = input$effectModel == "FE",
        comb.random = input$effectModel == "RE"
      )
      forest(m.subgroup, xlab = "Log Effect", prediction = TRUE)
    }
  })
  output$pcurve <- renderPlot({
    req(dataInput())
    dat <- dataInput()
    m.bin <- metabin(
      event.e = dat$eventintervention,
      n.e = dat$totalintervention,
      event.c = dat$eventcontrol,
      n.c = dat$totalcontrol,
      studlab = dat$author,
      data = dat,
      sm = input$effectMeasure,
      method = input$method,
      method.tau = input$method.tau,
      comb.fixed = input$effectModel == "FE",
      comb.random = input$effectModel == "RE"
    )
    pcurve(m.bin, effect.estimation = FALSE, dmin = 0, dmax = 1)
  })
  output$radial <- renderPlot({
    req(dataInput())
    dat <- dataInput()
    m.bin <- metabin(
      event.e = dat$eventintervention,
      n.e = dat$totalintervention,
      event.c = dat$eventcontrol,
      n.c = dat$totalcontrol,
      studlab = dat$author,
      data = dat,
      sm = input$effectMeasure,
      method = input$method,
      method.tau = input$method.tau,
      comb.fixed = input$effectModel == "FE",
      comb.random = input$effectModel == "RE"
    )
    radial(m.bin)
  })
  output$drapery <- renderPlot({
    req(dataInput())
    dat <- dataInput()
    m.bin <- metabin(
      event.e = dat$eventintervention,
      n.e = dat$totalintervention,
      event.c = dat$eventcontrol,
      n.c = dat$totalcontrol,
      studlab = dat$author,
      data = dat,
      sm = input$effectMeasure,
      method = input$method,
      method.tau = input$method.tau,
      comb.fixed = input$effectModel == "FE",
      comb.random = input$effectModel == "RE"
    )
    drapery(m.bin, labels = "studlab", type = "pval", legend = FALSE)
  })
  output$cumForestPlot <- renderPlot({
    req(dataInput())
    dat <- dataInput()
    m.bin <- metabin(
      event.e = dat$eventintervention,
      n.e = dat$totalintervention,
      event.c = dat$eventcontrol,
      n.c = dat$totalcontrol,
      studlab = dat$author,
      data = dat,
      sm = input$effectMeasure,
      method = input$method,
      method.tau = input$method.tau,
      comb.fixed = input$effectModel == "FE",
      comb.random = input$effectModel == "RE"
    )
    if("year" %in% names(dat)){
      m.cum <- metacum(m.bin, sortvar = dat$year)
    } else {
      m.cum <- metacum(m.bin)
    }
    forest(m.cum,
           col.diamond = input$col.diamond,
           col.diamond.lines = input$col.diamond.lines,
           col.square = input$col.square,
           col.square.lines = input$col.square.lines,
           col.study = input$col.study,
           col.circle = input$col.circle,
           col.circle.lines = input$col.circle.lines,
           labstudies = input$labstudies,
           cex = input$textsize,
           lwd = input$linewidth,
           label.left = input$label,
           label.right = input$labelr,
           xlim = input$xRange,
           prediction = TRUE)
  })
  output$cumMetaText <- renderPrint({
    req(dataInput())
    dat <- dataInput()
    m.bin <- metabin(
      event.e = dat$eventintervention,
      n.e = dat$totalintervention,
      event.c = dat$eventcontrol,
      n.c = dat$totalcontrol,
      studlab = dat$author,
      data = dat,
      sm = input$effectMeasure,
      method = input$method,
      method.tau = input$method.tau,
      comb.fixed = input$effectModel == "FE",
      comb.random = input$effectModel == "RE"
    )
    
    # If you have a 'year' column, sort by year; otherwise, use default order
    if("year" %in% names(dat)){
      m.cum <- metacum(m.bin, sortvar = dat$year)
    } else {
      m.cum <- metacum(m.bin)
    }
    print(summary(m.cum))
  })
  output$leaveOneOutText <- renderPrint({
    req(dataInput())
    dat <- dataInput()
    m.bin <- metabin(
      event.e = dat$eventintervention,
      n.e = dat$totalintervention,
      event.c = dat$eventcontrol,
      n.c = dat$totalcontrol,
      studlab = dat$author,
      data = dat,
      sm = input$effectMeasure,
      method = input$method,
      method.tau = input$method.tau,
      comb.fixed = input$effectModel == "FE",
      comb.random = input$effectModel == "RE"
    )
    loo <- metainf(m.bin)
    print(loo)
  })
  output$bayesianMetaText <- renderPrint({
    req(dataInput())
    dat <- dataInput()
    if(nrow(dat) < 2){
      cat("At least 2 studies are required for Bayesian meta-analysis.")
      return()
    }
    # Compute the binary meta-analysis (using metabin)
    m.bin <- metabin(
      event.e = dat$eventintervention,
      n.e = dat$totalintervention,
      event.c = dat$eventcontrol,
      n.c = dat$totalcontrol,
      studlab = dat$author,
      data = dat,
      sm = input$effectMeasure,       # "RR" or "OR"
      method = input$method,
      method.tau = input$method.tau,
      comb.fixed = input$effectModel == "FE",
      comb.random = input$effectModel == "RE"
    )
    
    # Extract the log effect size and its standard error
    log_effect <- m.bin$TE
    se_effect  <- m.bin$seTE
    
    # Run Bayesian meta-analysis using bayesmeta
    library(bayesmeta)
    bmeta <- bayesmeta(y = log_effect, sigma = se_effect)
    print(summary(bmeta))
  })
  
  # Meta-Regression Analysis with one, two, and three moderators
  output$metaRegressionResults <- renderPrint({
    req(dataInput())
    dat <- dataInput()
    
    # Build the basic meta-analysis object
    m.bin <- metabin(
      event.e = dat$eventintervention,
      n.e = dat$totalintervention,
      event.c = dat$eventcontrol,
      n.c = dat$totalcontrol,
      studlab = dat$author,
      data = dat,
      sm = input$effectMeasure,         # "RR" or "OR"
      method = input$method,
      method.tau = input$method.tau,
      comb.fixed = input$effectModel == "FE",
      comb.random = input$effectModel == "RE"
    )
    
    # Create a list to store results
    results <- list()
    
    # One moderator meta-regression (if "Reg" is present)
    if ("Reg" %in% names(dat)) {
      m.reg1 <- metareg(m.bin, ~ Reg)
      results$OneModerator <- summary(m.reg1)
    } else {
      results$OneModerator <- "Moderator 'Reg' not found in data."
    }
    
    # Two moderators meta-regression (if both "Reg" and "Reg2" are present)
    if (all(c("Reg", "Reg2") %in% names(dat))) {
      m.reg2 <- metareg(m.bin, ~ Reg + Reg2)
      results$TwoModerators <- summary(m.reg2)
    } else {
      results$TwoModerators <- "Moderators 'Reg' and/or 'Reg2' not found in data."
    }
    
    # Three moderators meta-regression (if "Reg", "Reg2" and "Reg3" are present)
    if (all(c("Reg", "Reg2", "Reg3") %in% names(dat))) {
      m.reg3 <- metareg(m.bin, ~ Reg + Reg2 + Reg3)
      results$ThreeModerators <- summary(m.reg3)
    } else {
      results$ThreeModerators <- "One or more of the moderators 'Reg', 'Reg2', 'Reg3' not found in data."
    }
    
    print(results)
  })
  
  # Bubble Plot for Meta-Regression with Choice of Moderator
  output$selectedBubblePlot <- renderPlot({
    req(dataInput())
    dat <- dataInput()
    
    # Use the chosen moderator from the UI selectInput
    chosenMod <- input$selectedModerator
    
    if (!(chosenMod %in% names(dat))) {
      plot.new()
      text(0.5, 0.5, paste("Moderator", chosenMod, "not found in data."), cex = 1.5)
      return()
    }
    
    # Build the basic meta-analysis object
    m.bin <- metabin(
      event.e = dat$eventintervention,
      n.e = dat$totalintervention,
      event.c = dat$eventcontrol,
      n.c = dat$totalcontrol,
      studlab = dat$author,
      data = dat,
      sm = input$effectMeasure,
      method = input$method,
      method.tau = input$method.tau,
      comb.fixed = input$effectModel == "FE",
      comb.random = input$effectModel == "RE"
    )
    
    # Build meta-regression model using the chosen moderator.
    mod_formula <- as.formula(paste("~", chosenMod))
    m.reg <- metareg(m.bin, mod_formula)
    
    # Create the bubble plot.
    bubble(m.reg, studlab = TRUE, main = paste("Bubble Plot for Moderator:", chosenMod))
  })
  # Add these download handlers to your server function
  
  # Download handler for main forest plot (already exists, but here for reference)
  output$downloadPlotmeta <- downloadHandler(
    filename = function() {
      paste("forest_plot_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file, width = input$plotWidth, height = input$plotHeight)
      dat <- dataInput()
      m.bin <- metabin(event.e = dat$eventintervention,
                       n.e = dat$totalintervention,
                       event.c = dat$eventcontrol,
                       n.c = dat$totalcontrol,
                       studlab = dat$author,
                       data = dat,
                       sm = input$effectMeasure,
                       method = input$method,
                       method.tau = input$method.tau,
                       comb.fixed = input$effectModel == "FE",
                       comb.random = input$effectModel == "RE")
      forest(m.bin, 
             col.diamond = input$col.diamond,
             col.diamond.lines = input$col.diamond.lines,
             col.square = input$col.square,
             col.square.lines = input$col.square.lines,
             col.study = input$col.study,
             col.circle = input$col.circle,
             col.circle.lines = input$col.circle.lines,
             labstudies = input$labstudies,
             cex = input$textsize,
             lwd = input$linewidth,
             label.left = input$label,
             label.right = input$labelr,
             xlim = input$xRange,
             prediction = TRUE)
      dev.off()
    }
  )
  
  # Download handler for Funnel Plot
  output$downloadFunnel <- downloadHandler(
    filename = function() {
      paste("funnel_plot_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file, width = input$plotWidth, height = input$plotHeight)
      dat <- dataInput()
      m.bin <- metabin(event.e = dat$eventintervention,
                       n.e = dat$totalintervention,
                       event.c = dat$eventcontrol,
                       n.c = dat$totalcontrol,
                       studlab = dat$author,
                       data = dat,
                       sm = input$effectMeasure,
                       method = input$method,
                       method.tau = input$method.tau,
                       comb.fixed = input$effectModel == "FE",
                       comb.random = input$effectModel == "RE")
      
      # Define fill colors for contour
      col.contour <- c("gray75", "gray85", "gray95")
      
      if (input$colorFunnel) {
        # Generate color-enhanced funnel plot
        funnel(m.bin, xlim = c(0.1, 5), contour = c(0.9, 0.95, 0.99), col.contour = col.contour)
        
        # Add a legend
        legend("topright", inset = 0.05,
               legend = c("p < 0.1", "p < 0.05", "p < 0.01"),
               fill = col.contour)
        
        # Add a title
        title(main = "Contour-Enhanced Funnel Plot")
      } else {
        # Generate standard funnel plot
        funnel(m.bin, xlim = input$funnelXRange)
      }
      dev.off()
    }
  )
  
  # Download handler for Trim & Fill Plot
  output$downloadTrimfill <- downloadHandler(
    filename = function() {
      paste("trimfill_plot_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file, width = input$plotWidth, height = input$plotHeight)
      dat <- dataInput()
      m.bin <- metabin(event.e = dat$eventintervention,
                       n.e = dat$totalintervention,
                       event.c = dat$eventcontrol,
                       n.c = dat$totalcontrol,
                       studlab = dat$author,
                       data = dat,
                       sm = input$effectMeasure,
                       method = input$method,
                       method.tau = input$method.tau,
                       comb.fixed = input$effectModel == "FE",
                       comb.random = input$effectModel == "RE")
      tf <- trimfill(m.bin)
      # Define fill colors for contour
      contour <- c(0.9, 0.95, 0.99)
      col.contour <- c("gray75", "gray85", "gray95")
      ld <- c("p < 0.1", "p < 0.05", "p < 0.01")
      
      # Contour-enhanced funnel plot (full data)
      meta::funnel(tf, 
                   contour = contour,
                   col.contour = col.contour)
      legend(x = 1.1, y = 0.01, 
             legend = ld, fill = col.contour)
      title("Funnel Plot (Trim & Fill Method)")
      dev.off()
    }
  )
  
  # Download handler for Limit Meta-Analysis Plot
  output$downloadLM <- downloadHandler(
    filename = function() {
      paste("limitmeta_plot_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file, width = input$plotWidth, height = input$plotHeight)
      dat <- dataInput()
      m.bin <- metabin(event.e = dat$eventintervention,
                       n.e = dat$totalintervention,
                       event.c = dat$eventcontrol,
                       n.c = dat$totalcontrol,
                       studlab = dat$author,
                       data = dat,
                       sm = input$effectMeasure,
                       method = input$method,
                       method.tau = input$method.tau,
                       comb.fixed = input$effectModel == "FE",
                       comb.random = input$effectModel == "RE")
      lm_obj <- limitmeta(m.bin)
      funnel.limitmeta(lm_obj)
      dev.off()
    }
  )
  
  # Download handler for Shrunken Limit Meta-Analysis Plot
  output$downloadLM2 <- downloadHandler(
    filename = function() {
      paste("shrunken_limitmeta_plot_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file, width = input$plotWidth, height = input$plotHeight)
      dat <- dataInput()
      m.bin <- metabin(event.e = dat$eventintervention,
                       n.e = dat$totalintervention,
                       event.c = dat$eventcontrol,
                       n.c = dat$totalcontrol,
                       studlab = dat$author,
                       data = dat,
                       sm = input$effectMeasure,
                       method = input$method,
                       method.tau = input$method.tau,
                       comb.fixed = input$effectModel == "FE",
                       comb.random = input$effectModel == "RE")
      lm_obj <- limitmeta(m.bin)
      funnel.limitmeta(lm_obj, shrunken = TRUE)
      dev.off()
    }
  )
  
  # Download handler for P-Curve Plot
  output$downloadPcurve <- downloadHandler(
    filename = function() {
      paste("pcurve_plot_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file, width = input$plotWidth, height = input$plotHeight)
      dat <- dataInput()
      m.bin <- metabin(event.e = dat$eventintervention,
                       n.e = dat$totalintervention,
                       event.c = dat$eventcontrol,
                       n.c = dat$totalcontrol,
                       studlab = dat$author,
                       data = dat,
                       sm = input$effectMeasure,
                       method = input$method,
                       method.tau = input$method.tau,
                       comb.fixed = input$effectModel == "FE",
                       comb.random = input$effectModel == "RE")
      pcurve(m.bin, effect.estimation = FALSE, dmin = 0, dmax = 1)
      dev.off()
    }
  )
  
  # Download handler for Beeswarm Plot
  output$downloadBeeswarm <- downloadHandler(
    filename = function() {
      paste("beeswarm_plot_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file, width = input$plotWidth, height = input$plotHeight)
      dat <- dataInput()
      m.bin <- metabin(event.e = dat$eventintervention,
                       n.e = dat$totalintervention,
                       event.c = dat$eventcontrol,
                       n.c = dat$totalcontrol,
                       studlab = dat$author,
                       data = dat,
                       sm = input$effectMeasure,
                       method = input$method,
                       method.tau = input$method.tau,
                       comb.fixed = input$effectModel == "FE",
                       comb.random = input$effectModel == "RE")
      
      summary_data <- summary(m.bin)
      plot_data <- data.frame(
        Study = dat$author,
        TE = summary_data$TE,
        SE = summary_data$seTE,
        Weight = summary_data$w.fixed + summary_data$w.random
      )
      
      print(ggplot(plot_data, aes(x = factor(1), y = TE, size = Weight)) +
              geom_beeswarm(cex = 1.5) +
              geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
              labs(x = "", y = "Effect Size (e.g., Odds Ratio)", title = "Bee Swarm Plot") +
              theme_minimal() +
              theme(axis.text.x = element_blank(), axis.title.x = element_blank()))
      dev.off()
    }
  )
  
  # Download handler for Albatross (CForest) Plot
  output$downloadCforest <- downloadHandler(
    filename = function() {
      paste("albatross_plot_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file, width = input$plotWidth, height = input$plotHeight)
      dat <- dataInput()
      m.bin <- metabin(event.e = dat$eventintervention,
                       n.e = dat$totalintervention,
                       event.c = dat$eventcontrol,
                       n.c = dat$totalcontrol,
                       studlab = dat$author,
                       data = dat,
                       sm = input$effectMeasure,
                       method = input$method,
                       method.tau = input$method.tau,
                       comb.fixed = input$effectModel == "FE",
                       comb.random = input$effectModel == "RE")
      
      z_values <- summary(m.bin)$TE / summary(m.bin)$seTE
      p_values <- 2 * (1 - pnorm(abs(z_values)))
      n_treatment <- dat$totalintervention
      n_control <- dat$totalcontrol
      plot_data <- data.frame(
        SampleSize = n_treatment + n_control,
        PValue = p_values
      )
      
      print(ggplot(plot_data, aes(x = log(SampleSize), y = -log10(PValue))) +
              geom_point() +
              geom_smooth(method = "lm", se = FALSE, color = "blue") +
              labs(x = "Log of Sample Size", y = "-Log10 of P-value", title = "Albatross Plot") +
              theme_minimal())
      dev.off()
    }
  )
  
  # Download handler for Baujat Plot
  output$downloadBaujat <- downloadHandler(
    filename = function() {
      paste("baujat_plot_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file, width = input$plotWidth, height = input$plotHeight)
      dat <- dataInput()
      m.bin <- metabin(event.e = dat$eventintervention,
                       n.e = dat$totalintervention,
                       event.c = dat$eventcontrol,
                       n.c = dat$totalcontrol,
                       studlab = dat$author,
                       data = dat,
                       sm = input$effectMeasure,
                       method = input$method,
                       method.tau = input$method.tau,
                       comb.fixed = input$effectModel == "FE",
                       comb.random = input$effectModel == "RE")
      m.gen.inf <- InfluenceAnalysis(m.bin, random = TRUE)
      plot(m.gen.inf, "baujat")
      dev.off()
    }
  )
  
  # Download handler for Influence Plot
  output$downloadInfluence <- downloadHandler(
    filename = function() {
      paste("influence_plot_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file, width = input$plotWidth, height = input$plotHeight)
      dat <- dataInput()
      m.bin <- metabin(event.e = dat$eventintervention,
                       n.e = dat$totalintervention,
                       event.c = dat$eventcontrol,
                       n.c = dat$totalcontrol,
                       studlab = dat$author,
                       data = dat,
                       sm = input$effectMeasure,
                       method = input$method,
                       method.tau = input$method.tau,
                       comb.fixed = input$effectModel == "FE",
                       comb.random = input$effectModel == "RE")
      m.gen.inf <- InfluenceAnalysis(m.bin, random = TRUE)
      plot(m.gen.inf, "influence")
      dev.off()
    }
  )
  
  # Download handler for Effect Size Plot
  output$downloadEffectsize <- downloadHandler(
    filename = function() {
      paste("effectsize_plot_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file, width = input$plotWidth, height = input$plotHeight)
      dat <- dataInput()
      m.bin <- metabin(event.e = dat$eventintervention,
                       n.e = dat$totalintervention,
                       event.c = dat$eventcontrol,
                       n.c = dat$totalcontrol,
                       studlab = dat$author,
                       data = dat,
                       sm = input$effectMeasure,
                       method = input$method,
                       method.tau = input$method.tau,
                       comb.fixed = input$effectModel == "FE",
                       comb.random = input$effectModel == "RE")
      m.gen.inf <- InfluenceAnalysis(m.bin, random = TRUE)
      plot(m.gen.inf, "es")
      dev.off()
    }
  )
  
  # Download handler for I² Plot
  output$downloadI2 <- downloadHandler(
    filename = function() {
      paste("i2_plot_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file, width = input$plotWidth, height = input$plotHeight)
      dat <- dataInput()
      m.bin <- metabin(event.e = dat$eventintervention,
                       n.e = dat$totalintervention,
                       event.c = dat$eventcontrol,
                       n.c = dat$totalcontrol,
                       studlab = dat$author,
                       data = dat,
                       sm = input$effectMeasure,
                       method = input$method,
                       method.tau = input$method.tau,
                       comb.fixed = input$effectModel == "FE",
                       comb.random = input$effectModel == "RE")
      m.gen.inf <- InfluenceAnalysis(m.bin, random = TRUE)
      plot(m.gen.inf, "i2")
      dev.off()
    }
  )
  
  # Download handler for Bubble Plot
  output$downloadBubblePlot <- downloadHandler(
    filename = function() {
      paste("bubble_plot_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file, width = input$plotWidth, height = input$plotHeight)
      dat <- dataInput()
      chosenMod <- input$selectedModerator
      if (!(chosenMod %in% names(dat))) {
        plot.new()
        text(0.5, 0.5, paste("Moderator", chosenMod, "not found in data."), cex = 1.5)
      } else {
        m.bin <- metabin(event.e = dat$eventintervention,
                         n.e = dat$totalintervention,
                         event.c = dat$eventcontrol,
                         n.c = dat$totalcontrol,
                         studlab = dat$author,
                         data = dat,
                         sm = input$effectMeasure,
                         method = input$method,
                         method.tau = input$method.tau,
                         comb.fixed = input$effectModel == "FE",
                         comb.random = input$effectModel == "RE")
        mod_formula <- as.formula(paste("~", chosenMod))
        m.reg <- metareg(m.bin, mod_formula)
        bubble(m.reg, studlab = TRUE, main = paste("Bubble Plot for Moderator:", chosenMod))
      }
      dev.off()
    }
  )
  
  # Download handler for L'Abbé Plot
  output$downloadLabbe <- downloadHandler(
    filename = function() {
      paste("labbe_plot_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file, width = input$plotWidth, height = input$plotHeight)
      dat <- dataInput()
      m.bin <- metabin(event.e = dat$eventintervention,
                       n.e = dat$totalintervention,
                       event.c = dat$eventcontrol,
                       n.c = dat$totalcontrol,
                       studlab = dat$author,
                       data = dat,
                       sm = input$effectMeasure,
                       method = input$method,
                       method.tau = input$method.tau,
                       comb.fixed = input$effectModel == "FE",
                       comb.random = input$effectModel == "RE")
      labbe(m.bin)
      dev.off()
    }
  )
  
  # Download handler for Radial Plot
  output$downloadRadial <- downloadHandler(
    filename = function() {
      paste("radial_plot_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file, width = input$plotWidth, height = input$plotHeight)
      dat <- dataInput()
      m.bin <- metabin(event.e = dat$eventintervention,
                       n.e = dat$totalintervention,
                       event.c = dat$eventcontrol,
                       n.c = dat$totalcontrol,
                       studlab = dat$author,
                       data = dat,
                       sm = input$effectMeasure,
                       method = input$method,
                       method.tau = input$method.tau,
                       comb.fixed = input$effectModel == "FE",
                       comb.random = input$effectModel == "RE")
      radial(m.bin)
      dev.off()
    }
  )
  
  # Download handler for Drapery Plot
  output$downloadDrapery <- downloadHandler(
    filename = function() {
      paste("drapery_plot_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file, width = input$plotWidth, height = input$plotHeight)
      dat <- dataInput()
      m.bin <- metabin(event.e = dat$eventintervention,
                       n.e = dat$totalintervention,
                       event.c = dat$eventcontrol,
                       n.c = dat$totalcontrol,
                       studlab = dat$author,
                       data = dat,
                       sm = input$effectMeasure,
                       method = input$method,
                       method.tau = input$method.tau,
                       comb.fixed = input$effectModel == "FE",
                       comb.random = input$effectModel == "RE")
      drapery(m.bin, labels = "studlab", type = "pval", legend = FALSE)
      dev.off()
    }
  )
  
  # Download handler for Cumulative Meta-Analysis Forest Plot
  output$downloadCumMeta <- downloadHandler(
    filename = function() {
      paste("cumulative_forest_plot_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file, width = input$plotWidth, height = input$plotHeight)
      dat <- dataInput()
      m.bin <- metabin(event.e = dat$eventintervention,
                       n.e = dat$totalintervention,
                       event.c = dat$eventcontrol,
                       n.c = dat$totalcontrol,
                       studlab = dat$author,
                       data = dat,
                       sm = input$effectMeasure,
                       method = input$method,
                       method.tau = input$method.tau,
                       comb.fixed = input$effectModel == "FE",
                       comb.random = input$effectModel == "RE")
      if("year" %in% names(dat)){
        m.cum <- metacum(m.bin, sortvar = dat$year)
      } else {
        m.cum <- metacum(m.bin)
      }
      forest(m.cum,
             col.diamond = input$col.diamond,
             col.diamond.lines = input$col.diamond.lines,
             col.square = input$col.square,
             col.square.lines = input$col.square.lines,
             col.study = input$col.study,
             col.circle = input$col.circle,
             col.circle.lines = input$col.circle.lines,
             labstudies = input$labstudies,
             cex = input$textsize,
             lwd = input$linewidth,
             label.left = input$label,
             label.right = input$labelr,
             xlim = input$xRange,
             prediction = TRUE)
      dev.off()
    }
  )
  
  # Download handler for Subgroup Forest Plot
  output$downloadSubgroupPlot <- downloadHandler(
    filename = function() {
      paste("subgroup_forest_plot_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file, width = input$plotWidth, height = input$plotHeight)
      dat <- dataInput()
      if(!("subgroup" %in% names(dat))){
        plot.new()
        text(0.5, 0.5, "No subgroup column found in the data.", cex = 1.5)
      } else {
        m.subgroup <- metabin(
          event.e = dat$eventintervention,
          n.e = dat$totalintervention,
          event.c = dat$eventcontrol,
          n.c = dat$totalcontrol,
          studlab = dat$author,
          data = dat,
          sm = input$effectMeasure,
          byvar = dat$subgroup,
          comb.fixed = input$effectModel == "FE",
          comb.random = input$effectModel == "RE"
        )
        forest(m.subgroup, xlab = "Log Effect", prediction = TRUE)
      }
      dev.off()
    }
  )
  
  # Download handler for Residual Plot
  output$downloadResidual <- downloadHandler(
    filename = function() {
      paste("residual_plot_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file, width = input$plotWidth, height = input$plotHeight)
      dat <- dataInput()
      if(!("Reg" %in% names(dat))){
        plot.new()
        text(0.5, 0.5, "Moderator 'Reg' not found.", cex = 1.5)
      } else {
        m.bin <- metabin(
          event.e = dat$eventintervention,
          n.e = dat$totalintervention,
          event.c = dat$eventcontrol,
          n.c = dat$totalcontrol,
          studlab = dat$author,
          data = dat,
          sm = input$effectMeasure,
          method = input$method,
          comb.fixed = input$effectModel == "FE",
          comb.random = input$effectModel == "RE"
        )
        m.reg <- metareg(m.bin, ~ Reg)
        plot(m.reg$fitted.values, resid(m.reg),
             xlab = "Fitted Values",
             ylab = "Residuals",
             main = "Residual Plot for Meta-Regression")
        abline(h = 0, col = "red", lty = 2)
      }
      dev.off()
    }
  )
  
  # Download handlers for text results
  
  # Download handler for meta-analysis results
  output$downloadResults <- downloadHandler(
    filename = function() {
      paste("meta_analysis_results_", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      # Redirect output to a file
      sink(file)
      dat <- dataInput()
      m.bin <- metabin(event.e = dat$eventintervention,
                       n.e = dat$totalintervention,
                       event.c = dat$eventcontrol,
                       n.c = dat$totalcontrol,
                       studlab = dat$author,
                       data = dat,
                       sm = input$effectMeasure,
                       method = input$method,
                       method.tau = input$method.tau,
                       comb.fixed = input$effectModel == "FE",
                       comb.random = input$effectModel == "RE")
      print(summary(m.bin, digits = 2, text.tau2 = TRUE, text.I2 = TRUE, text.Q = TRUE))
      # Stop redirecting output
      sink()
    }
  )
  
  # Download handler for RMA results
  output$downloadRMA <- downloadHandler(
    filename = function() {
      paste("rma_results_", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      sink(file)
      dat <- dataInput()
      dat <- dat %>%
        mutate(
          logRR = log((eventintervention + 0.5) / (totalintervention - eventintervention + 0.5) /
                        ((eventcontrol + 0.5) / (totalcontrol - eventcontrol + 0.5))),
          varLogRR = 1/(eventintervention + 0.5) + 1/(totalintervention - eventintervention + 0.5) +
            1/(eventcontrol + 0.5) + 1/(totalcontrol - eventcontrol + 0.5)
        )
      
      rma.model <- rma(yi = logRR, vi = varLogRR, data = dat, method = "REML")
      
      cat("Detailed Summary of Random-Effects Meta-Analysis:\n")
      cat("Model Results:\n")
      print(summary(rma.model))
      
      cat("\nConfidence Interval for the Overall Effect Size:\n")
      print(confint(rma.model))
      
      cat("\nTest for Residual Heterogeneity:\n")
      print(anova(rma.model))
      
      cat("\nTest for Overall Effect:\n")
      print(rma.model$test)
      
      cat("\nInfluential Studies Diagnostics:\n")
      inf <- influence(rma.model)
      print(inf)
      
      sink()
    }
  )
  
  # Download handler for OR RMA results
  output$downloadRMAodd <- downloadHandler(
    filename = function() {
      paste("or_rma_results_", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      sink(file)
      dat <- dataInput()
      dat <- dat %>%
        mutate(
          logOR = log((eventintervention + 0.5) / (totalintervention - eventintervention + 0.5) /
                        ((eventcontrol + 0.5) / (totalcontrol - eventcontrol + 0.5))),
          varLogOR = 1/(eventintervention + 0.5) + 1/(totalintervention - eventintervention + 0.5) +
            1/(eventcontrol + 0.5) + 1/(totalcontrol - eventcontrol + 0.5)
        )
      
      rma.model <- rma(yi = logOR, vi = varLogOR, data = dat, method = "REML")
      
      cat("Detailed Summary of Random-Effects Meta-Analysis:\n")
      cat("Model Results:\n")
      print(summary(rma.model))
      
      cat("\nConfidence Interval for the Overall Effect Size:\n")
      print(confint(rma.model))
      
      cat("\nTest for Residual Heterogeneity:\n")
      print(anova(rma.model))
      
      cat("\nTest for Overall Effect:\n")
      print(rma.model$test)
      
      cat("\nInfluential Studies Diagnostics:\n")
      inf <- influence(rma.model)
      print(inf)
      
      sink()
    }
  )
  
  # Download handler for publication bias analysis
  output$downloadPubbias <- downloadHandler(
    filename = function() {
      paste("publication_bias_analysis_", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      sink(file)
      dat <- dataInput()
      m.bin <- metabin(event.e = dat$eventintervention,
                       n.e = dat$totalintervention,
                       event.c = dat$eventcontrol,
                       n.c = dat$totalcontrol,
                       studlab = dat$author,
                       data = dat,
                       sm = input$effectMeasure,
                       method = input$method,
                       method.tau = input$method.tau,
                       comb.fixed = input$effectModel == "FE",
                       comb.random = input$effectModel == "RE")
      
      m.limitmeta <- limitmeta(m.bin)
      p.curve <- tryCatch({
        pcurve(m.bin, effect.estimation = FALSE, dmin = 0, dmax = 1)
      }, error = function(e) {
        NULL
      })
      
      print(summary(m.limitmeta))
      if (!is.null(p.curve)) {
        print(summary(p.curve))
      } else {
        cat("p-curve analysis could not be conducted.\n")
      }
      
      sink()
    }
  )
  
  # Download handler for heterogeneity analysis
  output$downloadHetero <- downloadHandler(
    filename = function() {
      paste("heterogeneity_analysis_", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      sink(file)
      dat <- dataInput()
      m.bin <- metabin(event.e = dat$eventintervention,
                       n.e = dat$totalintervention,
                       event.c = dat$eventcontrol,
                       n.c = dat$totalcontrol,
                       studlab = dat$author,
                       data = dat,
                       sm = input$effectMeasure,
                       method = input$method,
                       method.tau = input$method.tau,
                       comb.fixed = input$effectModel == "FE",
                       comb.random = input$effectModel == "RE")
      
      m.genp <- eggers.test(m.bin)
      m.gen.out <- find.outliers(m.bin)
      m.gen.inf <- InfluenceAnalysis(m.bin, random = TRUE)
      
      print(summary(m.genp))
      print(summary(m.gen.out))
      print(summary(m.gen.inf))
      
      heterogeneity_stats <- list(
        "Cochrane's Q" = m.bin$Q,
        "I2" = m.bin$I2,
        "H2" = m.bin$H2,
        "Tau^2" = m.bin$tau^2,
        "Tau" = sqrt(m.bin$tau^2)
      )
      
      print(heterogeneity_stats)
      
      sink()
    }
  )
  
  # Download handler for meta-regression results
  output$downloadMetaReg <- downloadHandler(
    filename = function() {
      paste("meta_regression_results_", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      sink(file)
      dat <- dataInput()
      m.bin <- metabin(
        event.e = dat$eventintervention,
        n.e = dat$totalintervention,
        event.c = dat$eventcontrol,
        n.c = dat$totalcontrol,
        studlab = dat$author,
        data = dat,
        sm = input$effectMeasure,
        method = input$method,
        method.tau = input$method.tau,
        comb.fixed = input$effectModel == "FE",
        comb.random = input$effectModel == "RE"
      )
      
      results <- list()
      if ("Reg" %in% names(dat)) {
        m.reg1 <- metareg(m.bin, ~ Reg)
        results$OneModerator <- summary(m.reg1)
      } else {
        results$OneModerator <- "Moderator 'Reg' not found in data."
      }
      if (all(c("Reg", "Reg2") %in% names(dat))) {
        m.reg2 <- metareg(m.bin, ~ Reg + Reg2)
        results$TwoModerators <- summary(m.reg2)
      } else {
        results$TwoModerators <- "Moderators 'Reg' and/or 'Reg2' not found in data."
      }
      if (all(c("Reg", "Reg2", "Reg3") %in% names(dat))) {
        m.reg3 <- metareg(m.bin, ~ Reg + Reg2 + Reg3)
        results$ThreeModerators <- summary(m.reg3)
      } else {
        results$ThreeModerators <- "One or more of the moderators 'Reg', 'Reg2', 'Reg3' not found in data."
      }
      
      print(results)
      
      sink()
    }
  )
  
  # Download handler for cumulative meta-analysis results
  output$downloadCumText <- downloadHandler(
    filename = function() {
      paste("cumulative_meta_analysis_", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      sink(file)
      dat <- dataInput()
      m.bin <- metabin(
        event.e = dat$eventintervention,
        n.e = dat$totalintervention,
        event.c = dat$eventcontrol,
        n.c = dat$totalcontrol,
        studlab = dat$author,
        data = dat,
        sm = input$effectMeasure,
        method = input$method,
        method.tau = input$method.tau,
        comb.fixed = input$effectModel == "FE",
        comb.random = input$effectModel == "RE"
      )
      
      if("year" %in% names(dat)){
        m.cum <- metacum(m.bin, sortvar = dat$year)
      } else {
        m.cum <- metacum(m.bin)
      }
      print(summary(m.cum))
      
      sink()
    }
  )
  
  # Download handler for subgroup analysis results
  output$downloadSubgroupText <- downloadHandler(
    filename = function() {
      paste("subgroup_analysis_", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      sink(file)
      dat <- dataInput()
      if(!("subgroup" %in% names(dat))){
        cat("No subgroup column found in the data.")
      } else {
        m.subgroup <- metabin(
          event.e = dat$eventintervention,
          n.e = dat$totalintervention,
          event.c = dat$eventcontrol,
          n.c = dat$totalcontrol,
          studlab = dat$author,
          data = dat,
          sm = input$effectMeasure,
          byvar = dat$subgroup,
          comb.fixed = input$effectModel == "FE",
          comb.random = input$effectModel == "RE"
        )
        print(summary(m.subgroup))
      }
      
      sink()
    }
  )
  
  
  # Bayesian Meta-Analysis
  output$bayesianMetaResult <- renderPrint({
    req(dataInput())
    dat <- dataInput()
    
    # Check if enough studies
    if (nrow(dat) < 2) {
      cat("At least 2 studies are required for Bayesian meta-analysis.")
      return(NULL)
    }
    
    # Run meta-analysis
    m.bin <- metabin(
      event.e = dat$eventintervention,
      n.e = dat$totalintervention,
      event.c = dat$eventcontrol,
      n.c = dat$totalcontrol,
      studlab = dat$author,
      data = dat,
      sm = input$effectMeasure,
      method = input$method,
      method.tau = input$method.tau,
      comb.fixed = (input$effectModel == "FE"),
      comb.random = (input$effectModel == "RE")
    )
    
    # Extract effect sizes and standard errors
    log_effect <- m.bin$TE
    se_effect <- m.bin$seTE
    
    # Check if bayesmeta package is installed
    if (!requireNamespace("bayesmeta", quietly = TRUE)) {
      cat("The 'bayesmeta' package is required for Bayesian meta-analysis.\n")
      cat("Please install it by running: install.packages('bayesmeta')\n")
      return(NULL)
    }
    
    # Run Bayesian meta-analysis
    tryCatch({
      bmeta <- bayesmeta::bayesmeta(
        y = log_effect, 
        sigma = se_effect,
        labels = dat$author
      )
      
      # Print detailed results
      cat("=================================================================\n")
      cat("BAYESIAN META-ANALYSIS\n")
      cat("=================================================================\n\n")
      print(summary(bmeta))
      
      # Print additional interpretation
      cat("\n=================================================================\n")
      cat("INTERPRETATION\n")
      cat("=================================================================\n\n")
      
      # Extract posterior mean and credible interval
      mu_mean <- bmeta$summary["mu", "mean"]
      mu_lower <- bmeta$summary["mu", "2.5%"]
      mu_upper <- bmeta$summary["mu", "97.5%"]
      
      cat("Pooled Effect:\n")
      cat("- Posterior mean of the pooled effect (log scale): ", round(mu_mean, 4), "\n", sep = "")
      cat("- 95% credible interval (log scale): [", round(mu_lower, 4), ", ", round(mu_upper, 4), "]\n", sep = "")
      
      # Convert to original scale
      if (input$effectMeasure == "RR" || input$effectMeasure == "OR") {
        cat("- Posterior mean of the pooled effect (original scale): ", round(exp(mu_mean), 4), " ", input$effectMeasure, "\n", sep = "")
        cat("- 95% credible interval (original scale): [", round(exp(mu_lower), 4), ", ", round(exp(mu_upper), 4), "] ", input$effectMeasure, "\n", sep = "")
        
        if (exp(mu_lower) > 1) {
          cat("- Since the entire 95% credible interval is above 1, there is strong evidence of a positive effect.\n")
        } else if (exp(mu_upper) < 1) {
          cat("- Since the entire 95% credible interval is below 1, there is strong evidence of a negative effect.\n")
        } else {
          cat("- Since the 95% credible interval includes 1, the evidence does not strongly support either a positive or negative effect.\n")
        }
      }
      
      # Extract heterogeneity
      tau_mean <- bmeta$summary["tau", "mean"]
      tau_lower <- bmeta$summary["tau", "2.5%"]
      tau_upper <- bmeta$summary["tau", "97.5%"]
      
      cat("\nHeterogeneity:\n")
      cat("- Posterior mean of tau (between-study standard deviation): ", round(tau_mean, 4), "\n", sep = "")
      cat("- 95% credible interval for tau: [", round(tau_lower, 4), ", ", round(tau_upper, 4), "]\n", sep = "")
      
      if (tau_lower < 0.1 && tau_mean < 0.3) {
        cat("- The between-study heterogeneity appears to be low.\n")
      } else if (tau_mean < 0.5) {
        cat("- The between-study heterogeneity appears to be moderate.\n")
      } else {
        cat("- The between-study heterogeneity appears to be high.\n")
      }
      
      # Probability of direction
      if (input$effectMeasure == "RR" || input$effectMeasure == "OR") {
        prob_positive <- mean(bmeta$pposterior$z > 0)
        cat("\nProbability of Direction:\n")
        cat("- Probability that the true effect is positive (", input$effectMeasure, " > 1): ", round(prob_positive * 100, 2), "%\n", sep = "")
        cat("- Probability that the true effect is negative (", input$effectMeasure, " < 1): ", round((1 - prob_positive) * 100, 2), "%\n", sep = "")
      }
      
    }, error = function(e) {
      cat("Error in Bayesian meta-analysis: ", e$message, "\n", sep = "")
      cat("This could be due to issues with the data, such as too few studies or extreme heterogeneity.\n")
    })
  })
  
  # Download handler for Bayesian meta-analysis
  output$downloadBayesianMeta <- downloadHandler(
    filename = function() {
      paste("bayesian_meta_analysis_", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      sink(file)
      
      dat <- dataInput()
      if (nrow(dat) < 2) {
        cat("At least 2 studies are required for Bayesian meta-analysis.")
      } else if (!requireNamespace("bayesmeta", quietly = TRUE)) {
        cat("The 'bayesmeta' package is required for Bayesian meta-analysis.\n")
        cat("Please install it by running: install.packages('bayesmeta')\n")
      } else {
        m.bin <- metabin(
          event.e = dat$eventintervention,
          n.e = dat$totalintervention,
          event.c = dat$eventcontrol,
          n.c = dat$totalcontrol,
          studlab = dat$author,
          data = dat,
          sm = input$effectMeasure,
          method = input$method,
          method.tau = input$method.tau,
          comb.fixed = (input$effectModel == "FE"),
          comb.random = (input$effectModel == "RE")
        )
        
        log_effect <- m.bin$TE
        se_effect <- m.bin$seTE
        
        tryCatch({
          bmeta <- bayesmeta::bayesmeta(
            y = log_effect, 
            sigma = se_effect,
            labels = dat$author
          )
          
          cat("=================================================================\n")
          cat("BAYESIAN META-ANALYSIS\n")
          cat("=================================================================\n\n")
          print(summary(bmeta))
          
          cat("\n=================================================================\n")
          cat("INTERPRETATION\n")
          cat("=================================================================\n\n")
          
          mu_mean <- bmeta$summary["mu", "mean"]
          mu_lower <- bmeta$summary["mu", "2.5%"]
          mu_upper <- bmeta$summary["mu", "97.5%"]
          
          cat("Pooled Effect:\n")
          cat("- Posterior mean of the pooled effect (log scale): ", round(mu_mean, 4), "\n", sep = "")
          cat("- 95% credible interval (log scale): [", round(mu_lower, 4), ", ", round(mu_upper, 4), "]\n", sep = "")
          
          if (input$effectMeasure == "RR" || input$effectMeasure == "OR") {
            cat("- Posterior mean of the pooled effect (original scale): ", round(exp(mu_mean), 4), " ", input$effectMeasure, "\n", sep = "")
            cat("- 95% credible interval (original scale): [", round(exp(mu_lower), 4), ", ", round(exp(mu_upper), 4), "] ", input$effectMeasure, "\n", sep = "")
            
            if (exp(mu_lower) > 1) {
              cat("- Since the entire 95% credible interval is above 1, there is strong evidence of a positive effect.\n")
            } else if (exp(mu_upper) < 1) {
              cat("- Since the entire 95% credible interval is below 1, there is strong evidence of a negative effect.\n")
            } else {
              cat("- Since the 95% credible interval includes 1, the evidence does not strongly support either a positive or negative effect.\n")
            }
          }
          
          tau_mean <- bmeta$summary["tau", "mean"]
          tau_lower <- bmeta$summary["tau", "2.5%"]
          tau_upper <- bmeta$summary["tau", "97.5%"]
          
          cat("\nHeterogeneity:\n")
          cat("- Posterior mean of tau (between-study standard deviation): ", round(tau_mean, 4), "\n", sep = "")
          cat("- 95% credible interval for tau: [", round(tau_lower, 4), ", ", round(tau_upper, 4), "]\n", sep = "")
          
          if (tau_lower < 0.1 && tau_mean < 0.3) {
            cat("- The between-study heterogeneity appears to be low.\n")
          } else if (tau_mean < 0.5) {
            cat("- The between-study heterogeneity appears to be moderate.\n")
          } else {
            cat("- The between-study heterogeneity appears to be high.\n")
          }
          
          if (input$effectMeasure == "RR" || input$effectMeasure == "OR") {
            prob_positive <- mean(bmeta$pposterior$z > 0)
            cat("\nProbability of Direction:\n")
            cat("- Probability that the true effect is positive (", input$effectMeasure, " > 1): ", round(prob_positive * 100, 2), "%\n", sep = "")
            cat("- Probability that the true effect is negative (", input$effectMeasure, " < 1): ", round((1 - prob_positive) * 100, 2), "%\n", sep = "")
          }
        }, error = function(e) {
          cat("Error in Bayesian meta-analysis: ", e$message, "\n", sep = "")
          cat("This could be due to issues with the data, such as too few studies or extreme heterogeneity.\n")
        })
      }
      
      sink()
    }
  )
  
  # Download handler for leave-one-out analysis
  output$downloadLeaveOneOut <- downloadHandler(
    filename = function() {
      paste("leave_one_out_analysis_", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      sink(file)
      dat <- dataInput()
      m.bin <- metabin(
        event.e = dat$eventintervention,
        n.e = dat$totalintervention,
        event.c = dat$eventcontrol,
        n.c = dat$totalcontrol,
        studlab = dat$author,
        data = dat,
        sm = input$effectMeasure,
        method = input$method,
        method.tau = input$method.tau,
        comb.fixed = input$effectModel == "FE",
        comb.random = input$effectModel == "RE"
      )
      loo <- metainf(m.bin)
      print(loo)
      
      sink()
    }
  )
  
  # One Moderator Meta-Regression
  output$metaRegOneModResult <- renderPrint({
    # First, ensure data is available and print diagnostic information
    req(dataInput())
    dat <- dataInput()
    
    # Print column names to diagnose issues
    cat("Available columns in the dataset:", paste(names(dat), collapse=", "), "\n\n")
    
    # Check if moderator is present
    if (!("Reg" %in% names(dat))) {
      cat("Moderator 'Reg' not found in the dataset. Please ensure your CSV contains a column named 'Reg'.")
      return(NULL)
    }
    
    # Check if required data columns are present
    required_cols <- c("eventintervention", "totalintervention", "eventcontrol", "totalcontrol", "author")
    missing_cols <- required_cols[!required_cols %in% names(dat)]
    
    if (length(missing_cols) > 0) {
      cat("Missing required columns:", paste(missing_cols, collapse=", "), 
          "\nPlease ensure your CSV contains these columns.")
      return(NULL)
    }
    
    # Print first few rows of data to diagnose issues
    cat("First rows of data (showing key columns):\n")
    print(head(dat[, c("author", "eventintervention", "totalintervention", "eventcontrol", "totalcontrol", "Reg")]))
    cat("\n")
    
    tryCatch({
      # Run meta-analysis with error handling
      m.bin <- metabin(
        event.e = dat$eventintervention,
        n.e = dat$totalintervention,
        event.c = dat$eventcontrol,
        n.c = dat$totalcontrol,
        studlab = dat$author,
        data = dat,
        sm = input$effectMeasure,
        method = input$method,
        method.tau = input$method.tau,
        comb.fixed = (input$effectModel == "FE"),
        comb.random = (input$effectModel == "RE")
      )
      
      # Run meta-regression with one moderator
      m.reg1 <- metareg(m.bin, ~ Reg)
      
      # Print detailed results
      cat("=================================================================\n")
      cat("META-REGRESSION WITH ONE MODERATOR\n")
      cat("=================================================================\n\n")
      cat("Moderator: Reg\n\n")
      print(summary(m.reg1))
      
      # Print additional interpretation
      cat("\n=================================================================\n")
      cat("INTERPRETATION\n")
      cat("=================================================================\n\n")
      
      # Extract coefficient and p-value
      coef <- m.reg1$beta[2]
      p_val <- m.reg1$pval[2]
      
      if (p_val < 0.05) {
        cat("The moderator 'Reg' has a statistically significant effect on the outcome (p = ", 
            round(p_val, 4), ").\n", sep = "")
        
        if (coef > 0) {
          cat("For each one-unit increase in 'Reg', the ", input$effectMeasure, " increases by approximately ", 
              round(exp(coef), 2), " (exp(", round(coef, 4), ")).\n", sep = "")
        } else {
          cat("For each one-unit increase in 'Reg', the ", input$effectMeasure, " decreases by approximately ", 
              round(exp(-coef), 2), " (exp(", round(-coef, 4), ")).\n", sep = "")
        }
      } else {
        cat("The moderator 'Reg' does not have a statistically significant effect on the outcome (p = ", 
            round(p_val, 4), ").\n", sep = "")
      }
      
      # Report heterogeneity
      cat("\nResidual heterogeneity: I² = ", round(m.reg1$I2.resid * 100, 1), "%\n", sep = "")
      if (m.reg1$I2.resid * 100 < 30) {
        cat("The remaining heterogeneity is low, suggesting that 'Reg' explains much of the variability between studies.\n")
      } else if (m.reg1$I2.resid * 100 < 60) {
        cat("The remaining heterogeneity is moderate, suggesting that 'Reg' explains some but not all of the variability between studies.\n")
      } else {
        cat("The remaining heterogeneity is high, suggesting that 'Reg' explains only a small portion of the variability between studies.\n")
      }
    }, error = function(e) {
      cat("Error in meta-analysis or meta-regression: ", e$message, "\n")
      cat("Please check your data and input parameters.\n")
    })
  })
  
  # Download handler for one-moderator meta-regression
  output$downloadMetaRegOne <- downloadHandler(
    filename = function() {
      paste("one_moderator_meta_regression_", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      sink(file)
      
      dat <- dataInput()
      if (!("Reg" %in% names(dat))) {
        cat("Moderator 'Reg' not found in the dataset. Please ensure your CSV contains a column named 'Reg'.")
      } else {
        tryCatch({
          m.bin <- metabin(
            event.e = dat$eventintervention,
            n.e = dat$totalintervention,
            event.c = dat$eventcontrol,
            n.c = dat$totalcontrol,
            studlab = dat$author,
            data = dat,
            sm = input$effectMeasure,
            method = input$method,
            method.tau = input$method.tau,
            comb.fixed = (input$effectModel == "FE"),
            comb.random = (input$effectModel == "RE")
          )
          
          m.reg1 <- metareg(m.bin, ~ Reg)
          
          cat("=================================================================\n")
          cat("META-REGRESSION WITH ONE MODERATOR\n")
          cat("=================================================================\n\n")
          cat("Moderator: Reg\n\n")
          print(summary(m.reg1))
          
          # Print additional interpretation
          cat("\n=================================================================\n")
          cat("INTERPRETATION\n")
          cat("=================================================================\n\n")
          
          coef <- m.reg1$beta[2]
          p_val <- m.reg1$pval[2]
          
          if (p_val < 0.05) {
            cat("The moderator 'Reg' has a statistically significant effect on the outcome (p = ", 
                round(p_val, 4), ").\n", sep = "")
            
            if (coef > 0) {
              cat("For each one-unit increase in 'Reg', the ", input$effectMeasure, " increases by approximately ", 
                  round(exp(coef), 2), " (exp(", round(coef, 4), ")).\n", sep = "")
            } else {
              cat("For each one-unit increase in 'Reg', the ", input$effectMeasure, " decreases by approximately ", 
                  round(exp(-coef), 2), " (exp(", round(-coef, 4), ")).\n", sep = "")
            }
          } else {
            cat("The moderator 'Reg' does not have a statistically significant effect on the outcome (p = ", 
                round(p_val, 4), ").\n", sep = "")
          }
          
          cat("\nResidual heterogeneity: I² = ", round(m.reg1$I2.resid * 100, 1), "%\n", sep = "")
          if (m.reg1$I2.resid * 100 < 30) {
            cat("The remaining heterogeneity is low, suggesting that 'Reg' explains much of the variability between studies.\n")
          } else if (m.reg1$I2.resid * 100 < 60) {
            cat("The remaining heterogeneity is moderate, suggesting that 'Reg' explains some but not all of the variability between studies.\n")
          } else {
            cat("The remaining heterogeneity is high, suggesting that 'Reg' explains only a small portion of the variability between studies.\n")
          }
        }, error = function(e) {
          cat("Error in meta-analysis or meta-regression: ", e$message, "\n")
          cat("Please check your data and input parameters.\n")
        })
      }
      
      sink()
    }
  )
  
  # Two Moderators Meta-Regression (this one is working, but adding error handling for consistency)
  output$metaRegTwoModResult <- renderPrint({
    req(dataInput())
    dat <- dataInput()
    
    # Check if moderators are present
    if (!all(c("Reg", "Reg2") %in% names(dat))) {
      cat("One or both moderators ('Reg', 'Reg2') are not found in the dataset. Please ensure your CSV contains columns named 'Reg' and 'Reg2'.")
      return(NULL)
    }
    
    # Check if required data columns are present
    required_cols <- c("eventintervention", "totalintervention", "eventcontrol", "totalcontrol", "author")
    missing_cols <- required_cols[!required_cols %in% names(dat)]
    
    if (length(missing_cols) > 0) {
      cat("Missing required columns:", paste(missing_cols, collapse=", "), 
          "\nPlease ensure your CSV contains these columns.")
      return(NULL)
    }
    
    tryCatch({
      # Run meta-analysis
      m.bin <- metabin(
        event.e = dat$eventintervention,
        n.e = dat$totalintervention,
        event.c = dat$eventcontrol,
        n.c = dat$totalcontrol,
        studlab = dat$author,
        data = dat,
        sm = input$effectMeasure,
        method = input$method,
        method.tau = input$method.tau,
        comb.fixed = (input$effectModel == "FE"),
        comb.random = (input$effectModel == "RE")
      )
      
      # Run meta-regression with two moderators
      m.reg2 <- metareg(m.bin, ~ Reg + Reg2)
      
      # Print detailed results
      cat("=================================================================\n")
      cat("META-REGRESSION WITH TWO MODERATORS\n")
      cat("=================================================================\n\n")
      cat("Moderators: Reg, Reg2\n\n")
      print(summary(m.reg2))
      
      # Print additional interpretation
      cat("\n=================================================================\n")
      cat("INTERPRETATION\n")
      cat("=================================================================\n\n")
      
      # Check correlation between moderators
      cor_val <- cor(dat$Reg, dat$Reg2)
      cat("Correlation between moderators: r = ", round(cor_val, 4), "\n", sep = "")
      
      if (abs(cor_val) > 0.7) {
        cat("WARNING: The two moderators are highly correlated (r > 0.7), which may lead to multicollinearity issues.\n\n")
      }
      
      # Extract coefficients and p-values
      coef1 <- m.reg2$beta[2]
      p_val1 <- m.reg2$pval[2]
      coef2 <- m.reg2$beta[3]
      p_val2 <- m.reg2$pval[3]
      
      # Interpret moderator 1
      cat("Moderator 1 (Reg):\n")
      if (p_val1 < 0.05) {
        cat("- Statistically significant effect (p = ", round(p_val1, 4), ")\n", sep = "")
        if (coef1 > 0) {
          cat("- For each one-unit increase in 'Reg' (controlling for 'Reg2'), the ", input$effectMeasure, 
              " increases by approximately ", round(exp(coef1), 2), " (exp(", round(coef1, 4), "))\n", sep = "")
        } else {
          cat("- For each one-unit increase in 'Reg' (controlling for 'Reg2'), the ", input$effectMeasure, 
              " decreases by approximately ", round(exp(-coef1), 2), " (exp(", round(-coef1, 4), "))\n", sep = "")
        }
      } else {
        cat("- No statistically significant effect (p = ", round(p_val1, 4), ")\n", sep = "")
      }
      
      # Interpret moderator 2
      cat("\nModerator 2 (Reg2):\n")
      if (p_val2 < 0.05) {
        cat("- Statistically significant effect (p = ", round(p_val2, 4), ")\n", sep = "")
        if (coef2 > 0) {
          cat("- For each one-unit increase in 'Reg2' (controlling for 'Reg'), the ", input$effectMeasure, 
              " increases by approximately ", round(exp(coef2), 2), " (exp(", round(coef2, 4), "))\n", sep = "")
        } else {
          cat("- For each one-unit increase in 'Reg2' (controlling for 'Reg'), the ", input$effectMeasure, 
              " decreases by approximately ", round(exp(-coef2), 2), " (exp(", round(-coef2, 4), "))\n", sep = "")
        }
      } else {
        cat("- No statistically significant effect (p = ", round(p_val2, 4), ")\n", sep = "")
      }
      
      # Report model fit
      cat("\nModel fit:\n")
      cat("- R² (amount of heterogeneity explained): ", round(m.reg2$R2, 4) * 100, "%\n", sep = "")
      cat("- Residual heterogeneity: I² = ", round(m.reg2$I2.resid * 100, 1), "%\n", sep = "")
      
      if (m.reg2$R2 < 0.3) {
        cat("- The moderators explain a small proportion of the variability between studies.\n")
      } else if (m.reg2$R2 < 0.6) {
        cat("- The moderators explain a moderate proportion of the variability between studies.\n")
      } else {
        cat("- The moderators explain a large proportion of the variability between studies.\n")
      }
    }, error = function(e) {
      cat("Error in meta-analysis or meta-regression: ", e$message, "\n")
      cat("Please check your data and input parameters.\n")
    })
  })
  
  # Three Moderators Meta-Regression
  output$metaRegThreeModResult <- renderPrint({
    req(dataInput())
    dat <- dataInput()
    
    # Print column names to diagnose issues
    cat("Available columns in the dataset:", paste(names(dat), collapse=", "), "\n\n")
    
    # Check if moderators are present
    if (!all(c("Reg", "Reg2", "Reg3") %in% names(dat))) {
      cat("One or more moderators ('Reg', 'Reg2', 'Reg3') are not found in the dataset. Please ensure your CSV contains columns named 'Reg', 'Reg2', and 'Reg3'.")
      return(NULL)
    }
    
    # Check if required data columns are present
    required_cols <- c("eventintervention", "totalintervention", "eventcontrol", "totalcontrol", "author")
    missing_cols <- required_cols[!required_cols %in% names(dat)]
    
    if (length(missing_cols) > 0) {
      cat("Missing required columns:", paste(missing_cols, collapse=", "), 
          "\nPlease ensure your CSV contains these columns.")
      return(NULL)
    }
    
    # Print first few rows of data to diagnose issues
    cat("First rows of data (showing key columns):\n")
    print(head(dat[, c("author", "eventintervention", "totalintervention", "eventcontrol", "totalcontrol", "Reg", "Reg2", "Reg3")]))
    cat("\n")
    
    tryCatch({
      # Run meta-analysis
      m.bin <- metabin(
        event.e = dat$eventintervention,
        n.e = dat$totalintervention,
        event.c = dat$eventcontrol,
        n.c = dat$totalcontrol,
        studlab = dat$author,
        data = dat,
        sm = input$effectMeasure,
        method = input$method,
        method.tau = input$method.tau,
        comb.fixed = (input$effectModel == "FE"),
        comb.random = (input$effectModel == "RE")
      )
      
      # Run meta-regression with three moderators
      m.reg3 <- metareg(m.bin, ~ Reg + Reg2 + Reg3)
      
      # Print detailed results
      cat("=================================================================\n")
      cat("META-REGRESSION WITH THREE MODERATORS\n")
      cat("=================================================================\n\n")
      cat("Moderators: Reg, Reg2, Reg3\n\n")
      print(summary(m.reg3))
      
      # Print additional interpretation
      cat("\n=================================================================\n")
      cat("INTERPRETATION\n")
      cat("=================================================================\n\n")
      
      # Check correlation matrix between moderators
      cor_matrix <- cor(dat[, c("Reg", "Reg2", "Reg3")])
      cat("Correlation matrix between moderators:\n")
      print(round(cor_matrix, 4))
      cat("\n")
      
      # Check for high correlations
      high_cor <- FALSE
      if (abs(cor_matrix[1,2]) > 0.7 || abs(cor_matrix[1,3]) > 0.7 || abs(cor_matrix[2,3]) > 0.7) {
        cat("WARNING: Some moderators are highly correlated (r > 0.7), which may lead to multicollinearity issues.\n\n")
        high_cor <- TRUE
      }
      
      # Calculate Variance Inflation Factors if possible
      if (requireNamespace("car", quietly = TRUE)) {
        # Use linear regression to approximate VIFs
        temp_df <- data.frame(y = m.bin$TE, dat[, c("Reg", "Reg2", "Reg3")])
        temp_lm <- lm(y ~ Reg + Reg2 + Reg3, data = temp_df)
        vifs <- car::vif(temp_lm)
        
        cat("Variance Inflation Factors (VIF):\n")
        print(vifs)
        cat("\n")
        
        if (any(vifs > 5)) {
          cat("WARNING: VIF values > 5 indicate potential multicollinearity issues.\n\n")
        }
      } else if (high_cor) {
        cat("Note: Install the 'car' package to calculate Variance Inflation Factors (VIF) for a more detailed assessment of multicollinearity.\n\n")
      }
      
      # Extract coefficients and p-values
      coef1 <- m.reg3$beta[2]
      p_val1 <- m.reg3$pval[2]
      coef2 <- m.reg3$beta[3]
      p_val2 <- m.reg3$pval[3]
      coef3 <- m.reg3$beta[4]
      p_val3 <- m.reg3$pval[4]
      
      # Interpret moderator 1
      cat("Moderator 1 (Reg):\n")
      if (p_val1 < 0.05) {
        cat("- Statistically significant effect (p = ", round(p_val1, 4), ")\n", sep = "")
        if (coef1 > 0) {
          cat("- For each one-unit increase in 'Reg' (controlling for other moderators), the ", input$effectMeasure, 
              " increases by approximately ", round(exp(coef1), 2), " (exp(", round(coef1, 4), "))\n", sep = "")
        } else {
          cat("- For each one-unit increase in 'Reg' (controlling for other moderators), the ", input$effectMeasure, 
              " decreases by approximately ", round(exp(-coef1), 2), " (exp(", round(-coef1, 4), "))\n", sep = "")
        }
      } else {
        cat("- No statistically significant effect (p = ", round(p_val1, 4), ")\n", sep = "")
      }
      
      # Interpret moderator 2
      cat("\nModerator 2 (Reg2):\n")
      if (p_val2 < 0.05) {
        cat("- Statistically significant effect (p = ", round(p_val2, 4), ")\n", sep = "")
        if (coef2 > 0) {
          cat("- For each one-unit increase in 'Reg2' (controlling for other moderators), the ", input$effectMeasure, 
              " increases by approximately ", round(exp(coef2), 2), " (exp(", round(coef2, 4), "))\n", sep = "")
        } else {
          cat("- For each one-unit increase in 'Reg2' (controlling for other moderators), the ", input$effectMeasure, 
              " decreases by approximately ", round(exp(-coef2), 2), " (exp(", round(-coef2, 4), "))\n", sep = "")
        }
      } else {
        cat("- No statistically significant effect (p = ", round(p_val2, 4), ")\n", sep = "")
      }
      
      # Interpret moderator 3
      cat("\nModerator 3 (Reg3):\n")
      if (p_val3 < 0.05) {
        cat("- Statistically significant effect (p = ", round(p_val3, 4), ")\n", sep = "")
        if (coef3 > 0) {
          cat("- For each one-unit increase in 'Reg3' (controlling for other moderators), the ", input$effectMeasure, 
              " increases by approximately ", round(exp(coef3), 2), " (exp(", round(coef3, 4), "))\n", sep = "")
        } else {
          cat("- For each one-unit increase in 'Reg3' (controlling for other moderators), the ", input$effectMeasure, 
              " decreases by approximately ", round(exp(-coef3), 2), " (exp(", round(-coef3, 4), "))\n", sep = "")
        }
      } else {
        cat("- No statistically significant effect (p = ", round(p_val3, 4), ")\n", sep = "")
      }
      
      # Report model fit
      cat("\nModel fit:\n")
      cat("- R² (amount of heterogeneity explained): ", round(m.reg3$R2, 4) * 100, "%\n", sep = "")
      cat("- Residual heterogeneity: I² = ", round(m.reg3$I2.resid * 100, 1), "%\n", sep = "")
      
      if (m.reg3$R2 < 0.3) {
        cat("- The moderators explain a small proportion of the variability between studies.\n")
      } else if (m.reg3$R2 < 0.6) {
        cat("- The moderators explain a moderate proportion of the variability between studies.\n")
      } else {
        cat("- The moderators explain a large proportion of the variability between studies.\n")
      }
      
      # Model comparison recommendations
      cat("\nModel Selection:\n")
      cat("To determine if all three moderators are necessary, consider comparing this model with simpler models using fewer moderators. Look at the changes in R² and AIC values. A significant decrease in AIC and increase in R² would suggest that the more complex model is justified.\n")
    }, error = function(e) {
      cat("Error in meta-analysis or meta-regression: ", e$message, "\n")
      cat("Please check your data and input parameters.\n")
    })
  })
  
  # Download handler for three-moderator meta-regression
  output$downloadMetaRegThree <- downloadHandler(
    filename = function() {
      paste("three_moderator_meta_regression_", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      sink(file)
      
      dat <- dataInput()
      if (!all(c("Reg", "Reg2", "Reg3") %in% names(dat))) {
        cat("One or more moderators ('Reg', 'Reg2', 'Reg3') are not found in the dataset. Please ensure your CSV contains columns named 'Reg', 'Reg2', and 'Reg3'.")
      } else {
        tryCatch({
          m.bin <- metabin(
            event.e = dat$eventintervention,
            n.e = dat$totalintervention,
            event.c = dat$eventcontrol,
            n.c = dat$totalcontrol,
            studlab = dat$author,
            data = dat,
            sm = input$effectMeasure,
            method = input$method,
            method.tau = input$method.tau,
            comb.fixed = (input$effectModel == "FE"),
            comb.random = (input$effectModel == "RE")
          )
          
          m.reg3 <- metareg(m.bin, ~ Reg + Reg2 + Reg3)
          
          cat("=================================================================\n")
          cat("META-REGRESSION WITH THREE MODERATORS\n")
          cat("=================================================================\n\n")
          cat("Moderators: Reg, Reg2, Reg3\n\n")
          print(summary(m.reg3))
          
          # Print additional interpretation
          cat("\n=================================================================\n")
          cat("INTERPRETATION\n")
          cat("=================================================================\n\n")
          
          cor_matrix <- cor(dat[, c("Reg", "Reg2", "Reg3")])
          cat("Correlation matrix between moderators:\n")
          print(round(cor_matrix, 4))
          cat("\n")
          
          high_cor <- FALSE
          if (abs(cor_matrix[1,2]) > 0.7 || abs(cor_matrix[1,3]) > 0.7 || abs(cor_matrix[2,3]) > 0.7) {
            cat("WARNING: Some moderators are highly correlated (r > 0.7), which may lead to multicollinearity issues.\n\n")
            high_cor <- TRUE
          }
          
          if (requireNamespace("car", quietly = TRUE)) {
            temp_df <- data.frame(y = m.bin$TE, dat[, c("Reg", "Reg2", "Reg3")])
            temp_lm <- lm(y ~ Reg + Reg2 + Reg3, data = temp_df)
            vifs <- car::vif(temp_lm)
            
            cat("Variance Inflation Factors (VIF):\n")
            print(vifs)
            cat("\n")
            
            if (any(vifs > 5)) {
              cat("WARNING: VIF values > 5 indicate potential multicollinearity issues.\n\n")
            }
          } else if (high_cor) {
            cat("Note: Install the 'car' package to calculate Variance Inflation Factors (VIF) for a more detailed assessment of multicollinearity.\n\n")
          }
          
          coef1 <- m.reg3$beta[2]
          p_val1 <- m.reg3$pval[2]
          coef2 <- m.reg3$beta[3]
          p_val2 <- m.reg3$pval[3]
          coef3 <- m.reg3$beta[4]
          p_val3 <- m.reg3$pval[4]
          
          cat("Moderator 1 (Reg):\n")
          if (p_val1 < 0.05) {
            cat("- Statistically significant effect (p = ", round(p_val1, 4), ")\n", sep = "")
            if (coef1 > 0) {
              cat("- For each one-unit increase in 'Reg' (controlling for other moderators), the ", input$effectMeasure, 
                  " increases by approximately ", round(exp(coef1), 2), " (exp(", round(coef1, 4), "))\n", sep = "")
            } else {
              cat("- For each one-unit increase in 'Reg' (controlling for other moderators), the ", input$effectMeasure, 
                  " decreases by approximately ", round(exp(-coef1), 2), " (exp(", round(-coef1, 4), "))\n", sep = "")
            }
          } else {
            cat("- No statistically significant effect (p = ", round(p_val1, 4), ")\n", sep = "")
          }
          
          cat("\nModerator 2 (Reg2):\n")
          if (p_val2 < 0.05) {
            cat("- Statistically significant effect (p = ", round(p_val2, 4), ")\n", sep = "")
            if (coef2 > 0) {
              cat("- For each one-unit increase in 'Reg2' (controlling for other moderators), the ", input$effectMeasure, 
                  " increases by approximately ", round(exp(coef2), 2), " (exp(", round(coef2, 4), "))\n", sep = "")
            } else {
              cat("- For each one-unit increase in 'Reg2' (controlling for other moderators), the ", input$effectMeasure, 
                  " decreases by approximately ", round(exp(-coef2), 2), " (exp(", round(-coef2, 4), "))\n", sep = "")
            }
          } else {
            cat("- No statistically significant effect (p = ", round(p_val2, 4), ")\n", sep = "")
          }
          
          cat("\nModerator 3 (Reg3):\n")
          if (p_val3 < 0.05) {
            cat("- Statistically significant effect (p = ", round(p_val3, 4), ")\n", sep = "")
            if (coef3 > 0) {
              cat("- For each one-unit increase in 'Reg3' (controlling for other moderators), the ", input$effectMeasure, 
                  " increases by approximately ", round(exp(coef3), 2), " (exp(", round(coef3, 4), "))\n", sep = "")
            } else {
              cat("- For each one-unit increase in 'Reg3' (controlling for other moderators), the ", input$effectMeasure, 
                  " decreases by approximately ", round(exp(-coef3), 2), " (exp(", round(-coef3, 4), "))\n", sep = "")
            }
          } else {
            cat("- No statistically significant effect (p = ", round(p_val3, 4), ")\n", sep = "")
          }
          
          cat("\nModel fit:\n")
          cat("- R² (amount of heterogeneity explained): ", round(m.reg3$R2, 4) * 100, "%\n", sep = "")
          cat("- Residual heterogeneity: I² = ", round(m.reg3$I2.resid * 100, 1), "%\n", sep = "")
          
          if (m.reg3$R2 < 0.3) {
            cat("- The moderators explain a small proportion of the variability between studies.\n")
          } else if (m.reg3$R2 < 0.6) {
            cat("- The moderators explain a moderate proportion of the variability between studies.\n")
          } else {
            cat("- The moderators explain a large proportion of the variability between studies.\n")
          }
          
          cat("\nModel Selection:\n")
          cat("To determine if all three moderators are necessary, consider comparing this model with simpler models using fewer moderators. Look at the changes in R² and AIC values. A significant decrease in AIC and increase in R² would suggest that the more complex model is justified.\n")
        }, error = function(e) {
          cat("Error in meta-analysis or meta-regression: ", e$message, "\n")
          cat("Please check your data and input parameters.\n")
        })
      }
      
      sink()
    }
  )
  
  output$funnelPlot <- renderPlot({
    req(dataInput())
    dat <- dataInput()
    m.bin <- metabin(event.e = dat$eventintervention,
                     n.e = dat$totalintervention,
                     event.c = dat$eventcontrol,
                     n.c = dat$totalcontrol,
                     studlab = dat$author,
                     data = dat,
                     sm = input$effectMeasure,
                     method = input$method,
                     method.tau = input$method.tau,
                     comb.fixed = input$effectModel == "FE",
                     comb.random = input$effectModel == "RE")
    
    # Define fill colors for contour
    col.contour <- c("gray75", "gray85", "gray95")
    
    if (input$colorFunnel) {
      # Generate color-enhanced funnel plot
      funnel(m.bin, xlim = c(0.1, 5), contour = c(0.9, 0.95, 0.99), col.contour = col.contour)
      
      # Add a legend
      legend("topright", inset = 0.05,
             legend = c("p < 0.1", "p < 0.05", "p < 0.01"),
             fill = col.contour)
      
      # Add a title
      title(main = "Contour-Enhanced Funnel Plot")
    } else {
      # Generate standard funnel plot
      funnel(m.bin, xlim = input$xAxisRange)
    }
  })
}


# Run the application
shinyApp(ui = ui, server = server) 
