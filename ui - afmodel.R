library(shiny)
library(excelR)

ui <- fluidPage(
  tags$head(
    tags$script(src="https://bossanova.uk/jspreadsheet/v3/jexcel.js"),
    tags$link(rel="stylesheet", href="https://bossanova.uk/jspreadsheet/v3/jexcel.css", type="text/css"),
    tags$script(src="https://jsuites.net/v3/jsuites.js"),
    tags$link(rel="stylesheet", href="https://jsuites.net/v3/jsuites.css", type="text/css"),
    
    tags$link(rel = "stylesheet", type = "text/css", href = "table.css"),
  ),
  
  # theme = bslib::bs_theme(bootswatch = "readable"),
  titlePanel("SExI-FS Tree Species Parameters"),
  tags$hr(),
    fluidRow(
      column(4, fileInput("file1", "Load species (*.trs) file",
        accept = c("SExI-FS species parameters file",".trs"))),
      column(8, wellPanel(
        actionButton("add_species", label = "Add default species", icon = icon("plus")),
        actionButton("remove", label = "Remove", icon = icon("minus")),
        actionButton("duplicate", label = "Duplicate", icon = icon("copy")),
        downloadButton('downloadData', 'Download')
      ))
  ),
  navlistPanel( "Parameters", widths = c(3, 9),
    tabPanel(get_params_groups()[1],
      h3(parameters[[1]]$title),
      excelOutput(get_params_groups()[1])
    ),
    tabPanel(get_params_groups()[2], h3(parameters[[2]]$title), uiOutput('page_2')),
    tabPanel(get_params_groups()[3], h3(parameters[[3]]$title), uiOutput('page_3')),
    tabPanel(get_params_groups()[4], h3(parameters[[4]]$title), uiOutput('page_4')),
    tabPanel(get_params_groups()[5], h3(parameters[[5]]$title), uiOutput('page_5')),
    tabPanel(get_params_groups()[6],
             h3(parameters[[6]]$title),
             excelOutput(get_params_groups()[6])),
    tabPanel(get_params_groups()[7],
             h3(parameters[[7]]$title),
             excelOutput(get_params_groups()[7])),
    tabPanel(get_params_groups()[8],
             h3(parameters[[8]]$title),
             excelOutput(get_params_groups()[8])),
    tabPanel(get_params_groups()[9],
             h3(parameters[[9]]$title),
             excelOutput(get_params_groups()[9]))
    
  )

  
)