library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(excelR)
library(fresh)
library(shinyjs)
library(markdown)


COLOR_DARK <- "#6c584c"
COLOR_LIGHT <- "#F0EAD2"

mytheme <- create_theme(
  adminlte_color(light_blue = COLOR_DARK),
  adminlte_sidebar(
    dark_bg = COLOR_LIGHT,
    dark_hover_bg = "#adc178",
    dark_hover_color = "#000",
    dark_color = COLOR_DARK,
    dark_submenu_color = COLOR_DARK,
    
    dark_submenu_hover_color = "#AC0000",
    dark_submenu_bg = "#DDE5B6"
    
  ),
  adminlte_global(
    content_bg = "#FFF",
    #f0ead2",
    box_bg = "#FFF",
    info_box_bg = "#E0FFD1"
  )
)

create_params_page <- function(i) {
  tabItem(tabName = gsub(" ", "_", get_params_groups()[i]),
          h3(parameters[[i]]$title),
          uiOutput(paste0('page_', i)))
}

ui <- dashboardPage(
  title = "FORCASTREE",
  
  dashboardHeader(title = tagList(
    span(
      class = "logo-lg",
      HTML(
        "F<img src = 'images/fct_logo.svg' style = 'width: 35px; padding: 0px 4px 6px 4px;'>RCASTREE"
      )
    ),
    img(src = "images/fct_logo.svg", style = "width: 20px")
  )),
  
  dashboardSidebar(
    minified = TRUE,
    # collapsed = T,
    sidebarMenu(
      id = "sidemenu",
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem(
        "Tree Parameters",
        tabName = "parameters",
        icon = icon("seedling"),
        lapply(get_params_groups(), function(x) {
          menuSubItem(x, tabName = gsub(" ", "_", x))
        })
      ),
      menuItem(
        "SExI-FS Output",
        tabName = "sexi_output_tab",
        icon = icon("chart-simple")
      ),
      menuItem(
        "Virtual Forest",
        tabName = "Virtual_Forest_tab",
        icon = icon("mountain-sun")
      ),
      menuItem("About", tabName = "about_tab", icon = icon("circle-info"))
    )
  ),
  
  #ICON: tractor, trowel, shoe-prints, person-digging, panorama,
  
  dashboardBody(
    tags$style(
      "
      hr.brown {
        border-top: 3px solid #F0EAD2;
      }

      .borderbox {
        background:#F0EAD2;
        margin:20px 0px;
        padding:10px;
        border-radius:5px;
      }
      
      .btn-file { background-color:#F0EAD2;}

      .progress-bar { background-color: #6c584c;}
    "
    ),

    use_theme(mytheme),
    #setShadow(class = "box"),
    
    tags$script(src = "jexcel.js"),
    tags$link(rel = "stylesheet", href = "jexcel.css", type = "text/css"),
    tags$script(src = "jsuites.js"),
    tags$link(rel = "stylesheet", href = "jsuites.css", type = "text/css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "table.css"),
    
    tabItems(
      tabItem(
        tabName = "home",
        h1(
          img(src = "images/fct_logo.svg", style = "width: 48px; padding: 0px 0px 6px 0px;"),
          tags$b("FORCASTREE"),
          style = 'color:#6c584c'
        ),
        h4(
          HTML(
            "A model <b class='h3' >FOR</b> simulating growth and
              <b class='h3'>CA</b>rbon of fore<b class='h3' >ST</b>
              and agrofores<b class='h3'>TREE</b>"
          )
        ),
        hr(class = "brown"),
        uiOutput("home_page"),
        includeMarkdown("home.md")
      ),
      
      tabItem(
        tabName = sub(" ", "_", get_params_groups()[1]),
        h2(parameters[[1]]$title),
        hr(class = "brown"),
        fileInput(
          "file1",
          "Load species (*.trs) file",
          accept = c("SExI-FS species parameters file", ".trs")
        ),
        div(
          class = "borderbox",
          actionButton("add_species", label = "Add default species", icon = icon("plus")),
          actionButton("remove", label = "Remove", icon = icon("minus")),
          actionButton("duplicate", label = "Duplicate", icon = icon("copy")),
          downloadButton('downloadData', 'Download')
        ),
        excelOutput(get_params_groups()[1])
      ),
      
      create_params_page(2),
      create_params_page(3),
      create_params_page(4),
      create_params_page(5),
      create_params_page(6),
      create_params_page(7),
      create_params_page(8),
      create_params_page(9),
      tabItem(
        tabName = "sexi_output_tab",
        h2("SExI-FS Output Visualizer"),
        hr(class = "brown"),
        uiOutput("sexi_output")
      ),
      tabItem(
        tabName = "Virtual_Forest_tab",
        h2("Virtual Forest"),
        hr(class = "brown"),
        tags$i("To be developed")
      ),
      tabItem(
        tabName = "about_tab",
        h2("About"),
        hr(class = "brown"),
        includeMarkdown("about.md")
      )
      
    )
  )
  
)
