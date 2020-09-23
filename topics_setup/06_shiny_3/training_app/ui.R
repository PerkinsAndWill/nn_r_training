ui = dashboardPage(
  
  dashboardHeader(title = 'Belmont Station Inventory',titleWidth = 300),
  
  dashboardSidebar(
    sidebarMenu(
      id='tabs',
      menuItem('Select Countries/States',tabName ='geogs',icon = icon('map'),selected = TRUE),
      menuItem('Browse Results',tabName ='browse',icon = icon('search'))
    ),
    bsButton('execute',label = 'Execute Filters',icon = icon('refresh')),
    pickerInput(inputId = 'maj_sty',label = 'Major Style',
                choices = 'Loading...',multiple = TRUE,
                options = list(
                  `actions-box` = TRUE, 
                  size = 10,
                  `selected-text-format` = "count > 3"
                )),
    pickerInput(inputId = 'min_sty',label='Minor Style',
                choices = 'Loading...',multiple = TRUE,
                options = list(
                  `actions-box` = TRUE, 
                  size = 10,
                  `selected-text-format` = "count > 3"
                ))
  ),
  
  dashboardBody(

    tabItems(
      tabItem('geogs',
              h5('This is a demonstration app based on the live Belmont Station bottle shop inventory available here: https://www.belmont-station.com/bottles'),
              h5('The training content for this app is linked here: https://perkinsandwill.github.io/nn_r_training/topics_output/shiny_3.html'),
              h4('Select countries and/or US states of origin you would like to see inventory for.'),
              leafletOutput('select_map',height = 600),
              actionButton('select_all','Select All States',icon =  icon('check')),
              actionButton('deselect_all','De-select All States',icon = icon('times'))
      ),
      tabItem('browse',
              fluidRow(
                box('Major Style by Geography',
                    width = 12,
                    plotOutput('geog_style_abs',height = 500),
                    plotOutput('geog_style_prop',height = 500)
                ),
                box('Minor Style (within Major Style)',
                    width = 12,
                    plotOutput('minor_style',height = 1500)
                ),
                box(title = 'Belmont Station Inventory (for Selections)',
                    width=12,height = 800,
                    dataTableOutput('inventory',height=600)
                )
              )
              )
              
    )
    
  ),
  
)