source('setup.R')

shinyUI(fluidPage(
  headerPanel('Differential ChIPSeq methods comparison'),
  fluidRow(
    column(width = 6, checkboxGroupInput(inputId = 'display_filter', label = 'Point Display Filtering', choices = display_filter_choices, selected = display_filter_choices)),
    column(width = 6, radioButtons(inputId = 'selection_filter', label = 'Point Selection Filtering', choices = selection_filter_choices, selected = selection_filter_choices[1])),
    column(width = 6, checkboxGroupInput(inputId = 'available_methods', label = 'Differential Methods', choices = selection_method_choices, selected = selection_method_choices[1]))
  ),
  fluidRow(
    column(width = 6, uiOutput(outputId = 'x_select')),
    column(width = 6, uiOutput(outputId = 'y_select'))
  ),
#   fluidRow(
#     column(width = 3,radioButtons(inputId = 'updown', label = 'Fold change direction', choices = c('up', 'down', 'either', 'no change'), selected = 'either'))
#   ),
  fluidRow(
    column(width = 4, sliderInput('pval_threshold', label = '-log10 p-value threshold', min = 0, max = 150, value = 9)),
    column(width = 4, sliderInput('fc_threshold', label = 'log2 fold-change threshold', min = 0, max = 10, value = 2, step = .5))#,
    #column(width = 4, sliderInput('maxes_threshold', label = 'log2 max threshold', min = 0, max = 16, value = 2))
  ),
  fluidRow(
    column(plotOutput('volcano',
                      dblclick = "volcano_dblclick",
                      click = 'volcano_click',
                      brush = brushOpts(id = 'volcano_brush', delay = 600, delayType = 'debounce', resetOnNew = T),
                      hover = 'volcano_hover'),
           width = 6),
    column(plotOutput('venn'), width = 6)
    
  )#,
  #uiOutput('select_gene_list')
)

)