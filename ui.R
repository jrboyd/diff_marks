library('shiny')

shinyUI(fluidPage(
  headerPanel('Differential ChIPSeq methods comparison'),
  fluidRow(
    column(width = 6, radioButtons(inputId = 'selection_filter', label = 'Point Selection Filtering', choices = c('No filter', 'List A', 'List B', 'Exclude Lists'), selected = 'No filter')),
    column(width = 6, checkboxGroupInput(inputId = 'available_lists', label = 'Available List', choices = c('Fold Change', 'MAnorm', 'MACS2 bdgdiff'), selected = 'Fold Change'))
  ),
  fluidRow(
    column(width = 3,radioButtons(inputId = 'updown', label = 'Fold change direction', choices = c('up', 'down', 'either', 'no change'), selected = 'either'))
  ),
  fluidRow(
    column(width = 4, sliderInput('padj_threshold', label = '-log10 p-value threshold', min = 0, max = 9, value = 2)),
    column(width = 4, sliderInput('fc_threshold', label = 'log2 fold-change threshold', min = 0, max = 10, value = 2, step = .5)),
    column(width = 4, sliderInput('maxes_threshold', label = 'log2 max threshold', min = 0, max = 16, value = 2))
  ),
  fluidRow(
    column(plotOutput('volcano', dblclick = "volcano_dblclick", click = 'volcano_click', brush = brushOpts(id = 'volcano_brush', delay = 600, delayType = 'debounce'), hover = 'volcano_hover'), width = 6),
    column(plotOutput('venn'), width = 6)
    
  ),
  uiOutput('select_gene_list')
)

)