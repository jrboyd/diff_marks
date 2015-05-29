library('shiny')

shinyUI(fluidPage(
  headerPanel('Differential ChIPSeq methods comparison'),
  fluidRow(
    column(width = 6, radioButtons(inputId = 'type', label = 'Select Gene Type', choices = c('Protein Coding', 'lncoding', 'Both'), selected = 'Protein Coding'))
  ),
  fluidRow(
    column(width = 3,radioButtons(inputId = 'updown', label = 'Fold change direction', choices = c('up', 'down', 'either', 'no change'), selected = 'either'))
  ),
  fluidRow(
    column(width = 4, sliderInput('padj_threshold', label = '-log10 p-value threshold', min = 0, max = 9, value = 2)),
    column(width = 4, sliderInput('fc_threshold', label = 'log2 fold-change threshold', min = 0, max = 10, value = 1)),
    column(width = 4, sliderInput('maxes_threshold', label = 'log2 max threshold', min = 0, max = 16, value = 2))
  ),
  fluidRow(
    column(plotOutput('venn'), width = 6),
    column(plotOutput('volcano'), width = 6)
  ),
  uiOutput('select_gene_list')
)

)