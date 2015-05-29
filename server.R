source('scripts/diffpeaks_vs_manorm_vs_fe.R')

shinyServer(function(input, output, session) {
  
  plot0 = function(drawBorder = T){
    plot(c(0,1), c(0,1), type = 'n', axes = F, xlab = '', ylab = '')
    if(drawBorder)box()
  }
  
  output$venn = renderPlot({
    plot0()
    text(.5,.5, 'venn')
  })  
  
  output$volcano = renderPlot({
    plot0()
    text(.5,.5, 'volcano')
  })
  
  output$select_gene_list = renderUI({
    all_choices = c('a', 'b')
    
    return(selectInput(width = '50%',
      inputId = 'selected_gene_list', 
      label = 'Select group for export', 
      choices = all_choices,
      selected = all_choices[1]
      ))    
  })
  
  
  
  
})