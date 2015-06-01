source('scripts/diffpeaks_vs_manorm_vs_fe.R')

load('data/my_fe_corrected.save')
debug = T

shinyServer(function(input, output, session) {
  
  plot0 = function(drawBorder = T){
    plot(c(0,1), c(0,1), type = 'n', axes = F, xlab = '', ylab = '')
    if(drawBorder)box()
  }
  
  output$venn = renderPlot({
    plot0()
    text(.5,.5, 'venn')
    #ma_dp_fc_compare(a, b, p_thresh = 12)
#     plot0()
#     text(.5,.5, 'venn')
  })  
  
  output$volcano =  renderPlot({
    
    lines = c('MCF10A', 'MCF7', 'MDA231', 'MCF10A', 'MCF7', 'MDA231')
    mods = c(rep('H3K4ac', 3), rep('H3K4me3', 3))
    a = 1
    b = 2
    list_a = rownames(my_fe)[1:5]
    list_b = rownames(my_fe)[10:12]

    if(!is.null(v$brush)){
      keep = (my_fe[,a] > v$brush$xmin & my_fe[,a] < v$brush$xmax) &
        (my_fe[,b] > v$brush$ymin & my_fe[,b] < v$brush$ymax)
      v$selected = rownames(my_fe)[keep]
    }
    
    filter = input$selection_filter
    if(length(v$selected) > 0){
      if(filter == 'List A'){
        v$selected = intersect(v$selected, list_a)
      }else if(filter == 'List B'){
        v$selected = intersect(v$selected, list_b)
      }else if(filter == 'Exclude Lists'){
        v$selected = setdiff(v$selected, list_a)
        v$selected = setdiff(v$selected, list_b)
      }
    }
    name_a = paste(lines[a], mods[a])
    name_b = paste(lines[b], mods[b])
    scale = rep(1, nrow(my_fe)) #max(my_fe)
    names(scale) = rownames(my_fe)
    colors = scale_colors(data = my_fe, scale = scale, list_in = list_a, bg_color = rgb(0,0,0,1), list_color = rgb(1,0,0,1))
    colors = scale_colors(data = my_fe, scale = scale, list_in = list_b, bg_color = rgb(0,0,0,1), list_color = rgb(0,1,0,1), colors = colors)
    colors = scale_colors(data = my_fe, scale = scale, list_in = v$selected, bg_color = rgb(0,0,0,1), list_color = rgb(0,0,1,1), colors = colors)
    
    max_str = max(nchar(name_a), nchar(name_b))
    
    note = paste0(format(name_a, width = max_str), ' - ', length(list_a), '\n', format(name_b, width = max_str), ' - ', length(list_b))
    plot_merge(data = my_fe, list_a = list_a, list_b = list_b, colors = colors, a = a, b = b, note = note,
               xlab = paste(name_a, 'log2 FE'), ylab = paste(name_b, 'log2 FE'))
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
  
v <- reactiveValues(
  click1 = NULL,  # Represents the first mouse click, if any
  n = 0,
  brush = NULL,
  selected = character()
)

# Handle clicks on the plot
observeEvent(input$volcano_dblclick, {
  if(debug) print('dblclick')
  v$click1 <- input$volcano_dblclick
  data_dist = abs(my_fe[,1] - v$click1$x) + abs(my_fe[,2] - v$click1$y)
  v$selected = names(sort(data_dist))[1]
  v$brush = NULL
})

observeEvent(input$volcano_click, {
  if(debug) print('click')
  v$selected = character()
  v$brush = NULL
})


# Handle bush on the plot
observeEvent(input$volcano_brush, {
  if(debug) print('brush')
  v$brush = input$volcano_brush
  v$n <- v$n + 1
  #print(v$n)
  vb = v$brush
  #print(names(vb))
  #v$select_rect = v$brush
  #print(vb$domain$left)
  #print(vb$range)
})

# Handle bush on the plot
observeEvent(input$volcano_hover, {
  #print('click')
  v$hover = input$volcano_hover
  #print(v$hover)
})

observeEvent(input$reset, {
  # Reset both the range and the first click, if any.
  v$range <- NULL
  v$click1 <- NULL
})
  
  
})