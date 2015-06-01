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
  })  
  
  output$volcano =  renderPlot({
    
    lines = c('MCF10A', 'MCF7', 'MDA231', 'MCF10A', 'MCF7', 'MDA231')
    mods = c(rep('H3K4ac', 3), rep('H3K4me3', 3))
    i_x = react_index_x()
    i_y = react_index_y()
    list_a = react_list_a()
    list_b = react_list_b()

    
    name_a = paste(lines[i_x], mods[i_x])
    name_b = paste(lines[i_y], mods[i_y])
    scale = rep(1, nrow(my_fe)) #max(my_fe)
    names(scale) = rownames(my_fe)
    colors = scale_colors(data = my_fe, scale = scale, list_in = list_a, bg_color = rgb(0,0,0,1), list_color = rgb(1,0,0,1))
    colors = scale_colors(data = my_fe, scale = scale, list_in = list_b, bg_color = rgb(0,0,0,1), list_color = rgb(0,1,0,1), colors = colors)
    sel = react_selected()
    print(sel)
    if(length(sel) > 0){
      colors = scale_colors(data = my_fe, scale = scale, list_in = react_selected(), bg_color = rgb(0,0,0,1), list_color = rgb(0,0,1,.7), colors = colors)
    }
    max_str = max(nchar(name_a), nchar(name_b))
    
    note = paste0(format(name_a, width = max_str), ' - ', length(list_a), '\n', format(name_b, width = max_str), ' - ', length(list_b))
    plot_merge(data = my_fe, list_a = list_a, list_b = list_b, colors = colors, a = i_x, b = i_y, note = note,
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
  

react_list_a = reactive({
  return(rownames(my_fe)[1:5])
})

react_list_b = reactive({
  return(rownames(my_fe)[10:12])
})

react_index_x = reactive({
  return(1)
})

react_index_y = reactive({
  return(2)
})

react_selected = reactive({
  new_selection = character()
  i_x = react_index_x()
  i_y = react_idnex_y()
  if(!is.null(v$brush)){
    keep = (my_fe[,i_x] > v$brush$xmin & my_fe[,i_x] < v$brush$xmax) &
      (my_fe[,i_y] > v$brush$ymin & my_fe[,i_y] < v$brush$ymax)
    new_selection = rownames(my_fe)[keep]
    
  }
  if(!is.null(v$click1)){
    data_dist = abs(my_fe[,i_x] - v$click1$x) + abs(my_fe[,i_y] - v$click1$y)
    new_selection = names(sort(data_dist))[1]
    
  }
  filter = input$selection_filter
  if(length(new_selection) > 0){
    if(filter == 'List A'){
      new_selection = intersect(new_selection, list_a)
      print(new_selection)
    }else if(filter == 'List B'){
      new_selection = intersect(new_selection, list_b)
    }else if(filter == 'Exclude Lists'){
      new_selection = setdiff(new_selection, list_a)
      new_selection = setdiff(new_selection, list_b)
    }
  }
  return(new_selection)
  })

v <- reactiveValues(
  click1 = NULL,  # Represents the first mouse click, if any
  n = 0,
  brush = NULL
  #selected = character()
)

# Handle clicks on the plot
observeEvent(input$volcano_dblclick, {
  if(debug) print('dblclick')
  v$click1 <- input$volcano_dblclick

  v$brush = NULL
})

observeEvent(input$volcano_click, {
  if(debug) print('click')
  #v$selected = character()
  #v$brush = NULL
})


# Handle bush on the plot
observeEvent(input$volcano_brush, {
  if(debug) print('brush')
  v$brush = input$volcano_brush
  v$n <- v$n + 1
  #print(v$n)
  vb = v$brush
  v$click1 = NULL
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