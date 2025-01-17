source('setup.R')


shinyServer(function(input, output, session) {
  

  
  
  output$detail_plot = renderPlot({
    i_x = react_index_x()
    i_y = react_index_y()
    
    disp_data = react_displayed()
    
    list_up = react_list_up()
    list_up = intersect(rownames(disp_data), list_up)
    list_dn = react_list_dn()
    list_dn = intersect(rownames(disp_data), list_dn)
    sel = react_selected()
    sel = intersect(rownames(disp_data), sel)
    
    clear_hmap_res = T
    
    if(input$detail_type == detail_plot_types[2]){#ngs profiles
      if(length(sel) < 1){
        plot0()
        text(.5,.5, 'nothing selected')
      }else{
        if(is.na(ngs_profiles)[1]){
          ngs_profiles <<- load_ngsprofiles(my_fe)
        }
        plotNGS_wBG(sel, bg_ENSGcut_list = NA, list_name = colnames(my_fe)[react_index_x()], sel_name = 'Selected', linesToPlot = c(lines[i_x], lines[i_y]), smoothing = input$smoothing_window)
        #plot(colMeans(ngs_x[sel,]))
      }
    }else if(input$detail_type == detail_plot_types[3]){#ngs heatmaps
      if(length(sel) < 1){
        plot0()
        text(.5,.5, 'nothing selected')
      }else{
        if(is.na(ngs_profiles)[1]){
          ngs_profiles <<- load_ngsprofiles(my_fe)
        }
        sample_a = sub(' ', '_', colnames(my_fe)[i_x])
        sample_b = sub(' ', '_', colnames(my_fe)[i_y])
        res = plotNGS_heatmap(sel, c(sample_a, sample_b))
        v$hmap_res = res
        clear_hmap_res = F
        #plot(colMeans(ngs_x[sel,]))
      }
    }else if(input$detail_type == detail_plot_types[4]){#heatmap of all cell lines and mods
      if(length(sel) < 1){
        plot0()
        text(.5,.5, 'nothing selected')
      }else if(length(sel) == 1){
        par(mai = c(2,1,1,1))
        plot(1:ncol(disp_data), disp_data[sel,], axes = F, xlab = '', ylab = 'log2 FE')
        box()
        axis(side = 2)
        axis(side = 1, at = 1:ncol(my_fe), labels = colnames(my_fe), las = 2)
        title(ensg_dict[sel,]$gene_name)
      }else{
        res = heatmap.3(disp_data[sel,], nsplits = 2, classCount = min(6, length(sel)), main = paste(length(sel), 'selected genes'), key.xlab = 'log2 FE', key.title = '')
        v$hmap_res = res
        clear_hmap_res = F
      }
      
    }else{
      plot0()
      text(.5,.5, 'no detail plot type selected')
    }
    if(clear_hmap_res) v$hmap_res = NULL
  })
  
  
  ngs_x = reactive({
    return(ngs_profiles[[colnames(my_fe)[react_index_x()]]])
  })
  
  ngs_y = reactive({
    return(ngs_profiles[[colnames(my_fe)[react_index_y()]]])
  })
  
  output$volcano =  renderPlot({
    
    #lines = c('MCF10A', 'MCF7', 'MDA231', 'MCF10A', 'MCF7', 'MDA231')
    #mods = c(rep('H3K4ac', 3), rep('H3K4me3', 3))
    i_x = react_index_x()
    i_y = react_index_y()
    
    disp_data = react_displayed()
    
    list_up = react_list_up()
    list_up = intersect(rownames(disp_data), list_up)
    list_dn = react_list_dn()
    list_dn = intersect(rownames(disp_data), list_dn)
    
    sel = react_selected()
    sel = intersect(rownames(disp_data), sel)
    
    #print(lines)
    try({
      #print(i_x)
      name_a = column_choices[i_x]
      #print(name_a)
      name_b = column_choices[i_y]
      
      scale = rep(1, nrow(disp_data)) #max(disp_data)
      names(scale) = rownames(disp_data)
      colors = rep(rgb(0,0,0,input$bg_opacity), nrow(disp_data))
      names(colors) = rownames(disp_data)
      if(length(list_up) > 0){
      colors = scale_colors(data = disp_data, scale = scale, list_in = list_up, bg_color = rgb(0,0,0,input$bg_opacity), list_color = rgb(1,0,0,input$fg_opacity), colors = colors)
      }
      if(length(list_dn) > 0){
      colors = scale_colors(data = disp_data, scale = scale, list_in = list_dn, bg_color = rgb(0,0,0,input$bg_opacity), list_color = rgb(0,1,0,input$fg_opacity), colors = colors)
      }
      
      #print(sel)
      if(length(sel) > 0){
        colors = scale_colors(data = disp_data, scale = scale, list_in = sel, bg_color = rgb(0,0,0,input$bg_opacity), list_color = rgb(0,0,1,input$fg_opacity), colors = colors)
      }
      max_str = max(nchar(name_a), nchar(name_b))
      
      note = paste0(format(name_a, width = max_str), ' - ', length(list_up), '\n', format(name_b, width = max_str), ' - ', length(list_dn))
      MIN = min(my_fe[,c(i_x, i_y)])
      MAX = max(my_fe[,c(i_x, i_y)])
      plot_merge(data = disp_data, list_a = list_up, list_b = list_dn, colors = colors, a = i_x, b = i_y, note = note,
                 xlab = paste(name_a, 'log2 FE'), ylab = paste(name_b, 'log2 FE'), xlim = c(MIN, MAX), ylim = c(MIN, MAX), cex = .8)
      detect_thresh = input$detect_threshold
      if(detect_thresh > 0){
        lines(c(MIN, detect_thresh), c(detect_thresh, detect_thresh), col = 'yellow')
        lines(c(detect_thresh, detect_thresh), c(MIN, detect_thresh),  col = 'yellow')
      }
    }, silent = F)
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
  
  react_displayed = reactive({
    if(debug) print('react_displayed')
    keep = apply(my_fe,1,max) > input$detect_threshold
    displayed_data = my_fe[keep,]
    displayed_groups = input$display_filter
    #print(displayed_groups)
    list_up = react_list_up()
    list_dn = react_list_dn()
    for(disp_grp in display_filter_choices){
      if(!any(disp_grp == displayed_groups)){#group not selected for display, remove from displayed_data.
        if(disp_grp == display_filter_choices[1]){#bg
          kept = union(list_up, list_dn)
          displayed_data = displayed_data[kept,]
        }else if(disp_grp == display_filter_choices[2]){#up
          kept = setdiff(rownames(displayed_data), list_up)
          #print(kept)
          displayed_data = displayed_data[kept,]
        }else if(disp_grp == display_filter_choices[3]){#dn
          kept = setdiff(rownames(displayed_data), list_dn)
          displayed_data = displayed_data[kept,]
        }else{
          stop('react_displayed : unrecognized display grp')
        }
      }
    }
    return(displayed_data)
  })
  
  react_FC = reactive({
    if(debug) print('react_FC')
    fc_thresh = input$fc_threshold
    out = list()
    i_x = react_index_x()
    i_y = react_index_y()
    disp_data = my_fe
    keep = disp_data[,i_y] > (disp_data[,i_x] + fc_thresh)
    out$up = rownames(disp_data)[keep]
    keep = disp_data[,i_x] > (disp_data[,i_y] + fc_thresh)
    out$down = rownames(disp_data)[keep]
    return(out)
  })
  
  
  react_loadMAnorm = reactive({
    if(debug) print('react_loadMAnorm')
    out = load_MAnorm(react_index_x(), react_index_y())
    return(out)
  })
  
  react_MAnorm = reactive({
    if(debug) print('react_MAnorm')
    out = react_loadMAnorm()
    pval_thresh = input$pval_threshold
    keep = out$res[out$up,5] > pval_thresh
    out$up = out$up[keep]
    keep = out$res[out$down,5] > pval_thresh
    out$down = out$down[keep]
    return(out)
  })
    
  react_loadMACS2 = reactive({
    if(debug) print('react_loadMACS2')
    out = load_MACS2_bdgdiff(react_index_x(), react_index_y())
    return(out)
  })
  
  react_MACS2 = reactive({
    if(debug) print('react_MACS2')
    out = react_loadMACS2()
    pval_thresh = input$pval_threshold
    keep = out$res[out$up,5] > pval_thresh
    out$up = out$up[keep]
    keep = out$res[out$down,5] > pval_thresh
    out$down = out$down[keep]
    return(out)
  })
  
  process_lists = function(direction, sel_methods){
    new_list = character()
    list_fun = list(
      react_FC(),
      react_MAnorm(),
      react_MACS2())
    names(list_fun) = selection_method_choices#hardcoded from ui.R, room for improvement
    if(is.null(sel_methods)){
      print('no lists selected')
      
    }else{
      for(i in 1:length(sel_methods)){
        list_i = list_fun[[sel_methods[i]]][[direction]]
        if(i == 1){
          new_list = list_i
        }else{
          new_list = intersect(new_list, list_i )
        }
      }
    }
    return(new_list)
  }
  
  react_list_up = reactive({
    if(debug) print('react_list_up')
    direction = 'up'
    sel_methods = input$available_methods
    return(process_lists(direction, sel_methods))
  })
  
  
  
  react_list_dn = reactive({
    if(debug) print('react_list_dn')
    direction = 'down'
    sel_methods = input$available_methods
    return(process_lists(direction, sel_methods))
  })
  
  
  
  react_index_x = reactive({
    if(debug) print('react_index_x')
    sel = name2index[input$x_values]
    if(is.null(sel)) sel = 1
    if(length(sel) < 1) sel = 1
    print(sel)
    return(sel)
  })
  
  react_index_y = reactive({
    if(debug) print('react_index_y')
    sel = name2index[input$y_values]
    if(is.null(sel)) sel = 2
    if(length(sel) < 1) sel = 2
    return(sel)
  })
  
  filter_selections = function(filter, sel, list_up, list_dn){
    filter = input$selection_filter
    if(length(sel) > 0){
      if(filter == selection_filter_choices[1]){#up or down
        sel = intersect(sel, union(list_up, list_dn))
        #print(sel)
      }else if(filter == selection_filter_choices[2]){#up
        sel = intersect(sel, list_up)
        #print(sel)
      }else if(filter == selection_filter_choices[3]){#down
        sel = intersect(sel, list_dn)
      }else if(filter == selection_filter_choices[4]){#unchanged
        sel = setdiff(sel, list_up)
        sel = setdiff(sel, list_dn)
      }
    }
    return(sel)
  }
  
  react_selected = reactive({
    if(debug) print('react_selected')
    new_selection = character()
    list_up = react_list_up()
    list_dn = react_list_dn()
    i_x = react_index_x()
    i_y = react_index_y()
    filter = input$selection_filter
    disp_data = react_displayed()
    
    if(!is.null(v$brush)){
      keep = (disp_data[,i_x] > v$brush$xmin & disp_data[,i_x] < v$brush$xmax) &
        (disp_data[,i_y] > v$brush$ymin & disp_data[,i_y] < v$brush$ymax)
      new_selection = rownames(disp_data)[keep]
      new_selection = filter_selections(filter, new_selection, list_up, list_dn)
    }else if(!is.null(v$click1)){
      data_dist = abs(disp_data[,i_x] - v$click1$x) + abs(disp_data[,i_y] - v$click1$y)
      closest = names(sort(data_dist))
      closest = filter_selections(filter, closest, list_up, list_dn)
      new_selection = closest[1]
      
    }else{
      new_selection = filter_selections(filter, new_selection, list_up, list_dn)
    }
    
    
    return(new_selection)
  })
  
  v <- reactiveValues(
    click1 = NULL,  # Represents the first mouse click, if any
    n = 0,
    brush = NULL,
    hmap_res = NULL
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
  
  output$x_select = renderUI({
    choices = paste(lines, mods)
    
    return(selectInput(inputId = 'x_values', label = 'Select X value ', choices = choices, selected = choices[1]))
  })
  
  output$y_select = renderUI({
    choices = paste(lines, mods)
    
    return(selectInput(inputId = 'y_values', label = 'Select Y value ', choices = choices, selected = choices[2]))
  })
  
  output$selTable = renderTable({
    disp_data = react_displayed()
    sel = react_selected()
    sel = intersect(rownames(disp_data), sel)
    
    if(length(sel) < 1){
      return(xtable(as.data.frame('no data selected')))
    }
    
    
    if(is.null(v$hmap_res)){
      sel_as_symbols = ensg_dict[sel,]$gene_name
      sel_as_position = ensg_dict[sel,]$ucsc
      base_url = 'https://genome.ucsc.edu/cgi-bin/hgTracks?hgS_doOtherUser=submit&hgS_otherUserName=jrboyd&hgS_otherUserSessionName=TM_K4_with_peaks'
      sel_as_urls = paste0(base_url, '&position=', sel_as_position)
      sel_as_urls = paste0('<a target="_blank" href="', sel_as_urls, '">On UCSC</a>') 
      out_table = xtable(as.data.frame(cbind(sel, sel_as_symbols, sel_as_position, sel_as_urls)))
      colnames(out_table) = c('ENSG ID', 'Gene Symbol', 'Position', 'Promoter in UCSC')
      
      return(out_table)
    }else{
      res = v$hmap_res
      colors = res[[4]]
      asPlotted = rownames(res[[3]])
      classSizes = res[[1]]
      ensg2colors = rep(colors[1], length(asPlotted))
      names(ensg2colors) = asPlotted
      for(i in 2:length(colors)){
        start = sum(classSizes[1:(i-1)]) + 1
        end = sum(classSizes[1:i])
        ensg2colors[start:end] = colors[i]
      }
      sel = asPlotted
      sel_as_symbols = ensg_dict[sel,]$gene_name
      sel_as_position = ensg_dict[sel,]$ucsc
      base_url = 'https://genome.ucsc.edu/cgi-bin/hgTracks?hgS_doOtherUser=submit&hgS_otherUserName=jrboyd&hgS_otherUserSessionName=TM_K4_with_peaks'
      sel_as_urls = paste0(base_url, '&position=', sel_as_position)
      sel_as_urls = paste0('<a target="_blank" href="', sel_as_urls, '">On UCSC</a>') 
      out_table = xtable(as.data.frame(cbind(sel, sel_as_symbols, sel_as_position, sel_as_urls, paste0('<td bgcolor="', ensg2colors, '">',sel_as_symbols,'</td>'))))
      colnames(out_table) = c('ENSG ID', 'Gene Symbol', 'Position', 'Promoter in UCSC', 'Cluster')
      rownames(out_table) = NULL
      return(out_table)
    }
    
  }, sanitize.text.function = force)    
})