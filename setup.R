library("shiny")
library("xtable")
library("RColorBrewer")
source("scripts/diffpeaks_vs_manorm_vs_fe.R")
source("scripts//process_MACS2_bdgdiff.R")
source("scripts//process_MAnorm.R")
source("scripts//heatmap.3-split.R")

load("data/my_fe_corrected.save")
debug = F

display_filter_choices = c("Background", "Up", "Down")
selection_filter_choices = c("No filter", "Up", "Down", "Unchanged")
selection_method_choices = c("Fold Change", "MAnorm", "MACS2 bdgdiff")
detail_plot_types = c("None", "ngsplots - profiles", "ngsplots - heatmap", "FE heatmap")

lines = c("MCF10A", "MCF7", "MDA231")
lines = rep(lines, 2)
mods = c(rep("H3K4AC", 3), rep("H3K4ME3", 3))
column_choices = paste(lines, mods)
name2index = 1:6
names(name2index) = column_choices

source("scripts//functions_ngsplot.R")
if(!exists('ngs_profiles')) ngs_profiles = NA

ID_SET = rownames(my_fe)
l2col = RColorBrewer::brewer.pal(n = 3, "Dark2")
names(l2col) = lines[1:3] 

plot0 = function(drawBorder = T){
  plot(c(0,1), c(0,1), type = 'n', axes = F, xlab = '', ylab = '')
  if(drawBorder)box()
}
