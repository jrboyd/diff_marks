library('shiny')
library('RColorBrewer')
source('scripts/diffpeaks_vs_manorm_vs_fe.R')
source('scripts//process_MACS2_bdgdiff.R')
source('scripts//process_MAnorm.R')

load('data/my_fe_corrected.save')
debug = F

display_filter_choices = c('Background', 'Up', 'Down')
selection_filter_choices = c('No filter', 'Up', 'Down', 'Unchanged')
selection_method_choices = c('Fold Change', 'MAnorm', 'MACS2 bdgdiff')


lines = c("MCF10A", "MCF7", "MDA231")
lines = rep(lines, 2)
mods = c(rep("H3K4AC",3), rep("H3K4ME3",3))
column_choices = paste(lines, mods)
name2index = 1:6
names(name2index) = column_choices

source('scripts//functions_ngsplot.R')
ngs_profiles = load_ngsprofiles(my_fe)

ID_SET = rownames(my_fe)
l2col = RColorBrewer::brewer.pal(n = 3, 'Dark2')
names(l2col) = lines[1:3]