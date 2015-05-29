# load results from MAnorm and diffpeaks (MACS2) intersect with promoter annation to assign ensg ids plot
# out FE data coded with MAnorm and diffpeaks results
#load("enst_dicts.save")
source("scripts/process_gtf_attributes.R")
source("scripts/diffpeaks_vs_manorm_vs_fe_functions.R")
source("scripts/compare_diff_function.R")

lines = c('MCF10A', 'MCF7', 'MDA231', 'MCF10A', 'MCF7', 'MDA231')
mods = c(rep('H3K4ac', 3), rep('H3K4me3', 3))

# a_list = c(1, 1,2,4,4,5)
# b_list = c(2,3,3,5,6,6)
# for(i in 1:length(a_list)){
#   a = a_list[i]
#   b = b_list[i]
#   ma_dp_fc_compare(a, b, file_prefix = paste(mods[a], '-', lines[a], 'vs', lines[b]), p_thresh = 12)
# }

# ma_dp_fc_compare(1, 3)
# ma_dp_fc_compare(2, 3)
# 
# ma_dp_fc_compare(4, 5)
# ma_dp_fc_compare(4, 6)
# ma_dp_fc_compare(5, 6)

