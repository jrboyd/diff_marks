load('ref/ensg_dicts.save')
if(!exists('my_fe')) load('data//my_fe_corrected.save')
source('scripts//diffpeaks_vs_manorm_vs_fe_functions.R')

load_MACS2_bdgdiff = function(a, b, p_thresh = 0){
  lines = c("MCF10A", "MCF7", "MDA231")
  lines = rep(lines, 2)
  mods = c("H3K4AC", "H3K4ME3")
  l1 = lines[a]
  l2 = lines[b]
  m = mods[1]
  if(a > 3 & b > 3){
    m = mods[2]
  }else if(!(a <= 3 & b <= 3)){
    stop("histone marks don't match!")
  }
  #columns of res tables will be chr,start,end,up/down,log10pval,...
  save_name = paste0("data/diff_peak_results/", paste(l1, m, 'vs', l2, m, sep = "_"), ".save")
  if(file.exists(save_name)){
    print('loading precalculated results...')
    load(save_name)
    
  }else{
    diffpeak_res = matrix("", nrow = 0, ncol = 5)
    bed_files = dir(path = "data/diff_peak_results", full.names = T, pattern = paste0(paste(l1, m, sep = "_"), ".+", 
                                                                                 paste(l2, m, sep = "_"), ".+.bed"))
    
    for (fname in bed_files) {
      print(paste(fname,' from MACS2 bdgdiff loading...'))
      tmp = read.table(fname, header = F, skip = 1, stringsAsFactors = F)
      diffpeak_res = rbind(diffpeak_res, tmp)
    }
    keep = !grepl(pattern = "common", diffpeak_res[, 4])
    diffpeak_res = diffpeak_res[keep,]
    
    keep = grepl(pattern = "cond1", diffpeak_res[, 4])
    diffpeak_res[keep, 4] = "cond1"
    
    keep = grepl(pattern = "cond2", diffpeak_res[, 4])
    diffpeak_res[keep, 4] = "cond2"
    
    ref = ensg_dict[,c('chrm','start','end')]
    
    print('Matching MACS2 bdgdiff results to promoters...')
    diffpeak_res = assign_ensg(diffpeak_res, ref)
    
    keep = diffpeak_res[,5] > p_thresh
    diffpeak_res = diffpeak_res[keep,]
    
    print('Analyzing comparisons...')
    up = get_ensgs(diffpeak_res, is1 = F, rownames(my_fe))
    down = get_ensgs(diffpeak_res, is1 = T, rownames(my_fe))
    res_MACS2bdgdiff = list(up = up, down = down, res = diffpeak_res)
    save(res_MACS2bdgdiff, file = save_name)
  }
  return(res_MACS2bdgdiff)
}
