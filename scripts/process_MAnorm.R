load('ref/ensg_dicts.save')
if(!exists('my_fe')) load('data//my_fe_corrected.save')
source('scripts//diffpeaks_vs_manorm_vs_fe_functions.R')

load_MAnorm = function(a, b, p_thresh = 0, default_pval = 50){
  l1 = lines[a]
  l2 = lines[b]
  if(mods[a] != mods[b])   stop("histone marks don't match!")
  m = mods[a]
  #columns of res tables will be chr,start,end,up/down,log10pval,...
  save_name = paste0("data/ma_norm_results/", paste(l1, m, 'vs', l2, m, sep = "_"), ".save")
  if(file.exists(save_name)){
    print('loading precalculated results...')
    load(save_name)
    
  }else{
    fname = dir(path = "data/ma_norm_results", full.names = T, pattern = paste0(paste(l1, m, sep = "_"), ".+", paste(l2, 
                                                                                                                m, sep = "_"), ".+result.xls"))
    print(paste(fname,'from MAnorm loading...'))
    ma_res = read.table(fname, header = T, sep = "\t", comment.char = "", stringsAsFactors = F)
    ma_res = ma_res[,c(1:4,9)]
    
    keep = !grepl(pattern = "common", ma_res[, 4])#remove common peaks, only interesetd in diff
    ma_res = ma_res[keep,]
    
    keep = grepl(pattern = "unique_peak1", ma_res[, 4])#unify terminology for convenience
    ma_res[keep, 4] = "cond1"
    
    keep = grepl(pattern = "unique_peak2", ma_res[, 4])
    ma_res[keep, 4] = "cond2"
    
    ref = ensg_dict[,c('chrm','start','end')]
    
    print('Matching MAnorm results to promoters...')
    ma_res = assign_ensg(ma_res, ref) 
    
    MIN_PVAL = p_thresh
    MAX_PVAL = default_pval
    
    is_na = is.na(ma_res[,5])#after consulting tracks, NA ma_norm diff peaks are exclusive to one cell line, should be high p-value
    is_na = rownames(ma_res)[is_na]
    ma_res[is_na,5] = MAX_PVAL 
    is_inf = is.infinite(ma_res[,5])#inf values should also be high
    is_inf = rownames(ma_res)[is_inf]
    ma_res[is_inf,5] = MAX_PVAL  
    
    print('Analyzing comparisons...')
    up = get_ensgs(ma_res, is1 = F, rownames(my_fe))
    down = get_ensgs(ma_res, is1 = T, rownames(my_fe))
    res_MAnorm = list(up = up, down = down, res = ma_res)
    save(res_MAnorm, file = save_name)
  }
  return(res_MAnorm)
}
