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

fname = dir(path = "ma_norm_results", full.names = T, pattern = paste0(paste(l1, m, sep = "_"), ".+", paste(l2, 
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

