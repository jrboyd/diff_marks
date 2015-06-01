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

diffpeak_res = matrix("", nrow = 0, ncol = 5)
bed_files = dir(path = "diff_peak_results", full.names = T, pattern = paste0(paste(l1, m, sep = "_"), ".+", 
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