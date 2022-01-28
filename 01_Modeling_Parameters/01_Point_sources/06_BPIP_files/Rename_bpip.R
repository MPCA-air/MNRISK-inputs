
# Get current file names
bpips <- list.files(pattern = ".bpi")


# Remove facility name and add state code
new_names <- sapply(bpips, FUN = function(x) strsplit(x, "_")[[1]][1] %>% paste0("27", ., ".bpi"), USE.NAMES = F)

file.rename(bpips, new_names)


##
