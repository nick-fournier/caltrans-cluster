library(data.table)
library(xlsx)


# Load data
load("./R/site selection/cluster_data_image.RData")


#### Format summary for excel ####
data_clustered <- data_clustered_clara

#Rename some columns
newnames = c("ADT_AMT_PER_LANE"='ADT',
        "POPULATION_CODE" = 'CONTEXT',
        "DESIGN_SPEED" = 'DESIGN_SPEED', 
        "LANES_AMT" = 'LANES',
        "O_SHD_TOT_WIDTH" = 'SHOULDER',
        "MEDIAN_WIDTH" = 'MEDIAN')

newnames <- newnames[names(newnames) %in% colnames(data_clustered)]
setnames(data_clustered, new = newnames, old = names(newnames))#, skip_absent = T)


# Summarize
pam_results <- data_clustered[ , !c("SEG_ID","OBJECTID")] %>%
  group_by(CLUSTER) %>%
  do(the_summary = summary(.))
pam_results$the_summary


formatted_table <- lapply(pam_results$the_summary, function(the_summary) {
  # Convert to data table and clean up
  dtsummary <- as.data.table(the_summary)[,-1]
  dtsummary[ , c("STAT", "VAL") := tstrsplit(N, ":", fixed=TRUE)]
  dtsummary <- dtsummary[ , lapply(.SD, stringr::str_trim)]
  dtsummary <- dtsummary[ , !"N"]
  
  # Split into numeric and categorical tables
  numcats <- dtsummary[V2=="CLUSTER" & !is.na(STAT), STAT]
  dtsummary.num <- dtsummary[STAT %in% numcats, ]
  dtsummary.cat <- dtsummary[!(STAT %in% numcats) & !is.na(STAT), ]
  dtsummary.cat[, VARID := seq_len(.N), by = V2]
  
  # Long to wide format
  dtsummary.num <- dcast(dtsummary.num, STAT~V2, value.var = 'VAL')
  dtsummary.cat <- dcast(dtsummary.cat, VARID~V2, value.var = c('VAL','STAT'))
  
  # Convert to numeric
  dtsummary.num[ , (colnames(dtsummary.num)[-1]) := lapply(.SD, as.numeric),
                 .SDcols = colnames(dtsummary.num)[-1]]
  
  valcols <- colnames(dtsummary.cat)[grepl('VAL', colnames(dtsummary.cat))]
  dtsummary.cat[ , (valcols) := lapply(.SD, as.integer), .SDcols = valcols]
  
  # Sorting colnames to line up
  colnames(dtsummary.cat) <- sapply(colnames(dtsummary.cat), function(name) {
    if(grepl('VAR_|VAL_', name)){
      paste(substr(name, 5, nchar(name)), substr(name, 1, 3), sep="_")
    } else {
      name
    }
  })
  
  dtsummary.cat <- dtsummary.cat[ , sort(colnames(dtsummary.cat), decreasing=T), with=F]
  
  
  #Combined output
  if(nrow(dtsummary.cat) > nrow(dtsummary.num)) {
    dtsummary.output <- cbind(dtsummary.cat[,-1], dtsummary.num[1:nrow(dtsummary.cat),])
  } else {
    dtsummary.output <- cbind(dtsummary.cat[1:nrow(dtsummary.num),-1], dtsummary.num)
  }
  #replace NAs with blanks ""
  #dtsummary.output <- dtsummary.output[ , lapply(.SD, function(x) ifelse(is.na(x),"",x))]
  
  #Dividing line
  # dtsummary.output <- rbind(dtsummary.output,
  #                           dtsummary.output[nrow(dtsummary.output), 
  #                                            lapply(.SD, function(x) '--------')])
  
  return(dtsummary.output)
})
formatted_table <- rbindlist(formatted_table)


formatted_selection <- lapply(pam_results$the_summary, function(the_summary) {
  # Convert to data table and clean up
  dtsummary <- as.data.table(the_summary)[,-1]
  dtsummary[ , c("STAT", "VAL") := tstrsplit(N, ":", fixed=TRUE)]
  dtsummary <- dtsummary[ , lapply(.SD, stringr::str_trim)]
  dtsummary <- dtsummary[ , !"N"]
  
  # Split into numeric and categorical tables
  numcats <- dtsummary[V2=="CLUSTER" & !is.na(STAT), STAT]
  dtsummary.num <- dtsummary[STAT %in% numcats, ]
  dtsummary.cat <- dtsummary[!(STAT %in% numcats) & !is.na(STAT), ]
  #dtsummary.cat[, VARID := seq_len(.N), by = V2]

  # Get selection
  dtsummary.cat[, VARID := "Max"]
  dtsummary.cat <- dtsummary.cat[dtsummary.cat[ , .I[which.max(VAL)], by=V2]$V1, ]
  dtsummary.num <- dtsummary.num[dtsummary.num[ , .I[which(STAT=='Median')], by=V2]$V1, ]
  
  # Long to wide format
  dtsummary.num <- dcast(dtsummary.num, STAT~V2, value.var = 'VAL')
  dtsummary.cat <- dcast(dtsummary.cat, VARID~V2, value.var = 'STAT')
  
  
  # Convert to numeric
  dtsummary.num[ , (colnames(dtsummary.num)[-1]) := lapply(.SD, as.numeric),
                 .SDcols = colnames(dtsummary.num)[-1]]
  
  #valcols <- colnames(dtsummary.cat)[grepl('VAL', colnames(dtsummary.cat))]
  #dtsummary.cat[ , (valcols) := lapply(.SD, as.integer), .SDcols = valcols]
  
  # Sorting colnames to line up
  colnames(dtsummary.cat) <- sapply(colnames(dtsummary.cat), function(name) {
    if(grepl('VAR_|VAL_', name)){
      paste(substr(name, 5, nchar(name)), substr(name, 1, 3), sep="_")
    } else {
      name
    }
  })
  
  dtsummary.cat <- dtsummary.cat[ , sort(colnames(dtsummary.cat), decreasing=T), with=F]
  
  
  #Combined output
  if(nrow(dtsummary.cat) > nrow(dtsummary.num)) {
    dtsummary.output <- cbind(dtsummary.cat[,-1], dtsummary.num[1:nrow(dtsummary.cat),])
  } else {
    dtsummary.output <- cbind(dtsummary.cat[1:nrow(dtsummary.num),-1], dtsummary.num)
  }

  return(dtsummary.output)
})
formatted_selection <- rbindlist(formatted_selection)


#### Binning ####
# Gets the low mid high break points for the data
calc_lowmidhi_breaks <- function(x) {
  # q = quantile(x) # Quarters
  # breaks = c(paste0("LOW (<=", q[[3]], ")"),          # <=50%
  #            paste0("MID (", q[[3]],'-',q[[4]], ")"), # 50% - 75%
  #            paste0('HIGH (>',q[[4]], ")"))           # >75%
  
  
  q = round(quantile(x, seq(0,1,1/3))) # Thirds
  breaks = c(paste0("LOW (<=", q[[2]], ")"),          # <=33%
             paste0("MID (", q[[2]],'-',q[[3]], ")"), # 33% - 66%
             paste0('HIGH (>',q[[3]], ")"))           # >66%
  
  names(breaks) = c('LOW','MID','HIGH')
  return(breaks)
}

# Converts data to discrete low|mid|high Using pre-binned data
get_lowmidhi <- function(v, bins = NA) {
  # Check if our target is bin-able
  if(!(class(v) %in% c('numeric', 'integer'))) 
    return(v)
  
  # Readable bins and the break points
  if(all(is.na(bins))) bins <- calc_lowmidhi_breaks(v)
  breaks <- as.numeric(strsplit(gsub("MID \\(|\\)","", bins), '-')[[2]])
  
  # Bin the data
  res <- sapply(v, function(x) {
    if(x <= breaks[1]) { #Low
      return(bins[1])
    } 
    else if(x > breaks[2]) { #HIGH
      return(bins[3])
    } 
    else { #MID
      return(bins[2])
    }
  })
  return(res)
}


# Get break point values
# bincols = c('ADT_AMT_PER_LANE', 'DESIGN_SPEED', 'MEDIAN_WIDTH', 'LANES_AMT', 'O_SHD_TOT_WIDTH')
bincols <- colnames(data_clustered)[sapply(data_clustered, class) %in% c('integer','numeric')]
bincols <- bincols[!(bincols %in% c('OBJECTID','SEG_ID','CLUSTER'))]

# Gets the breakpoint values from full data
lomidhi_vals <- data_clustered[ , lapply(.SD, calc_lowmidhi_breaks), .SDcols = bincols]


# Bins the data
#data_clustered[ , (bincols) := lapply(.SD, get_lowmidhi), .SDcols = bincols]
binned_selection <- data.table(sapply(bincols, function(n) {
  get_lowmidhi(v = formatted_selection[[n]], bins = lomidhi_vals[[n]])
}))
colnames(binned_selection) <- paste0(colnames(binned_selection),"_BIN")
formatted_selection <- cbind(formatted_selection, binned_selection)

# Sort column order
base = c('CLUSTER','STAT','CONTEXT') #'TYPE'
sorted <- c(base, sort(
  colnames(formatted_selection)[!(colnames(formatted_selection) %in% base)]
  ))

formatted_selection <- formatted_selection[, sorted, with=F]


# Remove duplicates
# bincols <- c(c('CONTEXT'), colnames(formatted_selection)[grep('BIN',colnames(formatted_selection))])
# formatted_selection <- formatted_selection[!duplicated(formatted_selection[,bincols,with=F]), ]
# formatted_table <- formatted_table[CLUSTER %in% formatted_selection$CLUSTER, ]


#### Save to excel ####
write.xlsx(formatted_table,
           file = './output/site selection/clusters/cluster_summary.xlsx',
           sheetName = 'Clusters',
           showNA = F,
           row.names = F)


# Save to excel
write.xlsx(formatted_selection,
           file = './output/site selection/clusters/cluster_selection.xlsx',
           sheetName = 'Clusters',
           showNA = F,
           row.names = F)


# Save data to csv for GIS
data_clustered_xy <- merge(data_clustered, rawdata[,.(OBJECTID,X,Y)], by = 'OBJECTID')



csvt <- as.vector(sapply(data_clustered_xy, function(x) {
  key_map <- c('integer'="Integer",'numeric'="Real",
               'character'="String",'factor'='String')
  key_map[class(x)]
  }))
fwrite(data.table(t(csvt)), './output/site selection/SB127_cluster_labels.csvt',
       quote = T, col.names = F)
       
fwrite(data_clustered_xy, './output/site selection/SB127_cluster_labels.csv')



# Most dissimilar
# gower_mat <- as.matrix(gower_dist)
# data_unique[which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]), arr.ind = TRUE)[1, ], ]


# # Save image
# rm(data, data_unique, data_clustered, data_unscaled, gower_dist, gower_mat, pam_fit,
#    pam_fit_list, pam_results, sil_width, tsne_data, tsne_obj,
#    res.pca, rawdata, csvt, k, minmax)
# save.image(file = "./R/site selection/final_cluster_data_image.RData")
# 



# Cleanup
rm(list=ls())
gc()

