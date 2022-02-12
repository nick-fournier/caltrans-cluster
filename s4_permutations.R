library(xlsx)
library(data.table)
library(combinat)


# Notes:
# 1) Rather than pure orthogonal design with every possible combination,
# we use clustering to identify more typical roadway environments in Caltrans' network
# 
# 2) Then from there I apply each case to each possible treatment type (bike infra).
# However, we remove impossible or known bad cases such as 
# bike lane with no shoulder or high speed shared lanes

# 3) For development, I split the cases into "scenes" based on the cluster.
# This will serve as the basic continuous simulation environments so that 
# participants don't transition from urban to rural between intersections.

# 4) Last I use sequential monadic design to generate random permutation
# sequences for each scene to reduce ordering bias. 
# Ideally we would fully rotate all permutations,
# but that would result in 14! + 8! combinations


#### 1. Load data ####
results <- read.xlsx('./output/site selection/clusters/cluster_selection.xlsx', sheetIndex = 1)
results <- data.table(results)

#get just the binned
bincols <- c(c('CLUSTER', 'CONTEXT'), colnames(results)[grep('BIN',colnames(results))])
results <- results[ , bincols, with = F]
rm(bincols)

# Remove duplicates
results <- results[!duplicated(results[ , !'CLUSTER']), ]



# Combination with bike infra
mitigation = c("1 None", "2 Sharrow", '3 Bike lane', "4 Buffered", "5 Separated bike lane")
results <- merge(results, expand.grid('CLUSTER' = results$CLUSTER, 'TREATMENT' = mitigation), by = 'CLUSTER')

##### 2. Remove unrealistic / impossible combinations ####
# No bike lanes where no shoulder
# results <- results[!(grepl('LOW', SHOULDER_BIN) & grepl('3|4|5|6', TREATMENT)), ]

# No protected / parking bike lane in rural settings
# results <- results[!(grepl('Rural', CONTEXT) & grepl('5|6', TREATMENT)), ]

# No sharrow/none in mid/high speed or high volume
results <- results[!(grepl('HIGH|MID', DESIGN_SPEED_BIN) & grepl('1|2', TREATMENT)), ]
results <- results[!(grepl('HIGH', ADT_BIN) & grepl('1|2', TREATMENT)), ]


# Assign unique scene ID
results[order(CONTEXT), SCENARIO_ID := 1:nrow(results)]


# There are no HIGH lanes category, just turn MID to HIGH
results[ , LANES_BIN := gsub('MID \\(1-2\\)', 'HIGH \\(>1\\)', LANES_BIN)]

# Remove treatment numbers
results[, TREATMENT := trimws(gsub('[0-9]+', '', TREATMENT))]


#### 3. Split into groups and get permutations ####
# Before splitting into cluster-scenes, cases with just two can be combined 

# Split into "scenes" based on land use contexts and number of lanes
scenes <- split(results, f = list(results$CONTEXT, results$LANES_BIN))

# Some formatting
scenes <- lapply(names(scenes), function(x) {
  scenes[[x]][ , SCENE := x]
  scenes[[x]][ , SCENE := gsub('.HIGH \\(>1\\)', '_MULTILANE', SCENE)]
  scenes[[x]][ , SCENE := gsub('.LOW \\(<=1\\)', '_SINGLELANE', SCENE)]
})
names(scenes) <- sapply(scenes, function(x) x$SCENE[1])

# Remove empty
scenes[which(is.na(names(scenes)))] <- NULL

# Get permutations in each scene
# to avoid testing n! permutations, just randomize sequence and use reverse
# scenes <- lapply(scenes, function(x) {
#   perms <- sample(1:nrow(x))
#   A = x[perms, ]
#   A[ , PERM := 'A']
#   B = x[rev(perms), ]
#   B[ , PERM := 'B']
#   return(rbindlist(list(A, B)))
# })

# Flatten and split by permutation
scenes <- rbindlist(scenes)

# scenes <- split(scenes, f = list(scenes$SCENE, scenes$PERM))
scenes <- split(scenes, f = scenes$SCENE)


# Cleanup first
if(file.exists('./output/permutations.xlsx')) file.remove('./output/permutations.xlsx')

# Write new one
res <- lapply(names(scenes), function(x) {
  write.xlsx(scenes[[x]],
             file = './output/permutations.xlsx',
             sheetName = paste('Permutation', x),
             append=TRUE,
             showNA = F,
             row.names = F)
})
rm(res)





