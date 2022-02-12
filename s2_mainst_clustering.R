library(data.table)
library(pbapply)
library(ape)
library(dplyr) # for data cleaning
library(cluster) # for gower similarity and pam
library(Rtsne) # for t-SNE plot
library(ggplot2) # for visualization
library(PCAmixdata)
library(factoextra)
library(NbClust)


CODES <- list(TERRAIN_CODE = c('F' = 'Flat', 'R' = 'Rolling', 'M' = 'Mountainous'),
              HIGHWAY_GROUP_CODE = c('D' = 'Divided', 'U' = 'Undivided',
                                     'R' = 'Independent Alignment',
                                     'L' = 'Independent Alignment'),
              # Combine urban/suburban
              #POPULATION_CODE = c('U' = 'Urbanized (suburban)', 'R' = 'Rural', 'B' = 'Urban'))
              POPULATION_CODE = c('U' = 'Urban', 'R' = 'Rural', 'B' = 'Urban'))


# PERFORMS CLUSTER ANALYSIS TO IDENTIFY KEY CANDIDATES

# Import data
rawdata <- fread('./output/site selection/SB127_matched_pm.csv')


#### Cleanup ####
data <- copy(rawdata)

# Use segment IDs instead of postmiles to reduce duplicated routes
data <- data[!duplicated(SEG_ID), ]


# ADT per lane
data[ , ADT_AMT_PER_LANE := ADT_AMT / LANES_AMT]

# Labels
for(code in names(CODES)) {
  data[[code]] <- as.vector(sapply(data[[code]], function(x) CODES[[code]][x]))
}


# Remove unmatched NAs
data <- data[!is.na(DISTRICT), ]

# Select variables
vars = c("OBJECTID", "SEG_ID", "ADT_AMT_PER_LANE", 
         "POPULATION_CODE", "DESIGN_SPEED", 'ACCESS_CODE_DESC', #"TERRAIN_CODE", "HIGHWAY_GROUP_CODE",
         "LANES_AMT")#, "MEDIAN_WIDTH") # "O_SHD_TOT_WIDTH",
data <- data[, vars, with=F]

# re-check for NAs
any(sapply(data, function(x) any(is.na(x))))
sapply(data, function(x) if(class(x)=='character' | class(x)=='factor') unique(x) else 'numeric')



#### # Bin continuous variables
# Lanes to factor
data[ , LANES_AMT := LANES_AMT/2]
svg("./output/site selection/figures/hist_lanes.svg")
hist(data$LANES_AMT, breaks=20, right=T)
dev.off()

# Speed
svg("./output/site selection/figures/hist_speed.svg")
hist(data$DESIGN_SPEED, breaks=10)
dev.off()

# Median width
# hist(log(data$MEDIAN_WIDTH), breaks=30)
# Shoulder
#data[ , O_SHD_TOT_WIDTH := O_SHD_TOT_WIDTH/2]
# ADT
svg("./output/site selection/figures/hist_ADT.svg")
hist(log(data$ADT_AMT_PER_LANE), breaks=40)
dev.off()

#### CATEGORICAL
# HIGHWAY GROUP CODE
# data[HIGHWAY_GROUP_CODE == 'Independent Alignment', HIGHWAY_GROUP_CODE := 'Divided']
# qplot(data=data, x=HIGHWAY_GROUP_CODE) + geom_bar()

# POPULATION CODE (LAND USE)
svg("./output/site selection/figures/hist_context.svg")
qplot(data=data, x=POPULATION_CODE) + geom_bar() + theme_classic()
dev.off()

# ACCESS_CODE_DESC
svg("./output/site selection/figures/hist_accesscode.svg")
qplot(data=data, x=ACCESS_CODE_DESC) + geom_bar() + theme_classic()
data <- data[ , !"ACCESS_CODE_DESC"]
dev.off()

# ### Binned
# # LANES_AMT
# qplot(data=data, x=LANES_AMT) + geom_bar()
# 
# # DESIGN SPEED
# qplot(data=data, x=DESIGN_SPEED) + geom_bar()
# 
# # Median width
# svg("./output/site selection/figures/hist_median.svg")
# hist(log(scale(data$MEDIAN_WIDTH)), breaks=20)
# dev.off()

# 
# # Shoulder
# svg("./output/site selection/figures/hist_shoulder.svg")
# hist(log(scale(data$O_SHD_TOT_WIDTH)), breaks=20)
# dev.off()

# 
# # ADT
# qplot(data=data, x=ADT_AMT_PER_LANE) + geom_bar()



##### Convert char to factors
data <- data[ , lapply(.SD, function(x) { if(class(x)=='character') as.factor(x) else x })]


## Normalization
minmax <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}


# Normalize
numcols <- colnames(data)[which(sapply(data,class) %in% c('numeric', 'integer'))]
numcols <- numcols[numcols != 'SEG_ID' & numcols != 'OBJECTID']

data_unscaled = copy(data)
if(length(numcols)>0) {
  data[ , (numcols) := lapply(.SD, minmax), .SDcols = numcols]
  #data[ , (numcols) := lapply(.SD, scale), .SDcols = numcols]
  # PCA
  res.pca <- PCAmix(X.quanti = data[ , c(numcols), with=F], 
                    X.quali = data[ , !c(numcols, 'SEG_ID', 'OBJECTID'), with=F], 
                    rename.level = TRUE, graph = FALSE)
} else {
  res.pca <- PCAmix(X.quali = data[ , !c(numcols, 'SEG_ID', 'OBJECTID'), with=F], 
                    rename.level = TRUE, graph = FALSE)
}

#Factors
# jpeg("./output/site selection/figures/PCA_factors.jpg", width = 800, height = 800)
# plot(res.pca,choice="levels", xlim=c(-3,3), ylim=c(-3,2), main="Levels")
# dev.off()

#Numbers
# jpeg("./output/site selection/figures/PCA_numbers.jpg", width = 800, height = 800)
# plot(res.pca,choice="cor",main="Numerical variables")
#dev.off()

#Both
# /jpeg("./output/site selection/figures/PCA_all.jpg", width = 800, height = 800)
svg("./output/site selection/figures/PCA_all.svg")
plot(res.pca,choice="sqload", coloring.var=T, leg=TRUE,
     xlim = c(-1,1), ylim = c(-1,1),
     posleg="topright",
     main="All variables")
dev.off()


#### Inspect data ####
# hist(data[, log(LANES_AMT)], breaks = 25)          # log normal
# hist(data[, DESIGN_SPEED], breaks = 10)       # truncated normal
# hist(data[, ADT_AMT_PER_LANE], breaks = 100)  # log normal
# hist(data[, MEDIAN_WIDTH], breaks = 25)       # log normal
# hist(data[, log(I_SHD_TOT_WIDTH)])            # log normal
# hist(data[, log(O_SHD_TOT_WIDTH)])            # log normal
# hist(data[, log(TRAV_WAY_WIDTH)], breaks = 10,
#      xlim = c(0, log(max(data$TRAV_WAY_WIDTH))))# log normal
# 
# barplot(prop.table(table(data$POPULATION_CODE)))
# barplot(prop.table(table(data$TERRAIN_CODE)))
# barplot(prop.table(table(data$HIGHWAY_GROUP_CODE))) #Asymmetric
# barplot(prop.table(table(data$MEDIAN_BARRIER_CODE))) #Asymmetric
# barplot(prop.table(table(data$MEDIAN_TYPE_CODE))) #Asymmetric
# barplot(prop.table(table(data$SURF_TYPE_CODE))) #Asymmetric
# 



#### Hierarchical Agglomerative ####

#Using Gower Distance and PAM, but this is a manual iterative sampling method
#Clustering Large Applications (CLARA) does this more efficiently

fun_gowerpam <- function() {
  #Logarithmic transforms
  logcols = c('ADT_AMT_PER_LANE', 'O_SHD_TOT_WIDTH', 'MEDIAN_WIDTH')
  logcols = which(colnames(data[,!c('SEG_ID', 'OBJECTID')]) %in% logcols)
  
  # asymmcols = c('POPULATION_CODE', 'TERRAIN_CODE')
  # symmcols = c('HIGHWAY_GROUP_CODE', 'MEDIAN_BARRIER_CODE', 
  #              'MEDIAN_TYPE_CODE', 'SURF_TYPE_CODE')
  
  # Data for gower
  if(nrow(data) > 10000){
    #### Bootstrapping Gower distance if data too big
    # 1) Get unique rows, calculate distance
    # 2) sample N-unique random rows from the full data set, calculate distance
    # 3) Average the two, repeat 2 and 3
    # Get unique rows
    data_unique <- unique(data[ , !c('SEG_ID', 'OBJECTID')])
    
    # calculate gower dist matrix for unique rows
    gower_dist_unique <- daisy(data_unique,
                               metric = "gower",
                               type = list(logratio = logcols))
    # Bootstrap algorithm
    gower_dist <- gower_dist_unique   # Set current matrix values
    k <- 30                           # Set maximum iterations
    diff <- c(NA)                     # Initialize difference vector
    crit <- 0.01                      # Algorithm stops when criteria reached
    for(i in 1:k) {
      data_samp <- data[sample(1:nrow(data), nrow(data_unique)), !c('SEG_ID', 'OBJECTID')]
      gower_dist_samp <- daisy(data_samp,
                               metric = "gower",
                               type = list(logratio = logcols))
      
      # Current sum value
      old_sum <- sum(gower_dist)
      
      # Calculate new weighted mean
      gower_dist <- (i*gower_dist + gower_dist_samp) / i
      
      # Get new some and calculate the difference
      new_sum <- sum(gower_dist)
      diff[i] <- (new_sum - old_sum) / old_sum
      print(paste("Sampling", i, "of", k, "Current diff:",
                  paste0(round(100*diff[i],2), "%")))
      if(i %% 5 == 0) {
        ggplot(data.frame(x=1:i, y=100*diff), aes(x,y)) + 
          geom_point() + geom_line() +
          labs(x = 'n Samples', y = 'Difference (%)') + theme_classic()
      }
      
      if(diff[i] < crit) break
    }
    ggsave("./output/site selection/figures/gower_dist_bootstrap.svg")
  } else {
    # calculate gower dist matrix for entire data set
    gower_dist <- daisy(data[ , !c('SEG_ID', 'OBJECTID')],
                        metric = "gower",
                        type = list(logratio = logcols))
  }
  
  
  #### Get optimal number of clusters ####
  # Calculate silhouette width for many k using PAM
  nseq = round(1.6^(1:7))
  nseq = c(2, 4, 8, 12, 16, 24, 32, 48, 64, 72)
  #nseq = 2:10
  cl = parallel::makeCluster(min(7, length(nseq)))
  parallel::clusterExport(cl=cl, c('pam','gower_dist'))
  pam_fit_list <- pblapply(nseq, function(i){
    pam_fit <- pam(gower_dist, diss = TRUE, k = i)
    return(pam_fit)
  }, cl=cl)
  parallel::stopCluster(cl)
  names(pam_fit_list) <- nseq
  
  #Extract widths
  sil_width <- sapply(pam_fit_list, function(pam) pam$silinfo$avg.width)
  
  # Plot sihouette width (higher is better)
  ggplot(data.frame(x=nseq, y=sil_width), aes(x,y)) + 
    scale_x_continuous(breaks = nseq) +
    geom_point() + geom_line() + theme_classic()
  
  ggsave("./output/site selection/figures/k-cluster_silhouettes.svg")
  
  #### FINAL CLUSTERING RUN
  # Final k clusters
  names(pam_fit_list)
  k <- 72
  pam_fit <- pam_fit_list[[as.character(k)]]
  
  #### Visualization ###
  tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)
  tsne_data <- setNames(data.table(tsne_obj$Y, factor(pam_fit$clustering)),
                        c('X','Y', 'cluster'))
  
  # Are points on the same spot?
  tsne_data_unique <- tsne_data[!duplicated(X,Y), ]
  
  #
  cluster_counts <- data.frame(sort(table(tsne_data_unique$cluster), decreasing = T))
  
  
  for(n in c(nrow(cluster_counts), rev((nrow(cluster_counts) %/% 6)*c(1,2,3,4,5,6)))) {
    cluster_counts[1:n,]$Group <- paste0('<',n)
  }
  
  
  ggplot(cluster_counts, aes(x=Var1, y=Freq, fill=Group)) +
    geom_hline(yintercept = 12) +
    geom_col() + theme_classic()
  ggsave("./output/site selection/figures/cluster_sizes.svg")
  
  #Clusters
  ggplot(aes(x = X, y = Y), data = tsne_data) +
    geom_point(aes(color = cluster), shape=1) +
    geom_text(data=  tsne_data[, .SD[which.max(Y)], by = cluster],
      aes(x=X, y=Y, label = cluster)) +
    coord_fixed() + theme_bw()
  ggsave("./output/site selection/figures/k-cluster_xy.svg")
  
  # Assign to the data table and merge onto full unscaled data set
  if('data_unique' %in% ls()) {
    data_unique[ , CLUSTER := pam_fit$clustering]
    data_clustered <- merge(data, data_unique)
  } else {
    data_clustered <- copy(data)
    data_clustered[ , CLUSTER := pam_fit$clustering]
  }
  
 
  data_clustered <- merge(data_clustered[ , .(SEG_ID, CLUSTER)],
                          data_unscaled,
                          by='SEG_ID')
  
  
  return(list("data_clustered" = data_clustered,
              #"gower_dist" = gower_dist,
              "tsne_data" = tsne_data,
              "pam_fit_list" = pam_fit_list))
  
}


fun_clarapam <- function() {
  #### Optimal clusters
  df = copy(data)
  df[ , POPCODE := as.integer(POPULATION_CODE)]
  df <- df[ , !c('SEG_ID','OBJECTID','POPULATION_CODE')]
  
  fviz_nbclust(df, clara, method = "silhouette", k.max = 20) + theme_classic() + ggtitle(NULL)
  ggsave("./output/site selection/figures/clara_silhouette.svg", height=3.5, width=4)
  fviz_nbclust(df, clara, method = "wss", k.max = 20) + theme_classic() + ggtitle(NULL)
  ggsave("./output/site selection/figures/clara_wss.svg", height=3.5, width=4)
  # fviz_nbclust(df, clara, method = "gap_stat", k.max = 30, nboot=10) + theme_classic()
  
  # Clustering
  clarax <- clara(df, k = 13)

  #### Plots
  # Silhouettes
  fviz_silhouette(clarax, ggtheme = theme_classic())
  ggsave("./output/site selection/figures/clara_silhouette_widths.svg")
  
  # Points
  fviz_cluster(clarax,
               #palette = c("#00AFBB", "#FC4E07"), # color palette
               ellipse.type = "t", # Concentration ellipse
               geom = "point", pointsize = 1,
               ggtheme = theme_classic()
  ) + ggtitle(NULL)
  ggsave("./output/site selection/figures/clara_cluster_x-y.svg")
  
  
  # Assign to data
  data_clustered <- cbind(data_unscaled, "CLUSTER" = clarax$cluster)
  
  
  return(list("clarax" = clarax, "data_clustered" = data_clustered))
}


#### Run gower dist function with PAM
# gowerpam <- fun_gowerpam()
# data_clustered_gower <- gowerpam$data_clustered


clarapam <- fun_clarapam()
data_clustered_clara <- clarapam$data_clustered


# Save data image
rm(code, numcols, vars, CODES)
save.image(file = "./R/site selection/cluster_data_image.RData")

rm(list=ls())
gc()





