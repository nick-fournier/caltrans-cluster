library(data.table)
library(pbapply)


#### MATCHES THE POSTMILE POINTS TO THE TASAS LINEAR SEGMENT DATA ####

#Load data
pmdata <- fread('./output/site selection/SB127_collapsed_events_postmiles.csv')
segment <- fread('./raw data/segment_clean_2015-2019.csv')

# Get most recent data
segment[ , date := lubridate::mdy(BEGIN_DATE)]
segment <- segment[segment[order(-date), .I[1L], LOC]$V1][ , !"date"]
segment <- segment[END_DATE=="", ]

# Get alignment
pmdata[ , ALIGN := substr(AlignCode,0,1)]
segment[ , ALIGN := ifelse(PM_PFX=='R', 'R', ifelse(PM_PFX=='L','L',NA))]

# Missing routes? We would rather have all segments be in post miles than have missing segments in postmiles
# Segments not in postmiles?
setdiff(unique(segment$ROUTE), unique(pmdata$Route))

# Postmiles not in segments?
setdiff(unique(pmdata$Route), unique(segment$ROUTE))

#Remove object ID
# pmdata <- pmdata[ , !'OBJECTID']


# Create new IDs
pmdata[ , PM_ID := 1:nrow(pmdata)]
segment[ , SEG_ID := 1:nrow(segment)]


# Vectorized list matching by postmile on route
matchfunc_vec <- function(pm) {
  # print(pm)
  matches <- segment[ROUTE ==  pm$Route &
                       DISTRICT == pm$District &
                       COUNTY == pm$County &
                       ALIGN ==  pm$ALIGN &
                       RTE_SFX == pm$Rtesuffix &
                       END_PM >  pm$PM & 
                       BEGIN_PM <=  pm$PM
                     , ]
  
  # If no matches, remove alignment check
  if(nrow(matches)==0) {
    matches <- segment[ROUTE ==  pm$Route &
                         DISTRICT == pm$District &
                         COUNTY == pm$County &
                         END_PM >  pm$PM &
                         BEGIN_PM <=  pm$PM
                       , ]
  }
  
  # If no matches still, remove post mile distance
  if(nrow(matches)==0) {
    matches <- segment[ROUTE ==  pm$Route &
                         DISTRICT == pm$District &
                         COUNTY == pm$County &
                         ALIGN == pm$ALIGN
                       , ]
  
  }
  
  # If no matches still, remove post mile and alignment!
  if(nrow(matches)==0) {
    matches <- segment[ROUTE ==  pm$Route &
                         DISTRICT == pm$District &
                         COUNTY == pm$County
                       , ]
    
  }
  
  # If multiple matches, keep nearest segment
  if(nrow(matches)>1) {
    idx <- which.min(apply(matches[ , .(abs(BEGIN_PM-pm$PM), abs(END_PM-pm$PM))], 1, min))
    matches <- matches[idx, ]
  }
  
  #Bind the match together
  match <- cbind(pm,matches[ , !"ALIGN"])
  return(match)
}

# Error handling
try_matchfunc_vec <- function(pm) {
  tryCatch(
    {
      matchfunc_vec(pm)
      },
    error = function(e) {
      print(paste('Error on', pm)); print(paste(e)) 
      }
    )
}


## Vectorize the target
range <- 1:nrow(pmdata) # Full range, or just a sample
# range <- (floor(nrow(pmdata)*0.255/1e4)*1e4):(ceiling(nrow(pmdata)*0.26/1e4)*1e4)
# range <- sample(range, 1e4)
pmsplit <- split(pmdata[range, .(OBJECTID, PM_ID, Route, District, County, ALIGN, PM, RteSuffix)], by="PM_ID")

# Setup parallel
cl = parallel::makeCluster(7)
parallel::clusterExport(cl=cl, c('segment','data.table','matchfunc_vec'))

#run in parallel
matchedlist <- pblapply(pmsplit, try_matchfunc_vec, cl=cl)
parallel::stopCluster(cl)


#### Check results ####
# Recombine
matched <- rbindlist(matchedlist)


#Check if any have multiple rows
which(lapply(matchedlist, nrow)>1)
# matchedlist[['401649']]
# pm = pmsplit[['401649']]

#Check if any missing
print(paste0("Percent matched: ",
            round(100*nrow(matched[!is.na(ROUTE), ])/nrow(matched),2),
            "%"
))

# Add extra PM data
matched <- merge(pmdata[, .(PM_ID, X, Y, Odometer)], matched, by = 'PM_ID')


#Saving
fwrite(matched, './output/site selection/SB127_matched_pm.csv')


rm(list=ls())
gc()


























