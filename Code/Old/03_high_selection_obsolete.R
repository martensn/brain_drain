

##    Obsolete High School Selection

```{r hs select, include=FALSE}

# Includes two types of observations:
# unoriginal high schools which need to be combined
# multiple observations of degrees where one ob is missing data
# Filter to only include users attending high school with unoriginal name
unoriginal_hs_user = merge_col_hs[, .(count = .N), by = user_id] %>%
  table.express::filter(count > 1)
unoriginal_hs_user[,c("count") := NULL]

# Number of potential CBSAs attached to each user
unoriginal_hs = merge_col_hs[user_id %in% unoriginal_hs_user$user_id]
unoriginal_hs = unoriginal_hs[, .(count = .N), by = .(user_id,hs_cbsa_code)]

rm(unoriginal_hs_user)

# Imputation of HS only matters if CBSA is ambiguous
# Any user with a known CBSA should be removed before starting the matching
# These are observations with the same high school, but one might contain missing data
# (109290013)
hs_cbsa_known = unoriginal_hs[, .(rows = .N), by = user_id] %>%
  table.express::filter(rows == 1)
hs_cbsa_known[,c("rows") := NULL]

hs_merging = merge_col_hs[user_id %in% hs_cbsa_known$user_id]
#sapply(hs_merging, function(x) x[!is.na(x)][1])
hs_merge = do.call(rbind, lapply(split(hs_merging, hs_merging$user_id), 
                                 function(a) sapply(a, function(x) x[!is.na(x)][1])))





# Filter out false matches and coerce date back into iDate rather than integer
# Simplifies future merge
hs_merge = as.data.table(hs_merge) %>%
  table.express::filter(is.na(col_name)==FALSE) %>%
  table.express::mutate(work_start = as.IDate(as.numeric(work_start)))


# Users where CBSA code is ambiguous without imputation of high school
hs_cbsa_unknown = unoriginal_hs[, .(rows = .N), by = user_id] %>%
  table.express::filter(rows != 1)
hs_imputation = merge_col_hs[user_id %in% hs_cbsa_unknown$user_id]

# https://stackoverflow.com/questions/55408526/r-find-the-distance-between-two-us-zipcode-columns
## Convert the zip codes to data.table so we can join on them
## using the centroid of the zipcodes (lng and lat).
dt_zips <- as.data.table(read.csv(file.path(directory,"Data/03",zipcodefilename),
                                  colClasses = c("character","numeric","numeric")))

dt_zips[,c("X") := NULL]
# Ensure ZIP code for educational institution is standard five digits
hs_imputation[
  , `:=`(
    col_zip = str_sub(col_zip,1,5),
    hs_zip = str_sub(hs_zip,1,5)
  )
]

## Attach origin lon & lat using a join
hs_imputation[
  dt_zips
  , on = .(col_zip = zipcode)
  , `:=`(
    lng_start = lng
    , lat_start = lat
  )
]

## Attach destination lon & lat using a join
hs_imputation[
  dt_zips
  , on = .(hs_zip = zipcode)
  , `:=`(
    lng_end = lng
    , lat_end = lat
  )
]

## Calculate the distance
hs_imputation[
  , distance_metres := geodist::geodist_vec(
    x1 = lng_start
    , y1 = lat_start
    , x2 = lng_end
    , y2 = lat_end
    , paired = TRUE
    , measure = "haversine"
  )
]

# Select the closest college
hs_imputation = hs_imputation[hs_imputation[, .I[which.min(distance_metres)], by=user_id]$V1]

# Remove columns from distance calculation to ease rbind
hs_imputation[,c("lng_start","lat_start","lng_end","lat_end","distance_metres") := NULL]

# Combine users with duplicate entries and ambiguous high schools
unoriginal = rbind(hs_merge,hs_imputation)

# Users at high schools with unique names (CBSA known from HS name)
original_hs_user = merge_col_hs[, .(count = .N), by = user_id] %>%
  table.express::filter(count == 1)
original_hs_user[,c("count") := NULL]
original = merge_col_hs[user_id %in% original_hs_user$user_id]

merge_col_hs = rbind(unoriginal,original)

rm(hs_cbsa_known,hs_cbsa_unknown,hs_imputation,dt_zips,
   original_hs_user,hs_merge,hs_merging,original,unoriginal)
gc()
`