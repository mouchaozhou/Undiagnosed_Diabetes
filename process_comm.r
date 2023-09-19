merged_all_file <- 'data/Save/merged_data/merge_all_data.csv'
processed_data_dir <- 'data/Save/process_data/'

# Read the data merged with individual and community data
d <- read.csv(merged_all_file, header = TRUE)

# 删除ID缺失的数据行
cat('data lines before ID delete:', nrow(d), '\n')
d <- d[which(d$ID > 0),]
cat('data lines after ID delete:', nrow(d), '\n')
cat('=======================================================================\n')

# City
d$city <- NA
d$city[d$urban_nbs == 1] <- 1  # Rural
d$city[d$urban_nbs == 2] <- 0  # Urban

# Facility
d$facility <- 1   # Yes
d$facility[
  (d$jf001s1 == 1 & d$jf002_1_ == 2) |  # General hospitals
  (d$jf001s2 == 2 & d$jf002_2_ == 2) |  # Specialized hospitals
  (d$jf001s3 == 3 & d$jf002_3_ == 2) |  # Chinese medicine hospitals
  (d$jf001s4 == 4 & d$jf002_4_ == 2) |  # Nearby pharmacy store
  (d$jf001s5 == 5 & d$jf002_5_ == 2) |  # Community health care center
  (d$jf001s6 == 6 & d$jf002_6_ == 2) |  # Community health care medical post
  (d$jf001s7 == 7 & d$jf002_7_ == 2) |  # Township health clinic hospital
  (d$jf001s8 == 8 & d$jf002_8_ == 2)    # Village medical post
] <- 0  # No

# Areas
d$region <- NA

d$region[ 
  (d$communityID >= 0304761 & d$communityID <= 0346763) | 
  (d$communityID >= 0604401 & d$communityID <= 0655463) | 
  (d$communityID >= 0940041 & d$communityID <= 0940043) | 
  (d$communityID >= 1101311 & d$communityID <= 1182763) | 
  (d$communityID >= 1504781 & d$communityID <= 1582783) | 
  (d$communityID >= 1601041 & d$communityID <= 1682313) | 
  (d$communityID >= 1840371 & d$communityID <= 1840373) | 
  (d$communityID >= 1940371 & d$communityID <= 1940373) | 
  (d$communityID >= 2001041 & d$communityID <= 2074283) | 
  (d$communityID >= 2916761 & d$communityID <= 2986313) ] <- 1  # West
d$region[
  (d$communityID >= 0724281 & d$communityID <= 0782333) | 
  (d$communityID >= 1411541 & d$communityID <= 1455283) | 
  (d$communityID >= 1711591 & d$communityID <= 1774023) | 
  (d$communityID >= 2111541 & d$communityID <= 2124283) | 
  (d$communityID >= 2616761 & d$communityID <= 2686063) | 
  (d$communityID >= 2704021 & d$communityID <= 2755593) | 
  (d$communityID >= 3201461 & d$communityID <= 3263593) | 
  (d$communityID >= 3301021 & d$communityID <= 3382313) ] <- 2  # Middle
d$region[  
  (d$communityID >= 0101041 & d$communityID <= 0182063) | 
  (d$communityID >= 0467461 & d$communityID <= 0467463) | 
  (d$communityID >= 0516061 & d$communityID <= 0586593) | 
  (d$communityID >= 0896761 & d$communityID <= 0896763) | 
  (d$communityID >= 1017911 & d$communityID <= 1082593) | 
  (d$communityID >= 1224901 & d$communityID <= 1240993) | 
  (d$communityID >= 1340041 & d$communityID <= 1382023) | 
  (d$communityID >= 2411161 & d$communityID <= 2446763) | 
  (d$communityID >= 2801541 & d$communityID <= 2882513) | 
  (d$communityID >= 3452331 & d$communityID <= 3477633) ] <- 3  # East
# d$region没有缺失值

# Save the processed data
write.csv(d, 
          file = paste(processed_data_dir, 'process_all_data.csv', sep = ''),
          row.names = FALSE) # Not generate automatic row names 1 2 3 ...

print('Done!')
