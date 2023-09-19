library(dplyr)
source('utils.r')

data_root <- 'data/CHARLS/2011/'
processed_indiv_file <- 'data/Save/process_data/process_indiv_data.csv'
merged_data_dir <- 'data/Save/merged_data/'

# Processed individual data
d <- read.csv(processed_indiv_file, header = TRUE)
 
# Read all the necessary community data

data_file <- 'community.dta'
c_data <- read_data(
  data_file = paste(data_root, data_file, sep = ''),
  features = c('communityID',  # Community ID
               'jf001s1',    # Choose Which Types of Medical Facilities
               'jf001s2',    # Choose Which Types of Medical Facilities
               'jf001s3',    # Choose Which Types of Medical Facilities
               'jf001s4',    # Choose Which Types of Medical Facilities
               'jf001s5',    # Choose Which Types of Medical Facilities
               'jf001s6',    # Choose Which Types of Medical Facilities
               'jf001s7',    # Choose Which Types of Medical Facilities
               'jf001s8',    # Choose Which Types of Medical Facilities
               'jf002_1_',   # Does Your V/C (village /community) Have
               'jf002_2_',   # Does Your V/C Have
               'jf002_3_',   # Does Your V/C Have
               'jf002_4_',   # Does Your V/C Have
               'jf002_5_',   # Does Your V/C Have
               'jf002_6_',   # Does Your V/C Have
               'jf002_7_',   # Does Your V/C Have
               'jf002_8_'))  # Does Your V/C Have

# 把所有的数据类型转换为numeric型
c_data <- as.data.frame(sapply(c_data, as.numeric))

data_file <- 'psu.dta'
psu_data <- read_data(
  data_file = paste(data_root, data_file, sep = ''),
  features = c('communityID',  # Community ID
               'urban_nbs'))   # Urban or Rural according to NBS

# 把所有的数据类型转换为numeric型
psu_data <- as.data.frame(sapply(psu_data, as.numeric))

# Merge the data
d <- Reduce(
  f = function(df1, df2)
    full_join(df1, df2, by = 'communityID'),
  list(d, c_data, psu_data),
  accumulate = FALSE)

# 合并完会多出重复的行，不知道为什么，见鬼了！
d = d[!duplicated(d$ID),]

cat('All data lines:', nrow(d), '\n')
cat('=======================================================================\n')

# Save the merged data
write.csv(d, 
          file = paste(merged_data_dir, 'merge_all_data.csv', sep = ''),
          row.names = FALSE) # Not generate automatic row names 1 2 3 ...

print('Done!')