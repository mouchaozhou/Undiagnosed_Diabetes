library(dplyr)
source('utils.r')


data_root <- 'data/CHARLS/2011/'
merged_data_dir <- 'data/Save/merged_data/'

# Read all the necessary individual data

data_file <- 'Blood_20140429.dta'
b2_data <- read_data(
  data_file = paste(data_root, data_file, sep = ''),
  features = c('ID',
               'newglu',       # Glucose
               'newhba1c',     # Glycated Hemoglobin
               'qc1_va003'))   # Fasting or Not

data_file <- 'health_status_and_functioning.dta'
hsf_data <- read_data(
  data_file = paste(data_root, data_file, sep = ''),
  features = c('ID',
               'da001',       # Self Comment of Your Health
               'da002',       # Self Comment of Your Health
               'da007_3_',    # Diabetes or High Blood Sugar
               'da014s1',     # Take Chinese Traditional Medicine for Diabetes
               'da014s2',     # Take Western Modern Medicine for Diabetes
               'da014s3',     # Taking Insulin Injections for Diabetes
               'da014s4',     # None of the Above for Diabetes
               'da015_1_a',   # How Many Times Have You Had Diebetes Tests of Blood Glucose Test
               'da015_2_a',   # How Many Times Have You Had Diebetes Tests of Urine Glucose Test
               'da015_3_a',   # How Many Times Have You Had Diebetes Tests of Fundus Examination
               'da015_4_a',   # How Many Times Have You Had Diebetes Tests of Micro-albuminuria Test
               'da015s1',     # Have You Had Diebetes Tests of Blood Glucose Test
               'da015s2',     # Have You Had Diebetes Tests of Urine Glucose Test
               'da015s3',     # Have You Had Diebetes Tests of Fundus Examination
               'da015s4',     # Have You Had Diebetes Tests of Micro-albuminuria Test
               'da015s5',     # None Diebetes Tests Have Had
               'da016s1',     # Got Any Health Advice from doctor of Weight Control
               'da016s2',     # Got Any Health Advice from doctor of Exercise
               'da016s3',     # Got Any Health Advice from doctor of Diet
               'da016s4',     # Got Any Health Advice from doctor of Smoking Control
               'da016s5',     # Got Any Health Advice from doctor of Foot Self-care
               'da016s6',     # Got Any Health Advice from doctor of None of the Above
               'da059',       # Smoke or Not
               'da061',       # Still Smoking or Not
               'da051_1_',    # Y/N Do Vigorous Activities At Least 10 Minutes Continuously
               'da052_1_',    # Days Do Vigorous Activities At Least 10 Minutes Continuously
               'da053_1_',    # 2 Hours Everyday Do Vigorous Activities At Least 10 Minutes Continuously
               'da054_1_',    # Minutes Everyday Do Vigorous Activities At Least 10 Minutes Continuously
               'da055_1_',    # 4 Hours Everyday Do Vigorous Activities At Least 10 Minutes Continuously
               'da051_2_',    # Y/N Do Moderate Physical Effort At Least 10 Minutes Continuously
               'da052_2_',    # Days Do Moderate Physical Effort At Least 10 Minutes Continuously
               'da053_2_',    # 2 Hours Everyday Do Moderate Physical Effort At Least 10 Minutes Continuously
               'da054_2_',    # Minutes Everyday Do Moderate Physical Effort At Least 10 Minutes Continuously
               'da055_2_',    # 4 Hours Everyday Do Moderate Physical Effort At Least 10 Minutes Continuously
               'da051_3_',    # Y/N Walking At Least 10 Minutes Continuously
               'da052_3_',    # Days Walking At Least 10 Minutes Continuously
               'da053_3_',    # 2 Hours Everyday Walking At Least 10 Minutes Continuously
               'da054_3_',    # Minutes Everyday Walking At Least 10 Minutes Continuously
               'da055_3_',    # 4 Hours Everyday Walking At Least 10 Minutes Continuously
               'da007_1_',    # Hypertension
               'da007_2_',    # Dyslipidemia
               'da007_4_',    # Cancer or Malignant Tumor
               'da007_5_',    # Chronic Lung Diseases
               'da007_6_',    # Liver Disease
               'da007_7_',    # Heart Problems
               'da007_8_',    # Stroke
               'da007_9_',    # Kidney Disease
               'da007_10_',   # Stomach or Other Digestive Disease
               'da007_11_',   # Emotional, Nervous, or Psychiatric Problems
               'da007_12_',   # Memory-Related Disease
               'da007_13_',   # Arthritis or Rheumatism
               'da007_14_',   # Asthma
               'dc009',       # Bothered by Things
               'dc010',       # Had Trouble Keeping Mind
               'dc011',       # Felt Depressed
               'dc012',       # Felt Everything I Did Was An Effort
               'dc013',       # Felt Hopeful About the Future
               'dc014',       # Felt fearful
               'dc015',       # Sleep Was Restless
               'dc016',       # Happy
               'dc017',       # Felt Lonely
               'dc018'))      # Could Not Get Going

data_file <- 'demographic_background.dta'
db_data <- read_data(
  data_file = paste(data_root, data_file, sep = ''),
  features = c('ID',
               'ba002_1',      # Birth Year
               'ba002_2',      # Birth Month
               'ba002_3',      # Birth Day
               'ba003',        # Calender Type
               'ba004',        # Age
               'bc001',        # What is Your Current Hukou Status
               'bc002',        # What is Your Hukou Status before You Have the Unified Residence Hukou
               'bd001',        # Highest Level of Education Attained
               'be001',        # Marital Status
               'be002',        # Are You Unmarried but Living with a Partner
               'rgender'))     # Gender

data_file <- 'biomarkers.dta'
b_data <- read_data(
  data_file = paste(data_root, data_file, sep = ''),
  features = c('ID',
               'qi002',   # Height
               'ql002',   # Weight Measurement
               'qm002'))  # Waist Measurement

data_file <- 'health_care_and_insurance.dta'
hci_data <- read_data(
  data_file = paste(data_root, data_file, sep=''),
  features = c('ID',
               'ea001s1',    # Y/N Urban Employee Medical Insurance
               'ea001s2',    # Y/N Urban Employee Medical Insurance
               'ea001s3',    # Y/N New Cooperative Medical Insurance
               'ea001s4',    # Y/N Urban and Rural Resident Medical Insurance
               'ea001s5',    # Y/N Government Medical Insurance
               'ea001s6',    # Y/N Medical Aid
               'ea001s7',    # Y/N Private Medical Insurance: Purchased By R's Union
               'ea001s8',    # Y/N Private Medical Insurance: Purchased By Individual
               'ea001s9',    # Y/N Other Medical Insurance
               'ea001s10'))  # Y/N No Insurance

data_file <- 'individual_income.dta'
ii_data <- read_data(
  data_file = paste(data_root, data_file, sep = ''),
  features = c('ID',
               'g001',   # Rate Standard of Living of Mr Wang
               'g002',   # Rate Standard of Living of Mr Zhang
               'g003'))  # Rate Your Standard of Living

data_file <- 'weight.dta'
w_data <- read_data(
  data_file = paste(data_root, data_file, sep = ''),
  features = c('ID',
               'communityID', # Community ID
               'ind_weight_ad2'))  # Individual Weight with Household and Individual Non-response Adjustment

# Merge the data
d <- Reduce(
  f = function(df1, df2)
    full_join(df1, df2, by = 'ID'),
  list(w_data, db_data, hsf_data, hci_data, ii_data, b_data, b2_data),
  accumulate = FALSE)

cat('Individual data lines:', nrow(d), '\n')
cat('=======================================================================\n')

# 把所有的数据类型转换为numeric型
d <- as.data.frame(sapply(d, as.numeric))

# 按ID排序
d <- d[order(d$ID),]

# Save the merged data
dir.create(merged_data_dir, recursive = TRUE, showWarnings = FALSE)

write.csv(d, 
          file = paste(merged_data_dir, 'merge_indiv_data.csv', sep = ''),
          row.names = FALSE) # Not generate automatic row names 1 2 3 ...

print('Done!')
