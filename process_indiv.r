library(mice)
library(DMwR)
source('utils.r')


merged_indiv_file <- 'data/Save/merged_data/merge_indiv_data.csv'
processed_data_dir <- 'data/Save/process_data/'

d <- read.csv(merged_indiv_file, header = TRUE)

# 删除糖尿病三项指标全为缺失值的数据行
cat('data lines before blood sample delete:', nrow(d), '\n')
d <- d[-which(is.na(d$newglu) & is.na(d$newhba1c) & is.na(d$qc1_va003)),]
cat('data lines after blood sample delete:', nrow(d), '\n')
cat('=======================================================================\n')

# Multiple imputations for blood sample features
m <- 5  # Imputation times
imp <- mice(d[c('newglu', 'newhba1c', 'qc1_va003')], method = 'sample',
            m = m, seed = 1243, printFlag = FALSE)
d[c('newglu', 'newhba1c', 'qc1_va003')] <- complete(imp, action = m)

# Diabetes definition by blood test
d$diabetes.b <- 0   # No
d$diabetes.b[d$newglu >= 200.0 | d$newhba1c >= 6.5 | 
             (d$qc1_va003 == 2 & d$newglu >= 126)] <- 1  # Yes
 
# Diabetes definition by self-report
d$diabetes.s <- 0  # No
d$diabetes.s[d$da007_3_ == 1] <- 1  # Yes

cat('diabetes definition by blood test:', sum(d$diabetes.b), '\n')
cat('diabetes definition by self-report:', sum(d$diabetes.s), '\n')
cat('=======================================================================\n')

# # Diabetes patients who take medicines
# d$diabetes.t <- 1   # Not take
# d$diabetes.t[d$da014s1 == 1 | d$da014s2 == 1 | d$da014s3 == 1] <- 0  # Take

# Diabetes category combined by self-report and blood test
d$diabetes.c <- NA
# Those whose self-report is no, blood test is no:
d$diabetes.c[d$diabetes.s == 0 & d$diabetes.b == 0] <- 0
# Those whose self-report is no, blood test is yes
d$diabetes.c[d$diabetes.s == 0 & d$diabetes.b == 1] <- 1
# Those whose self-report is yes, blood test is no
d$diabetes.c[d$diabetes.s == 1 & d$diabetes.b == 0] <- 2
# Those whose self-report is yes, blood test is yes
d$diabetes.c[d$diabetes.s == 1 & d$diabetes.b == 1] <- 3
# d$diabetes.c无缺失值

# Case
d$case <- NA
# Those whose self-report is no, blood test is yes (Undiagnosed)
d$case[which(d$diabetes.c == 1)] <- 1
# Those whose self-report is yes (Diagnosed)
d$case[which(d$diabetes.c == 2 | d$diabetes.c == 3)] <- 0
# d$case有缺失值

# Age
d$age <- 2011 - d$ba002_1   # d$age有缺失值

cat('data lines before age delete:', nrow(d), '\n')
# 在SAS中，NA是小于任意一个非缺失值的
d <- d[which(d$age >= 45),]  # 筛选完后，age中再无NA 
cat('data lines after age delete:', nrow(d), '\n')

# 计算相关数据
cat('Age mean:', mean(d$age), '\n')
cat('Age min:', min(d$age), '\n')
cat('Age max:', max(d$age), '\n')
cat('=======================================================================\n')

cat('After age selection:\n')
cat('Those whose self-report is no, blood test is no:',
    sum(d$diabetes.c == 0), '\n')
cat('Those whose self-report is no, blood test is yes:',
    sum(d$diabetes.c == 1), '\n')
# cat('Those whose self-report is yes, blood test is no:',
#     sum(d$diabetes.c == 2), '\n')
# cat('Those whose self-report is yes, blood test is yes:',
#     sum(d$diabetes.c == 3), '\n')
cat('Those whose self-report is yes:',
      sum(d$diabetes.c == 2) + sum(d$diabetes.c == 3), '\n')
cat('=======================================================================\n')

# 因为d$case中有NA，直接用逻辑表达式在选取所有列的时候无法排除NA，
#   所以这里用which会忽略掉d$case中的NA
# 注：只有在选取部分行的时候会用到which，下面在设置元素的时候不会出现这个问题
d <- d[which(d$case == 1 | d$case == 0),]

cat('data lines after case delete:', nrow(d), '\n')
cat('=======================================================================\n')

# Gender
d$gender <- NA
# 设置元素的时候不需要使用which，即使d$rgender中有NA
d$gender[d$rgender == 2] <- 0  # Women
d$gender[d$rgender == 1] <- 1  # Men
# d$gender没有缺失值

# Layered age
d$senior <- 0  # Junior: 45 <= age < 75
d$senior[d$age >= 75] <- 1   # Senior: age >= 75
# d$senior <- 1
# d$senior[d$age < 60] <- 1
# d$senior[d$age >= 60 & d$age < 75] <- 2
# d$senior[d$age >= 75] <- 3
# d$senior <- 1
# d$senior[d$age < 65] <- 1
# d$senior[d$age >= 65 & d$age < 75] <- 2
# d$senior[d$age >= 75] <- 3

# Hukou status
# d$rural <- 1   # Rural
# d$rural[d$bc001 == 2 | d$bc002 == 2] <- 0   # Urban

# # Schooling
# d$school <- NA
# d$school[d$bd001 > 5] <- 1   # High
# d$school[d$bd001 == 2 | d$bd001 == 3 | d$bd001 == 4] <- 2  # Middle
# d$school[d$bd001 == 1] <- 3  # Low

# Education
d$education <- 0  # Yes
d$education[d$bd001 == 1] <- 1  # No

# Marriage
d$marriage <- 0  # Married
d$marriage[d$be001 >= 2 | d$be002 == 2] <- 1  # Not married/Single

# ------------------------------------------------------------------------------
# Relative income level by use of rated living standard
# ------------------------------------------------------------------------------

# Single imputation (median) for g003 (Rate Your Standard of Living)
d$g003 <- centralImputation(data.frame(d$g003))$d.g003

# Relative income level (3 classes)
# d$level <- NA
# d$level[d$g003 == 1 | d$g003 == 2] <- 1  # High
# d$level[d$g003 == 3] <- 2  # Average
# d$level[d$g003 == 4 | d$g003 == 5] <- 3  # Poor

# Relative income level (2 classes)
d$income <- 1   # Poor
d$income[d$g003 == 1 | d$g003 == 2 | d$g003 == 3] <- 0  # Not poor

# ------------------------------------------------------------------------------
# Self-rated health status
# ------------------------------------------------------------------------------

# Self-rated health
d$health <- 2
d$health[d$da001 == 1 | d$da001 == 2 | d$da001 == 3 | d$da002 == 1 |
         d$da002 == 2] <- 1  # Good
d$health[d$da001 == 4 | d$da002 == 3] <- 2  # Fair
d$health[d$da001 == 5 | d$da002 == 4 | d$da002 == 5] <- 3  # Poor

# ------------------------------------------------------------------------------
# Overweight & obesity based on BMI calculation
# ------------------------------------------------------------------------------

# # 处理错误数据
# d$qi002[d$qi002 == 993 | d$qi002 == 999] <- NA
# d$qi002[d$qi002 == 1.57] <- 157
# d$qi002[d$qi002 == 1.65] <- 165
# d$ql002[d$ql002 == 993 | d$ql002 == 999 | d$ql002 == 0] <- NA
# d$qm002[d$qm002 == 993 | d$qm002 == 999] <- NA
#
# # Single imputation (median) for
# # Height
# d$qi002 <- centralImputation(data.frame(d$qi002))$d.qi002
# # Weight Measurement
# d$ql002 <- centralImputation(data.frame(d$ql002))$d.ql002
# # Waist Measurement
# d$qm002 <- centralImputation(data.frame(d$qm002))$d.qm002
#
# # Height
# d$height <- d$qi002 / 100
#
# # BMI
# d$BMI <- d$ql002 / d$height^2
#
# # Overweight
# d$overweight <- 0  # Not overweight
# d$overweight[d$BMI > 24] <- 1  # Overweight
#
# # Obesity
# d$obseity <- 0   # Not Obesity
# d$obseity[(d$gender == 1 & d$qm002 >= 90) |
#           (d$gender == 0 & d$qm002 >= 80)] <- 1  # Obesity

# ------------------------------------------------------------------------------
# Other chronic diseases
# ------------------------------------------------------------------------------

# 高血压
d$hyperten <- 0
d$hyperten[d$da007_1_ == 1] <- 1

# 血脂异常
d$dyslipid <- 0
d$dyslipid[d$da007_2_ == 1] <- 1

# 癌症或恶性肿瘤
d$tumor <- 0
d$tumor[d$da007_4_ == 1] <- 1

# 慢性肺疾病
d$bronchi <- 0
d$bronchi[d$da007_5_ == 1] <- 1

# 肝病
d$liver <- 0
d$liver[d$da007_6_ == 1] <- 1

# 心脏问题
d$heart <- 0
d$heart[d$da007_7_ == 1] <- 1

# 中风
d$stroke <- 0
d$stroke[d$da007_8_ == 1] <- 1

# 肾病
d$kidney <- 0
d$kidney[d$da007_9_ == 1] <- 1

# 胃或其他消化系统疾病
d$digest <- 0
d$digest[d$da007_10_ == 1] <- 1

# 情绪、紧张或精神问题
d$emotion <- 0
d$emotion[d$da007_11_ == 1] <- 1

# 记忆力相关的疾病
d$memory <- 0
d$memory[d$da007_12_ == 1] <- 1

# 关节炎、风湿病
d$arthritis <- 0
d$arthritis[d$da007_13_ == 1] <- 1

# 哮喘
d$asthma <- 0
d$asthma[d$da007_14_ == 1] <- 1

# 慢性病
d$chronic <- d$hyperten + d$dyslipid + d$tumor + d$bronchi + d$liver +
             d$heart + d$stroke + d$kidney + d$digest + d$emotion +
             d$memory + d$arthritis + d$asthma

# 疾病
d$disease <- 1  # Yes
d$disease[d$chronic == 0] <- 0  # No

# ------------------------------------------------------------------------------
# Health promotion and lifestyle in case being diagnosed as diabetes
# ------------------------------------------------------------------------------

# # Diabetes test
# d$test <- 1   # Yes
# d$test[d$da015s1 == 1 | d$da015s2 == 2 |
#        d$da015s3 == 3 | d$da015s4 == 4] <- 0   # No
#
# # Health advice
# d$prevention <- 1  # Yes
# d$prevention[d$da016s1 == 1 | d$da016s2 == 2 | d$da016s3 == 3 |
#              d$da016s4 == 4 | d$da016s5 == 5] <- 0  # No
#
# # Current smoking
# d$smoke <- 0  # No
# d$smoke[d$da061 == 1] <- 1  # Yes
#
# # Physical activities
# d$pa <- 0
#
# # Vigorous activities
# # Do vigorous activities >= 10 min: Yes and
# # Days do Vigorous activities >= 10 min: >= 3 days and
# # 2 hours everyday do vigorous activities >= 10 min: >= 2 hours or
# # Minutes everyday do vigorous activities >= 10 min: >= 30 min  or
# # 4 hours everyday do vigorous activities >= 10 min: >= 4 hours
# d$pa[(d$da051_1_ == 1 & d$da052_1_ >=3 & d$da053_1_ == 2) |
#      (d$da051_1_ == 1 & d$da052_1_ >=3 & d$da054_1_ == 2) |
#      (d$da051_1_ == 1 & d$da052_1_ >=3 & d$da055_1_ == 2) ] <- 1
#
# # Moderate Physical Effort
# # Do moderate Physical Effort >= 10 min: Yes and
# # Days do moderate physical effort >= 10 min: >= 3 days and
# # 2 hours everyday do moderate physical effort >= 10 min: >= 2 hours or
# # Minutes everyday do moderate physical effort >= 10 min: >= 30 min  or
# # 4 hours everyday do moderate physical effort >= 10 min: >= 4 hours
# d$pa[(d$da051_2_ == 1 & d$da052_2_ >=3 & d$da053_2_ == 2) |
#      (d$da051_2_ == 1 & d$da052_2_ >=3 & d$da054_2_ == 2) |
#      (d$da051_2_ == 1 & d$da052_2_ >=3 & d$da055_2_ == 2) ] <- 2
#
# # Walking
# # Walking >= 10 min: Yes and
# # Days walking >= 10 min: >= 3 days and
# # 2 hours everyday walking >= 10 min: >= 2 hours or
# # minutes everyday walking >= 10 min: >= 30 min  or
# # 4 hours everyday walking >= 10 min: >= 4 hours
# d$pa[(d$da051_3_ == 1 & d$da052_3_ >=3 & d$da053_3_ == 2) |
#      (d$da051_3_ == 1 & d$da052_3_ >=3 & d$da054_3_ == 2) |
#      (d$da051_3_ == 1 & d$da052_3_ >=3 & d$da055_3_ == 2) ] <- 3
#
# # Exercise
# d$exercise <- 1  # Not do exercise
# d$exercise[d$pa == 1 | d$pa == 2 | d$pa == 3] <- 0  # Do exercise

# ------------------------------------------------------------------------------
# Health care & medical insurance
# ------------------------------------------------------------------------------

# # Insurance
# d$insurance <- NA
#
# # Urban employee, government, private (purchased by R's union) medical insurance
# d$insurance[d$ea001s1 == 1 | d$ea001s5 == 5 | d$ea001s7 == 7] <- 1
#
# # Urban resident, new cooperative, urban and rural resident, private
# #   (purchased by individual), other medical insurance
# d$insurance[d$ea001s2 == 2 | d$ea001s3 == 3 | d$ea001s4 == 4 |
#             d$ea001s8 == 8 | d$ea001s9 == 9] <- 2
#
# # Medical aid, no insurance
# d$insurance[d$ea001s6 == 6 | d$ea001s10 == 10] <- 3
# d$insurance[is.na(d$ea001s1) & is.na(d$ea001s2) & is.na(d$ea001s3) &
#             is.na(d$ea001s4) & is.na(d$ea001s5) & is.na(d$ea001s6) &
#             is.na(d$ea001s7) & is.na(d$ea001s8) & is.na(d$ea001s9) &
#             is.na(d$ea001s10)] <- 3

# Private medical insurance
d$medinsurance <- 1
# Urban employee, government medical insurance
d$medinsurance[d$ea001s1 == 1 | d$ea001s5 == 5] <- 0
# ------------------------------------------------------------------------------

# Save the processed data
dir.create(processed_data_dir, recursive = TRUE, showWarnings = FALSE)
write.csv(d,
    file = paste(processed_data_dir, 'process_indiv_data.csv', sep = ''),
    row.names = FALSE) # Not generate automatic row names 1 2 3 ...

print('Done!')

