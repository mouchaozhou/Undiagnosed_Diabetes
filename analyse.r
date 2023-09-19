library(Hmisc)
library(DAAG)
library(car)
source('utils.r')


processed_all_file <- 'data/Save/process_data/process_all_data.csv'

# Read the merged and processed individual and community data
d <- read.csv(processed_all_file, header = TRUE)

# 取出因、自变量
var <-
  d[c(
    # 因变量
    'case',
    # 自变量
    # Predisposing factors
    'gender',
    'senior',
    'education',
    'marriage',
    # Enabling factors
    'income',
    'medinsurance',
    'facility',
    'city',  
    'region', # 3分类
    # Need factors
    'disease',
    'health'  # 3分类
  )]

# 自变量的名称
tab_names <- c(
  'Diabetes',
  'Gender',
  'Age',
  'Education',
  'Marriage status',
  'Household income',
  'Medical insurance',
  'Medical facilities',
  'Residential places',
  'Geographical regions',
  'Other chronic diseases',
  'Perceived health'
)

# 自变量取值的名称
row_names <- list(
  c('Diagnosed', 'Undiagnosed'),
  c('Women', 'Men'),
  c('45-75', '$\\geq$ 75'),
  # c('45-60', '60-75', '$\\geq$ 75'),
  # c('45-65', '65-75', '$\\geq$ 75'),
  c('Literate', 'Illiterate'),
  c('Having spouse or partner', 'No spouse or partner'),
  c('Average or Above', 'Below average'),
  c('High reimbursement rate', 'Low reimbursement rate'),
  c('Yes', 'No'),
  c('Urban', 'Rural'),
  c('East', 'Middle', 'West'),
  c('No', 'Yes'),
  c('Good', 'Fair', 'Poor')
)


# 画流程图
# flow_chart(fig_file = 'data/Save/results/flow_chart.eps')

# 计算并输出一维、二维列联表的频数、频率、OR、95%CI和P值
univar_tab(var, tab_names = tab_names, row_names = row_names,
           out_file = 'data/Save/results/univar_table.txt')

# 逻辑斯特回归：OR、95%CI和P值
tab.var <- var[c('case', 'senior', 'medinsurance', 
                 'city', 'region', 'disease', 'health')]
tab.tab_names <- tab_names[c(1, 3, 7, 9:12)]
tab.row_names <- row_names[c(1, 3, 7, 9:12)]

prob <- logist_tab(tab.var, tab_names = tab.tab_names,
                            row_names = tab.row_names,
                            out_file = 'data/Save/results/logist_tab.txt')
# 保存进原始数据
var$prob <- prob

# 画ROC图
plot_roc(var, fig_file = 'data/Save/results/roc.eps')

print('Done!')