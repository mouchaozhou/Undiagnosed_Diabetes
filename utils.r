library(foreign)
library(fmsb)
library(epiDisplay)
library(diagram)
library(pROC)

options(scipen = 100) # 小数点后100位不使用科学计数法


#' Read the data
#'
#' @param data_file: the full path of the data file
#' @param features: which feature (column) to keep 
#'
#' @return the subset of the raw data

read_data <- function (data_file, features) {
  data <- read.dta(data_file)
  
  # 取出指定的feature
  data <- data[features]
  
  # fixed = TRUE 是完全匹配，不使用正则表达式
  cat(strsplit(basename(data_file), '.', fixed = TRUE)[[1]][1], 'lines:', 
      nrow(data), '\n')
  
  return(data) 
}
# -----------------------------------------------------------------------------

#' Calculate the missing data percentage per feature
#'
#' @param data 
#'
#' @return the missing data percentage

miss_pct <- function (data) {
  return(colSums(is.na(data)) / nrow(data) * 100)
}
# -----------------------------------------------------------------------------

#' 生成paper中的flow chart
#'
#' @param fig_file 
flow_chart <- function (fig_file) {
  # 设置EPS
  setEPS(horizontal = FALSE, onefile = FALSE, paper = "special")
  postscript(fig_file)
  
  # Margin of bottom left top right
  par(mar = c(1, 1, 1, 1))
  
  # Creates an empty plot
  openplotmat()
  
  # Calculates coordinates of elements, neatly arranged in rows/columns
  # 元素排列，每行个数为1，1和2
  elpos <- coordinates(c(1, 1, 2))
  
  # 箭头指向的元素编号
  fromto <- matrix(ncol = 2, byrow = TRUE, 
                   data = c(1, 2, 
                            2, 3, 
                            2, 4))
  # 箭头的位置矩阵
  nr <- nrow(fromto)
  arrpos <- matrix(ncol = 2, nrow = nr)
  
  # 画出所有箭头
  for (i in 1:nr) {
    # Adds straight arrow between two points
    straightarrow(
      from = elpos[fromto[i, 1],],
      to = elpos[fromto[i, 2],],
      lwd = 2, arr.pos = 0.6, arr.length = 0.5)
  }
  
  textrect(elpos[1,], radx = 0.32, rady = 0.05, lwd = 2,
    lab = paste('Sampled respondents from CHARLS baseline: 17708\n', 
                'Venous blood sample: 11847', sep = ''))
  
  textrect(elpos[2,], radx = 0.35, rady = 0.03, lwd = 2,
    lab = expression(paste('Eligible for analysis: ', age >= 45,
                           ' and useful data value: 11587', sep = '')))

  textrect(elpos[3,], radx = 0.20, rady = 0.07, lwd = 2,
    lab = paste('Undiagnosed diabetes (with no \n', 
                'previously diagnosed but \n', 
                'confirmed by blood test): 560',
                sep = ''))
  
  textrect(elpos[4,], radx = 0.22, rady = 0.03, lwd = 2,
    lab = paste('Previously diagnosed diabetes: 674',
                sep = ''))
  
  dev.off()
}
# -----------------------------------------------------------------------------

#' 生成paper中的单变量分析table
#'
#' @param x 
#' @param tab_names 
#' @param row_names 
#' @param out_file 
univar_tab <- function (x, tab_names, row_names, out_file) {
  # 打开文件
  f <- file(out_file, 'w')
  
  # 表头
  write('\\begin{specialtable}[H]', f)
  write(paste('    \\caption{Univariate analysis of variables associated ', 
        'with diabetes diagnosis \\label{tab:uni}}', sep = ''), f)
  write('    \\begin{tabular}{llccc}', f)
  write('    \\toprule', f)
  write(paste('    \\textbf{Variables}	& ',
              '\\multicolumn{1}{c}{\\textbf{n (\\%)}} & ', 
              '\\multicolumn{2}{c}{\\textbf{Diabetes}} & ',
              '\\textbf{Chi-square} \\\\', sep = ''), f) 
  write('    \\cline{3-4}', f)
  
  write(paste('    & & \\textbf{', row_names[[1]][2], '} ', 
              '& \\textbf{', row_names[[1]][1], '} ',
              '& \\textbf{\\textit{P} value} \\\\',
              sep = ''), f) 

  # 生成Diabetes的一维列联表（频数）
  tab <- table(x$case)
  # 取出频数
  n <- as.numeric(tab)

  write(paste('    & & \\textbf{(\\textit{N} = ', n[2], ')} ', 
              '& \\textbf{(\\textit{N} = ', n[1], ')} & \\\\',
              sep = ''), f) 
  write('    & & \\textbf{n (\\%)} & \\textbf{n (\\%)} & \\\\', f)
  write('    \\midrule', f)
  
  # 表内容
  for (i in 2:ncol(x)) {
    if (i == 2) {
      write(paste('    \\textbf{Predisposing factors} & & & & \\\\', 
                  sep = ''), f)
    } else if (i == 6) {
      write(paste('    \\textbf{Enabling factors} & & & & \\\\', 
                  sep = ''), f)
    } else if (i == 11) {
      write(paste('    \\textbf{Need factors} & & & & \\\\', 
                  sep = ''), f)
    }
    
    # 生成一维列联表（频数）
    tab1 <- table(x[[i]])
    # 取出频数
    n1 <- as.numeric(tab1)
    # 生成一维列联表（频率）
    n_p1 <- sprintf('%0.2f', as.numeric(prop.table(tab1)) * 100)
    
    # 卡方检验，取出P值
    chi <- chisq.test(x = x[[i]], y = x$case)
    if (chi$p.value < 0.0001) {
      p.value <- '<.0001'
    } else if (chi$p.value < 0.01) {
      p.value <- sprintf('%0.4f', chi$p.value)
    } else {
      p.value <- sprintf('%0.2f', chi$p.value)
    }

    write(paste('    \\quad \\textbf{', tab_names[i], '} & & & & ',
                     p.value, ' \\\\', sep = ''), f)

    # 计算二维列联表的频数表
    tab <- table(x[[i]], x$case)
    # 取出频数
    n <- as.numeric(tab)

    # 计算二维列联表的频率表
    n_p <- sprintf('%0.2f', as.numeric(prop.table(tab, margin = 1)) * 100)

    # 先转化为矩阵
    mat <- matrix(n, ncol = 2)

    # 因为Undiagnosed和Diagnosed倒过来了，所以这里对下标用该方法处理
    l <- length(n) / 2
    for (j in 1:l) {
      write(paste('    \\quad \\quad ', row_names[[i]][j], ' & ',
                  n1[j], ' (', n_p1[j], ') & ', 
                  n[j + l], ' (', n_p[j + l], ') & ',
                  n[j], ' (', n_p[j], ') & \\\\', sep = ''), f)
    }
  }

  # 表尾
  write('    \\bottomrule', f)
  write('    \\end{tabular}', f)
  write('\\end{specialtable}', f)

  # 关闭文件
  close(f)
}
# -----------------------------------------------------------------------------

#' 生成paper中的逻辑斯特回归table
#'
#' @param x 
#' @param tab_names 
#' @param row_names 
#' @param out_file 
#' 
#' @return
logist_tab <- function (x, tab_names, row_names, out_file) {
  # 打开文件
  f <- file(out_file, 'w')
  
  write('\\begin{specialtable}[H]', f)
  write(paste('    \\caption{Variables related to undiagnosed diabetes ', 
              'in multiple logistic regression analysis \\label{tab:log}}', 
              sep = ''), f)
  write('    \\begin{threeparttable}', f)
  write('    \\begin{tabular}{llcc}', f)
  write('    \\toprule', f)
  write(paste('    \\textbf{Variables}	& ', 
              '\\multicolumn{2}{c}{\\textbf{Undiagnosed Diabetes}} & ',
              '\\textbf{\\textit{P} value} \\\\',
              sep = ''), f) 
  write('    \\cline{2-3}', f)
  write('    & \\textbf{OR} & \\textbf{95\\%CI} & \\\\', f)  
  write('    \\midrule', f)
  
  # 逻辑斯特回归
  fit <- glm(case ~ senior + medinsurance + city +
             factor(region) + disease + factor(health),
             family = binomial(link = 'logit'),
             data = x)
  
  v <- vif(fit)
  cat('VIF:', v, '\n')
  cat('max vif:', max(v), '\n')
  
  # 求得OR、95%CI、P值
  # 先取出计算好的table，再转换为data frame
  df <- as.data.frame(logistic.display(fit)$table)

  k <- 1
  for (i in 2:ncol(x)) {
    if (i == 2) {
      write(paste('    \\textbf{Predisposing factors} & & \\\\',
                  sep = ''), f)
    } else if (i == 3) {
      write(paste('    \\textbf{Enabling factors} & & \\\\',
                  sep = ''), f)
    } else if (i == 6) {
      write(paste('    \\textbf{Need factors} & & \\\\',
                  sep = ''), f)
    }

    write(paste('    \\quad \\textbf{', tab_names[i], '} & & \\\\',
                     sep = ''), f)

    # 就是为了计算每个特征的取值个数
    n <- as.numeric(table(x[[i]]))

    for (j in 1:length(n)) {
      # 规范化显示的值
      if (j == 1) {
        or <- 1
        ci <- ''
        p.value <- ''
      } else {
        # OR
        or <- sprintf('%0.2f', df$OR[k])
        # 95%CI
        ci <- paste(sprintf('%0.2f', df$lower95ci[k]),
                    sprintf('%0.2f', df$upper95ci[k]), sep = '-')
        # P值
        if (df$Pr[k] < 0.0001) {
          p.value <- '<.0001'
          or <- paste(or, '$^{**}$', sep ='')
        } else if (df$Pr[k] < 0.01) {
          p.value <- sprintf('%0.4f', df$Pr[k])
          or <- paste(or, '$^{**}$', sep ='')
        } else if (df$Pr[k] < 0.05) {
          p.value <- sprintf('%0.2f', df$Pr[k])
          or <- paste(or, '$^*$', sep ='')
        } else {
          p.value <- sprintf('%0.2f', df$Pr[k])
        }

        k <- k + 1
      }
      write(paste('    \\quad \\quad ', row_names[[i]][j], ' & ',
                       or, ' & ',  ci, ' & ', p.value,
                       ' \\\\', sep = ''), f)
    }
  }

  write('    \\bottomrule', f)
  write('    \\end{tabular}', f)
  
  # 表格脚注
  write('    \\begin{tablenotes}', f)
  write('        \\footnotesize', f)
  write(paste('        \\item[1] Note: ',
              '$^*$\\textit{P} < 0.05; ',
              '$^{**}$\\textit{P} < 0.01. ',
              sep = ''), f)
  write('    \\end{tablenotes}', f)
  
  write('    \\end{threeparttable}', f)
  write('\\end{specialtable}', f)
  
  # 关闭文件
  close(f)
  
  # 返回预测结果，供画Roc曲线用
  return(predict(fit))
}
# ----------------------------------------------------------------------------

#' 画ROC曲线
#'
#' @param x 
#' @param fig_file 
plot_roc <- function(x, fig_file) {
  # 设置EPS
  setEPS(horizontal = FALSE, onefile = FALSE, paper = "special")
  postscript(fig_file)
  
  # Margin of bottom left top right
  par(mar = c(4, 4, 2, 1))
  
  # 计算ROC
  g <- roc(case ~ prob, data = x, direction = "<")
  
  # 画ROC曲线
  plot(g, main = 'ROC Curve for Selected Model',
       print.auc = TRUE, auc.polygon = TRUE, # grid = c(0.1, 0.2), grid.col = c('black', 'black'), 
       max.auc.polygon = TRUE,
       auc.polygon.col = "skyblue", # print.thres = TRUE,
       legacy.axes = TRUE)  # 画成 1 - Specificity
  
  dev.off()
}





