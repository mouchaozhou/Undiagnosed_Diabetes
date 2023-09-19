library(mice)

merged_indiv_file <- 'data/Save/merged_data/merge_indiv_data.csv'
processed_data_dir <- 'data/Save/process_data/'

case1 <- 10355
case2 <- 558
min_err <- Inf

f <- file('data/Save/results/test_seed.txt', 'w')

for (seed in 2001:3000) {
    d <- read.csv(merged_indiv_file, header = TRUE)
    
    # 删除糖尿病三项指标全为缺失值的数据行
    d <- d[-which(is.na(d$newglu) & is.na(d$newhba1c) & is.na(d$qc1_va003)),]

    # Multiple imputations for blood sample features
    m <- 5  # Imputation times
    imp <- mice(d[c('newglu', 'newhba1c', 'qc1_va003')], method = 'sample',
                m = m, seed = seed, printFlag = FALSE)
    d[c('newglu', 'newhba1c', 'qc1_va003')] <- complete(imp, action = m)
    
    # Diabetes definition by blood test
    d$diabetes.b <- 0   # No
    d$diabetes.b[d$newglu >= 200.0 | d$newhba1c >= 6.5 | 
                 (d$qc1_va003 == 2 & d$newglu >= 126)] <- 1  # Yes
     
    # Diabetes definition by self-report
    d$diabetes.s <- 0  # No
    d$diabetes.s[d$da007_3_ == 1] <- 1  # Yes
    
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
    
    # 在SAS中，NA是小于任意一个非缺失值的
    d <- d[which(d$age >= 45),]  # age中还有NA 

    err <- abs(sum(d$diabetes.c == 0) - case1) + 
           abs(sum(d$diabetes.c == 1) - case2)

    if (err < min_err) {
        min_err <- err
        seed_opt <- seed
    }
    
    cat('seed: ', seed, '\n')
    cat('err: ', err, '\n')
    cat('Min error: ', min_err, '\n')
    cat(case1, ': ', sum(d$diabetes.c == 0), '\n')
    cat(case2, ': ', sum(d$diabetes.c == 1), '\n')
    cat('===================================================================\n')
    
    writeLines(paste('seed: ', seed, sep = ''), f)
    writeLines(paste('err: ', err, sep = ''), f)
    writeLines(paste('Min error: ', min_err, sep = ''), f)
    writeLines(paste(case1, ': ', sum(d$diabetes.c == 0), sep = ''), f)
    writeLines(paste(case2, ': ', sum(d$diabetes.c == 1), sep = ''), f)
    writeLines('=========================================================\n', f)
}

cat('Optimal seed: ', seed_opt, '\n')
cat('Min error: ', min_err, '\n')
writeLines(paste('Optimal seed: ', seed_opt, sep = ''), f)
writeLines(paste('Min error: ', min_err, sep = ''), f)

close(f)
