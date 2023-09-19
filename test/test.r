library(dplyr)
library(DMwR)
# d1 <- data.frame(
#   x = c(101, 102, 104, 105),
#   z1 = c(1, 3, 5, 5)
# )
# 
# d2 <- data.frame(
#   x = c(101, 102, 103, 104),
#   y = c(1, 1, 2, 2),
#   z2 = c(11, 33, 55, 55)
# )
# 
# d3 <- data.frame(
#   x = c(101, 102, 106, 107, 108),
#   y = c(1, 1, 3, 4, 4),
#   z3 = c(111, 333, 555, 555, 555)
# )
# 
# #print(d1)
# #print(d2)
# # 
# res <- Reduce(
#   f = function(df1, df2)
#     full_join(df1, df2, by = c('x', 'y')),
#   list(d1, d2, d3),
#   accumulate = FALSE
# )
# 
#res1 <- full_join(d1, d2)
#res1 <- full_join(d1, d2, by = c('x', 'y'))
# res1 <- inner_join(d1, d2, by = c('x' = 'y'))
#res1 <- merge(d1, d2)
# res2 <- full_join(d1, d2, by = c('id', 'cid'))
# 
# res <- arrange(res, x)
# print(res)
# print(res2)
# 
# res2 <- res2[-which(res2$cid == 1 | res2$Language == 'Chinese'), ]
# 
# print(res2)


#print(head(merged_data, n = 1))
#print(sapply(merged_data, class))




# df = data.frame(num = c(2, NA, 3, 1, NA, 1))
# 
# x = c('2 b', NA, '3 c', '1 a', NA, '1 a')
# # x = numeric(x)
# df2 = data.frame(num = x)
# #print(df2)
# #print(class(data.frame(df2$num)))
# #a = centralImputation(data.frame(df2$num))
# #a = centralImputation(data.frame(df2, stringsAsFactors = FALSE))
# c = centralImputation(data.frame(df2, stringsAsFactors = FALSE))

# d1 <- data.frame(
#   id = c('1001', '1001', '1002', '1004'),
#   cid = c(1, 1, 2, 4),
#   Language = factor(c('1 Chinese', '2 English', '1 Chinese', '2 English')),
#   score = c(85.5, 76.3, 89.0, 90.8)
# )
# 
x = as.numeric(factor(c('2 b', NA, 'c', '1 a', NA, '1 a')))
print(x)
# print(sapply(d1, class))
# out1 <- as.data.frame(sapply(d1, as.numeric))
# print(out1)
# #print(class(out1))
# print(sapply(out1, class))

# x<-data.frame(a = c(1, 4, NA, NA, 2, 1, NA),
#               b = c('a', 'b', 'c', 'd', 'e', 'f', 'g') )
# x <- x[x$a == 1,]
# print(x)

# 计算并输出频数表
# freq <- apply(var_i, MARGIN = 2, table)  # list，包含table
# print(freq_i)
# cat('=======================================================================\n')
#
# # 输出频率表
# print(lapply(freq, prop.table))  # prop.table()作用于table
# cat('=======================================================================\n')
