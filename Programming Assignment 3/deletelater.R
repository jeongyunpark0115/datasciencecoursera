list.dfs <- list(df1 = data.frame(var1 = c(1:3), var2 = c(1:3), var3 = c(1:3)), 
                 df2 = data.frame(var1= c(1:3), var2 = c(1:3), var3 = c(1:3)), 
                 df3 = data.frame(var1= c(1:3), var2 = c(1:3), var3 = c(3:1)))

list.dfs[[3]][order(list.dfs[[3]][[3]]), ]

list.dfs[[3]]
list.dfs[[3]][[3]]
list.dfs[[3]][order(list.dfs[[3]]$var3), ]
list.dfs[[3]][order(list.dfs[[3]][[3]])]

lapply(list.dfs, function(x) x[order(x$var3), ])




lapply(whole_list, fun, argument)




# testing


for (i in hospitals) {
  for (j in i) {
    print(j)
  }
}
