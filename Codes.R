library(readxl)
inFile <- "dta.xlsx"
df <- read_xlsx(inFile)

col_1 <- 1
col_2 <- 2
var_1 <- 1
Var_2 <- 2
All_First <- "all"

if(All_First == "all") {
  df_table <- table(df[, c(col_1, col_2)])
} else {
  df_table <- table(df[(df[, col_1] == var_1 | df[, col_1] == var_2), c(col_1, col_2)])
}

property <- df[, col_1]
level_prop <- unique(property)
paste0("The levels of the property column are: ", level_prop[1,])
print("The levels of the property column are: ")
outprint <- level_prop[1, ]
for(i in 2:nrow(level_prop)) {
  outprint <- paste0(outprint, "; ", level_prop[i, ])
}

