library(tidyverse)

options(echo = TRUE)  # If you want to see commands in the output file
args <- commandArgs(trailingOnly = TRUE)
print(args)

print(con=stdout(), "Reading Data")

options(echo=TRUE)
arg = commandArgs(trailingOnly = TRUE)
print(con=stdout(), arg)

if (length(arg) == 1) {me <- basename(arg[1])} else {print(con=stdout(), "Enter file in terminal")}
stopifnot(length(arg) == 1)

ow <- strsplit(me, "\\.")

df <- read.csv(me, header=TRUE, stringsAsFactors=FALSE)


sample <- df %>% select('x','y')

print(con=stdout(), glimpse(sample))

print(con=stdout(), sample %>% sample_n(size = 4))

print(con=stdout(), "Creating Scatter Plot")

plt1 <- ggplot(data = df, mapping = aes(x = x, y = y)) + geom_point() + labs(x = "x", x = "y") + theme(axis.title = element_text(size = 20))


print(con=stdout(), "Creating Linear Model")

regr <- lm(y ~ x, data = df)


print(con=stdout(), regr)

print(con=stdout(), "Calculating r_sqd")

corr_val <- df %>% summarize(correlation = cor(x,y))


print(con=stdout(), corr_val)


plt2 <- ggplot(data = df, mapping = aes(x = x, y = y)) + geom_smooth(method = "lm", se = FALSE) + labs(x = "x", x = "y") + theme(axis.title = element_text(size = 20))


plt3 <- ggplot(data = df, mapping = aes(x = x, y = y)) + geom_point() + geom_smooth(method = "lm", se = FALSE) + labs(x = "x", x = "y") + theme(axis.title = element_text(size = 20))

ggsave(str_c(ow[[1]][1], "_scatter_plot.png"), plt1, width = 12, height = 8)

ggsave(str_c(ow[[1]][1], "_linear_model.png"), plt2, width = 12, height = 8)

ggsave(str_c(ow[[1]][1], "_linear_model_scatter_plot.png"), plt3, width = 12, height = 8)



