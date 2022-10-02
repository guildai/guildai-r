#| echo: false

i <- 123L
f <- 1.123
s <- "Howdy Guild"
b <- FALSE
cx <- 1+1i

print(guildai:::as_yaml(as.list(environment())))
