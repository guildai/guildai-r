#| echo: false

var <- ""

if(var == "")
  var <- NULL

guildai:::print.yaml(as.list(Sys.getenv(var)))
