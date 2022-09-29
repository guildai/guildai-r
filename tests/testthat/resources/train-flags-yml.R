#| flags-dest: config:flags.yml
#| echo: false

#Sys.getenv()

cat(commandArgs(), "\n")
cat(getwd(), "\n")

config <- guildai::read_yaml("flags.yml")
print(config)

x = 1
x = 2
print(x)
