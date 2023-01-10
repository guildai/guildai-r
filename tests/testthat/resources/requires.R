#| requires:
#|   - file: "mtcars.csv"



# Here is an example for how we can inform `guild run` that an
# R script requires a file.
# E.g., say in our current working directory, there is a file "mtcars.csv"
# from `readr::write_csv(mtcars, "mtcars.csv")`
# by default, `guild run` will not copy this file over to the run directory.
# the front matter directive informs guild that this script op requires the file
# and guild will copy or symlink it into the run directory
# run with `guild run requires.R`

cat(commandArgs(), "\n")
cat(getwd(), "\n")
print(list.files())

writeLines(readLines("mtcars.csv", 3))


# here are two constants defined at the top level of the script.
# guild will only inject the flag value once, on the first time it's encountered.

x = 1
x = 2
print(x)

# guild should never clobber the 2nd x = assignment.
# even if user supplied a flag `guild run requires.R x=4`
stopifnot(x == 2)
