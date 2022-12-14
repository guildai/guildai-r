
stop("bar")
position <- c(0)
for(i in 1:1) {
  position <- position + sample(c(1, -1), 1)
  cat("step:", i, "\n")
  cat("position:", position, "\n")
}

fn <- function() stop("foo")
if(TRUE) if(TRUE) if(TRUE)
fn()


