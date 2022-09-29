


library(tensorflow)

writer = tf$summary$create_file_writer("a-events-file2")

with(writer$as_default(), {

  for(step in seq(10))
    tf$summary$scalar("my_metric", 0.5, step=step)
    # other model code would go here
})


