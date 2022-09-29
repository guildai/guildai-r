


library(tensorflow)

writer = tf$summary$create_file_writer("a-events-file2")

with writer.as_default():
  for step in range(100):
    # other model code would go here
    tf.summary.scalar("my_metric", 0.5, step=step)


tf$summary$create_
