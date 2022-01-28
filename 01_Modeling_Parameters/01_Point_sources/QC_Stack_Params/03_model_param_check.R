
library(measurements)

#-----------------------------------------------------#
# Reasonable input range in Kelvin, meters, and m/s
#-----------------------------------------------------#
input_range_metric <- data.frame(temp_min     = 280,
                                 temp_max     = 1200,

                                 height_min   = 1,
                                 height_max   = 200,

                                 diameter_min = 0.1,
                                 diameter_max = 15,

                                 velocity_min = 0.001,
                                 velocity_max = 40,
                                 stringsAsFactors = FALSE)


input_range <- mutate(input_range_metric,
                      temp_min   = conv_unit(temp_min, "K", "F"),
                      temp_max   = conv_unit(temp_max, "K", "F"),

                      height_min = conv_unit(height_min, "m", "ft"),
                      height_max = conv_unit(height_max, "m", "ft"),

                      diameter_min = conv_unit(diameter_min, "m", "ft"),
                      diameter_max = conv_unit(diameter_max, "m", "ft"),

                      velocity_min = conv_unit(velocity_min, "m_per_sec", "ft_per_sec"),
                      velocity_max = conv_unit(velocity_max, "m_per_sec", "ft_per_sec")
                      )


#write.csv(input_range, "acceptable_input_range_metric.csv", quote = T, row.names = F)


value_check <- function(value, min_cut, max_cut) {

  out <- min(value, max_cut)

  out <- max(out, min_cut)

  return(out)
}


check_input <- function(value = 1000, input_name = "height", metric = TRUE) {

  inputs <- input_range_metric

  if(!metric) inputs <- input_range

  out <- value_check(value,
                     inputs[ , paste0(input_name, "_min")][[1]],
                     inputs[ , paste0(input_name, "_max")][[1]])

  return(out)
}

