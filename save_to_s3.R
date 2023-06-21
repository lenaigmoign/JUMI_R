# A function to put data from local machine to S3
put_to_s3 <- function(from, to) {
  aws.s3::put_object(
    file = from,
    object = to,
    bucket = "fbedecarrats",
    region = "",
    multipart = TRUE)
}

data_files <- list.files("data", recursive = TRUE)

dest_files <- paste0("fbedecarrats/diffusion/JUMI")

map2(data_files, dest_files, put_to_s3)