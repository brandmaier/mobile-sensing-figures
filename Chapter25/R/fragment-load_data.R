path <- "~/Downloads/wisdm-dataset/raw/watch/accel/data_1600_accel_watch.txt"
dat <- readr::read_csv(path)
names(dat) <- c("Participant","Activity","Timestamp","X","Y","Z")

head(dat)

path <- "~/Downloads/wisdm-dataset/raw/phone/accel/data_1600_accel_phone.txt"
dat2 <- readr::read_csv(path)
names(dat2) <- c("Participant","Activity","Timestamp","X","Y","Z")

head(dat)