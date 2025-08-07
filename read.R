pdf("A.pdf", width = 24, height = 24)
par(mar = c(5.1, 7, 4.1, 2.1))
df <- read.table("20250806-212445-0212BD-SF.txt")
df2 <- read.table("extradata.txt")
names(df) <- c("pass", "chip", "info", "date", "time", "info1", "info2")
names(df2) <- c("pass", "chip", "info", "date", "time", "info1", "info2")
df$type <- "chip_read"
df2$type <- "manual"
df <- rbind(df, df2)
df$type <- c("black", "grey")[as.numeric(as.factor(df$type))]
df$datetime <- as.POSIXlt(paste(df$date, df$time), format = "%Y-%m-%d %H:%M:%OS")
cutoff1 <- as.POSIXlt("2025-08-06 19:03:00.000", format = "%Y-%m-%d %H:%M:%OS")
cutoff2 <- as.POSIXlt("2025-08-06 19:43:00.000", format = "%Y-%m-%d %H:%M:%OS")
df2 <- df[df$datetime > cutoff1 & df$datetime < cutoff2, ]
df2 <- df2[order(df2$datetime), ]
df2$chip <- as.factor(df2$chip)
plot(y = df2$chip, x = df2$datetime,
     type = "p", pch = 20, yaxt = "n",
     ylab = "", xlab = "", xaxt = "n",
     main = "A grupp", col = df2$type, cex = 2)
labsy <- levels(df2$chip)
axis(side = 2, at = sort(as.numeric(unique(df2$chip))),
     labels = labsy, las = 1)
labsx <- seq(from = cutoff1,
             to = cutoff2,
             length.out = 41)
labsx <- format(as.POSIXlt(labsx), "%H:%M:%S")
axis(side = 1, at = seq(from = cutoff1,
                        to = cutoff2,
                        length.out = 41), las = 2, labels = labsx)
dev.off()

pdf("C_ungdom_damer.pdf", width = 24, height = 24)
par(mar = c(5.1, 7, 4.1, 2.1))
df <- read.table("20250806-212445-0212BD-SF.txt")
df2 <- read.table("extradata.txt")
names(df) <- c("pass", "chip", "info", "date", "time", "info1", "info2")
names(df2) <- c("pass", "chip", "info", "date", "time", "info1", "info2")
df$type <- "chip_read"
df2$type <- "manual"
df <- rbind(df, df2)
df$type <- c("black", "grey")[as.numeric(as.factor(df$type))]
df$datetime <- as.POSIXlt(paste(df$date, df$time), format = "%Y-%m-%d %H:%M:%OS")
cutoff1 <- as.POSIXlt("2025-08-06 19:47:00.000", format = "%Y-%m-%d %H:%M:%OS")
cutoff2 <- as.POSIXlt("2025-08-06 20:27:00.000", format = "%Y-%m-%d %H:%M:%OS")
df2 <- df[df$datetime > cutoff1 & df$datetime < cutoff2, ]
df2 <- df2[order(df2$datetime), ]
df2$chip <- as.factor(df2$chip)
plot(y = df2$chip, x = df2$datetime,
     type = "p", pch = 20, yaxt = "n",
     ylab = "", xlab = "", xaxt = "n",
     main = "C, ungdom och damer", col = df2$type, cex = 2)
labsy <- levels(df2$chip)
axis(side = 2, at = sort(as.numeric(unique(df2$chip))),
     labels = labsy, las = 1)
labsx <- seq(from = cutoff1,
             to = cutoff2,
             length.out = 41)
labsx <- format(as.POSIXlt(labsx), "%H:%M:%S")
axis(side = 1, at = seq(from = cutoff1,
                        to = cutoff2,
                        length.out = 41), las = 2, labels = labsx)
dev.off()

pdf("B.pdf", width = 24, height = 24)
par(mar = c(5.1, 7, 4.1, 2.1))
df <- read.table("20250806-212445-0212BD-SF.txt")
df2 <- read.table("extradata.txt")
names(df) <- c("pass", "chip", "info", "date", "time", "info1", "info2")
names(df2) <- c("pass", "chip", "info", "date", "time", "info1", "info2")
df$type <- "chip_read"
df2$type <- "manual"
df <- rbind(df, df2)
df$type <- c("black", "grey")[as.numeric(as.factor(df$type))]
df$datetime <- as.POSIXlt(paste(df$date, df$time), format = "%Y-%m-%d %H:%M:%OS")
cutoff1 <- as.POSIXlt("2025-08-06 20:32:00.000", format = "%Y-%m-%d %H:%M:%OS")
cutoff2 <- as.POSIXlt("2025-08-06 21:14:00.000", format = "%Y-%m-%d %H:%M:%OS")
df2 <- df[df$datetime > cutoff1 & df$datetime < cutoff2, ]
df2 <- df2[order(df2$datetime), ]
df2$chip <- as.factor(df2$chip)
plot(y = df2$chip, x = df2$datetime,
     type = "p", pch = 20, yaxt = "n",
     ylab = "", xlab = "", xaxt = "n",
     main = "B gruppen", col = df2$type, cex = 2)
labsy <- levels(df2$chip)
axis(side = 2, at = sort(as.numeric(unique(df2$chip))),
     labels = labsy, las = 1)
labsx <- seq(from = cutoff1,
             to = cutoff2,
             length.out = 43)
labsx <- format(as.POSIXlt(labsx), "%H:%M:%S")
times <- seq(from = cutoff1,
             to = cutoff2,
             length.out = 43)
axis(side = 1, at = times, las = 2, labels = labsx)
abline(v = times)
dev.off()
