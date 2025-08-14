library(readxl)

## Read the startlist
startlist <- readxl::read_xlsx("startlist.xlsx")

## Read the results and som manual entries
df <- read.table("20250806-212445-0212BD-SF.txt")
df2 <- read.table("extradata.txt")
names(df) <- c("pass", "chip", "info", "date", "time", "info1", "info2")
names(df2) <- c("pass", "chip", "info", "date", "time", "info1", "info2")
df$type <- "chip_read"
df2$type <- "manual"
df <- rbind(df, df2)
df$type <- c("black", "grey10")[as.numeric(as.factor(df$type))]
df$pch <- c(20, 4)[as.numeric(as.factor(df$type))]
df$datetime <- as.POSIXlt(paste(df$date, df$time), format = "%Y-%m-%d %H:%M:%OS")

## Add the name and class to the results
df$name <- startlist$Namn[match(df$chip, startlist$ChipNr)]
df$class <- startlist$Klass[match(df$chip, startlist$ChipNr)]

## limit to just today
df <- df[df$datetime >= as.POSIXlt("2025-08-06 19:03:00.000", format = "%Y-%m-%d %H:%M:%OS"), ]

## Check for anyone with no Chip times:
startlist[!startlist$ChipNr %in% df$chip, ]
## We know that Henrik Holm started and was first in B

## Graph the results of A
svg("images/A.svg", width = 24, height = 18)
par(mar = c(5.1, 20, 4.1, 2.1))
cutoff1 <- as.POSIXlt("2025-08-06 19:03:00.000", format = "%Y-%m-%d %H:%M:%OS")
cutoff2 <- as.POSIXlt("2025-08-06 19:43:00.000", format = "%Y-%m-%d %H:%M:%OS")
df2 <- df[df$datetime > cutoff1 & df$datetime < cutoff2, ]
df2 <- df2[order(df2$chip, df2$datetime), ]
## Didn't start?
startlist[startlist$Klass == "A" & (!startlist$ChipNr %in% df2$chip), ]
##
## Add the lap number
df2$lap <- as.numeric(ave(df2$datetime, df2$chip, FUN = rank))
df2$placing <- as.numeric(ave(df2$datetime, df2$lap, FUN = rank))
## Drop laps after finish
df2 <- df2[df2$lap <= 23, ]
## Order by rank at last lap
finishrank <- NULL
for (i in rev(unique(df2$lap))) {
    dfinner <- df2[df2$lap == i, ]
    finishers <- as.character(dfinner$chip[order(dfinner$placing)])
    finishrank <- c(finishrank, finishers[!(finishers %in% finishrank)])
}
df2$chip <- factor(df2$chip, levels = rev(finishrank))
##
plot(y = df2$chip, x = df2$datetime,
     type = "p", pch = df2$pch, yaxt = "n",
     ylab = "", xlab = "", xaxt = "n",
     main = "A grupp - Vinnare 23 varv", col = df2$type, cex = 2)
labsy <- levels(df2$chip)
name <- startlist$Namn[match(labsy, startlist$ChipNr)]
class <- startlist$Klass[match(labsy, startlist$ChipNr)]
axis(side = 2, at = sort(as.numeric(unique(df2$chip))),
     labels = paste0(name, "(", class, ")", ", ", labsy), las = 1)
labsx <- seq(from = cutoff1,
             to = cutoff2,
             length.out = 41)
labsx <- format(as.POSIXlt(labsx), "%H:%M:%S")
times <- seq(from = cutoff1,
             to = cutoff2,
             length.out = 41)
axis(side = 1, at = times, las = 2, labels = labsx)
abline(v = times, col = "grey90")
## Add lap number and rank
text(x = df2$datetime,
     y = as.numeric(df2$chip) + 0.2,
     labels = paste0("varv: ", df2$lap), cex = 0.75)
text(x = df2$datetime,
     y = as.numeric(df2$chip) - 0.2,
     labels = paste0("plats: ", df2$placing), cex = 0.75)
dev.off()

svg("images/Damer.svg", width = 24, height = 12)
par(mar = c(5.1, 20, 4.1, 2.1))
cutoff1 <- as.POSIXlt("2025-08-06 19:47:00.000", format = "%Y-%m-%d %H:%M:%OS")
cutoff2 <- as.POSIXlt("2025-08-06 20:27:00.000", format = "%Y-%m-%d %H:%M:%OS")
df2 <- df[df$datetime > cutoff1 & df$datetime < cutoff2 & df$class == "Damer", ]
df2 <- df2[order(df2$chip, df2$datetime), ]
## Didn't start?
startlist[startlist$Klass == "Damer" & (!startlist$ChipNr %in% df2$chip), ]
##
## Add the lap number
df2$lap <- as.numeric(ave(df2$datetime, df2$chip, FUN = rank))
df2$placing <- as.numeric(ave(df2$datetime, df2$lap, FUN = rank))
## Drop laps after finish
df2 <- df2[df2$lap <= 19, ]
## Order by rank at last lap
finishrank <- NULL
for (i in rev(unique(df2$lap))) {
    dfinner <- df2[df2$lap == i, ]
    finishers <- as.character(dfinner$chip[order(dfinner$placing)])
    finishrank <- c(finishrank, finishers[!(finishers %in% finishrank)])
}
df2$chip <- factor(df2$chip, levels = rev(finishrank))
##
plot(y = df2$chip, x = df2$datetime,
     type = "p", pch = df2$pch, yaxt = "n",
     ylab = "", xlab = "", xaxt = "n",
     main = "Damer - Målgång 19 varv", col = df2$type, cex = 2)
labsy <- levels(df2$chip)
name <- startlist$Namn[match(labsy, startlist$ChipNr)]
class <- startlist$Klass[match(labsy, startlist$ChipNr)]
axis(side = 2, at = sort(as.numeric(unique(df2$chip))),
     labels = paste0(name, "(", class, ")", ", ", labsy), las = 1)
labsx <- seq(from = cutoff1,
             to = cutoff2,
             length.out = 41)
labsx <- format(as.POSIXlt(labsx), "%H:%M:%S")
times <- seq(from = cutoff1,
             to = cutoff2,
             length.out = 41)
axis(side = 1, at = times, las = 2, labels = labsx)
abline(v = times, col = "grey90")
## Add lap number and rank
text(x = df2$datetime,
     y = as.numeric(df2$chip) + 0.1,
     labels = paste0("varv: ", df2$lap), cex = 0.75)
text(x = df2$datetime,
     y = as.numeric(df2$chip) - 0.1,
     labels = paste0("plats: ", df2$placing), cex = 0.75)
dev.off()

svg("images/Ungdomar.svg", width = 24, height = 6)
par(mar = c(5.1, 20, 4.1, 2.1))
cutoff1 <- as.POSIXlt("2025-08-06 19:47:00.000", format = "%Y-%m-%d %H:%M:%OS")
cutoff2 <- as.POSIXlt("2025-08-06 20:27:00.000", format = "%Y-%m-%d %H:%M:%OS")
df2 <- df[df$datetime > cutoff1 & df$datetime < cutoff2 & df$class == "Ungdom", ]
df2 <- df2[order(df2$chip, df2$datetime), ]
## Didn't start?
startlist[startlist$Klass == "Ungdom" & (!startlist$ChipNr %in% df2$chip), ]
##
## Add the lap number
df2$lap <- as.numeric(ave(df2$datetime, df2$chip, FUN = rank))
df2$placing <- as.numeric(ave(df2$datetime, df2$lap, FUN = rank))
## Drop laps after finish
df2 <- df2[df2$lap <= 19, ]
## Order by rank at last lap
finishrank <- NULL
for (i in rev(unique(df2$lap))) {
    dfinner <- df2[df2$lap == i, ]
    finishers <- as.character(dfinner$chip[order(dfinner$placing)])
    finishrank <- c(finishrank, finishers[!(finishers %in% finishrank)])
}
df2$chip <- factor(df2$chip, levels = rev(finishrank))
##
plot(y = df2$chip, x = df2$datetime,
     type = "p", pch = df2$pch, yaxt = "n",
     ylab = "", xlab = "", xaxt = "n",
     main = "Ungdom - Målgång 19 varv", col = df2$type, cex = 2)
labsy <- levels(df2$chip)
name <- startlist$Namn[match(labsy, startlist$ChipNr)]
class <- startlist$Klass[match(labsy, startlist$ChipNr)]
axis(side = 2, at = sort(as.numeric(unique(df2$chip))),
     labels = paste0(name, "(", class, ")", ", ", labsy), las = 1)
labsx <- seq(from = cutoff1,
             to = cutoff2,
             length.out = 41)
labsx <- format(as.POSIXlt(labsx), "%H:%M:%S")
times <- seq(from = cutoff1,
             to = cutoff2,
             length.out = 41)
axis(side = 1, at = times, las = 2, labels = labsx)
abline(v = times, col = "grey90")
## Add lap number and rank
text(x = df2$datetime,
     y = as.numeric(df2$chip) + 0.05,
     labels = paste0("varv: ", df2$lap), cex = 0.75)
text(x = df2$datetime,
     y = as.numeric(df2$chip) - 0.05,
     labels = paste0("plats: ", df2$placing), cex = 0.75)
dev.off()

svg("images/C.svg", width = 24, height = 12)
par(mar = c(5.1, 20, 4.1, 2.1))
cutoff1 <- as.POSIXlt("2025-08-06 19:47:00.000", format = "%Y-%m-%d %H:%M:%OS")
cutoff2 <- as.POSIXlt("2025-08-06 20:27:00.000", format = "%Y-%m-%d %H:%M:%OS")
df2 <- df[df$datetime > cutoff1 & df$datetime < cutoff2 & df$class == "C", ]
df2 <- df2[order(df2$chip, df2$datetime), ]
## Didn't start?
startlist[startlist$Klass == "C" & (!startlist$ChipNr %in% df2$chip), ]
##
## Add the lap number
df2$lap <- as.numeric(ave(df2$datetime, df2$chip, FUN = rank))
df2$placing <- as.numeric(ave(df2$datetime, df2$lap, FUN = rank))
## Drop laps after finish
df2 <- df2[df2$lap <= 19, ]
## Order by rank at last lap
finishrank <- NULL
for (i in rev(unique(df2$lap))) {
    dfinner <- df2[df2$lap == i, ]
    finishers <- as.character(dfinner$chip[order(dfinner$placing)])
    finishrank <- c(finishrank, finishers[!(finishers %in% finishrank)])
}
df2$chip <- factor(df2$chip, levels = rev(finishrank))
##
plot(y = df2$chip, x = df2$datetime,
     type = "p", pch = df2$pch, yaxt = "n",
     ylab = "", xlab = "", xaxt = "n",
     main = "C - Målgång 19 varv", col = df2$type, cex = 2)
labsy <- levels(df2$chip)
name <- startlist$Namn[match(labsy, startlist$ChipNr)]
class <- startlist$Klass[match(labsy, startlist$ChipNr)]
axis(side = 2, at = sort(as.numeric(unique(df2$chip))),
     labels = paste0(name, "(", class, ")", ", ", labsy), las = 1)
labsx <- seq(from = cutoff1,
             to = cutoff2,
             length.out = 41)
labsx <- format(as.POSIXlt(labsx), "%H:%M:%S")
times <- seq(from = cutoff1,
             to = cutoff2,
             length.out = 41)
axis(side = 1, at = times, las = 2, labels = labsx)
abline(v = times, col = "grey90")
## Add lap number and rank
text(x = df2$datetime,
     y = as.numeric(df2$chip) + 0.2,
     labels = paste0("varv: ", df2$lap), cex = 0.75)
text(x = df2$datetime,
     y = as.numeric(df2$chip) - 0.2,
     labels = paste0("plats: ", df2$placing), cex = 0.75)
dev.off()

svg("images/B.svg", width = 24, height = 18)
par(mar = c(5.1, 20, 4.1, 2.1))
cutoff1 <- as.POSIXlt("2025-08-06 20:32:00.000", format = "%Y-%m-%d %H:%M:%OS")
cutoff2 <- as.POSIXlt("2025-08-06 21:14:00.000", format = "%Y-%m-%d %H:%M:%OS")
df2 <- df[df$datetime > cutoff1 & df$datetime < cutoff2 & df$class == "B", ]
df2 <- df2[order(df2$chip, df2$datetime), ]
## Didn't start?
startlist[startlist$Klass == "B" & (!startlist$ChipNr %in% df2$chip), ]
##
## Add the lap number
df2$lap <- as.numeric(ave(df2$datetime, df2$chip, FUN = rank))
df2$placing <- as.numeric(ave(df2$datetime, df2$lap, FUN = rank))
## Drop laps after finish
df2 <- df2[df2$lap <= 23, ]
## Order by rank at last lap
finishrank <- NULL
for (i in rev(unique(df2$lap))) {
    dfinner <- df2[df2$lap == i, ]
    finishers <- as.character(dfinner$chip[order(dfinner$placing)])
    finishrank <- c(finishrank, finishers[!(finishers %in% finishrank)])
}
df2$chip <- factor(df2$chip, levels = rev(finishrank))
##
plot(y = df2$chip, x = df2$datetime,
     type = "p", pch = df2$pch, yaxt = "n",
     ylab = "", xlab = "", xaxt = "n",
     main = "B - Målgång 23 varv", col = df2$type, cex = 1.5)
labsy <- levels(df2$chip)
name <- startlist$Namn[match(labsy, startlist$ChipNr)]
class <- startlist$Klass[match(labsy, startlist$ChipNr)]
axis(side = 2, at = sort(as.numeric(unique(df2$chip))),
     labels = paste0(name, "(", class, ")", ", ", labsy), las = 1)
labsx <- seq(from = cutoff1,
             to = cutoff2,
             length.out = 43)
labsx <- format(as.POSIXlt(labsx), "%H:%M:%S")
times <- seq(from = cutoff1,
             to = cutoff2,
             length.out = 43)
axis(side = 1, at = times, las = 2, labels = labsx)
abline(v = times, col = "grey90")
## Add lap number and rank
text(x = df2$datetime,
     y = as.numeric(df2$chip) + 0.2,
     labels = paste0("varv: ", df2$lap), cex = 0.75)
text(x = df2$datetime,
     y = as.numeric(df2$chip) - 0.2,
     labels = paste0("plats: ", df2$placing), cex = 0.75)
dev.off()
