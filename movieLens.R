movies = read.table("movieLens.txt", header = F, sep = "|", quote = "\"")
colnames(movies) = c("ID", "Title", "ReleaseDate", "VideoReleaseDate", "IMDB", "Unknown", )