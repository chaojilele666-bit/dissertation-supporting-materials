data_path <- "Appendix_A_Coded_Review_Dataset_2000.csv"
out_dir <- "."

df <- read.csv(data_path, stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM")

# Basic descriptive statistics
region_counts <- table(df$Region)
platform_counts <- table(df$Platform)
valence_counts <- table(df$Trust.Valence)
theme_counts <- table(df$Trust.Theme)
touchpoint_counts <- sort(table(df$AI.Digital.Touchpoint), decreasing = TRUE)

print(region_counts)
print(platform_counts)
print(valence_counts)
print(theme_counts)
print(head(touchpoint_counts, 15))

# Cross-tabulations
region_valence <- table(df$Region, df$Trust.Valence)
region_theme <- table(df$Region, df$Trust.Theme)
touchpoint_valence <- table(df$AI.Digital.Touchpoint, df$Trust.Valence)
platform_valence <- table(df$Platform, df$Trust.Valence)

print(region_valence)
print(region_theme)
print(head(touchpoint_valence, 15))
print(platform_valence)

# Chi-square tests
test_region_valence <- chisq.test(region_valence)
test_region_theme <- chisq.test(region_theme)
test_touchpoint_valence <- chisq.test(touchpoint_valence)
test_platform_valence <- chisq.test(platform_valence)

print(test_region_valence)
print(test_region_theme)
print(test_touchpoint_valence)
print(test_platform_valence)

# Export simple CSV tables
write.csv(as.data.frame(region_valence), file.path(out_dir, "r_region_valence.csv"), row.names = FALSE)
write.csv(as.data.frame(region_theme), file.path(out_dir, "r_region_theme.csv"), row.names = FALSE)
write.csv(as.data.frame(touchpoint_valence), file.path(out_dir, "r_touchpoint_valence.csv"), row.names = FALSE)
write.csv(as.data.frame(platform_valence), file.path(out_dir, "r_platform_valence.csv"), row.names = FALSE)
