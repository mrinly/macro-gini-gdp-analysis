library(tidyverse)
library(modelsummary)
library(readxl)
library(ggplot2)
library(corrplot)
library(fixest)

data <- read_excel("C:/Users/arina/Downloads/Telegram Desktop/gini/data_growth.xlsx", sheet = "data")

#building a panel data table

panel <- pivot_longer(data, cols = !Country,
                     names_to = c(".value", "year"),
                     names_pattern = "(.*)_(.*)")
names(panel) <- c('country', 'year', 'gdp', 'gini', 'urban', 'law', 'educ', 'growth', 'oecd')
panel$ln_gdp <- log(panel$gdp)
panel <- panel %>%
  mutate(year = factor(year, levels = c("1980", "1995", "2010")))


#main statistics table

datasummary((`GDP growth (annual)` = growth) +
              (`Log GDP per capita` = ln_gdp) +      
              (`Gini coefficient (0-1)` = gini) + 
              (`Urbanization rate (share)` = urban) + 
              (`Education enrollment (share)` = educ) + 
              (`Rule of Law (0-1)` = law) ~
              Mean + SD + Min + Max + N,
            data = panel,
            fmt = 3,
            title = "Table 2. Descriptive statistics",
            notes = c(
              "GDP growth: geometric average annual growth rate (dependent variable)",
              "All other variables measured at start of 15-year period",
              "Variables in shares (0-1) unless otherwise noted"
            ))

#Table 3A: Annual averages
datasummary(Factor(year) ~ (growth + ln_gdp + gini + urban + educ + law) * Mean,
            data = panel,
            fmt = 3,
            title = "Table 3a. Statistics by year, Mean")
#Table 3B: Standard deviations by year 
datasummary(Factor(year) ~ (growth + ln_gdp + gini + urban + educ + law) * SD,
            data = panel,
            fmt = 3,
            title = "Table 3b. Statistics by year, SD")


#Determine stile

year_colors <- c("1980" = "#FF7F50", "1995" = "#EE6A50", "2010" = "#8B3E2F")

my_theme <- function() {
  theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      legend.title = element_text(size = 11, face = "bold"),
      legend.text = element_text(size = 10),
      legend.position = "bottom",
      panel.grid.major = element_line(color = "gray90", linewidth = 0.2),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "gray80", fill = NA, linewidth = 0.5)
    )
}


# Inequality and Growth Dotplot
dotplot <- ggplot(panel, aes(x = gini, y = growth)) +
  geom_point(aes(color = year), size = 1.5, alpha = 0.6) +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2),
              se = FALSE, color = "black", size = 1) +
  facet_wrap(~ year, ncol = 3) +
  scale_color_manual(values = year_colors, name = "Year") +
  labs(
    title = "Inequality and Economic Growth",
    x = "Gini",
    y = "GDP per capita Growth Rate"
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  my_theme() +
  theme(legend.position = "none")
print(dotplot)
ggsave("C:/Users/arina/Downloads/Telegram Desktop/gini/dotplot_gini.png", dotplot)


# the correlation matrix
cor_vars_oecd <- panel %>% 
  select(growth, ln_gdp, gini, urban, educ, law, oecd)

names(cor_vars_oecd) <- c(
  "GDP_Growth", 
  "Log_GDP", 
  "Gini", 
  "Urbanization", 
  "Education", 
  "Rule_of_Law",
  "OECD"
)

cor_matrix_oecd <- cor(cor_vars_oecd, use = "complete.obs")

corrplot(cor_matrix_oecd,
         method = "color",
         type = "upper",
         tl.cex = 0.9,
         tl.col = "black",
         addCoef.col = "black",
         number.cex = 0.8,
         number.digits = 2,
         title = "Correlation Matrix (including OECD dummy)",
         mar = c(0, 0, 2, 0),
         col = colorRampPalette(c("#8B3E2F", "white", "#FF7F50"))(200))


# Coefficients of variation
cv_table <- panel %>%
  summarise(
    `GDP Growth` = ifelse(abs(mean(growth, na.rm = TRUE)) > 0.001, 
                          sd(growth, na.rm = TRUE) / mean(growth, na.rm = TRUE), 
                          NA),
    `Log GDP` = sd(ln_gdp, na.rm = TRUE) / mean(ln_gdp, na.rm = TRUE),
    Gini = sd(gini, na.rm = TRUE) / mean(gini, na.rm = TRUE),
    Urbanization = sd(urban, na.rm = TRUE) / mean(urban, na.rm = TRUE),
    Education = sd(educ, na.rm = TRUE) / mean(educ, na.rm = TRUE),
    `Rule of Law` = sd(law, na.rm = TRUE) / mean(law, na.rm = TRUE)
  ) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "CV")

datasummary_df(
  panel %>%
    summarise(
      `GDP Growth` = mean(growth, na.rm = TRUE),
      `Log GDP` = mean(ln_gdp, na.rm = TRUE),
      Gini = mean(gini, na.rm = TRUE),
      Urbanization = mean(urban, na.rm = TRUE),
      Education = mean(educ, na.rm = TRUE),
      `Rule of Law` = mean(law, na.rm = TRUE)
    ) %>%
    pivot_longer(everything(), names_to = "Variable", values_to = "Mean") %>%
    left_join(
      panel %>%
        summarise(
          `GDP Growth` = sd(growth, na.rm = TRUE),
          `Log GDP` = sd(ln_gdp, na.rm = TRUE),
          Gini = sd(gini, na.rm = TRUE),
          Urbanization = sd(urban, na.rm = TRUE),
          Education = sd(educ, na.rm = TRUE),
          `Rule of Law` = sd(law, na.rm = TRUE)
        ) %>%
        pivot_longer(everything(), names_to = "Variable", values_to = "SD"),
      by = "Variable"
    ) %>%
    left_join(cv_table, by = "Variable") %>%
    select(Variable, Mean, SD, CV) %>%
    mutate(
      Mean = ifelse(Variable == "GDP Growth", Mean * 100, Mean),
      SD = ifelse(Variable == "GDP Growth", SD * 100, SD)
    ),
  fmt = "%.3f",
  title = "Table 5. Descriptive statistics with coefficients of variation",
  notes = "GDP Growth expressed in percentage points for readability"
)

# OECD, data
oecd_stats <- panel %>%
  group_by(year) %>%
  summarise(
    OECD = sum(oecd == 1, na.rm = TRUE),
    Total = n(),
    Percent = round(OECD/Total*100, 1)
  )
print(oecd_stats)

#growth rate distribution
growth <- ggplot(data = panel, aes(x = growth, group = year, fill = year)) +
  geom_density(alpha = 0.5, bins = 30) + 
  scale_fill_manual(
    values = c("1980" = "#FF7F50", "1995" = "#EE6A50", "2010" = "#8B3E2F"),
    name = "Year"
  ) +
  labs(
    title = "Density function", 
    x = "GDP per capita growth rate", 
    y = "Density"
  ) + 
  theme_minimal()
print(growth)
ggsave("C:/Users/arina/Downloads/Telegram Desktop/gini/gdp_growth.png", growth)


#gini distribution
gini <- ggplot(data = panel, aes(x = gini, group = year, fill = year)) +
  geom_density(alpha = 0.5) + 
  scale_fill_manual(
    values = c("1980" = "#FF7F50", "1995" = "#EE6A50", "2010" = "#8B3E2F"),
    name = "Year"
  ) +
  labs(
    title = "Density function", 
    x = "Gini", 
    y = "Density"
  ) + 
  theme_minimal()
print(gini)
ggsave("C:/Users/arina/Downloads/Telegram Desktop/gini/gini.png", gini)


#Rule of Law vs Growth by period
law <- ggplot(data = panel, aes(x = law, y = growth)) +
  geom_point(aes(color = factor(year)), alpha = 0.6, size = 2) +
  facet_wrap(~ year, ncol = 3, scales = "free_x") +  # 3 графика в ряд
  scale_color_manual(
    values = c("1980" = "#FF7F50", "1995" = "#EE6A50", "2010" = "#8B3E2F"),
    name = "Year"
  ) +
  labs(
    title = "Relationship between Law and Economic Growth",
    x = "Law", 
    y = "GDP per capita Growth Rate"
  ) + 
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "none",  # Убираем легенду, так как года указаны на графиках
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 11)
  )
ggsave("C:/Users/arina/Downloads/Telegram Desktop/gini/dotplot_law.png", law)

#Urbanization vs Growth by period
urban <- ggplot(data = panel, aes(x = urban, y = growth)) +
  geom_point(aes(color = factor(year)), alpha = 0.6, size = 2) +
  facet_wrap(~ year, ncol = 3, scales = "free_x") +  # 3 графика в ряд
  scale_color_manual(
    values = c("1980" = "#FF7F50", "1995" = "#EE6A50", "2010" = "#8B3E2F"),
    name = "Year"
  ) +
  labs(
    title = "Relationship between Urbanisation and Economic Growth",
    x = "Urbanisation", 
    y = "GDP per capita Growth Rate"
  ) + 
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "none",  # Убираем легенду, так как года указаны на графиках
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 11)
  )
print(urban)
ggsave("C:/Users/arina/Downloads/Telegram Desktop/gini/dotplot_urban.png", urban)


# Education vs Growth by period
educ_dot <- ggplot(data = panel, aes(x = educ, y = growth)) +
  geom_point(aes(color = factor(year)), alpha = 0.6, size = 2) +
  facet_wrap(~ year, ncol = 3, scales = "free_x") +  # 3 графика в ряд
  scale_color_manual(
    values = c("1980" = "#FF7F50", "1995" = "#EE6A50", "2010" = "#8B3E2F"),
    name = "Year"
  ) +
  labs(
    title = "Relationship between Education and Economic Growth",
    x = "Education", 
    y = "GDP per capita Growth Rate"
  ) + 
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "none",  # Убираем легенду, так как года указаны на графиках
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 11)
  )
print(educ_dot)
ggsave("C:/Users/arina/Downloads/Telegram Desktop/gini/dotplot_educ.png", educ_dot)


# Regression analysis: Four model specifications
pooled <- feols(growth ~ 1 + gini + I(gini^2) + ln_gdp + law + law:gini + urban + I(urban^2) + educ,
                data = panel, vcov = "hetero")
tfe <- feols(growth ~ gini + I(gini^2) + ln_gdp + law + law:gini + urban+ I(urban^2) + educ| year,
             data = panel, vcov = ~ country)
ife <- feols(growth ~ gini + I(gini^2) + ln_gdp + law + law:gini + urban+ I(urban^2)  + educ| country,
             data = panel, vcov =  ~ country)
itfe <- feols(growth ~ gini + I(gini^2) + ln_gdp + law + law:gini + urban + I(urban^2) + educ| year + country,
              data = panel, vcov =  ~country)
modelsummary(list(pooled, tfe, ife, itfe), stars= TRUE)

# Extended models with OECD dummy and interaction terms
pooled <- feols(growth ~ 1 + gini + I(gini^2) + ln_gdp + law + law:gini + urban + I(urban^2) + educ + oecd + oecd:gini,
                data = panel, vcov = "hetero")
tfe <- feols(growth ~ gini + I(gini^2) + ln_gdp + law + law:gini + urban+ I(urban^2) + educ + oecd +  oecd:gini| year,
             data = panel, vcov = ~ country)
ife <- feols(growth ~ gini + I(gini^2) + ln_gdp + law + law:gini + urban+ I(urban^2)  + educ + oecd + oecd:gini| country,
             data = panel, vcov =  ~ country)
itfe <- feols(growth ~ gini + I(gini^2) + ln_gdp + law + law:gini + urban + I(urban^2) + educ  + oecd +  oecd:gini| year + country,
              data = panel, vcov =  ~country)
modelsummary(list(pooled, tfe, ife, itfe), stars= TRUE)
