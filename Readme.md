# The Hotel Reservations Dashboard (Shiny)

Interactive analytics and a booking risk assistant for hotel reservations. Explore demand, prices, cancellations, and run a quick prediction to estimate cancellation risk for a new booking.

• Live demo: https://dsio4z-abdillah-ilham.shinyapps.io/Final-Project-Data-Mining/


## Overview

This dashboard analyzes hotel reservation data to help stakeholders understand booking behavior, demand patterns, and price trends. It also includes an experimental booking form that feeds a pre-trained Random Forest model to estimate whether a reservation is likely to be canceled.

The app is built with R, Shiny, and bs4Dash, uses Plotly for interactive charts, and reads data from `Datasets/Hotel Reservations.csv` and `Datasets/test_data.csv`. A pre-trained model is loaded from `rf_model.Rdata`.


## Features

- Home
	- Welcome page with project context and authors’ information.

- Executive Report
	- Summary Statistics: high-level counts (e.g., cancellations) and descriptive statistics for numerical and categorical variables.
	- Key Insights (Line Charts):
		- Room Demand over time by room type.
		- Average Room Price by month/year.
	- Interactive Chi-Square Test: select two categorical variables and run a chi-square test; results shown with significance feedback.

- Interactive Plot
	- Numerical Plots: histogram, boxplot, and 2D heatmap for pairs of numeric variables.
	- Count Plots: single-variable bar chart or two-variable grouped bar chart; sunburst chart for hierarchical category breakdowns.
	- Numerical vs Categorical: violin plot with optional box overlays and mean lines.

- Booking System (Cancellation Risk Assistant)
	- Form inputs for adults/children, nights, meal plan, room type, market segment, average price, requests, and dates.
	- On submit, passes inputs to a pre-trained Random Forest classifier and displays a result via an in-app alert.


## Data and Methods

- Data Sources
	- `Datasets/Hotel Reservations.csv` (main dataset)
	- `Datasets/test_data.csv` (structure reference for prediction input)
	- Note: The dataset license is referenced in-app (Creative Commons Attribution 4.0 International).

- Preprocessing (as implemented in `app.R`)
	- Construct `arrival_date` from year, month, and day columns and drop the year/month columns.
	- Cast categorical features to factors.
	- Derive `booking_date = arrival_date - lead_time`.
	- Drop rows with missing values for a clean working set.

- Statistical/Exploratory Analyses
	- Descriptive stats for numeric variables (central tendency, spread) using `psych::describe`.
	- Frequency summaries for categorical variables.
	- Chi-Square tests of association for selected categorical pairs with interactive feedback (`shinyalert`).

- Predictive Modeling
	- Pre-trained Random Forest classifier loaded from `rf_model.Rdata`.
	- Input features include: adults, children, weekend/weekday nights, meal plan, room type, lead time (derived from dates), market segment, average price per room, and special requests.
	- Prediction used to provide a simple cancellation risk message inside the app.


## Tech Stack

- R, Shiny, bs4Dash (UI layout and components)
- dplyr, tidyr, lubridate (data wrangling)
- plotly (interactive charts)
- psych, DT (descriptive stats, data tables)
- randomForest, caret, pROC (ML and evaluation tooling)
- shinyalert (inline notifications)


## Project Structure

```
Final-Project-Data-Mining/
├─ app.R                        # Shiny app: UI + server + model inference
├─ rf_model.Rdata              # Pre-trained Random Forest model (RDS)
├─ Datasets/
│  ├─ Hotel Reservations.csv   # Main dataset
│  └─ test_data.csv            # Reference row structure for prediction
├─ Images/
│  ├─ farham.jpeg
│  └─ ilham.png
├─ rsconnect/                  # Deployment metadata (shinyapps.io)
└─ Readme.md                   # This file
```


## How to Run Locally

Prerequisites
- R 4.0+ and an internet connection to install packages
- Files present as per the structure above (datasets and model file)

1) Install required packages (run in R/RStudio):

```r
pkgs <- c(
	"shiny","bs4Dash","readxl","dplyr","tidyr","lubridate","htmltools",
	"plotly","psych","DT","randomForest","caret","pROC","shinyalert"
)
install.packages(setdiff(pkgs, rownames(installed.packages())))
```

2) Launch the app from the project root:

```r
shiny::runApp(".")
```

Optional (Windows PowerShell):

```powershell
R -e "shiny::runApp('.')"
```

Deployment
- The repo includes `rsconnect/` metadata for shinyapps.io. From R, you can deploy with:

```r
# install.packages("rsconnect")
rsconnect::deployApp(appDir = ".")
```


## Known Limitations and Notes

- Some UI controls referenced in the server logic (e.g., certain date ranges or histogram bin sliders) may not be wired into the visible UI in the current `app.R`. If a control appears missing, add the corresponding Shiny input in the relevant tab to fully enable that feature.
- The Random Forest model is loaded pre-trained; training scripts and metrics are not included. For reproducibility, consider adding a separate training notebook/script and pinning package versions (e.g., with `renv`).


## Roadmap / Next-Gen Ideas

- Data/Model
	- Add a full training pipeline with model validation, hyperparameter tuning, and periodic retraining.
	- Consider gradient boosting (xgboost/lightgbm) and calibration for better risk probabilities.
	- Integrate renv for reproducible environments.

- Product Features
	- Complete and polish all interactive controls (date filters, bins, etc.).
	- Add cohort analyses (by market segment, room type) and seasonality decompositions.
	- Support what-if simulations (price changes, lead-time adjustments) and basic revenue management levers.
	- Exportable reports (PDF/HTML) and sharable links for filtered views.

- Integration
	- API endpoints for real-time scoring from external booking systems.
	- Connect to a transactional database or data lake for live data ingestion.
	- SSO/auth and role-based access control for enterprise deployments.


## Authors

- Abdillah Ilham (5003211069)
- Farham Ramadhani (5003211165)


## Acknowledgments

- Dataset referenced with Creative Commons Attribution 4.0 International (CC BY 4.0).
- Built with R Shiny, bs4Dash, and Plotly.


## License

No license has been provided in this repository. If you intend to share or reuse the code, consider adding a LICENSE file (e.g., MIT, Apache-2.0). Dataset licensing may differ; review the dataset’s CC BY 4.0 terms.
