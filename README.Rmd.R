---
  output: github_document
always_allow_html: yes
---
  knitr::knit("README.Rmd")
<!-- README.md is generated from README.Rmd. Please edit that file -->
  
  ```{r setup, include = FALSE}
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "man/figures/README-",
  out.width = "100%"
)
```
```{r packages, include = FALSE}
library(pensionviewr)
library(tidyverse)
library(kableExtra)
library(reasontheme)
```
# pensionviewr

The goal of `pensionviewr` is to simplify the process of gathering and visualizing public pension plan data from the Reason pension database. This repo contains the functions of the `pensionviewr` package, which once installed locally, provides helpful functions for creating and exporting graphics made in ggplot in the style used by the Reason Pension Integrity Project team.

## Create Token:
To use devtools you'd need to authenticate yourself by
creating Personal Access Tokens (PAT):

  - Obtain a PAT by typing `usethis::browse_github_pat()`.
Click "Generate token" and Copy to Clipboard the displayed string of 40 letters/digits.
  - Find your .Renviron in your home directory by typing:
`usethis::edit_r_environ()`
  - Put your PAT in your .Renviron file. Have a line that looks like this:
`GITHUB_PAT=8c70fd8419398999c9ac5bacf3192882193cadf2` (but use your own PAT instead)
  - Save edited .Renviron file, and
  - Lastly, restart R & check your PAT by typing:
`Sys.getenv("GITHUB_PAT")`
## Installing pensionviewr

`pensionviewr` is not on CRAN, so you will have to install it directly from Github using `devtools`. 

If you do not have the `devtools` package installed, you will have to run the first line in the code below as well. 

``` r
install.packages('devtools')
devtools::install_github("ReasonFoundation/pensionviewr")
```

## Using the functions:

* The package has seven functions for data pulling and preparation: 
`planList()`, 
`pullData()`, 
`pullStateData()`,
`pullSourceData()`,
`filterData()`,
`masterView()`,
`loadData()`, and 
`selectedData()`.

* The package has four functions for plots: 
`glPlot()`, 
`linePlot()`, 
`areaPlot()`, and 
`savePlot()`.

* The package has Reason color palette: 
`palette_reason` (e.g. `palette_reason$Orange`))

A basic explanation and summary here:

### `planList()`

1. `planList()`: returns a stripped down list of the pension plans in the database along with their state and the internal databse id.

Example of how it is used in a standard workflow:

```{r planList}
pl <- planList()
pl %>% 
  head() %>%
  kable() %>%
  kable_styling(full_width = FALSE, position = "left")
```

### `pullData()`

2. `pullData()`:  pulls the data for a specified plan from the Reason pension database. `pullData` has two arguments:
`pullData(pl, plan_name)`

* `pl`: A dataframe containing the list of plan names, states, and ids in the form produced by the `planList()` function.
* `plan_name`: A string enclosed in quotation marks containing a plan name as it is listed in the Reason pension database.

Example of how it is used in a standard workflow:

The next step would be to load the data for the specific plan of interest. Let's use the Vermont State Retirement System as an example. Let's first see what plans in Vermont are available:

```{r Kansas}
VT <- pl %>% filter(state == 'Vermont')
VT %>% 
  kable() %>%
  kable_styling(full_width = FALSE, position = "left")
```

The full plan name we are interested in is there listed as "Vermon State Retirement System". We can pull the data for it now:

```{r pullData}
vtsrs_data <- pullData(pl, plan_name = "Vermont State Retirement System")
vtsrs_data %>% 
  head() %>%
  kable() %>%
  kable_styling(full_width = FALSE, position = "left")
```

### `pullStateData()`

3. `pullStateData()`: pulls all state-level pension data in wide format from the Reason pension database. `pullStateData` has single argument:
`pullStateData(fy)`

* `fy`: Starting fiscal year for the data pulled from the Reason pension database.

This could be the initial step for either making state-level pension analysis or then filter pulled data for a specific state plan.
Example of how it is used in a standard workflow:

```{r pullStateData}
state.data <- pullStateData(2001)
New_Mexico.data <- state.data %>% filter(state == 'New Mexico')
NMERB.data <- state.data %>% filter(display_name == "New Mexico Educational Retirement Board")
state.data <- state.data %>% filter(year == 2019)#filter for 2019 for display
state.data %>% 
  head() %>%
  kable() %>%
  kable_styling(full_width = FALSE, position = "left")
```

### `pullSourceData()`

4. `pullSourceData()`: pulls pension data for a specific plan, along with `data_source_name` column in wide format from the Reason pension database. `pullSourceData(pl,plan_name, fy)` has 3 arguments:

* `pl`: A dataframe containing the list of plan names, states, and ids in the form produced by the `planList()` function.
* `plan_name`: A string enclosed in quotation marks containing a plan name as it is listed in the Reason pension database.
* `fy`: Starting fiscal year for the data pulled from the Reason pension database.

Example how this function is used:

```{r pullSourceData}
NMERB.source <- pullSourceData(pl, "New Mexico Educational Retirement Board", 2001)
NMERB.source %>% 
  head() %>%
  kable() %>%
  kable_styling(full_width = FALSE, position = "left")
```

### `filterData()`

5. `filterData()`: filters existing data (data.frame/data.table format) keeping & renaming set of commonly used variables in pension analysis. `filterData(Data, fy, source = FALSE)` has 4 arguments:

* `Data`: A data table already pulled with `pullData`, `pullStateData`, or other ways. 
* `fy`: Starting fiscal year for the data pulled from the Reason pension database.
* `employee`: Character designating type of employees covered (e.g. "teacher", "state", "local", "state and local", "police and fire").
* `source`: Set to `FALSE`. It should be set to `source = TRUE` if you filter data pulled with `pullSourceData` function.

Example of the workflow around the filtering function:

```{r filterData}
state.data <- pullStateData(2001)
filtered <- filterData(state.data, 2010, NULL, FALSE)
filtered %>% 
  head() %>%
  kable() %>%
  kable_styling(full_width = FALSE, position = "left")
```

### `masterView()`

6. `masterView()`: Allows to view already mapped & unmapped columns/variables per data source.
`masterView(source = NULL, expand = FALSE)` has 2 arguments:

* `source`: Name of the main data sources: "Reason", "Public Plans Database", "Census".
* `expand`: Set to `FALSE` (default) to see mapped variables. Change to `TRUE` if you want to see all unmapped variables.

Example of how to use this function:

```{r masterView}
view.reason <- masterView("Reason", FALSE)
view.reason %>% 
  head() %>%
  kable() %>%
  kable_styling(full_width = FALSE, position = "left")
```

### `loadData`

7. `loadData`: loads the data for a specified plan from an Excel file. `loadData` has one argument:

`loadData(file_name)`

* `file_name`: A string enclosed in quotation marks containing a file name with path of a pension plan Excel data file.

```
data_from_file <- loadData('data/NorthCarolina_PensionDatabase_TSERS.xlsx')
```

### `selectedData()`

8. `selectedData()`: selects the only the variables used in historical analyses. `selectedData` has one argument, `wide_data`, that is required:

`selectedData(wide_data)`

* `wide_data`: a datasource in wide format

Back to the Kansas Public Employees' example. That is a lot of variables. The `selectedData()` function selects only a handful of needed variables:
  
  ```{r selectedData}
df <- selectedData(vtsrs_data)
df %>% 
  head() %>%
  kable() %>%
  kable_styling(full_width = FALSE, position = "left")
```

### `glPlot()`

9. `glPlot()`: creates the 'Gain/Loss' plot using a CSV file as an input. glPlot has two arguments:
  
  `glPlot(filename, ylab_unit)`

* `filename`: a csv (comma separated value) file containing columns of gain loss category names with one row of values.
* `ylab_unit`: a string contained within quotation marks containing th y-axis label unit. The default value is "Billions"

Example of how it is used in a standard workflow:
  
  `filename <- "data/GainLoss_data.csv"`
`glPlot(filename)`

### `linePlot()`

10. `linePlot()`: creates a plot comparing two variables, such as ADEC vs. Actual contributions. `linePlot()` has six arguments, with `data` being required:
  
  `linePlot(data, .var1, .var2, labelY, label1, label2)`

* `data` a dataframe produced by the selectedData function or in the same format.
* `.var1` The name of the first variable to plat, default is adec_contribution_rates.
* `.var2` The name of the second variable to plot, default if actual_contribution_rates.
* `labelY` A label for the Y-axis.
* `label1` A label for the first variable.
* `label2` A label for the second variable.

### `areaPlot()`

11. `areaPlot()`: creates the "Mountain of Debt" chart or S&P500 chart. `areaPlot` has seven arguments, with `data` being required:
  
  `areaPlot(data, title, caption, grid, ticks, sp500, font)`

* `data` a dataframe or data.table produced by the pullStateData function or in the same format.
* `title` naming the chart (e.g. "Unfunded Liability Growth").
* `caption` set to TRUE to add "reason.org/pensions" caption at the bottom right corner
* `grid` set to TRUE to add major gridlines
* `ticks` Set to FALSE to remove ticks
* `sp500` default is FALSE ti create Mounain of Debt chart. Set sp500 to TRUE to visualize annual S&P500 Index values on the secondary Y-axis
* `font` directly paste name of a font (e.g. "Calibri") to change the default font of the text

Example of how it is used in a standard workflow:
  ```
debt <- areaPlot(PERSI.debt, caption = F, grid = F, ticks = F, sp500 = F, font = "Calibri")
```

### `savePlot()`

12. `savePlot()`: adds a source and save ggplot chart. `savePlot` takes five arguments:
  `savePlot(plot = myplot, source = "The source for my data", save_filepath = "filename_that_my_plot_should_be_saved_to.png", width_pixels = 648, height_pixels = 384.48)`

* `plot`: The variable name of the plot you have created that you want to format and save
* `source`: The text you want to come after the text 'Source:' in the bottom left hand side of your side
* `save_filepath`: Exact filepath that you want the plot to be saved to
* `width_pixels`: Width in pixels that you want to save your chart to - defaults to 648
* `height_pixels`: Height in pixels that you want to save your chart to - defaults to 384.48

```
savePlot(debt_plot, source = "Source: KPERS", save_filepath = "output/test.png")
```


The BBC has created a wonderful data journalism cookbook for R graphics located here:
  https://bbc.github.io/rcookbook/
  
  
  
  
  