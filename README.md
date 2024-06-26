<h1 align="center">
  <br>
Children's Social Care - Outcomes and Enablers Dashboard
  <br>
</h1>

---

<p align="center">
  <a href="#introduction">Introduction</a> |
  <a href="#requirements">Requirements</a> |
  <a href="#how-to-use">How to use</a> |
  <a href="#how-to-contribute">How to contribute</a> |
  <a href="#contact">Contact</a>
</p>

---

## Introduction 
The purpose of the Dashboard is to serve as a learning tool for both local and central Government and will provide a clear and consistent set of data indicators to help to understand progress towards the outcomes and enablers set out in the National Framework.

Insights and analysis gathered from this tool will support the Department’s wider strategic 
aim to make stronger, measurable decisions that achieve better outcomes for children, young people and families. Having a national set of indicators, used by all local authorities, with a mechanism to bring them together in one place is integral to supporting the system to collaborate. In this way, local authorities can reflect and learn from others on progress towards the outcomes and enablers in children’s social care.

As well as a learning tool for local government, the Dashboard will help central government to understand system-wide success and issues and how children, young people and families are being supported.

- Production - https://department-for-education.shinyapps.io/csc-outcomes-enablers/
- Development - https://department-for-education.shinyapps.io/dev-csc-outcomes-enablers/


---

## Requirements

### i. Software requirements (for running locally)

- Installation of R Studio 1.2.5033 or higher

- Installation of R 4.4.0 or higher

- Installation of RTools40 or higher

### ii. Programming skills required (for editing or troubleshooting)

- R at an intermediate level, [DfE R training guide](https://dfe-analytical-services.github.io/r-training-course/)

- Particularly [R Shiny](https://shiny.rstudio.com/)

### iii. Access requirements

- There are no other access requirements. All datasets are publicly available.
  
---

## How to use
### Running the app locally

1. Clone or download the repo. 

2. Open the R project in R Studio.

3. Run `renv::restore()` to install dependencies.

4. Run `shiny::runApp()` to run the app locally.


### Packages

Package control is handled using renv. As in the steps above, you will need to run `renv::restore()` if this is your first time using the project.

### Tests

UI tests have been created using shinytest2 that test the app loads. More should be added over time as extra features are added.

GitHub Actions provide CI by running the automated tests and checks for code styling. The yaml files for these workflows can be found in the .github/workflows folder.

The function run_tests_locally() is created in the Rprofile script and is available in the RStudio console at all times to run both the unit and ui tests.

### Deployment

- The app is deployed to the department's shinyapps.io subscription using GitHub actions. The yaml file for this can be found in the .github/workflows folder.

### Navigation

In general all .r files will have a usable outline, so make use of that for navigation if in RStudio: `Ctrl-Shift-O`.

### Code styling 

The function tidy_code() is created in the Rprofile script and therefore is always available in the RStudio console to tidy code according to tidyverse styling using the styler package. This function also helps to test the running of the code and for basic syntax errors such as missing commas and brackets.


### Technical details

For technical methodology and rationale on y-axis implementation, see [https://github.com/dfe-analytical-services/CSC_outcomes_and_enablers/blob/development/Y_AXIS_DOCUMENTATION.md](https://github.com/dfe-analytical-services/CSC_outcomes_and_enablers/blob/development/Y_AXIS_DOCUMENTATION.md). 



---

## How to contribute

### Flagging issues

If you spot any issues with the application, please check first that it has not been reported as an [issue](https://github.com/dfe-analytical-services/CSC_outcomes_and_enablers/issues) already.

If no issue is open for your bug, please flag it in the "Issues" tab of this repository, and label as a bug.
Alternatively, you can open a new [bug report here](https://github.com/dfe-analytical-services/CSC_outcomes_and_enablers/issues/new/choose).

### Adding new features

If there are new features you would like to suggest you can submit a [feature request here](https://github.com/dfe-analytical-services/CSC_outcomes_and_enablers/issues/new/choose).

### Merging pull requests

Only members of the team can merge pull requests. Add yauemily, nataliepaterson and jzaun444 as requested reviewers, and the team will review before merging.

---

## Contact

If you have any questions about the dashboard, please contact CSCDashboard.FEEDBACK@education.gov.uk . 
