# Short-Run-AD-AS-Model

This model was developed for the short-run macroeconomic analysis in my Intermediate Macroeconomics course.

---

# Economics-Notebook

## Files

- **shinyapp_ch13_AD_AS.R**  
  This is the main file for the Chapter 13 model.

- **ch13_master.qmd**  
  This file contains older models as well as current and additional code for Chapter 13 and/or the Short-Run model.

---

# App Structure

### 1. App Launch / Global Setup
1. **Packages and Scripts Loaded**  
   - Libraries are loaded and `source("modules/simulation_calc.R")` is executed.
2. **Helper Function Defined**  
   - `initializeExperimentSet(...)` is defined.
3. **UI Defined**  
   - The UI is laid out using `page_sidebar(...)`.
   - The main panel includes several tab panels: **World**, **Plots**, **Log/Ratio Scale**, and **Program Results** (with subtabs).  
   At this point, the UI is rendered in the browser, but no simulation data has been generated yet.

---

### 2. Server Function Initialization
When `s(ui, server)` starts, the server function is invoked once. Inside the server:
1. **Excel Import**  
   - `excel_list <- import_list("data/savings_rate_y.xlsx")` loads available Excel sheets for the “World” tab.
   - Observers for regions, years, and the button to calculate the average savings rate are initialized.

---

### 3. Experiments Table Management
- Create a `reactiveVal` to store experiments.
- Observe the “Add” button to insert a new row.
- Render the experiment table with “Delete” buttons.
- Observe “Delete” events to remove rows.

---

### 4. Simulation Triggers (Event Reactive Blocks)
When the user clicks **Simulate**:
1. **`simulate_first_exp_calculations`**  
   - An `eventReactive(input$simulate, { ... })` block is triggered.
   - It checks if the first experiment set is empty. If empty, a modal error is shown; otherwise, it calls Module Call 1 (`simulate_solow`) to run the simulation with the parameters and experiment set from Tab 1.
2. **`simulate_counter_exp_calculations`**  
   - Runs Module Call 1 again, but with no experiments.
3. **`simulate_second_exp_calculations`, `simulate_third_exp_calculations`, `simulate_fourth_exp_calculations`**  
   - Each block uses `eventReactive(input$simulate, { ... })` to run the same Module Call 1 but with experiments from sets 2, 3, or 4 (if available; otherwise, it returns `NULL`).

All these `eventReactive` blocks return their respective data frames of results when the user clicks **Simulate**.

---

### 5. Output Tables (Reactive)
1. **Tables Rendered**  
   - Output slots such as `results_first_exp_df`, `results_counterfactual_df`, `results_second_exp_df`, `results_third_exp_df`, and `results_fourth_exp_df` use `renderTable({ req(...); ... })`.
   - Each table displays the data frame returned by its respective simulation.

---

### 6. Plot Generation and Display
1. **`make_plot(...)` Function**  
   - A local helper function that constructs a ggplot2 plot.
2. **`plot_objects` ReactiveValues**  
   - A `reactiveValues()` object is used to store each ggplot output for inclusion in the ZIP.
3. **`plot_specs` List**  
   - A list of plot definitions (IDs, y-variable names, titles, labels).
4. **Loop to Render Plots**  
   - A `for (spec in plot_specs)` loop sets up `output$plot_... <- renderPlot({ ... })`.
   - Each time the user clicks **Simulate** or toggles the checkboxes, these plots refresh automatically if any reactive dependencies change.

---

### 7. Download Handlers
1. **Download Data (`downloadData`)**  
   - When clicked, this handler pulls the first experiment’s data from `simulate_first_exp_calculations()` and writes it to a CSV file.
2. **Download All Plots (`downloadPlots`)**  
   - This handler allows the user to download a ZIP file containing all the plots as PNGs.

---

### 8. User Interaction Flow Summary
A typical user flow might look like this:
1. The user launches the app, and the UI appears.
2. The user selects a region/country in the **World** tab; observers recalculate the valid years.
3. The user clicks **Get Avg Savings Rate**; the average is computed and populates the slider.
4. The user enters experiments in any of the 4 tabs (Sets 1, 2, 3, 4), either by manually adding rows or by uploading a file.
5. The user clicks **Simulate**, which:
   - Triggers all the `eventReactive` blocks:
     - The first scenario is simulated; if there are no rows in Set 1, an error is shown.
     - The counterfactual scenario is simulated (with no experiments).
     - Additional scenarios for sets 2–4 (if rows exist) are simulated.
   - All result data frames (`results_*_df`) refresh.
   - All plots refresh via `renderPlot`.
6. The user views the new results in the **Program Results** tab and sees updated plots in the **Plots** and **Log/Ratio Scale** tabs.
7. Optionally, the user toggles checkboxes to show or hide lines for the second, third, and fourth scenarios (or the counterfactual), and the plots update automatically.
8. Optionally, the user clicks **Download Results** to obtain a CSV of the first scenario or **Download All Plots** to download a ZIP file of PNGs.

