#' Get Text for Help Panel in Shiny App
#'
#' @param id id of selected tab
#'
#' @export
getHelp <- function(id) {
  if (id == "dataExplorer") {
    tagList(
      tags$b("Welcome to the IsoMemo app!"),
      tags$p(
        "The \"Data\" tab serves for selection of data from various isotopic data bases.
        With the \"Interactive map\" tab the selected data can be viewed
        in an interactive interface on the world map. The \"Modeling\" tab serves
        for the computation of models using the selected data
        (or user data provided in a data-file) for
        estimating a spatial average model, a spatio-temporal
        average model or a spread model. For a
        video introduction please have a look at:"
      ),
      tags$a(
        "https://www.youtube.com/watch?time_continue=1&v=B-isiVu09zQ"
      ),
      tags$p(""),
      tags$b("The \"Data\" tab"),
      tags$p(
        "On the left panel the user can select the
        databases and load them into the app by using the \"Load data\" button.
        Under \"Select categories\" the user may select categories or groups of fields.
        Each category has multiple fields or variables which can be selected individually
        by klicking on the drop down menu.
        A certain selection of fields can be saved and re-loaded by the \"Save data selection\" and
        \"Load data selection\" buttons."
      ),
      tags$p(
        "Once the data is loade, a spread sheet appears on the screen. It can be sorted using the
        values by certain fields by clicking
        on the field name and filter by clicking on the entry below the field name.
        The selected and filtered data set in the spread sheet can be exported in .xlsx,
        .csv or .json file format using the \"Export\" button at the bottom of the left panel."
      ),
      tags$p(
        "On the left panel the calibration options for radiocarbon dates can be found.
        By selecting a method all uncalibrated radiocarbon
        dates in the displayed spread-sheet are calibrated by the selected method.
        The coverage of the credible intervals in percent can
        be selected in the \"Calibration range field\".
        The calibration is done with the help of the \"Bchron\" R-package:"
      ),
      tags$a(
        "https://cran.r-project.org/web/packages/Bchron/index.html"
      ),
      shinyjs::useShinyjs()
      )
  }
  else if (id == "interactivemap") {
    tagList(
      tags$b("The \"Interactive map\" tab"),
      tags$p(
        "Once data is loaded into the dasboard view in the \"Data\" tab,
        all locations of the selected data are displayed on the world map.
        Keep in mind that the \"longitude\" and \"latitude\\ variables have to be
        available and selected."
      ),
      tags$p(
        "By clicking on a location the field values selected under \"Select categories\"
        in the \"Data\" tab can be seen."
      ),
      tags$p(
        "On the right panel one or two numeric fields of
        the selected data can be chosen.
        One- and two-dimensional summary statistics and
        plots (with different options) are
        computed and displayed. The statistics can be copied into
        the clipboard for further use.
        The plots can exported in various formats."
      ),
      shinyjs::useShinyjs()
      )
  } else if (id == "model2D") {
    tagList(
      tags$b("The \"Modeling\" tab - \"Local average model\""),
      tags$p(
        "Here, a spatial or local average of a user defined
        inpedendent variable is modeled.
        A GAM (Generalized Additive Model) or GAMM (Generalized Additive Mixed Model)
        is fitted here."
      ),
      tags$b("Left panel - data and model options:"),
      tags$p(
        "Data selection: Under \"Data\" source the user can select if he likes to use
        the data selected in the \"Data\" tab
        or to upload an .xlsx or .csv file. If the latter the user has to make sure
        that the first row
        of his tabular data set contains the variable/field names and select an
        appropriate coordinate format
        of the three choices: \"decimal degrees\", \"degrees decimal minutes\" or
        \"degrees minutes seconds\".
        If the option .csv is selected the user also has to provide the column and
        decimal separators as they
        may differ between different language versions of Excel."
      ),
      tags$p(
        "Variable selection: Here the user has to specify the independent variable,
         its uncertainty in standard deviations (optional),
        the longitude and latitude variable as well as the site/ID variable. The
        latter is optional but recommended.
        If selected, a random intercept is added to the model, which is able to capture
        dependencies within one site."
      ),
      tags$p(
        "Model options:
        The \"Number of basis functions\" governs the number of basis functions used
        for the approximation of the surface, so called approximate thin-plate-splines
        (see e.g. doi:10.1007/3-540-47977-5_2 or doi:10.1111/1467-9868.00374 for
        further information).
        A higher number delivers more exact results and enables the estimation of
        more complex
        2-D surfaces, but uses much more computational power. In general,
        there is little gain from very high numbers and there is no additional gain
        from selecting a higher
        number than there are unique combinations of longitude and latitude.
        By selecting the corresponding check-box, a Bayesian model is possible as well.
        However, the computation time may be quite high for large data sets.
        The user can select how much MCMC (Markov Chain Monte Carlo) iterations should be computed.
        Higher numbers deliver more exact
        results at the expense of computation time (usually 1000 iterations already give a pretty
        usable results for testing). If a Bayesian model is selected, the user can also modify the
        amount of smoothing. The model already selects an optimal amount of smoothing itself,
        but with this option the user gets the possibility to
        estimate a more or less smoother surface. The model can be estimated by clicking
        on the \"Run\" button."
      ),
      tags$b("Central panel - display, export and selection of map section:"),
      tags$p(
        "The estimated spatial average is displayed in the center.
         An export button can be found directly below,
         which offers various formats for plot export.
        With the two sliders below the map section can be selected."
      ),
      tags$b("Right panel - graphic options:"),
      tags$p(
        "After estimating the model, several options to modify the displayed
        plot appear on a rightern panel. If the dependent variable is range restricted between
        0 and 1 or 0 and 100, the user can select this on the top of the panel. Directly below,
        there is the choice to restrict the display the estimates on the terrestrial or aquatic
        areas or the complete surface. Furthermore, the location marks (if selected the size can be
        influenced bya slider), grid, scale and north arrow can be removed from the map.
        The \"Apply convex hull\" checkbox marks, whether estimates of locations within the convex
        hull of the data points should be displayed. In general, this is indicated
        as extrapolation beyond areas with available data is highly speculative.
        The slider \"Display up to max standard error\" governs the area where we have a standard
        error below the selected mark to prevent areas
        with low certainty being shown. Right under the colour options,
        a slider \"Plot resolution\" is available. A higher number delivers a
        higher quality picture of the coloured area with less pixaleted borders but needs much
        more drawing time. A lower value is recommended for
        setting the right picture options and map section and a high value for publication."
      ),
      tags$b("Location/Batch estimate:"),
      tags$p(
        "The options on the bottom of the rightern panel enable the user to get an estimate of
        a specified location (\"Center point latitude/longitude\") within a certain radius.
        The estimate with standard error will appear below the x-axis in the displayed plot.
        For getting estimates for a batch of points the \"Batch Estimes\"
        button opens a pop-up window, where a user-specified data set can be uploaded. A local
        estimate including standard error and 95 percent uncertainty
        intervals is added and can be exported. Alternatively, these estimates can be
        exported into the \"FRUITS\"-app (not functional yet)."
        ),
      shinyjs::useShinyjs()
        )
  } else if (id == "model3D") {
    tagList(
      tags$b("The \"Modeling\" tab - \"Spatio-temporal average model\""),
      tags$p(
        "Here, a spatio-temporal average of a user defined inpedendent variable is modeled.
        A GAM (Generalized Additive Model) or GAMM (Generalized Additive Mixed Model)
        is fitted here."
      ),
      tags$b("Left panel - data and model options:"),
      tags$p(
        "Data selection: Under \"Data\" source the user can select
        if he likes to use the data selected in the \"Data\" tab
        or to upload an .xlsx or .csv file.
        If the latter the user has to make sure that the first row
        of his tabular data set contains the variable/field names and select
        an appropriate coordinate format
        of the three choices: \"decimal degrees\", \"degrees decimal minutes\"
        or \"degrees minutes seconds\".
        If the option .csv is selected the user also has to provide the column
        and decimal separators as they
        may differ between different language versions of Excel."
      ),
      tags$p(
        "Variable selection: Here the user has to specify the independent variable,
        its uncertainty in standard deviations (optional),the date variables,
        the longitude and latitude variable as well as the site/ID variable.
        The latter is optional but
        recommended. If selected, a random intercept is added to the model, which is able to capture
        dependencies within one site. For the date variables, a dating type has to be specified
        in the \"Date type\" field,
        The user can select between \"Interval\" (lower and upper date variable required),
        \"Mean + 1 SD uncertainty\" and  \"Single point\"). Depending on this choice,
        the following two fields have to be filled with the appropriate field/variable names"
      ),
      tags$p(
        "Model options:
        The \"Number of basis functions\" governs the number of basis functions used for the
        approximation of the surface, so called approximate thin-plate-splines
        (see e.g. doi:10.1007/3-540-47977-5_2 or doi:10.1111/1467-9868.00374
        for further information).
        A higher number delivers more exact results and enables the estimation of more complex
        2-D surfaces, but uses much more computational power. In general,
        there is little gain from very high numbers and there is no additional
        gain from selecting a higher
        number than there are unique combinations of longitude and latitude and dates.
        By selecting the corresponding check-box, a Bayesian model is possible as well.
        However, the computation time may be VERY HIGH for large data sets,
        thus we recommend to estimate this kind of model within R. The user can select how much
        MCMC (Markov Chain Monte Carlo) iterations should be computed.
        Higher numbers deliver more exact
        results at the expense of computation time (usually 1000 iterations already give a pretty
        usable results for testing). If a Bayesian model is selected, the user can also modify the
        amount of smoothing. The model already selects an optimal amount of smoothing itself,
        but with this option the user gets the possibility to
        estimate a more or less smoother surface.
        The model can be estimated by clicking on the \"Run\" button."
      ),
      tags$b("Central panel - display, export and selection of map section:"),
      tags$p(
        "The estimated spatial average is displayed in the center.
         An export button can be found directly below, which offers various formats for plot export.
         With the lower two sliders the map section can be selected."
      ),
      tags$b("Right panel - graphic options:"),
      tags$p(
        "After estimating the model, several options to modify the displayed plot appear on a
         rightern panel. With \"Plot type = time course\" an alternative visualization is shown.
         Instead of a 2-D slice at a certain point in time it shows the time course of the estimate
         with uncertainty intervals for a user-defined location. If the dependent variable is range
         restricted between 0 and 1 or 0 and 100, the user can select this on the top of the panel.
         Directly below, there is the choice to restrict the display the estimates on the
         terrestrial or aquatic areas or the complete surface. Furthermore, the location marks
         (if selected the size can be influenced bya slider), grid, scale and north arrow can be
         removed from the map. The \"Apply convex hull\" checkbox marks, whether estimates of
         locations within the convex hull of the data points should be displayed.
         In general, this is indicated as extrapolation beyond areas with available data is
         highly speculative. The slider \"Display up to max standard error\" governs the area
         where we have a standard error below the selected mark to prevent areas
         with low certainty being shown. Right under the colour options, a slider
         \"Plot resolution\" is available. A higher number delivers a
         higher quality picture of the coloured area with less pixaleted borders but needs
         much more drawing time. A lower value is recommended for
         setting the right picture options and map section and a high value for publication."
      ),
      tags$b("Location/Batch estimate:"),
      tags$p(
        "The options on the bottom of the rightern panel enable the user to get an estimate of
        a specified location (\"Center point latitude/longitude\") within a certain radius.
        The estimate with standard error will appear below the x-axis in the displayed plot.
        For getting estimates for a batch of points the \"Batch Estimes\"
        button opens a pop-up window,
        where a user-specified data set can be uploaded. A local estimate including standard error
        and 95 percent uncertainty
        intervals is added and can be exported. Alternatively, these estimates can be exported
        into the \"FRUITS\"-app (not functional yet)."
        ),
      shinyjs::useShinyjs()
        )
  } else if (id == "spread") {
    tagList(
      tags$b("The \"Modeling\" tab - \"Spread model\""),
      tags$p(
        "Here, a time spread of a user defined date variable is modeled.
        An extremal quantile regression model (see e.g. doi:10.1214/009053604000001165)
        is fitted here to get an estimate very close to the local minimum or maximum.
        This is done using a Bayesian quantile approach (see e.g. doi:10.1016/S0167-7152(01)00124-9)
        with a quantile set near 0/1."
      ),
      tags$b("Left panel - data and model options:"),
      tags$p(
        "Data selection: Under \"Data\" source the user can select
        if he likes to use the data selected in the \"Data\" tab
        or to upload an .xlsx or .csv file.
        If the latter the user has to make sure that the first row
        of his tabular data set contains the variable/field names and
        select an appropriate coordinate format
        of the three choices: \"decimal degrees\", \"degrees decimal minutes\" or
        \"degrees minutes seconds\".
        If the option .csv is selected the user also has to provide the
        column and decimal separators as they
        may differ between different language versions of Excel."
      ),
      tags$p(
        "Variable selection: Here the user has to specify the date variables,
        the longitude and latitude variable as well as the site/ID variable.
        The latter is optional but recommended. If selected, a random intercept is added
        to the model, which is able to capture dependencies within one site. For the date variables,
        a dating type has to be specified in the \"Date type\" field,
        The user can select between \"Interval\" (lower and upper date variable required),
        \"Mean + 1 SD uncertainty\" and  \"Single point\"). Depending on this choice,
        the following two fields have to be filled with the
        appropriate field/variable names"
      ),
      tags$p(
        "Model options:
        At first, the user needs to choose between a \"Min\" or \"Max\"
        (corresponding to last or first appearance) model.
        The \"Number of basis functions\" governs the number of basis functions used for the
        approximation of the surface, so called approximate thin-plate-splines
        (see e.g. doi:10.1007/3-540-47977-5_2 or doi:10.1111/1467-9868.00374
        for further information).
        A higher number delivers more exact results and enables the estimation of more complex
        2-D surfaces, but uses much more computational power. In general,
        there is little gain from very high numbers
        and there is no additional gain from selecting a higher
        number than there are unique combinations of longitude and latitude and dates.
        By selecting the corresponding check-box, a Bayesian model is possible as well.
        However, the computation time may be VERY HIGH for large data sets,
        thus we recommend to estimate this kind of model within R. The user can select how much
        MCMC (Markov Chain Monte Carlo) iterations should be computed.
        Higher numbers deliver more exact results at the expense of computation time
        (usually 1000 iterations already give a pretty usable results for testing).
        If a Bayesian model is selected, the user can also modify the
        amount of smoothing. The model already selects an optimal amount of smoothing itself,
        but with this option the user gets the possibility to
        estimate a more or less smoother surface. The model can be estimated by clicking
        on the \"Run\" button."
      ),
      tags$b("Central panel - display, export and selection of map section:"),
      tags$p(
        "The estimated spatial average is displayed in the center.
         An export button can be found directly below,
         which offers various formats for plot export.
         The user can select the desired point in time for the estimate with the
         \"Time selection\" slider below.
         With the lower two sliders the map section can be selected."
      ),
      tags$b("Right panel - graphic options:"),
      tags$p(
        "After estimating the model, several options to modify the displayed plot
         appear on a rightern panel. The user can optionally select
        to display the spreading speed (gradient, in km/year) instead of the estimate
        on the top of the panel. Directly below,
        there is the choice to restrict the display the estimates on the terrestrial
        or aquatic areas or the complete surface. Furthermore,
        the location marks (if selected the size can be influenced bya slider),
        grid, scale and north arrow can be removed from the map.
        The \"Apply convex hull\" checkbox marks,
        whether estimates of locations within the convex hull
        of the data points should be displayed. In general, this is indicated
        as extrapolation beyond areas with available data is highly speculative.
        The slider \"Display up to max standard error\" governs the area where
        we have a standard error below the selected mark to prevent areas
        with low certainty being shown. Right under the colour options,
        a slider \"Plot resolution\" is available. A higher number delivers a
        higher quality picture of the coloured area with less pixaleted borders
        but needs much more drawing time. A lower value is recommended for
        setting the right picture options and map section and a high value for publication."
      ),
      tags$b("Location/Batch estimate:"),
      tags$p(
        "The options on the bottom of the rightern panel enable the user to get an estimate of
        a specified location (\"Center point latitude/longitude\") within a certain radius.
        The estimate with standard error will appear below the x-axis in the displayed plot.
        For getting estimates for a batch of points the
        \"Batch Estimes\" button opens a pop-up window,
        where a user-specified data set can be uploaded.
        A local estimate including standard error and 95 percent uncertainty
        intervals is added and can be exported. Alternatively,
        these estimates can be exported into the \"FRUITS\"-app (not functional yet)."
        ),
      shinyjs::useShinyjs()
        )
  }
}
