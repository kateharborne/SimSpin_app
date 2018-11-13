#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(SimSpin)
library(snapshot)
library(shinycssloaders)

options(shiny.maxRequestSize = 30*1024^2)

# Define UI for application that draws a histogram
shinyUI(navbarPage(

  theme = shinytheme("simplex"),
  # Application title
  title = "SimSpin",
  # sim_analysis panel
  tabPanel("sim_analysis",

           # Sidebar with a file input for simulation to be analysed
           sidebarLayout(
             sidebarPanel(
               fileInput("sim_file", label = "Simulation File:", multiple = FALSE, buttonLabel = "Browse...",
                         placeholder = "No file selected"),
               checkboxGroupInput("ptype", label = "Particle types:",
                                  choiceNames = c("Dark Matter", "Disc", "Bulge"),
                                  choiceValues = c(1, 2, 3), selected = c(2,3)),
               selectInput("DM_profile", label = "Dark Matter Profile:", choices = c("Hernquist", "NFW", "None")),
               # only show this panel if DM_profile == Hernquist
               conditionalPanel(
                 condition = "input.DM_profile == 'Hernquist'",
                 numericInput("DM_mass", label = "Mass:", value = 185.966),
                 numericInput("DM_a", label = "Scale radius:", value = 34.5)
               ),
               # only show this panel if DM_profile == NFW
               conditionalPanel(
                 condition = "input.DM_profile == 'NFW'",
                 numericInput("DM_vm", label = "Virial mass:", value = 185.966),
                 numericInput("DM_a", label = "Scale radius:", value = 34.5),
                 numericInput("DM_rhof", label = "Density at flattening radius:", value = 0.00035)
               ),
               selectInput("bin_type", label = "Segementation:", choices = c("r", "cr", "z"), selected = "r",
                           multiple = FALSE),
               sliderInput("rmax", label = "Maximum radius:", min = 1, max = 500, value = 200, step = 1),
               sliderInput("rbin", label = "Number of segments:", min = 1, max = 1000, value = 200, step = 1),
               actionButton("submit_1", label = "Go!")
             ),

          mainPanel(
            tabsetPanel(
              tabPanel("Segments",
                       h3(htmlOutput("segments_title")),
                       withSpinner(plotOutput("simulation_segments"),  type = 5, color="#581845"),
                       htmlOutput("user_specs")
                       ),
              tabPanel("Kinematics",
                       h3(htmlOutput("logp_title")),
                       withSpinner(plotOutput("logp_dist"),  type = 5, color="#FF5733"),
                       h3(htmlOutput("vrot_title")),
                       withSpinner(plotOutput("vrot_dist"),  type = 5, color="#FFC300"),
                       h5(htmlOutput("vrot_beware"))
                       ),
              tabPanel("Summary",
                       h3(htmlOutput("kin_title")),
                       withSpinner(tableOutput("kinematics"),  type = 5, color="#C70039"),
                       h5(htmlOutput("kin_beware")))
              ))
          )
        ),
  tabPanel("build_datacube",
           sidebarLayout(
             sidebarPanel(
               tags$head(tags$script('$(document).on("shiny:connected", function(e) {
                            Shiny.onInputChange("innerWidth", window.innerWidth);
                                     });
                                     $(window).resize(function(e) {
                                     Shiny.onInputChange("innerWidth", window.innerWidth);
                                     });
                                     ')),
               fileInput("sim_file_2", label = "Simulation File:", multiple = FALSE, buttonLabel = "Browse...",
                         placeholder = "No file selected"),
               checkboxGroupInput("ptype_2", label = "Particle types:",
                                  choiceNames = c("Dark Matter", "Disc", "Bulge"),
                                  choiceValues = c(1, 2, 3), selected = c(2,3)),
               numericInput("r200", label = "Virial radius:", value = 200),
               sliderInput("z", label = "Redshift:", min = 0.01, max = 0.1, value = 0.06),
               selectInput("survey", label = "Survey:", choices = c("SAMI", "MaNGA", "CALIFA", "Specified")),
               # only show this panel if survey == "Specified"
               conditionalPanel(
                 condition = "input.survey == 'Specified'",
                 numericInput("fov", label = "IFU field of view:", value = 15),
                 selectInput("ap_shape", label = "Aperture shape:", choices = c("circular", "square", "hexagonal")),
                 numericInput("central_wvl", label = HTML("Central filter wavelength / &#8491; :"), value = 4800),
                 numericInput("lsf_fwhm", label = "Line spread function:", value = 2.65),
                 sliderInput("pixel_sscale", label = "Spaxel scale / '' :", min = 0.25, max = 2, value = 0.5, step = 0.01),
                 sliderInput("pixel_vscale", label = HTML("Voxel scale / &#8491; :"), min = 0.5, max = 2, value = 1.04, step = 0.01),
                 numericInput("threshold", label = "Magnitude limit:", value = 25)
               ),
               sliderInput("inc_deg", label = "Inclination:", min = 0, max = 90, value = 90, step = 1),
               numericInput("m2l_disc", label = "Disc mass-to-light ratio:", value = 2),
               numericInput("m2l_bulge", label = "Bulge mass-to-light ratio:", value = 1),
               checkboxInput("blur", label = "Blur?", value = FALSE),
               # only show this panel if DM_profile == NFW
               conditionalPanel(
                 condition = "input.blur == true",
                 selectInput("psf", label = "PSF shape:", choices = c("Moffat", "Gaussian")),
                 sliderInput("fwhm", label = "FWHM:", min = 0, max = 5, value = 0.5)
               ),
               actionButton("submit_2", label = "Go!")
               ),
             mainPanel(
               h3(htmlOutput("bc_title")),
               withSpinner(plotOutput("datacube", height = "100%"),  type = 5, color="#581845"),
               h3(htmlOutput("build_sum_title")),
               withSpinner(tableOutput("build_summary"),  type = 5, color="#FF5733")
             )
            )
           ),
  tabPanel("find_lambda",
           sidebarLayout(
             sidebarPanel(
               tags$head(tags$script('$(document).on("shiny:connected", function(e) {
                                     Shiny.onInputChange("innerWidth", window.innerWidth);
                                     });
                                     $(window).resize(function(e) {
                                     Shiny.onInputChange("innerWidth", window.innerWidth);
                                     });
                                     ')),
               fileInput("sim_file_3", label = "Simulation File:", multiple = FALSE, buttonLabel = "Browse...",
                         placeholder = "No file selected"),
               checkboxGroupInput("ptype_3", label = "Particle types:",
                                  choiceNames = c("Dark Matter", "Disc", "Bulge"),
                                  choiceValues = c(1, 2, 3), selected = c(2,3)),
               numericInput("r200_2", label = "Virial radius:", value = 200),
               sliderInput("z_2", label = "Redshift:", min = 0.01, max = 0.1, value = 0.06),
               selectInput("survey_2", label = "Survey:", choices = c("SAMI", "MaNGA", "CALIFA", "Specified")),
               # only show this panel if survey == "Specified"
               conditionalPanel(
                 condition = "input.survey_2 == 'Specified'",
                 numericInput("fov_2", label = "IFU field of view:", value = 15),
                 selectInput("ap_shape_2", label = "Aperture shape:", choices = c("circular", "square", "hexagonal")),
                 numericInput("central_wvl_2", label = HTML("Central filter wavelength / &#8491; :"), value = 4800),
                 numericInput("lsf_fwhm_2", label = "Line spread function:", value = 2.65),
                 sliderInput("pixel_sscale_2", label = "Spaxel scale / '' :", min = 0.25, max = 2, value = 0.5, step = 0.01),
                 sliderInput("pixel_vscale_2", label = HTML("Voxel scale / &#8491; :"), min = 0.5, max = 2, value = 1.04, step = 0.01),
                 numericInput("threshold_2", label = "Magnitude limit:", value = 25)
               ),
               sliderInput("inc_deg_2", label = "Inclination:", min = 0, max = 90, value = 90, step = 1),
               numericInput("m2l_disc_2", label = "Disc mass-to-light ratio:", value = 2),
               numericInput("m2l_bulge_2", label = "Bulge mass-to-light ratio:", value = 1),
               checkboxInput("blur_2", label = "Blur?", value = FALSE),
               # only show this panel if DM_profile == NFW
               conditionalPanel(
                 condition = "input.blur_2 == true",
                 selectInput("psf_2", label = "PSF shape:", choices = c("Moffat", "Gaussian")),
                 sliderInput("fwhm_2", label = "FWHM:", min = 0, max = 5, value = 0.5, step = 0.1)
               ),
               selectInput("measure_type", label = "Measurment radius details:", choices = c("Fit", "Specified", "Fixed")),
               conditionalPanel(
                 condition = "input.measure_type == 'Fit'",
                 sliderInput("fac", label = HTML("Factor / R<sub>eff</sub>:"), min = 0, max = 5, value = 1, step = 0.1)
               ),
               conditionalPanel(
                 condition = "input.measure_type == 'Specified'",
                 sliderInput("fac", label = HTML("Factor / R<sub>eff</sub>:"), min = 0, max = 5, value = 1, step = 0.1),
                 numericInput("ar_a", label = "Semi-major, a / kpc", value = 2),
                 numericInput("ar_b", label = "Semi-minor, b / kpc", value = 1)
               ),
               conditionalPanel(
                 condition = "input.measure_type == 'Fixed'",
                 sliderInput("fac", label = HTML("Factor / R<sub>eff</sub>:"), min = 0, max = 5, value = 1, step = 0.1),
                 numericInput("ar_a", label = "Semi-major, a / kpc", value = 2),
                 numericInput("ar_b", label = "Semi-minor, b / kpc", value = 1)
               ),
               actionButton("submit_3", label = "Go!")
               ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Flux",
                          h3(htmlOutput("fl_flux_title")),
                          withSpinner(plotOutput("fl_flux_plot", height = "100%"),  type = 5, color="#FFC300")
                 ),
                 tabPanel("Velocity",
                          h3(htmlOutput("fl_vel_title")),
                          withSpinner(plotOutput("fl_vel_plot", height = "100%"),  type = 5, color="#C70039")
                 ),
                 tabPanel("Dispersion",
                          h3(htmlOutput("fl_dis_title")),
                          withSpinner(plotOutput("fl_dis_plot", height = "100%"),  type = 5, color="#581845")
                 ),
                 tabPanel("Summary",
                          h3(htmlOutput("find_sum_title")),
                          withSpinner(tableOutput("find_summary"),  type = 5, color="#FF5733")
                 )
               )
             )
           )
  )
))
