
library(shiny)
library(SimSpin)
library(snapshot)

shinyServer(function(input, output, session) {
  
  output$download_title <- renderUI({
    HTML(paste("SimSpin example simulation file <br />"))
  })

  output$download_specs <- renderUI({
    HTML(paste("If you would like to run this application with the default example file, you can download it here: <br /><br />"))
    
  })
  
  output$SimSpin_example <- downloadHandler(
    filename <- function() {
      paste("www/SimSpin_example", "hdf5", sep = ".")
    },
    content <- function(file) {
      file.copy("SimSpin_example.hdf5", file)
    },
    contentType = "application/hdf5"
  )
  
  # sim_analysis() tab ----------------------------------------------------------------------------
  
  df <- reactive({
    if (input$DM_profile == "Hernquist"){
      galaxy_file = SimSpin::sim_data(filename = input$sim_file$datapath,
                                      ptype = input$ptype)
      SimSpin::sim_analysis(simdata = galaxy_file,
                            bin_type = input$bin_type,
                            rmax = input$rmax,
                            rbin = input$rbin,
                            DM_profile = list("profile" = "Hernquist", "DM_mass" = input$DM_mass, "DM_a" = input$DM_a))
    } else if (input$DM_profile == "NFW"){
      galaxy_file = SimSpin::sim_data(filename = input$sim_file$datapath,
                                      ptype = input$ptype)
      SimSpin::sim_analysis(simdata = galaxy_file,
                            bin_type = input$bin_type,
                            rmax = input$rmax,
                            rbin = input$rbin,
                            DM_profile = list("profile"="NFW", "DM_vm" = input$DM_vm, "DM_a" = input$DM_a, "DM_rhof" = input$DM_rhof))
    } else if (input$DM_profile == "None") {
      galaxy_file = SimSpin::sim_data(filename = input$sim_file$datapath,
                                      ptype = input$ptype)
      validate(
        need(try(SimSpin::sim_analysis(simdata = galaxy_file,
                                       bin_type = input$bin_type,
                                       rmax = input$rmax,
                                       rbin = input$rbin,
                                       DM_profile = NA)), "DMpart Error: There are no dark matter particles in this model. Describe an analytic potential to calculate the total kinematic profile correctly.")
      )
      SimSpin::sim_analysis(simdata = galaxy_file,
                            bin_type = input$bin_type,
                            rmax = input$rmax,
                            rbin = input$rbin,
                            DM_profile = NA)

    }
  }) # running SimSpin::sim_analysis() outside the output generation to be accessed by each

  pal <- reactive({
    colorRampPalette(rev(RColorBrewer::brewer.pal(10,'Spectral')))(8)
  }) # colours for plots

  output$segments_title <- renderUI({
    HTML(paste("Simulation segments"))
  }) # header for segments

  output$simulation_segments <- renderPlot({
    if (input$submit_1 == 0){
      return()
    } # only create plot once "submit_1" button is pressed

    isolate({

      if (input$rmax >= input$rbin){            # if the max radius is greater than the number of bins 
        if (input$rmax / input$rbin < 5){       # if the spacing between bins is less than 2kpc
          lout = input$rmax / 5
        } else {lout = input$rbin}
      } else if (input$rbin > input$rmax){
        if (input$rmax / input$rbin < 5){
          lout = input$rmax / 5
        } else {lout = input$rbin}
      }

      if (input$bin_type == "r" | input$bin_type == "cr"){
        magicaxis::magplot(df()$part_data$x, df()$part_data$y, pch=".",
                           xlim = c(-(input$rmax + 10), input$rmax + 10),
                           ylim = c(-(input$rmax + 10), input$rmax + 10),
                           xlab = "x, kpc", ylab = "y, kpc", col="black", asp=1)
        plotrix::draw.circle(0,0, seq(input$rmax/input$rbin, input$rmax, length.out = lout), border="red")
      } else if (input$bin_type == "z"){
        magicaxis::magplot(df()$part_data$x, df()$part_data$z, pch=".",
                           xlim = c(-(input$rmax + 10), input$rmax + 10),
                           ylim = c(-(input$rmax + 10), input$rmax + 10),
                           xlab = "x, kpc", ylab = "z, kpc", col="black", asp=1)
        abline(h=0, col="red")
        abline(h=seq(input$rmax/input$rbin, input$rmax, length.out = lout), col="red")
      }
    })
  }) # drawing the particle distributions and bin divisions

  output$user_specs <- renderUI({
    if (input$submit_1 == 0){
      return()
    }
    isolate({
      galaxy_file = SimSpin::sim_data(filename = input$sim_file$datapath,
                                      ptype = input$ptype)
      ptypes = names(galaxy_file)
      p_dat = tags$b("This analysis contains:")
      DM_num = sprintf("%i Dark matter particles", if("PartType0" %in% ptypes){nrow(galaxy_file$PartType0$Part)}else{0})
      D_num  = sprintf("%i Disc particles", if("PartType2" %in% ptypes){nrow(galaxy_file$PartType2$Part)}else{0})
      B_num  = sprintf("%i Bulge particles", if("PartType3" %in% ptypes){nrow(galaxy_file$PartType3$Part)}else{0})
      u_choice = sprintf("You have chosen to divide this into <b>%i</b> '%s' bin(s) out to a maximum radius of <b>%i</b>.", input$rbin, input$bin_type, input$rmax)
      if (input$DM_profile == "Hernquist"){
        anpot_num = sprintf("You have supplied an <b><u>Hernquist</u></b> analytic potential with <u>total mass</u> of <b>%.3f</b> and <u>halo scale radius</u> of <b>%.1f</b>.", input$DM_mass, input$DM_a)
        HTML(paste("", p_dat, DM_num, D_num, B_num, "", anpot_num, "", u_choice, sep="<br/>"))
      } else if (input$DM_profile == "NFW"){
        anpot_num = sprintf("You have supplied an <b><u>NFW</u></b> analytic potential with a <u>virial mass</u> of <b>%.3f</b>, <u>halo scale radius</u> of <b>%.1f</b>, and a <u>density at the flattening radius</u> of <b>%.3f</b>", input$DM_vm, input$DM_a, input$DM_rhof)
        HTML(paste("", p_dat, DM_num, D_num, B_num, "", anpot_num, "", u_choice, sep="<br/>"))
      } else if (input$DM_profile == "None" && df()$head_data$Npart[2] == 0){
        anpot_num = sprintf("<b>BEWARE:</b> This simulation does not contain any dark matter particles and you have <emph>NOT</emph> specified an analytic potential. The kinematic velocity distribution will not be correct in this case.")
        HTML(paste("", p_dat, DM_num, D_num, B_num, "", anpot_num, "", u_choice, sep="<br/>"))
      } else {
        HTML(paste("", p_dat, DM_num, D_num, B_num, "", u_choice, sep="<br/>"))
      }
    })
  }) # writing the user specifications being used for kinematic calculation

  output$logp_title <- renderUI({
    HTML(paste("Density log(&#961;) distribution"))
    }) # header for density plot

  output$logp_dist <- renderPlot({if (input$submit_1 == 0){
    return()
  } # only create plot once "submit_1" button is pressed

    isolate({
      if (input$bin_type == "r"){
        magicaxis::magplot(df()$profile$r, df()$profile$logp, type="l", lty=1, lwd=3,
                           xlim = c(0, input$rmax),
                           xlab = "r, kpc", ylab = expression("log"[10]*"("*paste(rho)*"),  10"^{10}*" M"['\u0298']*"kpc"^{-3}),
                           col = pal()[1])
      } else if (input$bin_type == "cr"){
        magicaxis::magplot(df()$profile$cr, df()$profile$logp, type="l", lty=1, lwd=3,
                           xlim = c(0, input$rmax),
                           xlab = "cr, kpc", ylab = expression("log"[10]*"("*paste(rho)*"),  10"^{10}*" M"['\u0298']*"kpc"^{-3}),
                           col = pal()[1])
      } else if (input$bin_type == "z"){
        magicaxis::magplot(df()$profile$z, df()$profile$logp, type="l", lty=1, lwd=3,
                           xlim = c(0, input$rmax),
                           xlab = "z, kpc", ylab = expression("log"[10]*"("*paste(rho)*"),  10"^{10}*" M"['\u0298']*"kpc"^{-3}),
                           col = pal()[1])
      }
    })

  }) # plot density distribution

  output$vrot_title <- renderUI({
    HTML(paste("Velocity distribution"))
  }) # header for vrot plot

  output$vrot_dist <- renderPlot({
    if (input$submit_1 == 0){
      return()
      } # only create plot once "submit_1" button is pressed

    isolate({
      if (input$bin_type == "r"){
        magicaxis::magplot(df()$profile$r, df()$profile$vc, type="l", lty=1, lwd=3,
                           xlim = c(0, input$rmax),
                           xlab = "r, kpc", ylab = expression("v"[c]*", km/s"),
                           col = pal()[6])
      } else if (input$bin_type == "cr"){
        magicaxis::magplot(df()$profile$cr, df()$profile$vcr, type="p", pch=16, lwd=3,
                           xlim = c(0, input$rmax),
                           xlab = "cr, kpc", ylab = expression("v"[cr]*", km/s"),
                           col = pal()[6])
        s_vcr = sqrt(mean(df()$profile$vcr * df()$profile$vcr)-(mean(df()$profile$vcr))^2)
        abline(h=c(-s_vcr, s_vcr), col=pal()[8])
      } else if (input$bin_type == "z"){
        magicaxis::magplot(df()$profile$z, df()$profile$vz, type="p", pch=16, lwd=3,
                           xlim = c(0, input$rmax),
                           xlab = "z, kpc", ylab = expression("v"[z]*", km/s"),
                           col = pal()[6])
        s_vz = sqrt(mean(df()$profile$vz * df()$profile$vz, na.rm = T)-(mean(df()$profile$vz, na.rm = T))^2)
        abline(h=c(-s_vz, s_vz), col=pal()[8])
      }
    })

  }) # plot velocity distribution

  output$vrot_beware <- renderUI({
    if (input$submit_1 == 0){
      return()
    }
    isolate({
      if (input$DM_profile == "None" && df()$head_data$Npart[2] == 0){
        beware_1 = sprintf("<b>BEWARE:</b> This simulation does not contain any dark matter particles and you have <emph>NOT</emph> specified an analytic potential. The kinematic velocity distribution will not be correct in this case.")
        HTML(paste("", beware_1, "", sep="<br/>"))
      }
    })
  }) # writing beware if the analytic potential is not specified and no DM particles exist in the simulation

  output$kin_title <- renderUI({
    HTML(paste("General kinematic properties"))
  }) # header for kinematics table

  output$kinematics <- renderTable({
    if (input$submit_1 == 0){
      return()
    } # only create plot once "submit_1" button is pressed

    isolate({
      if (input$bin_type == "r"){
        kin_dat = data.frame("r<sub>max</sub> / kpc" = input$rmax,
                             "M(r<sub>max</sub>) / 10<sup>10</sup>M<sub>&#9737;</sub>" = max(df()$profile$Mass, na.rm = T),
                             "max(v<sub>c</sub>) / kms<sup>-1</sup>" = max(df()$profile$vc, na.rm = T),
                             "&#955;'(r<sub>max</sub>)" = df()$profile$lambda[input$rbin],
                             "&#963;<sub>z</sub> / kpc" = df()$sigma_z,
                             check.names = F)

      } else if (input$bin_type == "cr"){
        kin_dat = data.frame("r<sub>max</sub> / kpc" = input$rmax,
                             "M(r<sub>max</sub>) / 10<sup>10</sup>M<sub>&#9737;</sub>" = max(df()$profile$Mass, na.rm = T),
                             "&#963;<sub>vcr</sub> / kms<sup>-1</sup>" = sqrt(mean(df()$profile$vcr * df()$profile$vcr, na.rm = T)-(mean(df()$profile$vcr, na.rm = T))^2),
                             "&#963;<sub>z</sub> / kpc" = df()$sigma_z,
                             check.names = F)

      } else if (input$bin_type == "z"){
        kin_dat = data.frame("r<sub>max</sub> / kpc" = input$rmax,
                             "M(r<sub>max</sub>) / 10<sup>10</sup>M<sub>&#9737;</sub>" = max(df()$profile$Mass),
                             "&#963;<sub>vz</sub> / kms<sup>-1</sup>" = sqrt(mean(df()$profile$vz * df()$profile$vz, na.rm = T)-(mean(df()$profile$vz, na.rm = T))^2),
                             "&#963;<sub>z</sub> / kpc" = df()$sigma_z,
                             check.names = F)
      }

    })
  }, sanitize.colnames.function = function(x) x) # kinematics table

  output$kin_beware <- renderUI({
    if (input$submit_1 == 0){
      return()
    }
    isolate({
      if (input$DM_profile == "None" && df()$head_data$Npart[2] == 0){
        beware_2 = sprintf("<b>BEWARE:</b> This simulation does not contain any dark matter particles and you have <emph>NOT</emph> specified an analytic potential. The kinematic velocity distribution will not be correct in this case.")
        HTML(paste("", beware_2, "", sep="<br/>"))
      }
    })
  }) # writing beware if the analytic potential is not specified and no DM particles exist in the simulation

  # build_datacube() tab --------------------------------------------------------------------------
  dc <- reactive({
    if (input$blur == "FALSE"){
      if (input$survey == "SAMI"){
        galaxy_file = SimSpin::sim_data(filename = input$sim_file_2$datapath,
                                        ptype = input$ptype_2,
                                        m2l_disc = input$m2l_disc,
                                        m2l_bulge = input$m2l_bulge)
        SimSpin::build_datacube(simdata = galaxy_file,
                                r200 = input$r200,
                                z = input$z,
                                fov = 15,
                                ap_shape = "circular",
                                central_wvl = 4800,
                                lsf_fwhm = 2.65,
                                pixel_sscale = 0.5,
                                pixel_vscale = 1.04,
                                inc_deg = input$inc_deg,
                                threshold = 25)
      } else if (input$survey == "MaNGA"){
        galaxy_file = SimSpin::sim_data(filename = input$sim_file_2$datapath,
                                        ptype = input$ptype_2,
                                        m2l_disc = input$m2l_disc,
                                        m2l_bulge = input$m2l_bulge)
        SimSpin::build_datacube(simdata = galaxy_file,
                                r200 = input$r200,
                                z = input$z,
                                fov = 22,
                                ap_shape = "hexagonal",
                                central_wvl = 4950,
                                lsf_fwhm = 2.8,
                                pixel_sscale = 0.25,
                                pixel_vscale = 1.2,
                                inc_deg = input$inc_deg,
                                threshold = 25)
      } else if (input$survey == "CALIFA"){
        galaxy_file = SimSpin::sim_data(filename = input$sim_file_2$datapath,
                                        ptype = input$ptype_2,
                                        m2l_disc = input$m2l_disc,
                                        m2l_bulge = input$m2l_bulge)
        SimSpin::build_datacube(simdata = galaxy_file,
                                r200 = input$r200,
                                z = input$z,
                                fov = 30,
                                ap_shape = "hexagonal",
                                central_wvl = 4200,
                                lsf_fwhm = 5.65,
                                pixel_sscale = 1,
                                pixel_vscale = 2,
                                inc_deg = input$inc_deg,
                                threshold = 25)
      } else if (input$survey == "Specified"){
        galaxy_file = SimSpin::sim_data(filename = input$sim_file_2$datapath,
                                        ptype = input$ptype_2,
                                        m2l_disc = input$m2l_disc,
                                        m2l_bulge = input$m2l_bulge)
        SimSpin::build_datacube(simdata = galaxy_file,
                                r200 = input$r200,
                                z = input$z,
                                fov = input$fov,
                                ap_shape = input$ap_shape,
                                central_wvl = input$central_wvl,
                                lsf_fwhm = input$lsf_fwhm,
                                pixel_sscale = input$pixel_sscale,
                                pixel_vscale = input$pixel_vscale,
                                inc_deg = input$inc_deg,
                                threshold = input$threshold)
      }

      
    } else if (input$blur == "TRUE"){
      if (input$survey == "SAMI"){
        galaxy_file = SimSpin::sim_data(filename = input$sim_file_2$datapath,
                                        ptype = input$ptype_2,
                                        m2l_disc = input$m2l_disc,
                                        m2l_bulge = input$m2l_bulge)
        SimSpin::build_datacube(simdata = galaxy_file,
                                r200 = input$r200,
                                z = input$z,
                                fov = 15,
                                ap_shape = "circular",
                                central_wvl = 4800,
                                lsf_fwhm = 2.65,
                                pixel_sscale = 0.5,
                                pixel_vscale = 1.04,
                                inc_deg = input$inc_deg,
                                threshold = 25,
                                blur = list("psf" = input$psf, "fwhm" = input$fwhm))
      } else if (input$survey == "MaNGA"){
        galaxy_file = SimSpin::sim_data(filename = input$sim_file_2$datapath,
                                        ptype = input$ptype_2,
                                        m2l_disc = input$m2l_disc,
                                        m2l_bulge = input$m2l_bulge)
        SimSpin::build_datacube(simdata = galaxy_file,
                                r200 = input$r200,
                                z = input$z,
                                fov = 22,
                                ap_shape = "hexagonal",
                                central_wvl = 4950,
                                lsf_fwhm = 2.8,
                                pixel_sscale = 0.25,
                                pixel_vscale = 1.2,
                                inc_deg = input$inc_deg,
                                m2l_disc = input$m2l_disc,
                                m2l_bulge = input$m2l_bulge,
                                threshold = 25,
                                blur = list("psf" = input$psf, "fwhm" = input$fwhm))
      } else if (input$survey == "CALIFA"){
        galaxy_file = SimSpin::sim_data(filename = input$sim_file_2$datapath,
                                        ptype = input$ptype_2,
                                        m2l_disc = input$m2l_disc,
                                        m2l_bulge = input$m2l_bulge)
        SimSpin::build_datacube(simdata = galaxy_file,
                                r200 = input$r200,
                                z = input$z,
                                fov = 30,
                                ap_shape = "hexagonal",
                                central_wvl = 4200,
                                lsf_fwhm = 5.65,
                                pixel_sscale = 1,
                                pixel_vscale = 2,
                                inc_deg = input$inc_deg,
                                m2l_disc = input$m2l_disc,
                                m2l_bulge = input$m2l_bulge,
                                threshold = 25,
                                blur = list("psf" = input$psf, "fwhm" = input$fwhm))
      } else if (input$survey == "Specified"){
        galaxy_file = SimSpin::sim_data(filename = input$sim_file_2$datapath,
                                        ptype = input$ptype_2,
                                        m2l_disc = input$m2l_disc,
                                        m2l_bulge = input$m2l_bulge)
        SimSpin::build_datacube(simdata = galaxy_file,
                                r200 = input$r200,
                                z = input$z,
                                fov = input$fov,
                                ap_shape = input$ap_shape,
                                central_wvl = input$central_wvl,
                                lsf_fwhm = input$lsf_fwhm,
                                pixel_sscale = input$pixel_sscale,
                                pixel_vscale = input$pixel_vscale,
                                inc_deg = input$inc_deg,
                                m2l_disc = input$m2l_disc,
                                m2l_bulge = input$m2l_bulge,
                                threshold = input$threshold,
                                blur = list("psf" = input$psf, "fwhm" = input$fwhm))
      }
    }
  }) # running SimSpin::build_datacube() outside the output generation to be accessed by each

  output$bc_title <- renderUI({
    HTML(paste("Velocity data cube"))
  }) # header for build_datacube() tab

  output$datacube <- renderPlot({
    if (input$submit_2 == 0){
      return()
    } # only create plot once "submit_2" button is pressed

    isolate({

      dc_dim = dim(dc()$datacube)
      v_len = floor(dc_dim[3]*0.5)
      shuffle_dc = array(data = NA, dim = c(dc_dim[1], dc_dim[3], dc_dim[2]))
      test = array(data = 0, dim = c(dc_dim[1], dc_dim[3], dc_dim[2]))
      for (n in 1:dc_dim[1]){
        shuffle_dc[,,n] = dc()$datacube[,n,]
      }
      shuffle_dc[shuffle_dc < (max(shuffle_dc) * 0.05)] = NA

      plot3D::slice3D(x = seq(dc()$xbin_labels[1], dc()$xbin_labels[dc_dim[1]], length.out = dc_dim[1]),
                      y = seq(dc()$vbin_labels[floor((dc_dim[3]/2)-(v_len/2))], dc()$vbin_labels[floor((dc_dim[3]/2)+(v_len/2))], length.out = v_len),
                      z = seq(dc()$xbin_labels[1], dc()$xbin_labels[dc_dim[1]], length.out = dc_dim[1]),
                      colvar = shuffle_dc[,seq(floor((dc_dim[3]/2)-(v_len/2)), floor((dc_dim[3]/2)+(v_len/2)), length.out = v_len),],
                      ys = seq(dc()$vbin_labels[floor((dc_dim[3]/2)-(v_len/2))], dc()$vbin_labels[floor((dc_dim[3]/2)+(v_len/2))], length.out = v_len),
                      zs = NULL, xs = NULL,
                      phi = 50, theta = 35, colkey = FALSE,
                      NAcol=rgb(1,1,1,alpha=0.01),
                      xlab = "x", ylab = "velocity, km/s",
                      zlab = "y", ticktype = "detailed",
                      nticks = c(10,10,v_len))

      plot3D::slicecont3D(x = seq(dc()$xbin_labels[1], dc()$xbin_labels[dc_dim[1]], length.out = dc_dim[1]),
                          y = seq(dc()$vbin_labels[floor((dc_dim[3]/2)-(v_len/2))], dc()$vbin_labels[floor((dc_dim[3]/2)+(v_len/2))], length.out = v_len),
                          z = seq(dc()$xbin_labels[1], dc()$xbin_labels[dc_dim[1]], length.out = dc_dim[1]),
                          colvar = test[,seq(floor((dc_dim[3]/2)-(v_len/2)), floor((dc_dim[3]/2)+(v_len/2)), length.out = v_len),],
                          ys = seq(dc()$vbin_labels[floor((dc_dim[3]/2)-(v_len/2))], dc()$vbin_labels[floor((dc_dim[3]/2)+(v_len/2))], length.out = v_len),
                          zs = NULL, xs = NULL,
                          colkey = FALSE,
                          NAcol=rgb(1,1,1,alpha=0.01),
                          add=TRUE, border="black")

    })

  }, height=reactive(ifelse(!is.null(input$innerWidth),input$innerWidth*3/5,0)))
  # datacube plot

  output$build_sum_title <- renderUI({
    HTML(paste("Data cube summary"))
  }) # header for build_datacube() table

  output$build_summary <- renderTable({
    if (input$submit_2 == 0){
      return()
    } # only create plot once "submit_2" button is pressed

    isolate({
      if (input$blur == "FALSE"){
        build_dat = data.frame("Spaxel size / kpc" = abs(dc()$xbin_labels[2] - dc()$xbin_labels[1]),
                               "Voxel size / kms<sup>-1</sup>" = abs(dc()$vbin_labels[2] - dc()$vbin_labels[1]),
                               "Semi-major, a / kpc" = dc()$axis_ratio$a,
                               "Semi-minor, b / kpc" = dc()$axis_ratio$b,
                               check.names = F)

      } else if (input$blur == "TRUE"){
        build_dat = data.frame("Spaxel size / kpc" = abs(dc()$xbin_labels[2] - dc()$xbin_labels[1]),
                               "Voxel size / kms<sup>-1</sup>" = abs(dc()$vbin_labels[2] - dc()$vbin_labels[1]),
                               "Semi-major, a / kpc" = dc()$axis_ratio$a,
                               "Semi-minor, b / kpc" = dc()$axis_ratio$b,
                               "FWHM / ''" = input$fwhm,
                               check.names = F)
      }
    })
  }, sanitize.colnames.function = function(x) x) # kinematics table
  
  # find_lambda() tab -----------------------------------------------------------------------------
  fl <- reactive({

    if (input$blur_2 == "FALSE" && input$measure_type == "Fit" && input$survey_2 == "SAMI"){
      galaxy_file = SimSpin::sim_data(filename = input$sim_file_3$datapath,
                                      ptype = input$ptype_3,
                                      m2l_disc = input$m2l_disc_2,
                                      m2l_bulge = input$m2l_bulge_2)
      SimSpin::find_lambda(simdata = galaxy_file,
                           r200 = input$r200_2,
                           z = input$z_2,
                           fov = 15,
                           ap_shape = "circular",
                           central_wvl = 4800,
                           lsf_fwhm = 2.65,
                           pixel_sscale = 0.5,
                           pixel_vscale = 1.04,
                           inc_deg = input$inc_deg_2,
                           threshold = 25,
                           measure_type = list("type"="fit", "fac"=input$fac),
                           IFU_plot = FALSE)
    } else if (input$blur_2 == "FALSE" && input$measure_type == "Fit" && input$survey_2 == "MaNGA"){
      galaxy_file = SimSpin::sim_data(filename = input$sim_file_3$datapath,
                                      ptype = input$ptype_3,
                                      m2l_disc = input$m2l_disc_2,
                                      m2l_bulge = input$m2l_bulge_2)
      SimSpin::find_lambda(simdata = galaxy_file,
                           r200 = input$r200_2,
                           z = input$z_2,
                           fov = 22,
                           ap_shape = "hexagonal",
                           central_wvl = 4950,
                           lsf_fwhm = 2.8,
                           pixel_sscale = 0.25,
                           pixel_vscale = 1.2,
                           inc_deg = input$inc_deg_2,
                           threshold = 30,
                           measure_type = list("type"="fit", "fac"=input$fac),
                           IFU_plot = FALSE)
    } else if (input$blur_2 == "FALSE" && input$measure_type == "Fit" && input$survey_2 == "CALIFA"){
      galaxy_file = SimSpin::sim_data(filename = input$sim_file_3$datapath,
                                      ptype = input$ptype_3,
                                      m2l_disc = input$m2l_disc_2,
                                      m2l_bulge = input$m2l_bulge_2)
      SimSpin::find_lambda(simdata = galaxy_file,
                           r200 = input$r200_2,
                           z = input$z_2,
                           fov = 30,
                           ap_shape = "hexagonal",
                           central_wvl = 4200,
                           lsf_fwhm = 5.65,
                           pixel_sscale = 1,
                           pixel_vscale = 2,
                           inc_deg = input$inc_deg_2,
                           threshold = 25,
                           measure_type = list("type"="fit", "fac"=input$fac),
                           IFU_plot = FALSE)
    } else if (input$blur_2 == "FALSE" && input$measure_type == "Fit" && input$survey_2 == "Specified"){
      galaxy_file = SimSpin::sim_data(filename = input$sim_file_3$datapath,
                                      ptype = input$ptype_3,
                                      m2l_disc = input$m2l_disc_2,
                                      m2l_bulge = input$m2l_bulge_2)
      SimSpin::find_lambda(simdata = galaxy_file,
                           r200 = input$r200_2,
                           z = input$z_2,
                           fov = input$fov_2,
                           ap_shape = input$ap_shape_2,
                           central_wvl = input$central_wvl_2,
                           lsf_fwhm = input$lsf_fwhm_2,
                           pixel_sscale = input$pixel_sscale_2,
                           pixel_vscale = input$pixel_vscale_2,
                           inc_deg = input$inc_deg_2,
                           threshold = input$threshold_2,
                           measure_type = list("type"="fit", "fac"=input$fac),
                           IFU_plot = FALSE)
      
    } else if (input$blur_2 == "FALSE" && input$measure_type == "Specified" && input$survey_2 == "SAMI"){
      galaxy_file = SimSpin::sim_data(filename = input$sim_file_3$datapath,
                                      ptype = input$ptype_3,
                                      m2l_disc = input$m2l_disc_2,
                                      m2l_bulge = input$m2l_bulge_2)
      SimSpin::find_lambda(simdata = galaxy_file,
                           r200 = input$r200_2,
                           z = input$z_2,
                           fov = 15,
                           ap_shape = "circular",
                           central_wvl = 4800,
                           lsf_fwhm = 2.65,
                           pixel_sscale = 0.5,
                           pixel_vscale = 1.04,
                           inc_deg = input$inc_deg_2,
                           threshold = 25,
                           measure_type = list("type"="specified",
                                               "fract"=input$fract,
                                               "axis_ratio"=data.frame("a"=input$ar_a, "b"=input$ar_b)),
                           IFU_plot = FALSE)
    } else if (input$blur_2 == "FALSE" && input$measure_type == "Specified" && input$survey_2 == "MaNGA"){
      galaxy_file = SimSpin::sim_data(filename = input$sim_file_3$datapath,
                                      ptype = input$ptype_3,
                                      m2l_disc = input$m2l_disc_2,
                                      m2l_bulge = input$m2l_bulge_2)
      SimSpin::find_lambda(simdata = galaxy_file,
                           r200 = input$r200_2,
                           z = input$z_2,
                           fov = 22,
                           ap_shape = "hexagonal",
                           central_wvl = 4950,
                           lsf_fwhm = 2.8,
                           pixel_sscale = 0.25,
                           pixel_vscale = 1.2,
                           inc_deg = input$inc_deg_2,
                           threshold = 30,
                           measure_type = list("type"="specified",
                                               "fract"=input$fract,
                                               "axis_ratio"=data.frame("a"=input$ar_a, "b"=input$ar_b)),
                           IFU_plot = FALSE)
    } else if (input$blur_2 == "FALSE" && input$measure_type == "Specified" && input$survey_2 == "CALIFA"){
      galaxy_file = SimSpin::sim_data(filename = input$sim_file_3$datapath,
                                      ptype = input$ptype_3,
                                      m2l_disc = input$m2l_disc_2,
                                      m2l_bulge = input$m2l_bulge_2)
      SimSpin::find_lambda(simdata = galaxy_file,
                           r200 = input$r200_2,
                           z = input$z_2,
                           fov = 30,
                           ap_shape = "hexagonal",
                           central_wvl = 4200,
                           lsf_fwhm = 5.65,
                           pixel_sscale = 1,
                           pixel_vscale = 2,
                           inc_deg = input$inc_deg_2,
                           threshold = 25,
                           measure_type = list("type"="specified",
                                               "fract"=input$fract,
                                               "axis_ratio"=data.frame("a"=input$ar_a, "b"=input$ar_b)),
                           IFU_plot = FALSE)
    } else if (input$blur_2 == "FALSE" && input$measure_type == "Specified" && input$survey_2 == "Specified"){
      galaxy_file = SimSpin::sim_data(filename = input$sim_file_3$datapath,
                                      ptype = input$ptype_3,
                                      m2l_disc = input$m2l_disc_2,
                                      m2l_bulge = input$m2l_bulge_2)
      SimSpin::find_lambda(simdata = galaxy_file,
                           r200 = input$r200_2,
                           z = input$z_2,
                           fov = input$fov_2,
                           ap_shape = input$ap_shape_2,
                           central_wvl = input$central_wvl_2,
                           lsf_fwhm = input$lsf_fwhm_2,
                           pixel_sscale = input$pixel_sscale_2,
                           pixel_vscale = input$pixel_vscale_2,
                           inc_deg = input$inc_deg_2,
                           threshold = input$threshold_2,
                           measure_type = list("type"="specified",
                                               "fract"=input$fract,
                                               "axis_ratio"=data.frame("a"=input$ar_a, "b"=input$ar_b)),
                           IFU_plot = FALSE)

    } else if (input$blur_2 == "FALSE" && input$measure_type == "Fixed" && input$survey_2 == "SAMI"){
      galaxy_file = SimSpin::sim_data(filename = input$sim_file_3$datapath,
                                      ptype = input$ptype_3,
                                      m2l_disc = input$m2l_disc_2,
                                      m2l_bulge = input$m2l_bulge_2)
     SimSpin::find_lambda(simdata = galaxy_file,
                          r200 = input$r200_2,
                          z = input$z_2,
                          fov = 15,
                          ap_shape = "circular",
                          central_wvl = 4800,
                          lsf_fwhm = 2.65,
                          pixel_sscale = 0.5,
                          pixel_vscale = 1.04,
                          inc_deg = input$inc_deg_2,
                          threshold = 25,
                          measure_type = list("type"="fixed",
                                              "fac"=input$fac,
                                              "axis_ratio"=data.frame("a"=input$ar_a, "b"=input$ar_b)),
                          IFU_plot = FALSE)
    } else if (input$blur_2 == "FALSE" && input$measure_type == "Fixed" && input$survey_2 == "MaNGA"){
      galaxy_file = SimSpin::sim_data(filename = input$sim_file_3$datapath,
                                      ptype = input$ptype_3,
                                      m2l_disc = input$m2l_disc_2,
                                      m2l_bulge = input$m2l_bulge_2)
      SimSpin::find_lambda(simdata = galaxy_file,
                           r200 = input$r200_2,
                           z = input$z_2,
                           fov = 22,
                           ap_shape = "hexagonal",
                           central_wvl = 4950,
                           lsf_fwhm = 2.8,
                           pixel_sscale = 0.25,
                           pixel_vscale = 1.2,
                           inc_deg = input$inc_deg_2,
                           threshold = 30,
                           measure_type = list("type"="fixed",
                                               "fac"=input$fac,
                                               "axis_ratio"=data.frame("a"=input$ar_a, "b"=input$ar_b)),
                           IFU_plot = FALSE)
    } else if (input$blur_2 == "FALSE" && input$measure_type == "Fixed" && input$survey_2 == "CALIFA"){
      galaxy_file = SimSpin::sim_data(filename = input$sim_file_3$datapath,
                                      ptype = input$ptype_3,
                                      m2l_disc = input$m2l_disc_2,
                                      m2l_bulge = input$m2l_bulge_2)
      SimSpin::find_lambda(simdata = galaxy_file,
                           r200 = input$r200_2,
                           z = input$z_2,
                           fov = 30,
                           ap_shape = "hexagonal",
                           central_wvl = 4200,
                           lsf_fwhm = 5.65,
                           pixel_sscale = 1,
                           pixel_vscale = 2,
                           inc_deg = input$inc_deg_2,
                           threshold = 25,
                           measure_type = list("type"="fixed",
                                               "fac"=input$fac,
                                               "axis_ratio"=data.frame("a"=input$ar_a, "b"=input$ar_b)),
                           IFU_plot = FALSE)
    } else if (input$blur_2 == "FALSE" && input$measure_type == "Fixed" && input$survey_2 == "Specified"){
      galaxy_file = SimSpin::sim_data(filename = input$sim_file_3$datapath,
                                      ptype = input$ptype_3,
                                      m2l_disc = input$m2l_disc_2,
                                      m2l_bulge = input$m2l_bulge_2)
      SimSpin::find_lambda(simdata = galaxy_file,
                           r200 = input$r200_2,
                           z = input$z_2,
                           fov = input$fov_2,
                           ap_shape = input$ap_shape_2,
                           central_wvl = input$central_wvl_2,
                           lsf_fwhm = input$lsf_fwhm_2,
                           pixel_sscale = input$pixel_sscale_2,
                           pixel_vscale = input$pixel_vscale_2,
                           inc_deg = input$inc_deg_2,
                           threshold = input$threshold_2,
                           measure_type = list("type"="fixed",
                                               "fac"=input$fac,
                                               "axis_ratio"=data.frame("a"=input$ar_a, "b"=input$ar_b)),
                           IFU_plot = FALSE)

    } else if (input$blur_2 == "TRUE" && input$measure_type == "Fit" && input$survey_2 == "SAMI"){
      galaxy_file = SimSpin::sim_data(filename = input$sim_file_3$datapath,
                                      ptype = input$ptype_3,
                                      m2l_disc = input$m2l_disc_2,
                                      m2l_bulge = input$m2l_bulge_2)
      SimSpin::find_lambda(simdata = galaxy_file,
                           r200 = input$r200_2,
                           z = input$z_2,
                           fov = 15,
                           ap_shape = "circular",
                           central_wvl = 4800,
                           lsf_fwhm = 2.65,
                           pixel_sscale = 0.5,
                           pixel_vscale = 1.04,
                           inc_deg = input$inc_deg_2,
                           threshold = 25,
                           measure_type = list("type"="fit", "fac"=input$fac),
                           blur = list("psf"=input$psf_2, "fwhm"=input$fwhm_2),
                           IFU_plot = FALSE)
    } else if (input$blur_2 == "TRUE" && input$measure_type == "Fit" && input$survey_2 == "MaNGA"){
      galaxy_file = SimSpin::sim_data(filename = input$sim_file_3$datapath,
                                      ptype = input$ptype_3,
                                      m2l_disc = input$m2l_disc_2,
                                      m2l_bulge = input$m2l_bulge_2)
      SimSpin::find_lambda(simdata = galaxy_file,
                           r200 = input$r200_2,
                           z = input$z_2,
                           fov = 22,
                           ap_shape = "hexagonal",
                           central_wvl = 4950,
                           lsf_fwhm = 2.8,
                           pixel_sscale = 0.25,
                           pixel_vscale = 1.2,
                           inc_deg = input$inc_deg_2,
                           threshold = 30,
                           measure_type = list("type"="fit", "fac"=input$fac),
                           blur = list("psf"=input$psf_2, "fwhm"=input$fwhm_2),
                           IFU_plot = FALSE)
    } else if (input$blur_2 == "TRUE" && input$measure_type == "Fit" && input$survey_2 == "CALIFA"){
      galaxy_file = SimSpin::sim_data(filename = input$sim_file_3$datapath,
                                      ptype = input$ptype_3,
                                      m2l_disc = input$m2l_disc_2,
                                      m2l_bulge = input$m2l_bulge_2)
      SimSpin::find_lambda(simdata = galaxy_file,
                           r200 = input$r200_2,
                           z = input$z_2,
                           fov = 30,
                           ap_shape = "hexagonal",
                           central_wvl = 4200,
                           lsf_fwhm = 5.65,
                           pixel_sscale = 1,
                           pixel_vscale = 2,
                           inc_deg = input$inc_deg_2,
                           threshold = 25,
                           measure_type = list("type"="fit", "fac"=input$fac),
                           blur = list("psf"=input$psf_2, "fwhm"=input$fwhm_2),
                           IFU_plot = FALSE)
    } else if (input$blur_2 == "TRUE" && input$measure_type == "Fit" && input$survey_2 == "Specified"){
      galaxy_file = SimSpin::sim_data(filename = input$sim_file_3$datapath,
                                      ptype = input$ptype_3,
                                      m2l_disc = input$m2l_disc_2,
                                      m2l_bulge = input$m2l_bulge_2)
      SimSpin::find_lambda(simdata = galaxy_file,
                           r200 = input$r200_2,
                           z = input$z_2,
                           fov = input$fov_2,
                           ap_shape = input$ap_shape_2,
                           central_wvl = input$central_wvl_2,
                           lsf_fwhm = input$lsf_fwhm_2,
                           pixel_sscale = input$pixel_sscale_2,
                           pixel_vscale = input$pixel_vscale_2,
                           inc_deg = input$inc_deg_2,
                           threshold = input$threshold_2,
                           measure_type = list("type"="fit", "fac"=input$fac),
                           blur = list("psf"=input$psf_2, "fwhm"=input$fwhm_2),
                           IFU_plot = FALSE)
      
    } else if (input$blur_2 == "TRUE" && input$measure_type == "Specified" && input$survey_2 == "SAMI"){
      galaxy_file = SimSpin::sim_data(filename = input$sim_file_3$datapath,
                                      ptype = input$ptype_3,
                                      m2l_disc = input$m2l_disc_2,
                                      m2l_bulge = input$m2l_bulge_2)
      SimSpin::find_lambda(simdata = galaxy_file,
                           r200 = input$r200_2,
                           z = input$z_2,
                           fov = 15,
                           ap_shape = "circular",
                           central_wvl = 4800,
                           lsf_fwhm = 2.65,
                           pixel_sscale = 0.5,
                           pixel_vscale = 1.04,
                           inc_deg = input$inc_deg_2,
                           threshold = 25,
                           measure_type = list("type"="specified",
                                               "fract"=input$fract,
                                               "axis_ratio"=data.frame("a"=input$ar_a, "b"=input$ar_b)),
                           blur = list("psf"=input$psf_2, "fwhm"=input$fwhm_2),
                           IFU_plot = FALSE)
    } else if (input$blur_2 == "TRUE" && input$measure_type == "Specified" && input$survey_2 == "MaNGA"){
      galaxy_file = SimSpin::sim_data(filename = input$sim_file_3$datapath,
                                      ptype = input$ptype_3,
                                      m2l_disc = input$m2l_disc_2,
                                      m2l_bulge = input$m2l_bulge_2)
      SimSpin::find_lambda(simdata = galaxy_file,
                           r200 = input$r200_2,
                           z = input$z_2,
                           fov = 22,
                           ap_shape = "hexagonal",
                           central_wvl = 4950,
                           lsf_fwhm = 2.8,
                           pixel_sscale = 0.25,
                           pixel_vscale = 1.2,
                           inc_deg = input$inc_deg_2,
                           threshold = 30,
                           measure_type = list("type"="specified",
                                               "fract"=input$fract,
                                               "axis_ratio"=data.frame("a"=input$ar_a, "b"=input$ar_b)),
                           blur = list("psf"=input$psf_2, "fwhm"=input$fwhm_2),
                           IFU_plot = FALSE)
    } else if (input$blur_2 == "TRUE" && input$measure_type == "Specified" && input$survey_2 == "CALIFA"){
      galaxy_file = SimSpin::sim_data(filename = input$sim_file_3$datapath,
                                      ptype = input$ptype_3,
                                      m2l_disc = input$m2l_disc_2,
                                      m2l_bulge = input$m2l_bulge_2)
      SimSpin::find_lambda(simdata = galaxy_file,
                           r200 = input$r200_2,
                           z = input$z_2,
                           fov = 30,
                           ap_shape = "hexagonal",
                           central_wvl = 4200,
                           lsf_fwhm = 5.65,
                           pixel_sscale = 1,
                           pixel_vscale = 2,
                           inc_deg = input$inc_deg_2,
                           threshold = 25,
                           measure_type = list("type"="specified",
                                               "fract"=input$fract,
                                               "axis_ratio"=data.frame("a"=input$ar_a, "b"=input$ar_b)),
                           blur = list("psf"=input$psf_2, "fwhm"=input$fwhm_2),
                           IFU_plot = FALSE)
    } else if (input$blur_2 == "TRUE" && input$measure_type == "Specified" && input$survey_2 == "Specified"){
      galaxy_file = SimSpin::sim_data(filename = input$sim_file_3$datapath,
                                      ptype = input$ptype_3,
                                      m2l_disc = input$m2l_disc_2,
                                      m2l_bulge = input$m2l_bulge_2)
      SimSpin::find_lambda(simdata = galaxy_file,
                           r200 = input$r200_2,
                           z = input$z_2,
                           fov = input$fov_2,
                           ap_shape = input$ap_shape_2,
                           central_wvl = input$central_wvl_2,
                           lsf_fwhm = input$lsf_fwhm_2,
                           pixel_sscale = input$pixel_sscale_2,
                           pixel_vscale = input$pixel_vscale_2,
                           inc_deg = input$inc_deg_2,
                           threshold = input$threshold_2,
                           measure_type = list("type"="specified",
                                               "fract"=input$fract,
                                               "axis_ratio"=data.frame("a"=input$ar_a, "b"=input$ar_b)),
                           blur = list("psf"=input$psf_2, "fwhm"=input$fwhm_2),
                           IFU_plot = FALSE)

    } else if (input$blur_2 == "TRUE" && input$measure_type == "Fixed" && input$survey_2 == "SAMI"){
      galaxy_file = SimSpin::sim_data(filename = input$sim_file_3$datapath,
                                      ptype = input$ptype_3,
                                      m2l_disc = input$m2l_disc_2,
                                      m2l_bulge = input$m2l_bulge_2)
      SimSpin::find_lambda(simdata = galaxy_file,
                           r200 = input$r200_2,
                           z = input$z_2,
                           fov = 15,
                           ap_shape = "circular",
                           central_wvl = 4800,
                           lsf_fwhm = 2.65,
                           pixel_sscale = 0.5,
                           pixel_vscale = 1.04,
                           inc_deg = input$inc_deg_2,
                           threshold = 25,
                           measure_type = list("type"="fixed",
                                               "fac"=input$fac,
                                               "axis_ratio"=data.frame("a"=input$ar_a, "b"=input$ar_b)),
                           blur = list("psf"=input$psf_2, "fwhm"=input$fwhm_2),
                           IFU_plot = FALSE)
    } else if (input$blur_2 == "TRUE" && input$measure_type == "Fixed" && input$survey_2 == "MaNGA"){
      galaxy_file = SimSpin::sim_data(filename = input$sim_file_3$datapath,
                                      ptype = input$ptype_3,
                                      m2l_disc = input$m2l_disc_2,
                                      m2l_bulge = input$m2l_bulge_2)
      SimSpin::find_lambda(simdata = galaxy_file,
                           r200 = input$r200_2,
                           z = input$z_2,
                           fov = 22,
                           ap_shape = "hexagonal",
                           central_wvl = 4950,
                           lsf_fwhm = 2.8,
                           pixel_sscale = 0.25,
                           pixel_vscale = 1.2,
                           inc_deg = input$inc_deg_2,
                           threshold = 30,
                           measure_type = list("type"="fixed",
                                               "fac"=input$fac,
                                               "axis_ratio"=data.frame("a"=input$ar_a, "b"=input$ar_b)),
                           blur = list("psf"=input$psf_2, "fwhm"=input$fwhm_2),
                           IFU_plot = FALSE)
    } else if (input$blur_2 == "TRUE" && input$measure_type == "Fixed" && input$survey_2 == "CALIFA"){
      galaxy_file = SimSpin::sim_data(filename = input$sim_file_3$datapath,
                                      ptype = input$ptype_3,
                                      m2l_disc = input$m2l_disc_2,
                                      m2l_bulge = input$m2l_bulge_2)
      SimSpin::find_lambda(simdata = galaxy_file,
                           r200 = input$r200_2,
                           z = input$z_2,
                           fov = 30,
                           ap_shape = "hexagonal",
                           central_wvl = 4200,
                           lsf_fwhm = 5.65,
                           pixel_sscale = 1,
                           pixel_vscale = 2,
                           inc_deg = input$inc_deg_2,
                           threshold = 25,
                           measure_type = list("type"="fixed",
                                               "fac"=input$fac,
                                               "axis_ratio"=data.frame("a"=input$ar_a, "b"=input$ar_b)),
                           blur = list("psf"=input$psf_2, "fwhm"=input$fwhm_2),
                           IFU_plot = FALSE)
    } else if (input$blur_2 == "TRUE" && input$measure_type == "Fixed" && input$survey_2 == "Specified"){
      galaxy_file = SimSpin::sim_data(filename = input$sim_file_3$datapath,
                                      ptype = input$ptype_3,
                                      m2l_disc = input$m2l_disc_2,
                                      m2l_bulge = input$m2l_bulge_2)
      SimSpin::find_lambda(simdata = galaxy_file,
                           r200 = input$r200_2,
                           z = input$z_2,
                           fov = input$fov_2,
                           ap_shape = input$ap_shape_2,
                           central_wvl = input$central_wvl_2,
                           lsf_fwhm = input$lsf_fwhm_2,
                           pixel_sscale = input$pixel_sscale_2,
                           pixel_vscale = input$pixel_vscale_2,
                           inc_deg = input$inc_deg_2,
                           threshold = input$threshold_2,
                           measure_type = list("type"="fixed",
                                               "fac"=input$fac,
                                               "axis_ratio"=data.frame("a"=input$ar_a, "b"=input$ar_b)),
                           blur = list("psf"=input$psf_2, "fwhm"=input$fwhm_2),
                           IFU_plot = FALSE)
    }
  })

  output$fl_flux_title <- renderUI({
    HTML(paste("Flux map"))
  }) # header for flux image

  output$fl_flux_plot <- renderPlot({
    if (input$submit_3 == 0){
      return()
    } # only create plot once "submit_3" button is pressed
    isolate({
      appregion      = fl()$appregion
      appregion[appregion == 0] = NA
      counts_img     = fl()$counts_img * appregion
      axis_data      = fl()$axis_ratio
      sbin = dim(counts_img)[1]
      sbinsize = fl()$sbinsize
      xcen = (sbin/2)
      ycen = (sbin/2)

      par(mfcol=c(1,1), family="sans", font=2, cex=1.15)
      magicaxis::magimage(asinh(counts_img), xaxt="n", yaxt="n", ann=FALSE, col=rev(colorRampPalette(RColorBrewer::brewer.pal(9, "RdYlBu")[1:5])(100)), magmap=FALSE, zlim = range(c(asinh(fl()$counts_img))), family="sans", font=2, cex.lab=2)
      fields::image.plot(legend.only = TRUE, zlim = range(c(fl()$counts_img)), col = rev(colorRampPalette(RColorBrewer::brewer.pal(9, "RdYlBu")[1:5])(100)), horizontal = TRUE, family="sans", font=2, legend.lab = expression("flux, 10"^{-16} * "erg s"^{-1} * "cm"^{-2} * "arcsec"^{-2}), cex.lab=2)
      plotrix::draw.ellipse(x = xcen, y = ycen, a = axis_data$a_kpc / sbinsize, b = axis_data$b_kpc / sbinsize, angle = axis_data$angle - 90, border="red", lwd = 5, deg=TRUE)
      
    })
  }, height=reactive(ifelse(!is.null(input$innerWidth),input$innerWidth*3/5,0)))
  # plotting the flux images

  output$fl_vel_title <- renderUI({
    HTML(paste("Velocity map"))
  }) # header for velocity image

  output$fl_vel_plot <- renderPlot({
    if (input$submit_3 == 0){
      return()
    } # only create plot once "submit_3" button is pressed
    isolate({
      appregion      = fl()$appregion
      appregion[appregion == 0] = NA
      velocity_img   = fl()$velocity_img * appregion
      axis_data      = fl()$axis_ratio
      sbin = dim(velocity_img)[1]
      sbinsize = fl()$sbinsize
      xcen = (sbin/2)
      ycen = (sbin/2)

      par(family="sans", font=2, cex=1.15)
      magicaxis::magimage(velocity_img,  xaxt="n", yaxt="n", ann=FALSE, col=rev(colorRampPalette(RColorBrewer::brewer.pal(9, "RdYlBu"))(100)), magmap=FALSE, scale="linear")
      fields::image.plot(legend.only = TRUE, zlim = range(c(fl()$velocity_img)), col = rev(colorRampPalette(RColorBrewer::brewer.pal(9, "RdYlBu"))(100)), horizontal = TRUE, family="sans", font=2, legend.lab = expression("velocity"[LOS] * ", km s"^{-1}), cex.lab=2)
      plotrix::draw.ellipse(x = xcen, y = ycen, a = axis_data$a_kpc / sbinsize, b = axis_data$b_kpc / sbinsize, angle = axis_data$angle - 90, border="red", lwd = 5, deg=TRUE)
      
    })
  }, height=reactive(ifelse(!is.null(input$innerWidth),input$innerWidth*3/5,0)))
  # plotting the velocity image

  output$fl_dis_title <- renderUI({
    HTML(paste("Dispersion map"))
  }) # header for dispersion image

  output$fl_dis_plot <- renderPlot({
    if (input$submit_3 == 0){
      return()
    } # only create plot once "submit_3" button is pressed
    isolate({

      appregion      = fl()$appregion
      appregion[appregion == 0] = NA
      dispersion_img = fl()$dispersion_img * appregion
      axis_data      = fl()$axis_ratio
      sbin = dim(dispersion_img)[1]
      sbinsize = fl()$sbinsize
      xcen = (sbin/2)
      ycen = (sbin/2)
      
      par(family="sans", font=2, cex=1.15)
      magicaxis::magimage(dispersion_img,  xaxt="n", yaxt="n", ann=FALSE, col=rev(colorRampPalette(RColorBrewer::brewer.pal(9, "RdYlBu")[1:5])(100)), magmap=FALSE, scale="linear")
      fields::image.plot(legend.only = TRUE, zlim = range(c(fl()$dispersion_img)), col = rev(colorRampPalette(RColorBrewer::brewer.pal(9, "RdYlBu")[1:5])(100)), horizontal = TRUE, family="sans", font=2, legend.lab = expression("dispersion"[LOS] * ", km s"^{-1}), cex.lab=2)
      plotrix::draw.ellipse(x = xcen, y = ycen, a = axis_data$a_kpc / sbinsize, b = axis_data$b_kpc / sbinsize, angle = axis_data$angle - 90, border="red", lwd = 5, deg=TRUE)
      
    })
  }, height=reactive(ifelse(!is.null(input$innerWidth),input$innerWidth*3/5,0)))
  # plotting the dispersion images

  output$find_sum_title <- renderUI({
    HTML(paste("Kinematic summary"))
  }) # header for find_lambda() table

  output$find_summary <- renderTable({
    if (input$submit_3 == 0){
      return()
    } # only create plot once "submit_3" button is pressed

    isolate({
      if (input$blur == "FALSE"){
        find_dat = data.frame("Spaxel size / kpc" = abs(fl()$xbin_labels[2] - fl()$xbin_labels[1]),
                              "Voxel size / kms<sup>-1</sup>" = abs(fl()$vbin_labels[2] - fl()$vbin_labels[1]),
                              "Semi-major, a / kpc" = fl()$axis_ratio$a_kpc,
                              "Semi-minor, b / kpc" = fl()$axis_ratio$b_kpc,
                              "Angular size, kpc / ''" = fl()$angular_size,
                              "&#955;<sub>R</sub>" = fl()$lambda_r,
                              check.names = F)

      } else if (input$blur == "TRUE"){
        find_dat = data.frame("Spaxel size / kpc" = abs(fl()$xbin_labels[2] - fl()$xbin_labels[1]),
                              "Voxel size / kms<sup>-1</sup>" = abs(fl()$vbin_labels[2] - fl()$vbin_labels[1]),
                              "Semi-major, a / kpc" = fl()$axis_ratio$a_kpc,
                              "Semi-minor, b / kpc" = fl()$axis_ratio$b_kpc,
                              "Angular size, kpc / ''" = fl()$angular_size,
                              "&#955;<sub>R</sub>" = fl()$lambda_r,
                              "FWHM / ''" = input$fwhm,
                              check.names = F)
      }
    })
  }, sanitize.colnames.function = function(x) x) # kinematics table


})




