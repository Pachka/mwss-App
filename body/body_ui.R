body <- dashboardBody(
  tags$head(
    tags$style(
    HTML(
      ".shiny-notification {position:fixed; top: 30%;left: 40%;right: 40%;}
         .box {margin: 5px;}
         .small-box {height: 125px}
                    .box.box-solid.box-primary>.box-header {
                    color:#fff;
                    background:#f1d031
                    }
                    .box.box-solid.box-primary{
                    border-bottom-color:#222d32;
                    border-left-color:#222d32;
                    border-right-color:#222d32;
                    border-top-color:#222d32;
                    background:#f1d031
                    }
                    hr {border-top: 1px solid #000000;}
  .loadmessage {
  position:fixed;
  z-index:8;
  top:50%;
  left:50%;
  padding:10px;
  text-align:center;
  font-weight:bold;
  color:#000000;
  background-color:#CCFF66;
  }

  .loader {
  position:fixed;
  z-index:8;
  border:16px solid #999999;
  border-top: 16px solid #8B0000;
  border-radius: 50%;
  width: 150px;
  height: 150px;
  top:45%;
  left:45%;
  animation: spin 2s linear infinite;
  }

  .prevent_click{
  position:fixed;
  z-index:9;
  width:100%;
  height:100vh;
  background-color: transpare'nt;
  }

  @keyframes spin {
  0% { transform: rotate(0deg); }
  100% { transform: rotate(360deg); }
  }"
    )
  )),


  # setSliderColor(
  #   c("#FF3300", "#CC0000", "#FF3300 ", "#CC0000", "#CC0000", "black"),
  #   c(1:6)
  # ),
  useShinyjs(),
  tabItems(
    tabItemPresentation(),
    # tabItemInitialization(),
    tabItemSimple(),
    # tabItemParams(),
    tabItemAbout()
  )
)
