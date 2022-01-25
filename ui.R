#NO DB Lighting Calc Version: ui.r
#According to CJ, for this app, ui components are run through functions in server.

tagList( #needed for shinyjs
  useShinyalert(),
  useShinyjs(),  # Include shinyjs
  introjsUI(),
  withMathJax(),
  uiOutput('uibody') #UI from the renderUI function that creates the UI on the server side. renderUI  lets you generate calls to UI functions and make the results appear in a predetermined place in the UI.
)