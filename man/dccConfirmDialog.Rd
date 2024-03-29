% Auto-generated: do not edit by hand
\name{dccConfirmDialog}

\alias{dccConfirmDialog}

\title{ConfirmDialog component}

\description{
ConfirmDialog is used to display the browser's native "confirm" modal, with an optional message and two buttons ("OK" and "Cancel"). This ConfirmDialog can be used in conjunction with buttons when the user is performing an action that should require an extra step of verification.
}

\usage{
dccConfirmDialog(id=NULL, message=NULL, submit_n_clicks=NULL,
submit_n_clicks_timestamp=NULL, cancel_n_clicks=NULL,
cancel_n_clicks_timestamp=NULL, displayed=NULL)
}

\arguments{
\item{id}{Character. The ID of this component, used to identify dash components
in callbacks. The ID needs to be unique across all of the
components in an app.}

\item{message}{Character. Message to show in the popup.}

\item{submit_n_clicks}{Numeric. Number of times the submit button was clicked}

\item{submit_n_clicks_timestamp}{Numeric. Last time the submit button was clicked.}

\item{cancel_n_clicks}{Numeric. Number of times the popup was canceled.}

\item{cancel_n_clicks_timestamp}{Numeric. Last time the cancel button was clicked.}

\item{displayed}{Logical. Set to true to send the ConfirmDialog.}
}

\value{named list of JSON elements corresponding to React.js properties and their values}

\examples{
if (interactive()) {
    library(dash)

    app <- Dash$new()

    app$layout(
      htmlDiv(
        list(
          dccConfirmDialog(
            id='confirm',
            message='Danger danger! Are you sure you want to continue?'),
          dccDropdown(
            options=lapply(list('Safe', 'Danger!!'),function(x){list('label'= x, 'value'= x)}),
            id='dropdown'
          ),
          htmlDiv(id='output-confirm1')
        )
      )
    )

    app$callback(
      output = list(id = 'confirm', property = 'displayed'),
      params=list(input(id = 'dropdown', property = 'value')),
      function(value){
        if(value == 'Danger!!'){
          return(TRUE)}
        else{
          return(FALSE)}
      })

  app$run_server()
}
}
