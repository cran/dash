% Auto-generated: do not edit by hand
\name{dccLocation}

\alias{dccLocation}

\title{Location component}

\description{
Update and track the current window.location object through the window.history state. Use in conjunction with the `dash_core_components.Link` component to make apps with multiple pages.
}

\usage{
dccLocation(id=NULL, pathname=NULL, search=NULL, hash=NULL, href=NULL,
refresh=NULL)
}

\arguments{
\item{id}{Character. The ID of this component, used to identify dash components
in callbacks. The ID needs to be unique across all of the
components in an app.}

\item{pathname}{Character. pathname in window.location - e.g., "/my/full/pathname"}

\item{search}{Character. search in window.location - e.g., "?myargument=1"}

\item{hash}{Character. hash in window.location - e.g., "#myhash"}

\item{href}{Character. href in window.location - e.g., "/my/full/pathname?myargument=1#myhash"}

\item{refresh}{Logical. Refresh the page when the location is updated?}
}

\value{named list of JSON elements corresponding to React.js properties and their values}

\examples{
if (interactive()) {
    library(dash)

    app <- Dash$new()

    app$layout(htmlDiv(list(
              # represents the URL bar, doesn't render anything
              dccLocation(id = 'url', refresh=FALSE),
              dccLink('Navigate to "/"', href='/'),
              htmlBr(),
              dccLink('Navigate to "/page-2"', href='/page-2'),

              # content will be rendered in this element
              htmlDiv(id='page-content')
            )
        )
    )

    app$callback(output=list(id='page-content', property='children'),
                params=list(
              input(id='url', property='pathname')),
              function(pathname)
              {
              paste0('You are on page ', pathname)
              }
    )

  app$run_server()
}
}
