% Auto-generated: do not edit by hand
\name{htmlPlaintext}

\alias{htmlPlaintext}

\title{Plaintext component}

\description{
Plaintext is a wrapper for the <plaintext> HTML5 element.  OBSOLETE: <plaintext> is included for completeness, but should be avoided as browsers may remove it at any time, and its behavior when added dynamically by Dash is not what it would be statically on page load. Use <pre> or <code> instead.  For detailed attribute info see: https://developer.mozilla.org/en-US/docs/Web/HTML/Element/plaintext
}

\usage{
htmlPlaintext(children=NULL, id=NULL, n_clicks=NULL,
n_clicks_timestamp=NULL, key=NULL, role=NULL,
accessKey=NULL, className=NULL, contentEditable=NULL,
contextMenu=NULL, dir=NULL, draggable=NULL, hidden=NULL,
lang=NULL, spellCheck=NULL, style=NULL, tabIndex=NULL,
title=NULL, loading_state=NULL, ...)
}

\arguments{
\item{children}{A list of or a singular dash component, string or number. The children of this component}

\item{id}{Character. The ID of this component, used to identify dash components
in callbacks. The ID needs to be unique across all of the
components in an app.}

\item{n_clicks}{Numeric. An integer that represents the number of times
that this element has been clicked on.}

\item{n_clicks_timestamp}{Numeric. An integer that represents the time (in ms since 1970)
at which n_clicks changed. This can be used to tell
which button was changed most recently.}

\item{key}{Character. A unique identifier for the component, used to improve
performance by React.js while rendering components
See https://reactjs.org/docs/lists-and-keys.html for more info}

\item{role}{Character. The ARIA role attribute}

\item{accessKey}{Character. Keyboard shortcut to activate or add focus to the element.}

\item{className}{Character. Often used with CSS to style elements with common properties.}

\item{contentEditable}{Character. Indicates whether the element's content is editable.}

\item{contextMenu}{Character. Defines the ID of a <menu> element which will serve as the element's context menu.}

\item{dir}{Character. Defines the text direction. Allowed values are ltr (Left-To-Right) or rtl (Right-To-Left)}

\item{draggable}{Character. Defines whether the element can be dragged.}

\item{hidden}{A value equal to: 'hidden', 'hidden' | logical. Prevents rendering of given element, while keeping child elements, e.g. script elements, active.}

\item{lang}{Character. Defines the language used in the element.}

\item{spellCheck}{Character. Indicates whether spell checking is allowed for the element.}

\item{style}{Named list. Defines CSS styles which will override styles previously set.}

\item{tabIndex}{Character. Overrides the browser's default tab order and follows the one specified instead.}

\item{title}{Character. Text to be displayed in a tooltip when hovering over the element.}

\item{loading_state}{Lists containing elements 'is_loading', 'prop_name', 'component_name'.
those elements have the following types:
  - is_loading (logical; optional): determines if the component is loading or not
  - prop_name (character; optional): holds which property is loading
  - component_name (character; optional): holds the name of the component that is loading. Object that holds the loading state object coming from dash-renderer}


\item{...}{wildcards allowed have the form: `'data-*', 'aria-*'`}

}

\value{named list of JSON elements corresponding to React.js properties and their values}

\examples{
if (interactive()) {
  # Warning: The <plaintext> tag is obsolete,
  # it might not work as intended.
  # Use the <pre> tag instead.

  library(dash)

  app <- Dash$new()

  app$layout(
    htmlDiv(list(
      htmlPlaintext(),
      htmlBr(),
      htmlH4("The HTML Plaintext Element (<plaintext>) renders everything following
            the start tag as raw text, ignoring any following HTML. There is no closing tag,
            since everything after it is considered raw text.")
    ))
  )

  app$run_server()
}
}
