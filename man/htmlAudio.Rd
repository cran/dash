% Auto-generated: do not edit by hand
\name{htmlAudio}

\alias{htmlAudio}

\title{Audio component}

\description{
Audio is a wrapper for the <audio> HTML5 element. For detailed attribute info see: https://developer.mozilla.org/en-US/docs/Web/HTML/Element/audio
}

\usage{
htmlAudio(children=NULL, id=NULL, n_clicks=NULL,
n_clicks_timestamp=NULL, key=NULL, role=NULL, autoPlay=NULL,
controls=NULL, crossOrigin=NULL, loop=NULL, muted=NULL,
preload=NULL, src=NULL, accessKey=NULL, className=NULL,
contentEditable=NULL, contextMenu=NULL, dir=NULL,
draggable=NULL, hidden=NULL, lang=NULL, spellCheck=NULL,
style=NULL, tabIndex=NULL, title=NULL, loading_state=NULL,
...)
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

\item{autoPlay}{A value equal to: 'autoplay', 'autoplay', 'autoplay' | logical. The audio or video should play as soon as possible.}

\item{controls}{A value equal to: 'controls', 'controls' | logical. Indicates whether the browser should show playback controls to the user.}

\item{crossOrigin}{Character. How the element handles cross-origin requests}

\item{loop}{A value equal to: 'loop', 'loop' | logical. Indicates whether the media should start playing from the start when it's finished.}

\item{muted}{A value equal to: 'muted', 'muted' | logical. Indicates whether the audio will be initially silenced on page load.}

\item{preload}{Character. Indicates whether the whole resource, parts of it or nothing should be preloaded.}

\item{src}{Character. The URL of the embeddable content.}

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
  library(dash)

  app <- Dash$new()

  app$layout(
    htmlDiv(list(
      htmlAudio(src='https://www.nasa.gov/62284main_onesmall2.wav',
                controls=TRUE,
                title='Apollo 11 - July 16, 1969 - Neil Armstrong')
        )
    )
  )

  app$run_server()
}
}
