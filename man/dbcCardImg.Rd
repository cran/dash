% Auto-generated: do not edit by hand
\name{dbcCardImg}

\alias{dbcCardImg}

\title{CardImg component}

\description{
Use CardImg to add images to your cards.
}

\usage{
dbcCardImg(children=NULL, id=NULL, style=NULL, class_name=NULL,
className=NULL, key=NULL, tag=NULL, top=NULL, bottom=NULL,
src=NULL, alt=NULL, title=NULL, loading_state=NULL)
}

\arguments{
\item{children}{A list of or a singular dash component, string or number. The children of this component}

\item{id}{Character. The ID of this component, used to identify dash components
in callbacks. The ID needs to be unique across all of the
components in an app.}

\item{style}{Named list. Defines CSS styles which will override styles previously set.}

\item{class_name}{Character. Often used with CSS to style elements with common properties.}

\item{className}{Character. **DEPRECATED** Use `class_name` instead.

Often used with CSS to style elements with common properties.}

\item{key}{Character. A unique identifier for the component, used to improve
performance by React.js while rendering components
See https://reactjs.org/docs/lists-and-keys.html for more info}

\item{tag}{Character. HTML tag to use for the card body, default: div}

\item{top}{Logical. Set to True if image is at top of card. This will apply the card-img-top
class which rounds the top corners to match the corners of the card.}

\item{bottom}{Logical. Set to True if image is at bottom of card. This will apply the
card-img-bottom class which rounds the bottom corners to match the corners
of the card.}

\item{src}{Character. The URI of the embeddable content.}

\item{alt}{Character. Alternative text in case an image can't be displayed.}

\item{title}{Character. Text to be displayed as a tooltip when hovering}

\item{loading_state}{Lists containing elements 'is_loading', 'prop_name', 'component_name'.
those elements have the following types:
  - is_loading (logical; optional): determines if the component is loading or not
  - prop_name (character; optional): holds which property is loading
  - component_name (character; optional): holds the name of the component that is loading. Object that holds the loading state object coming from dash-renderer}
}

\value{named list of JSON elements corresponding to React.js properties and their values}

