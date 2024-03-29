% Auto-generated: do not edit by hand
\name{dbcPagination}

\alias{dbcPagination}

\title{Pagination component}

\description{
The container for presentational components for building a pagination UI. Individual pages should be added as children using the `PaginationItem` component.
}

\usage{
dbcPagination(id=NULL, class_name=NULL, className=NULL, style=NULL,
size=NULL, min_value=NULL, max_value=NULL, step=NULL,
active_page=NULL, fully_expanded=NULL, previous_next=NULL,
first_last=NULL, loading_state=NULL)
}

\arguments{
\item{id}{Character. The ID of this component, used to identify dash components
in callbacks. The ID needs to be unique across all of the
components in an app.}

\item{class_name}{Character. Often used with CSS to style elements with common properties.}

\item{className}{Character. **DEPRECATED** - Use class_name instead.

Often used with CSS to style elements with common properties.}

\item{style}{Named list. Defines CSS styles which will override styles previously set.}

\item{size}{A value equal to: 'sm', 'lg'. Set the size of all page items in the pagination.}

\item{min_value}{Numeric. Minimum (leftmost) value to appear in the pagination.}

\item{max_value}{Numeric. Maximum (rightmost) value to appear in the pagination. Must be defined.
If the `min_value` and `step` together cannot reach this value, then the
next stepped value is used as the maximum.}

\item{step}{Numeric. Page increment step.}

\item{active_page}{Numeric. The currently active page}

\item{fully_expanded}{Logical. When True, this will display all numbers between `min_value` and
`max_value`.}

\item{previous_next}{Logical. When True, this will display a previous and next icon before and after
the individual page numbers.}

\item{first_last}{Logical. When True, this will display a first and last icon at the beginning
and end of the component.}

\item{loading_state}{Lists containing elements 'is_loading', 'prop_name', 'component_name'.
those elements have the following types:
  - is_loading (logical; optional): determines if the component is loading or not
  - prop_name (character; optional): holds which property is loading
  - component_name (character; optional): holds the name of the component that is loading. Object that holds the loading state object coming from dash-renderer}
}

\value{named list of JSON elements corresponding to React.js properties and their values}

