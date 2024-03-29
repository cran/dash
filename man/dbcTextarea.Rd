% Auto-generated: do not edit by hand
\name{dbcTextarea}

\alias{dbcTextarea}

\title{Textarea component}

\description{
A basic HTML textarea for entering multiline text based on the corresponding component in dash-core-components
}

\usage{
dbcTextarea(id=NULL, key=NULL, value=NULL, autofocus=NULL,
autoFocus=NULL, cols=NULL, disabled=NULL, form=NULL,
maxlength=NULL, maxLength=NULL, minlength=NULL,
minLength=NULL, name=NULL, placeholder=NULL, readonly=NULL,
readOnly=NULL, required=NULL, rows=NULL, wrap=NULL,
accesskey=NULL, accessKey=NULL, class_name=NULL,
className=NULL, contenteditable=NULL, contentEditable=NULL,
contextmenu=NULL, contextMenu=NULL, dir=NULL,
draggable=NULL, hidden=NULL, lang=NULL, spellcheck=NULL,
spellCheck=NULL, style=NULL, tabindex=NULL, tabIndex=NULL,
title=NULL, size=NULL, valid=NULL, invalid=NULL,
n_blur=NULL, n_blur_timestamp=NULL, n_submit=NULL,
n_submit_timestamp=NULL, n_clicks=NULL,
n_clicks_timestamp=NULL, debounce=NULL, loading_state=NULL,
persistence=NULL, persisted_props=NULL,
persistence_type=NULL)
}

\arguments{
\item{id}{Character. The ID of this component, used to identify dash components
in callbacks. The ID needs to be unique across all of the
components in an app.}

\item{key}{Character. A unique identifier for the component, used to improve
performance by React.js while rendering components
See https://reactjs.org/docs/lists-and-keys.html for more info}

\item{value}{Character. The value of the textarea}

\item{autofocus}{Character. The element should be automatically focused after the page loaded.}

\item{autoFocus}{Character. **DEPRECATED** Use `autofocus` instead

The element should be automatically focused after the page loaded.}

\item{cols}{Character | numeric. Defines the number of columns in a textarea.}

\item{disabled}{Character | logical. Indicates whether the user can interact with the element.}

\item{form}{Character. Indicates the form that is the owner of the element.}

\item{maxlength}{Character | numeric. Defines the maximum number of characters allowed in the element.}

\item{maxLength}{Character | numeric. **DEPRECATED** Use `maxlength` instead

Defines the maximum number of characters allowed in the element.}

\item{minlength}{Character | numeric. Defines the minimum number of characters allowed in the element.}

\item{minLength}{Character | numeric. **DEPRECATED** Use `minlength` instead

Defines the minimum number of characters allowed in the element.}

\item{name}{Character. Name of the element. For example used by the server to identify the fields in form submits.}

\item{placeholder}{Character. Provides a hint to the user of what can be entered in the field.}

\item{readonly}{Logical | a value equal to: 'readonly', 'readonly', 'readonly'. Indicates whether the element can be edited.}

\item{readOnly}{Logical | a value equal to: 'readonly', 'readonly', 'readonly'. **DEPRECATED** Use `readonly` instead

Indicates whether the element can be edited.}

\item{required}{A value equal to: 'required', 'required' | logical. This attribute specifies that the user must fill in a value before
submitting a form. It cannot be used when the type attribute is hidden,
image, or a button type (submit, reset, or button). The :optional and
:required CSS pseudo-classes will be applied to the field as appropriate.
required is an HTML boolean attribute - it is enabled by a boolean or
'required'. Alternative capitalizations `REQUIRED`
are also acccepted.}

\item{rows}{Character | numeric. Defines the number of rows in a text area.}

\item{wrap}{Character. Indicates whether the text should be wrapped.}

\item{accesskey}{Character. Defines a keyboard shortcut to activate or add focus to the element.}

\item{accessKey}{Character. **DEPRECATED** Use `accesskey` instead

Defines a keyboard shortcut to activate or add focus to the element.}

\item{class_name}{Character. Often used with CSS to style elements with common properties.}

\item{className}{Character. **DEPRECATED** Use `class_name` instead.

Often used with CSS to style elements with common properties.}

\item{contenteditable}{Character | numeric. Indicates whether the element's content is editable.}

\item{contentEditable}{Character | numeric. **DEPRECATED** Use `contenteditable` instead

Indicates whether the element's content is editable.}

\item{contextmenu}{Character. Defines the ID of a <menu> element which will serve as the element's context menu.}

\item{contextMenu}{Character. **DEPRECATED** Use `contextmenu` instead

Defines the ID of a <menu> element which will serve as the element's context menu.}

\item{dir}{Character. Defines the text direction. Allowed values are ltr (Left-To-Right) or rtl (Right-To-Left)}

\item{draggable}{A value equal to: 'true', 'false' | logical. Defines whether the element can be dragged.}

\item{hidden}{Character. Prevents rendering of given element, while keeping child elements, e.g. script elements, active.}

\item{lang}{Character. Defines the language used in the element.}

\item{spellcheck}{A value equal to: 'true', 'false' | logical. Indicates whether spell checking is allowed for the element.}

\item{spellCheck}{A value equal to: 'true', 'false' | logical. **DEPRECATED** Use `spellcheck` instead

Indicates whether spell checking is allowed for the element.}

\item{style}{Named list. Defines CSS styles which will override styles previously set.}

\item{tabindex}{Character | numeric. Overrides the browser's default tab order and follows the one specified instead.}

\item{tabIndex}{Character | numeric. **DEPRECATED** Use `tabindex` instead

Overrides the browser's default tab order and follows the one specified instead.}

\item{title}{Character. Text to be displayed in a tooltip when hovering over the element.}

\item{size}{Character. Set the size of the Textarea, valid options are 'sm', 'md', or 'lg'}

\item{valid}{Logical. Apply valid style to the Textarea for feedback purposes. This will cause
any FormFeedback in the enclosing div with valid=True to display.}

\item{invalid}{Logical. Apply invalid style to the Textarea for feedback purposes. This will cause
any FormFeedback in the enclosing div with valid=False to display.}

\item{n_blur}{Numeric. Number of times the input lost focus.}

\item{n_blur_timestamp}{Numeric. Last time the input lost focus.}

\item{n_submit}{Numeric. Number of times the `Enter` key was pressed while the textarea had focus.}

\item{n_submit_timestamp}{Numeric. Last time that `Enter` was pressed.}

\item{n_clicks}{Numeric. An integer that represents the number of times
that this element has been clicked on.}

\item{n_clicks_timestamp}{Numeric. An integer that represents the time (in ms since 1970)
at which n_clicks changed. This can be used to tell
which button was changed most recently.}

\item{debounce}{Logical. If true, changes to input will be sent back to the Dash server only on enter or when losing focus.
If it's false, it will sent the value back on every change.}

\item{loading_state}{Lists containing elements 'is_loading', 'prop_name', 'component_name'.
those elements have the following types:
  - is_loading (logical; optional): determines if the component is loading or not
  - prop_name (character; optional): holds which property is loading
  - component_name (character; optional): holds the name of the component that is loading. Object that holds the loading state object coming from dash-renderer}

\item{persistence}{Logical | character | numeric. Used to allow user interactions in this component to be persisted when
the component - or the page - is refreshed. If `persisted` is truthy and
hasn't changed from its previous value, a `value` that the user has
changed while using the app will keep that change, as long as
the new `value` also matches what was given originally.
Used in conjunction with `persistence_type`.}

\item{persisted_props}{List of a value equal to: 'value's. Properties whose user interactions will persist after refreshing the
component or the page. Since only `value` is allowed this prop can
normally be ignored.}

\item{persistence_type}{A value equal to: 'local', 'session', 'memory'. Where persisted user changes will be stored:
memory: only kept in memory, reset on page refresh.
local: window.localStorage, data is kept after the browser quit.
session: window.sessionStorage, data is cleared once the browser quit.}
}

\value{named list of JSON elements corresponding to React.js properties and their values}

