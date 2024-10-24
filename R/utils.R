customDisconnectMessage <- function(refresh = "Refresh page",
                                    links = sites_list,
                                    publication_name = NULL,
                                    publication_link = NULL) {
  checkmate::assert_string(refresh)
  htmltools::tagList(
    htmltools::tags$script(
      paste0(
        "$(function() {",
        "  $(document).on('shiny:disconnected', function(event) {",
        "    $('#custom-disconnect-dialog').show();",
        "    $('#ss-overlay').show();",
        "  })",
        "});"
      )
    ),
    htmltools::tags$div(
      id = "custom-disconnect-dialog",
      style = "display: none !important;",
      htmltools::tags$div(
        id = "ss-connect-refresh",
        tags$p(
          "Sorry, you have lost connection to the", site_title, "dashboard at the moment, please ",
          tags$a(
            id = "ss-reload-link",
            href = "#", "refresh the page",
            onclick = "window.location.reload(true);",
            .noWS = c("after")
          ),
          "."
        ),
        if (length(links) > 1) {
          tags$p(
            "If you are still experiencing issues, please try our",
            tags$a(href = links[1], "alternative site", .noWS = c("after")),
            ". Apologies for the inconvenience."
          )
        },
        if (!is.null(publication_name)) {
          tags$p(
            "All the data used in this dashboard can also be viewed or downloaded via the ",
            tags$a(
              href = publication_link,
              publication_name
            ),
            "on Explore Education Statistics."
          )
        },
        tags$p(
          "Feel free to contact",
          tags$a(href = "mailto:explore.statistics@education.gov.uk", "explore.statistics@education.gov.uk"),
          "if you require further support."
        )
      )
    ),
    htmltools::tags$div(id = "ss-overlay", style = "display: none;"),
    htmltools::tags$head(htmltools::tags$style(
      glue::glue(
        .open = "{{", .close = "}}",
        "#custom-disconnect-dialog a {
             display: {{ if (refresh == '') 'none' else 'inline' }} !important;
             color: #1d70b8 !important;
          }"
      )
    ))
  )
}
