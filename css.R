library(htmltools)

preview_button <- function(theme) {
  browsable(tags$body(bs_theme_dependencies(theme), button))
}

theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#0B7285",
  base_font = font_google("Inter"),
  code_font  = font_google("Fira Mono"),
  heading_font = font_google("Inter")
)

# Title and subtitle
theme <- bslib::bs_add_rules(theme, "
  .navbar .navbar-brand {
    margin: 0 !important;
  }
  .navbar .navbar-brand .name-title {
  font-size: 2rem;
  }
  .navbar .navbar-brand .name-subtitle {
  font-size: 1rem; font-weight: 400; color: var(--bs-gray-500);
  }
  "
)

# Nav link colors
theme <- bslib::bs_add_rules(theme, "
  .nav {
    --bs-nav-link-color: var(--bs-primary-text-emphasis) !important;
    --bs-nav-link-hover-color: var(--bs-primary);
  }
  "
)
# Remove side and bottom borders for the top-level navset card
theme <- bslib::bs_add_rules(theme, "
  .html-fill-container {
    --bs-gutter-x: 0;
    min-height: auto !important;
  }
  "
)

# Sidebar card header padding
theme <- bslib::bs_add_rules(theme, "
  .sidebar-card .card-header {
    padding-top: 1rem !important;
    padding-bottom: 0.5rem !important;
  }
  "
)

# The gap between columns
theme <- bslib::bs_add_rules(theme, "
  .cols-tight {
    --tight-gap: 0.75rem;
    gap: var(--tight-gap) !important;
  }
  "
)

theme <- bslib::bs_add_rules(theme, "
  .card-header {
    background-color: var(--bs-primary-bg-subtle);
    color: var(--bs-primary-text-emphasis);
  }
  "
)

# Footer
theme <- bslib::bs_add_rules(theme, "
  body { padding-bottom: 48px; }
  .app-footer {
    position: fixed; left: 0; right: 0; bottom: 0;
    background: var(--bs-body-bg);
    padding-top: 0.5rem !important;
    padding-bottom: 0.5rem !important;
    border-top: 1px solid var(--bs-border-color);
    text-align: center;
    font-size: 0.85rem;
    color: #6c757d;
  }
  "
)
