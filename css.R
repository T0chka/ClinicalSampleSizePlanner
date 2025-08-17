library(htmltools)

preview_button <- function(theme) {
  browsable(tags$body(bs_theme_dependencies(theme), button))
}

theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#0B7285",
  base_font   = font_google("Inter"),
  code_font   = font_google("Fira Mono"),
  heading_font = font_google("Inter")
)

# Title and subtitle
theme <- bslib::bs_add_rules(theme, "
  .navbar .navbar-brand { margin: 0 !important; }
  .navbar .navbar-brand .name-title   { font-size: 2rem; }
  .navbar .navbar-brand .name-subtitle{
    font-size: 1rem; font-weight: 400; color: var(--bs-gray-500);
  }
")

# Nav link colors
theme <- bslib::bs_add_rules(theme, "
  .nav {
    --bs-nav-link-color: var(--bs-primary-text-emphasis) !important;
    --bs-nav-link-hover-color: var(--bs-primary);
  }
")

# Remove side and bottom borders for the top-level navset card
theme <- bslib::bs_add_rules(theme, "
  .html-fill-container {
    --bs-gutter-x: 0;
    min-height: auto !important;
  }
")

# Sidebar card header padding
theme <- bslib::bs_add_rules(theme, "
  .sidebar-card .card-header {
    padding-top: 1rem !important;
    padding-bottom: 0.5rem !important;
  }
")

# Columns gap
theme <- bslib::bs_add_rules(theme, "
  .cols-tight {
    --tight-gap: 0.75rem;
    gap: var(--tight-gap) !important;
  }
")

# Match card-header style to accordion-button
theme <- bslib::bs_add_rules(theme, "
  .card-header {
    background-color: var(--bs-primary-bg-subtle);
    color: var(--bs-primary-text-emphasis);
  }
")

# Accordion: remove extra borders/shadows
theme <- bslib::bs_add_rules(theme, "
  .accordion-button,
  .accordion-button:not(.collapsed),
  .accordion-button:focus {
    border: none !important;
    box-shadow: none !important;
    outline: none !important;
  }
")

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
")

# ===================== Components  ============================================

theme <- bslib::bs_add_rules(theme, "
  :root{
    --ssp-gap: .5rem;
    --ssp-pill-radius: 9999px;
    --ssp-totaln-label-size: .8rem;
    --ssp-totaln-value-size: 2rem;
    --ssp-pill-font-size: .85rem;
    --ssp-mini-alloc-font-size: .85rem;
  }

  /* Metrics (pill) */
  .metrics-bar{
    display:flex;
    gap:var(--ssp-gap);
    flex-wrap:wrap;
    align-items:center;
  }
  .metric{
    border:1px solid var(--bs-border-color);
    border-radius:var(--ssp-pill-radius);
    padding:.25rem .6rem;
    background:var(--bs-body-bg);
    font-size:var(--ssp-pill-font-size);
  }
  
  /* Value box settings */
  .bslib-value-box .value-box-value{
    font-size: 1rem !important;
    overflow-x: clip;
    margin-bottom: 0 !important;
    column-gap: 0;
  }
  .bslib-value-box .value-box-area{
    padding: 1.5rem 1.5rem 1.5rem 1rem !important;
  }

  /* Value box layout for Total N */
  .valuebox-grid{
    display:grid;
    grid-template-columns: 1fr 1fr;
    gap:.5rem 1rem;
    column-gap: .5rem;
    align-items: center;
  }
  .totaln-label{
    font-size:var(--ssp-totaln-label-size);
    color:var(--bs-secondary-color);
    text-transform:uppercase;
    letter-spacing:.02em;
    margin-bottom:.1rem;
  }
  .totaln-value{
    font-size:var(--ssp-totaln-value-size);
    font-weight:500;
  }

  /* Mini allocation table */
  .mini-alloc .table{
    justify-self: stretch;
    width: 100%;
    margin:0;
    font-size: var(--ssp-mini-alloc-font-size);
  }
  .mini-alloc .table > :not(caption) > * > th:last-child,
  .mini-alloc .table > :not(caption) > * > td:last-child{
    text-align: center !important;
  }
  
  /* stack TotalN grid on narrow screens */
  @media (max-width: 576px){
    .valuebox-grid{ grid-template-columns: 1fr; }
  }
")

# /* Scrollable details log */
# :root{ --ssp-pre-max-h: 320px;}
# .pre-scroll{ max-height:var(--ssp-pre-max-h); overflow:auto; }

# /* Emphasize metrics */
# .metric-strong{
#   background:var(--bs-primary-bg-subtle);
#   border-color:var(--bs-primary-border-subtle);
# }
