library(htmltools)

# Theme Configuration ----------------------------------------------------------

theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#0B7285",
  base_font   = font_google("Inter"),
  code_font   = font_google("Fira Mono"),
  heading_font = font_google("Inter")
)

# Navigation -------------------------------------------------------------------

# Navbar brand styling (Title and subtitle)
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

# Card header styling
theme <- bslib::bs_add_rules(theme, "
  .card-header {
    background-color: var(--bs-primary-bg-subtle);
    color: var(--bs-primary-text-emphasis);
  }
  .sidebar-card .card-header {
    padding-top: 1rem !important;
    padding-bottom: 0.5rem !important;
  }
")

# Layout & Spacing -------------------------------------------------------------

# Remove side and bottom borders for the top-level navset card
theme <- bslib::bs_add_rules(theme, "
  .html-fill-container {
    --bs-gutter-x: 0;
    min-height: auto !important;
  }
")

# Bottom spacing for content
theme <- bslib::bs_add_rules(theme, "
  /* Apply bottom spacing only to the main content container (after navbar) */
  .bslib-page-navbar > .navbar + .container-fluid {
    padding-bottom: 2rem;
  }
")

# Column layout
theme <- bslib::bs_add_rules(theme, "
  /* Column gap */
  .layout-columns {
    --tight-gap: 1rem;
    gap: var(--tight-gap) !important;
  }
  /* Remove row gap (between panels) */
  .bslib-gap-spacing{
    row-gap: 0 !important;
  }
  /* Remove top padding (below main navigation) */
  .html-fill-container>.html-fill-item.bslib-mb-spacing {
      margin-top: 0 !important;
  }
")

# Accordion --------------------------------------------------------------------

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

# Footer -----------------------------------------------------------------------

theme <- bslib::bs_add_rules(theme, "
  body { padding-bottom: 48px; }
  .app-footer {
    position: fixed; 
    left: 0; 
    right: 0; 
    bottom: 0;
    background: var(--bs-body-bg);
    padding-top: 0.5rem !important;
    padding-bottom: 0.5rem !important;
    border-top: 1px solid var(--bs-border-color);
    text-align: center;
    font-size: 0.70rem;
    color: #6c757d;
    z-index: 1030;
    display: flex;
    align-items: center;
    justify-content: center;
  }
")

# Details section components ---------------------------------------------------

# Copy button & codebox
theme <- bslib::bs_add_rules(theme, "
  .copy-btn {
    color: var(--bs-secondary);
    opacity: .7;
  }
  .copy-btn:hover {
    color: var(--bs-secondary);
    opacity: 1;
  }
  .copy-btn.copied {
    color: var(--bs-primary-text-emphasis);
  }

  .codebox {
    position: relative;
    display: flex;
    flex-direction: column;
    height: 100%;
    font-size: 14px;
  }
  .codebox .copy-btn {
    position: absolute;
    top: .5rem;
    right: .5rem;
    padding: 0;
    border: 0;
    background: transparent;
  }
  .codebox .copy-btn .fa {
    font-size: 1rem;
  }
  .codebox pre {
    padding-top: 2rem; /* keep button from overlapping code */
    flex: 1;
    margin-bottom: 0;
    min-height: 300px;
  }
  
  /* Ensure columns have equal height for details */
  .details-columns {
    display: flex;
    gap: 1rem;
    align-items: stretch;
  }
  .details-columns > div {
    flex: 1;
    display: flex;
    flex-direction: column;
  }
")


# Results Components -----------------------------------------------------------

theme <- bslib::bs_add_rules(theme, "
  :root{
    --sample-size-label-font-size: .8rem;
    --sample-size-value-font-size: 2rem;
    --badge-font-size: .85rem;
    --mini-alloc-table-font-size: .85rem;
  }
  
  /* Parameter badges */
  .metric,
  .formula-chip{
    border:1px solid var(--bs-border-color);
    border-radius:var(--bs-border-radius);
    padding:.25rem .6rem;
    background:var(--bs-body-bg);
    font-size:var(--badge-font-size);
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

  /* Sample size box layout */
  .valuebox-grid{
    display:grid;
    grid-template-columns: 1fr 1fr;
    gap: .5rem 1rem;
    align-items: center;
  }
  .sample-size-label{
    font-size:var(--sample-size-label-font-size);
    color:var(--bs-secondary-color);
    text-transform:uppercase;
    letter-spacing:.02em;
    margin-bottom:.1rem;
  }
  .sample-size-value{
    font-size:var(--sample-size-value-font-size);
    font-weight:500;
  }

  /* Mini allocation table */
  .mini-alloc-table .table{
    justify-self: stretch;
    width: 100%;
    margin:0;
    font-size: var(--mini-alloc-table-font-size);
  }
  .mini-alloc-table .table > :not(caption) > * > th:last-child,
  .mini-alloc-table .table > :not(caption) > * > td:last-child{
    text-align: center !important;
  }
")

# Metrics layout: left pill stack + centered endpoint cards and formula
theme <- bslib::bs_add_rules(theme, "
  /* grid: left pills, right comparison area */
  .methods-grid {
    display: grid;
    grid-template-columns: clamp(120px, 22vw, 132px) minmax(0, 1fr);
    gap: .75rem 1rem;
    align-items: stretch;
  }

  /* left column: vertical pills, width fits content */
  .parameters-stack {
    display: flex;
    flex-direction: column;
    gap: .5rem;
  }
  .parameters-stack > .metric {
    align-self: flex-start;
    width: max-content;
  }

  /* right column: center vertically and horizontally */
  .compare-cell {
    align-self: center;
  }
  .compare-center {
    display: flex;
    flex-wrap: wrap;
    gap: 1rem;
    justify-content: center;
    align-items: center;
    margin-top: 0;
  }

  /* endpoint card */
  .endpoint-card {
    display: inline-flex;
    flex-direction: column;
    justify-content: center;
    align-items: center;
    text-align: center;
    padding: .4rem .8rem;
    min-height: 66px;
    background: var(--bs-body-bg);
    border: 1px solid var(--bs-border-color);
    border-radius: var(--bs-border-radius);
  }
  .endpoint-line {
    font-weight: 500;
  }

  /* Comparison operator badge */
  .operator-badge {
    display: flex;
    align-items: center;
    justify-content: center;
    width: 2rem;
    border-radius: var(--bs-border-radius);
    font-weight: 600;
  }

  /* adaptive formula chips */
  .formula-row {
    display: flex;
    flex-wrap: wrap;
    gap: .5rem;
    justify-content: center;
    align-items: center;
    margin-top: 1.5rem !important;
  }
  
  .results-notes {
    margin-top: 1rem;
    font-size: 0.9rem;
    font-style: italic;
    color: #555;
  }

")

# Media Queries ----------------------------------------------------------------

theme <- bslib::bs_add_rules(theme, "
  /* Stack details columns on mobile */
  @media (max-width: 768px) {
    .details-columns {
      flex-direction: column;
    }
    .methods-grid {
      grid-template-columns: 1fr;
    }
  }
  
  /* Stack TotalN grid on narrow screens */
  @media (max-width: 576px){
    .valuebox-grid{ grid-template-columns: 1fr; }
  }
")

# JavaScript -------------------------------------------------------------------

copy_js <- "
window.copyResultsToClipboard = function(id, btnId){
  const el = document.getElementById(id);
  if(!el) return;
  const txt = el.textContent || '';
  if(navigator.clipboard && window.isSecureContext){
    navigator.clipboard.writeText(txt).then(function(){
      window.showCopySuccessIndicator(btnId);
    }).catch(function(err){
      console.error('Clipboard write failed', err);
    });
  } else {
    const ta = document.createElement('textarea');
    ta.value = txt;
    document.body.appendChild(ta);
    ta.focus(); ta.select();
    try {
      document.execCommand('copy');
      window.showCopySuccessIndicator(btnId);
    } catch (err) { console.error(err); }
    document.body.removeChild(ta);
  }
}

window.showCopySuccessIndicator = function(btnId){
  const btn = document.getElementById(btnId);
  if(!btn) return;
  const icon = btn.querySelector('i');
  if(!icon) return;
  icon.className = 'fa fa-clipboard-check';
  btn.classList.add('copied');
  setTimeout(function(){
    icon.className = 'fa fa-copy';
    btn.classList.remove('copied');
  }, 1000);
}
"
