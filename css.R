library(htmltools)

# Main -------------------------------------------------------------------------

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
    --tight-gap: 1rem;
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

# Details components -----------------------------------------------------------

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
  }
")


copy_js <- "
window.copyTextById = function(id, btnId){
  const el = document.getElementById(id);
  if(!el) return;
  const txt = el.textContent || '';
  if(navigator.clipboard && window.isSecureContext){
    navigator.clipboard.writeText(txt).then(function(){
      window.showCopyFeedback(btnId);
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
      window.showCopyFeedback(btnId);
    } catch (err) { console.error(err); }
    document.body.removeChild(ta);
  }
}

window.showCopyFeedback = function(btnId){
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

# Results Components -----------------------------------------------------------

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
  .metric,
  .formula-chip{
    border:1px solid var(--bs-border-color);
    border-radius:var(--bs-border-radius); /*--ssp-pill-radius*/
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

# Metrics layout: left pill stack + centered endpoint cards and formula
theme <- bslib::bs_add_rules(theme, "
  /* grid: left pills, right comparison area */
  .metrics-grid {
    display: grid;
    grid-template-columns: clamp(120px, 22vw, 132px) minmax(0, 1fr);
    gap: .75rem 1rem;
    align-items: stretch;
  }

  /* left column: vertical pills, width fits content */
  .metrics-stack {
    display: flex;
    flex-direction: column;
    gap: .5rem;
  }
  .metrics-stack > .metric {
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
  .ep-card {
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
  .ep-line {
    font-weight: 500;
  }

  /* operator bubble between endpoint cards */
  .op-bubble {
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
    margin-top: 1.5rem !important;
    gap: .5rem;
    justify-content: center;
    align-items: center;
    margin-top: .25rem;
  }
  
  .results-notes {
    margin-top: 1rem;
    font-size: 0.9rem;
    font-style: italic;
    color: #555;
  }
  
  @media (max-width: 768px) {
    .metrics-grid {
      grid-template-columns: 1fr;
    }
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


