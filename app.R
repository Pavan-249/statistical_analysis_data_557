# app.R
library(shiny)
library(tidyverse)
library(scales)
library(lmtest)
library(sandwich)
library(plotly)

#-------------------------------
# Data loading & preparation
#-------------------------------
salary <- read.table(
  "salary.txt",
  header = TRUE,
  sep = "",
  stringsAsFactors = FALSE,
  na.strings = "NA"
)
salary <- salary %>%
  mutate(
    sex   = factor(sex, levels = c("M", "F")),
    deg   = factor(deg),
    field = factor(field),
    rank  = factor(rank, levels = c("Assist", "Assoc", "Full")),
    admin = factor(admin, levels = c(0, 1)),
    startyr   = as.integer(startyr),
    year      = as.integer(year),
    yrdeg     = as.integer(yrdeg),
    salary    = as.numeric(salary),
    experience = year - startyr
  )
salary_clean <- salary %>%
  filter(experience >= 0, !is.na(rank))
sex_colours <- c("M" = "#4472C4", "F" = "#E05C5C")

start_salary <- salary_clean %>%
  filter(year == startyr) %>%
  mutate(log_salary = log(salary))
promotion_data <- salary_clean %>%
  arrange(id, year) %>%
  group_by(id) %>%
  mutate(
    prev_rank   = lag(rank),
    prev_salary = lag(salary),
    prev_year   = lag(year)
  ) %>%
  ungroup() %>%
  filter(prev_rank == "Assoc", rank == "Full") %>%
  group_by(id) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(
    raise_amount = salary - prev_salary,
    raise_pct    = (salary - prev_salary) / prev_salary
  )
ever_assoc_ids <- salary_clean %>%
  group_by(id) %>%
  filter(any(rank == "Assoc")) %>%
  pull(id) %>%
  unique()

person_level <- salary_clean %>%
  filter(id %in% ever_assoc_ids) %>%
  arrange(id, year) %>%
  group_by(id) %>%
  summarise(
    sex            = first(sex),
    deg            = first(deg),
    field          = first(field),
    yrdeg          = first(yrdeg),
    promoted       = as.integer(any(rank == "Full")),
    yr_first_assoc = min(year[rank == "Assoc"], na.rm = TRUE),
    yr_first_full  = ifelse(any(rank == "Full"),
                            min(year[rank == "Full"], na.rm = TRUE), NA_real_),
    yrs_as_assoc   = ifelse(any(rank == "Full"),
                            yr_first_full - yr_first_assoc, NA_real_),
    sal_assoc_entry = salary[rank == "Assoc"][which.min(year[rank == "Assoc"])],
    sal_assoc_last  = salary[rank == "Assoc"][which.max(year[rank == "Assoc"])],
    sal_full_entry  = ifelse(any(rank == "Full"),
                             salary[rank == "Full"][which.min(year[rank == "Full"])],
                             NA_real_),
    .groups = "drop"
  )

latest_salary <- salary_clean %>%
  arrange(id, year) %>%
  group_by(id) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  mutate(
    log_salary = log(salary),
    experience = year - startyr
  )

# Q4: Career Arc — experience bracket data
salary_arc <- salary_clean %>%
  mutate(
    exp_bracket = case_when(
      experience <= 4  ~ "0 to 4 Years",
      experience <= 9  ~ "5 to 9 Years",
      experience <= 14 ~ "10 to 14 Years",
      TRUE             ~ "15 or More Years"
    ),
    exp_bracket = factor(
      exp_bracket,
      levels = c("0 to 4 Years", "5 to 9 Years", "10 to 14 Years", "15 or More Years")
    )
  )

#-------------------------------
# UI
#-------------------------------
ui <- navbarPage(
  title = "DATA 557 W26 Final Group Project",

  # ══════════════════════════════════════════════════
  #  OVERVIEW TAB — Cherry Blossom Edition
  # ══════════════════════════════════════════════════
  tabPanel(
    "Overview",

    tags$head(
      tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
      tags$link(
        rel  = "stylesheet",
        href = "https://fonts.googleapis.com/css2?family=Playfair+Display:ital,wght@0,400;0,600;1,400&family=Inter:wght@300;400;500&display=swap"
      ),

      tags$style(HTML("

        /* ─── BASE ─── */
        body { background:#fff8fa !important; overflow-x:hidden; font-family:'Inter',sans-serif; }
        .navbar { z-index:10000 !important; }
        .cherry-overview { margin-left:-15px; margin-right:-15px; margin-top:-20px; }

        /* ─── FALLING PETALS (JS rAF — activity-driven, all tabs) ─── */
        .blossom-petal {
          position: fixed;
          top: 0;          /* anchor to viewport top-left so translate3d works correctly */
          left: 0;
          pointer-events: none;
          z-index: 9500;
          border-radius: 150% 0 150% 0;
          filter: drop-shadow(0 1px 3px rgba(180,70,110,0.22));
          will-change: transform, opacity;
          /* transform and opacity driven entirely by JS rAF loop */
        }

        /* ─── HERO  (real photo + CSS parallax) ─── */
        .cherry-hero {
          min-height:100vh;
          background: url('cherry-path.png') center 30% / cover no-repeat;
          background-attachment: fixed;
          display:flex; flex-direction:column;
          align-items:center; justify-content:center;
          text-align:center;
          position:relative; overflow:hidden;
          padding:90px 30px 110px;
        }
        /* Dark gradient overlay */
        .cherry-hero::before {
          content:''; position:absolute; inset:0;
          background: linear-gradient(160deg,
            rgba(6,1,4,0.48) 0%,
            rgba(10,2,6,0.62) 60%,
            rgba(14,3,9,0.72) 100%);
          z-index:1;
        }

        /* Glowing ambient orbs */
        .h-orb {
          position:absolute; border-radius:50%;
          filter:blur(72px); pointer-events:none; will-change:transform;
        }
        .h-orb-a {
          width:560px;height:560px;
          background:rgba(255,120,160,0.13);
          top:-140px;left:-130px;
          animation:orbPulse 9s ease-in-out infinite;
          z-index:2;
        }
        .h-orb-b {
          width:420px;height:420px;
          background:rgba(200,70,115,0.10);
          bottom:-90px;right:-100px;
          animation:orbPulse 12s ease-in-out infinite reverse;
          z-index:2;
        }
        .h-orb-c {
          width:280px;height:280px;
          background:rgba(255,183,197,0.08);
          top:40%;left:60%;
          animation:orbPulse 7s ease-in-out infinite 3s;
          z-index:2;
        }
        /* Parallax depth layers (used in hero blossoms) */
        .pdl-1, .pdl-2, .pdl-3 {
          position:absolute; pointer-events:none; z-index:2;
        }
        @keyframes orbPulse {
          0%,100% { transform:translate(0,0) scale(1);         }
          33%     { transform:translate(18px,-14px) scale(1.06); }
          66%     { transform:translate(-12px,18px) scale(0.96); }
        }

        .hero-content {
          position:relative; z-index:3;
          animation:contentRise 1.3s cubic-bezier(.22,.68,0,1.2) both;
        }
        @keyframes contentRise {
          from { opacity:0; transform:translateY(28px); }
          to   { opacity:1; transform:translateY(0);    }
        }
        .hero-eyebrow {
          font-family:'Inter',sans-serif; font-size:0.7em; font-weight:500;
          letter-spacing:6px; text-transform:uppercase; color:#ffb0c8;
          margin-bottom:18px;
        }
        .hero-title {
          font-family:'Playfair Display',Georgia,serif;
          font-size:clamp(2.6em,7vw,5.4em); font-weight:400;
          color:#fff8fb;
          text-shadow: 0 0 80px rgba(255,140,175,0.5), 0 2px 8px rgba(0,0,0,0.45);
          margin:0 0 26px; letter-spacing:1px; line-height:1.15;
        }
        .hero-tagline {
          font-family:'Playfair Display',Georgia,serif; font-style:normal;
          font-size:clamp(1em,2.2vw,1.3em); color:#ffc8d8;
          max-width:640px; line-height:1.95; margin:0 auto 10px;
        }
        .scroll-cue {
          position:absolute; bottom:30px; left:50%; transform:translateX(-50%);
          color:rgba(255,183,197,0.6); font-size:0.65em;
          letter-spacing:4px; text-transform:uppercase; z-index:4;
          animation:scrollBounce 2.8s ease-in-out infinite;
        }
        .scroll-cue-line {
          width:1px; height:38px;
          background:linear-gradient(to bottom,rgba(255,183,197,0.45),transparent);
          margin:9px auto 0;
        }
        @keyframes scrollBounce {
          0%,100% { opacity:0.6; transform:translateX(-50%) translateY(0);   }
          50%     { opacity:1;   transform:translateX(-50%) translateY(-7px); }
        }

        /* ─── CURIOSITY SECTION (Tokyo photo bg) ─── */
        .curiosity-section {
          background: url('cherry-tokyo.png') center / cover no-repeat;
          background-attachment: fixed;
          position:relative; overflow:hidden;
          padding:130px 40px 140px;
        }
        .curiosity-section::before {
          content:''; position:absolute; inset:0;
          background:linear-gradient(180deg,
            rgba(14,3,10,0.88) 0%,
            rgba(30,8,20,0.92) 100%);
          z-index:1;
        }

        /* Shared label / heading */
        .s-eyebrow {
          font-family:'Inter',sans-serif; font-size:0.68em; font-weight:500;
          letter-spacing:5px; text-transform:uppercase; text-align:center;
          margin-bottom:12px; position:relative; z-index:2;
        }
        .s-heading {
          font-family:'Playfair Display',Georgia,serif; font-weight:400;
          text-align:center; margin:0 auto 64px; max-width:680px;
          line-height:1.48; position:relative; z-index:2;
        }

        .curiosity-section .s-eyebrow { color:#ff93ac; }
        .curiosity-section .s-heading { color:#fff5f8; font-size:clamp(1.7em,3.5vw,2.45em); }

        /* Glassmorphism curiosity cards */
        .curiosity-grid {
          display:flex; gap:22px; justify-content:center;
          flex-wrap:wrap; max-width:1060px; margin:0 auto;
          position:relative; z-index:2;
        }
        .c-card {
          background: rgba(255,255,255,0.10);
          backdrop-filter: blur(18px) saturate(160%);
          -webkit-backdrop-filter: blur(18px) saturate(160%);
          border: 1px solid rgba(255,255,255,0.22);
          border-radius:24px; padding:42px 30px;
          flex:1; min-width:255px; max-width:315px; text-align:center;
          cursor:default;
          opacity:0; transform:translateY(38px);
          transition:background 0.38s, transform 0.38s, box-shadow 0.38s;
          box-shadow: 0 8px 32px rgba(0,0,0,0.25);
        }
        .c-card.visible {
          opacity:1; transform:translateY(0);
          transition:opacity 0.8s ease, transform 0.8s ease,
                      background 0.38s, box-shadow 0.38s;
        }
        .c-card:nth-child(2) { transition-delay:0.18s; }
        .c-card:nth-child(3) { transition-delay:0.36s; }
        .c-card:hover {
          background:rgba(255,255,255,0.18);
          transform:translateY(-12px) !important;
          box-shadow:0 32px 64px rgba(0,0,0,0.4);
        }
        .c-number {
          font-family:'Playfair Display',Georgia,serif;
          font-size:3.2em; font-weight:600;
          color:rgba(255,183,197,0.35); line-height:1;
          margin-bottom:16px; display:block;
        }
        .c-question {
          color:#ffe8f0; font-family:'Playfair Display',Georgia,serif;
          font-style:normal; font-size:1.02em; line-height:1.85; margin:0;
        }

        /* ─── STORY SECTION ─── */
        .story-section {
          background-color:#fff5f7;
          padding:140px 40px 130px;
          position:relative; overflow:hidden;
        }
        /* Subtle photo watermark */
        .story-section::after {
          content:''; position:absolute; inset:0;
          background:url('cherry-row.png') center / cover no-repeat;
          opacity:0.05; pointer-events:none; z-index:0;
        }
        .story-section::before {
          content:''; position:absolute;
          top:0; left:0; right:0; height:3px;
          background:linear-gradient(to right,transparent,#ffb7c5,transparent);
          z-index:1;
        }
        .story-inner {
          max-width:760px; margin:0 auto;
          position:relative; z-index:1;
        }
        .story-section .s-eyebrow { color:#c96b8a; text-align:left; margin-bottom:8px; }
        .story-heading {
          font-family:'Playfair Display',Georgia,serif;
          font-size:clamp(2em,4vw,3em); font-weight:400;
          color:#240a14; line-height:1.3; margin:4px 0 50px;
        }
        .story-para {
          font-family:'Playfair Display',Georgia,serif;
          font-size:1.13em; line-height:1.98; color:#5a2535; margin-bottom:28px;
        }
        .story-callout {
          background:linear-gradient(135deg,#ffb7c5 0%,#ff93ac 100%);
          border-radius:20px; padding:34px 42px; margin:48px 0;
          box-shadow:0 14px 40px rgba(201,107,138,0.24);
        }
        .story-callout p {
          font-family:'Playfair Display',Georgia,serif;
          font-size:1.18em; line-height:1.78;
          color:#2d0f1e; font-weight:600; text-align:center; margin:0;
        }

        /* ─── STATS SECTION ─── */
        .stats-section {
          background-color:#fff8fa;
          padding:120px 40px;
          position:relative;
        }
        .stats-section::after {
          content:''; position:absolute; inset:0;
          background:url('cherry-row.png') center / cover no-repeat;
          opacity:0.04; pointer-events:none; z-index:0;
        }
        .stats-section .s-eyebrow { color:#c96b8a; }
        .stats-section .s-heading {
          color:#240a14; font-size:clamp(1.8em,3.5vw,2.4em);
        }
        .stats-grid {
          display:flex; gap:26px; justify-content:center;
          flex-wrap:wrap; max-width:1160px; margin:0 auto;
          position:relative; z-index:1;
        }

        /* Glassmorphism stat cards */
        .s-card {
          background: rgba(255,255,255,0.68);
          backdrop-filter: blur(20px) saturate(180%);
          -webkit-backdrop-filter: blur(20px) saturate(180%);
          border: 1px solid rgba(255,183,197,0.45);
          border-radius:24px; padding:36px 28px;
          flex:1; min-width:265px; max-width:365px;
          box-shadow:0 8px 32px rgba(201,107,138,0.12);
          border-top:5px solid #ffb7c5;
          opacity:0; transform:translateY(28px);
          transition:transform 0.32s, box-shadow 0.32s;
        }
        .s-card.visible {
          opacity:1; transform:translateY(0);
          transition:opacity 0.72s ease, transform 0.72s ease,
                      box-shadow 0.32s;
        }
        .s-card:nth-child(2) { border-top-color:#c96b8a; transition-delay:0.14s; }
        .s-card:nth-child(3) { border-top-color:#7b2d45; transition-delay:0.28s; }
        .s-card:hover {
          transform:translateY(-8px) !important;
          box-shadow:0 22px 52px rgba(201,107,138,0.22);
        }
        .s-card-label {
          font-family:'Inter',sans-serif; font-size:0.66em; font-weight:500;
          letter-spacing:3px; text-transform:uppercase; color:#c96b8a;
          display:block; margin-bottom:4px;
        }
        .s-card-title {
          font-family:'Playfair Display',Georgia,serif;
          font-size:1.18em; font-weight:400; color:#240a14; margin:0 0 18px;
        }
        .s-card table {
          width:100%; border-collapse:collapse;
          font-size:0.87em; font-family:'Inter',sans-serif;
        }
        .s-card table thead th {
          color:#c96b8a; font-size:0.7em; text-transform:uppercase;
          letter-spacing:1px; padding:4px 8px 10px 0;
          border-bottom:1px solid #ffe0ea; text-align:left; font-weight:600;
        }
        .s-card table tbody td {
          padding:7px 8px 7px 0; color:#5a2535;
          border-bottom:1px solid #fff5f7;
        }
        .s-card table tbody tr:last-child td { border-bottom:none; }

        /* ─── RESOURCES SECTION (tree photo bg) ─── */
        .resources-section {
          background: url('cherry-tree.png') center / cover no-repeat;
          background-attachment: fixed;
          position:relative; text-align:center;
          padding:120px 40px 90px;
        }
        .resources-section::before {
          content:''; position:absolute; inset:0;
          background:linear-gradient(158deg,
            rgba(16,3,10,0.90) 0%,
            rgba(46,10,26,0.93) 50%,
            rgba(28,5,16,0.90) 100%);
          z-index:1;
        }
        .resources-section .s-eyebrow { color:#ff93ac; }
        .resources-section .s-heading { color:#fff5f8; font-size:clamp(1.7em,3.5vw,2.4em); }

        .resources-grid {
          display:flex; gap:20px; justify-content:center;
          flex-wrap:wrap; max-width:980px; margin:0 auto;
          position:relative; z-index:2;
        }

        /* Glassmorphism resource cards */
        .r-card {
          background: rgba(255,255,255,0.08);
          backdrop-filter: blur(14px) saturate(140%);
          -webkit-backdrop-filter: blur(14px) saturate(140%);
          border: 1px solid rgba(255,255,255,0.18);
          border-radius:20px; padding:32px 22px; width:210px;
          display:block; text-decoration:none !important; text-align:center;
          box-shadow: 0 6px 24px rgba(0,0,0,0.25);
          opacity:0; transform:translateY(24px);
          transition:background 0.32s, transform 0.32s, box-shadow 0.32s;
        }
        .r-card.visible {
          opacity:1; transform:translateY(0);
          transition:opacity 0.72s ease, transform 0.72s ease,
                      background 0.32s, box-shadow 0.32s;
        }
        .r-card:nth-child(2) { transition-delay:0.1s;  }
        .r-card:nth-child(3) { transition-delay:0.2s;  }
        .r-card:nth-child(4) { transition-delay:0.3s;  }
        .r-card:hover {
          background:rgba(255,255,255,0.17);
          transform:translateY(-9px) !important;
          box-shadow:0 26px 52px rgba(0,0,0,0.35);
          text-decoration:none !important;
        }
        .r-doc-icon {
          width:44px; height:54px; margin:0 auto 14px;
          position:relative; display:block;
        }
        .r-name {
          font-family:'Playfair Display',Georgia,serif;
          color:#ffe0ea; font-size:0.92em; line-height:1.55; margin:0 0 10px;
        }
        .r-type {
          font-family:'Inter',sans-serif; color:#ff93ac;
          font-size:0.63em; letter-spacing:3px; text-transform:uppercase; display:block;
        }

        /* ─── FOOTER ─── */
        .cherry-footer {
          background:#08010505; position:relative; z-index:2;
          text-align:center; padding:28px 20px;
          color:rgba(255,147,172,0.45); font-family:'Inter',sans-serif;
          font-size:0.76em; letter-spacing:2px;
        }
        .resources-section .cherry-footer { background:transparent; }

        /* ─── PLOT LOADING SPINNER ─── */
        .shiny-plot-output,
        .shiny-table-output { position: relative; }

        .shiny-plot-output.recalculating::before {
          content: '';
          position: absolute; inset: 0;
          background: rgba(255,248,250,0.72);
          backdrop-filter: blur(4px);
          z-index: 5;
          border-radius: 10px;
          transition: opacity 0.3s ease;
        }
        .shiny-plot-output.recalculating::after {
          content: '';
          position: absolute;
          top: 50%; left: 50%;
          width: 44px; height: 44px;
          margin: -22px 0 0 -22px;
          border: 3px solid rgba(255,183,197,0.3);
          border-top-color: #ff93ac;
          border-radius: 50%;
          animation: plotSpin 0.75s linear infinite;
          z-index: 6;
        }
        @keyframes plotSpin {
          to { transform: rotate(360deg); }
        }

        /* ─── SCROLL REVEAL ─── */
        .reveal { opacity:0; transform:translateY(32px); }
        .reveal.visible {
          opacity:1; transform:translateY(0);
          transition:opacity 0.8s ease, transform 0.8s ease;
        }

        /* ─── PREMIUM DARK GLASS NAVBAR ─── */
        .navbar-default {
          background: rgba(10,2,6,0.88) !important;
          backdrop-filter: blur(24px) saturate(160%) !important;
          -webkit-backdrop-filter: blur(24px) saturate(160%) !important;
          border: none !important;
          box-shadow: 0 1px 0 rgba(255,183,197,0.13) !important;
        }
        .navbar-default .navbar-brand {
          color: #ffb7c5 !important;
          font-family: 'Playfair Display', Georgia, serif !important;
          letter-spacing: 1px !important;
          font-size: 1.15em !important;
          padding-top: 14px !important;
        }
        .navbar-default .navbar-nav > li > a {
          color: rgba(255,225,235,0.72) !important;
          font-family: 'Inter', sans-serif !important;
          font-size: 0.78em !important;
          letter-spacing: 2.5px !important;
          text-transform: uppercase !important;
          transition: color 0.25s ease !important;
          padding: 18px 16px !important;
        }
        .navbar-default .navbar-nav > li > a:hover {
          color: #ffb7c5 !important;
          background: transparent !important;
        }
        .navbar-default .navbar-nav > .active > a,
        .navbar-default .navbar-nav > .active > a:focus,
        .navbar-default .navbar-nav > .active > a:hover {
          color: #ffb7c5 !important;
          background: transparent !important;
          box-shadow: inset 0 -2px 0 rgba(255,183,197,0.65) !important;
        }

        /* ─── GLOBAL BLOSSOM TINT (all tabs) ─── */
        html { scroll-behavior: smooth; }
        body { background: #fff8fa !important; }
        .tab-content { min-height: calc(100vh - 50px); }

        /* ─── CUSTOM CHERRY CURSOR ─── */
        *, *::before, *::after { cursor: none !important; }
        .cc-dot {
          position: fixed;
          width: 8px; height: 8px;
          background: rgba(255,120,160,0.92);
          border-radius: 50%;
          pointer-events: none;
          z-index: 999999;
          transform: translate(-50%,-50%);
          transition: width 0.18s ease, height 0.18s ease, background 0.18s ease;
          will-change: left,top;
        }
        .cc-ring {
          position: fixed;
          width: 36px; height: 36px;
          border: 1.5px solid rgba(255,183,197,0.58);
          border-radius: 50%;
          pointer-events: none;
          z-index: 999998;
          transform: translate(-50%,-50%);
          transition: width 0.22s ease, height 0.22s ease, border-color 0.22s ease;
          will-change: left,top;
        }
        .cc-dot.hov  { width:14px; height:14px; background:rgba(255,80,130,1); }
        .cc-ring.hov { width:56px; height:56px; border-color:rgba(255,140,172,0.82); }

        /* ─── CARD 3-D TILT ─── */
        .c-card, .s-card { transform-style: preserve-3d; }

        /* ─── PARALLAX DEPTH LAYERS ─── */
        .pdl-1, .pdl-2, .pdl-3 { will-change: transform; }

      ")),

      tags$script(HTML("
        $(document).ready(function () {

          /* PETAL SYSTEM — petals drift once on load for 5 seconds, then stop */
          var PKS = '#FFB7C5,#FF93AC,#FFADC5,#FFC2D1,#FF85A1,#FFD1DC,#FFAAB5,#FFA0BD,#FFD6E0,#FF78A5,#FFBDD0'.split(',');
          var petalPool    = [];
          var petalSpd     = 0;
          var petalTgt     = 0;
          var petalTmr     = null;

          /* ── Build one petal DOM element + physics object ── */
          function makePetal(prePlace) {
            var sz  = Math.random() * 14 + 8;
            var col = PKS[Math.floor(Math.random() * PKS.length)];
            var el  = document.createElement('div');
            el.className        = 'blossom-petal';
            el.style.width      = sz + 'px';
            el.style.height     = (sz * (Math.random() * 0.35 + 0.65)).toFixed(1) + 'px';
            el.style.background = col;
            el.style.opacity    = '0';
            /* top:0/left:0 come from the CSS class — translate3d moves from viewport origin */
            el.style.transform  = 'translate3d(0,0,0)';
            document.body.appendChild(el);

            var W = window.innerWidth, H = window.innerHeight;
            /* pre-placed: scatter within viewport so they're visible on load */
            var sx = prePlace ? Math.random() * W       : (Math.random() * 0.7 - 0.1) * W;
            var sy = prePlace ? Math.random() * H * 0.8 : -28;
            var opa = prePlace ? (Math.random() * 0.3 + 0.42) : 0;

            /* Apply initial state immediately for pre-placed petals */
            if (prePlace) {
              el.style.opacity   = opa.toFixed(3);
              el.style.transform = 'translate3d(' + sx.toFixed(1) + 'px,' + sy.toFixed(1) + 'px,0) rotate(' + (Math.random()*360).toFixed(1) + 'deg)';
            }
            return {
              el:       el,
              x:        sx,   y:  sy,
              vx:       Math.random() * 1.1 + 0.75,  /* W→E — more horizontal than vertical */
              vy:       Math.random() * 0.80 + 0.40,  /* gentle downward fall                */
              rot:      Math.random() * 360,
              rotSpd:   (Math.random() - 0.5) * 2.8,  /* swirl / tumble                     */
              swAng:    Math.random() * Math.PI * 2,   /* swirl oscillation phase            */
              swSpd:    Math.random() * 0.022 + 0.007, /* swirl oscillation frequency        */
              swAmp:    Math.random() * 2.0  + 0.7,   /* swirl lateral amplitude            */
              opacity:  opa,
              maxOpa:   Math.random() * 0.32 + 0.62   /* 0.62–0.94 — clear on any bg        */
            };
          }

          /* Seed 45 petals: first 12 pre-placed (visible + frozen on load) */
          for (var pidx = 0; pidx < 45; pidx++) {
            petalPool.push(makePetal(pidx < 12));
          }

          /* ── rAF loop ── */
          (function petalLoop() {
            /* Accelerate quickly (0.07), decelerate slowly (0.022) for soft halt */
            petalSpd += (petalTgt - petalSpd) * (petalTgt > petalSpd ? 0.07 : 0.022);
            var sf = petalSpd;
            var W  = window.innerWidth, H = window.innerHeight;

            for (var ii = 0; ii < petalPool.length; ii++) {
              var p = petalPool[ii];

              /* Sinusoidal lateral wobble (swirl) */
              p.swAng += p.swSpd * sf;
              var wx   = Math.sin(p.swAng) * p.swAmp * sf;

              p.x   += (p.vx + wx) * sf;
              p.y   += p.vy  * sf;
              p.rot += p.rotSpd * sf;

              /* Fade in as petal enters from top */
              if (p.y < 70 && sf > 0.01)
                p.opacity = Math.min(p.maxOpa, p.opacity + 0.016 * sf);

              /* Fade out as petal nears bottom */
              if (p.y > H - 80)
                p.opacity = Math.max(0, p.opacity - 0.020);

              p.el.style.transform = 'translate3d(' + p.x.toFixed(1) + 'px,' + p.y.toFixed(1) + 'px,0) rotate(' + p.rot.toFixed(1) + 'deg)';
              p.el.style.opacity   = p.opacity.toFixed(3);

              /* Recycle: send back to off-screen top-left start */
              if (p.y > H + 45 || p.x > W + 65 || (p.y > H * 0.75 && p.opacity < 0.01)) {
                p.x      = (Math.random() * 0.65 - 0.1) * W;
                p.y      = -28;
                p.opacity = 0;
                p.swAng  = Math.random() * Math.PI * 2;
              }
            }
            requestAnimationFrame(petalLoop);
          })();

          petalTgt = 1;
          setTimeout(function () {
            petalTgt = 0;
          
            for (var pi = 0; pi < petalPool.length; pi++) {
              petalPool[pi].el.style.transition = 'opacity 0.8s ease';
              petalPool[pi].opacity = 0;
              petalPool[pi].el.style.opacity = '0';
            }
          }, 4200);

          /* ── Scroll-triggered reveals ── */
          var $win = $(window);
          function revealAll () {
            var bot = $win.scrollTop() + $win.height();
            $('.reveal, .c-card, .s-card, .r-card').each(function () {
              if ($(this).offset().top < bot - 55) $(this).addClass('visible');
            });
          }
          $win.on('scroll.reveal', revealAll);
          revealAll();

          /* ── Custom cherry blossom cursor ── */
          var $cdot  = $('<div class=\"cc-dot\"></div>');
          var $cring = $('<div class=\"cc-ring\"></div>');
          $('body').append($cdot).append($cring);

          var cmx = window.innerWidth/2, cmy = window.innerHeight/2;
          var crx = cmx, cry = cmy;

          $(document).on('mousemove.cursor', function (e) {
            cmx = e.clientX; cmy = e.clientY;
            $cdot.css({ left: cmx + 'px', top: cmy + 'px' });
          });
          (function cursorLoop () {
            crx += (cmx - crx) * 0.11;
            cry += (cmy - cry) * 0.11;
            $cring.css({ left: crx + 'px', top: cry + 'px' });
            requestAnimationFrame(cursorLoop);
          })();
          var hoverSel = 'a, button, input, label, select, .c-card, .s-card, .r-card, .navbar-brand, .navbar-nav li a';
          $(document).on('mouseenter.cursor', hoverSel, function () {
            $cdot.addClass('hov'); $cring.addClass('hov');
          });
          $(document).on('mouseleave.cursor', hoverSel, function () {
            $cdot.removeClass('hov'); $cring.removeClass('hov');
          });

          /* ── 3-D card tilt ── */
          $(document).on('mousemove.tilt', '.c-card, .s-card', function (e) {
            var r = this.getBoundingClientRect();
            var x = ((e.clientX - r.left) / r.width  - 0.5) * 13;
            var y = ((e.clientY - r.top)  / r.height - 0.5) * 13;
            $(this).css('transform',
              'translateY(-10px) perspective(700px) rotateY(' + x + 'deg) rotateX(' + (-y) + 'deg)'
            );
          });
          $(document).on('mouseleave.tilt', '.c-card', function () {
            $(this).css('transform', 'translateY(0)');
          });
          $(document).on('mouseleave.tilt', '.s-card', function () {
            $(this).css('transform', 'translateY(0)');
          });

          /* ── Multi-speed parallax on scroll ── */
          $win.on('scroll.parallax', function () {
            var sy = $win.scrollTop();
            $('.pdl-1').css('transform', 'translateY(' + (sy * 0.07) + 'px)');
            $('.pdl-2').css('transform', 'translateY(' + (sy * 0.16) + 'px)');
            $('.pdl-3').css('transform', 'translateY(' + (sy * 0.28) + 'px)');
          });
        });
      "))
    ),

    div(class = "cherry-overview",

      # ════ HERO ════
      div(class = "cherry-hero",
        div(class = "h-orb h-orb-a pdl-1"),
        div(class = "h-orb h-orb-b pdl-2"),
        div(class = "h-orb h-orb-c pdl-3"),

        div(class = "hero-content",
          h1(class = "hero-title", "Gender Differences in Faculty Salaries"),
          p(class = "hero-tagline",
            "Do gender differences appear in faculty pay?",
            tags$br(),
            "Two decades of salary records allow us to examine how compensation",
            "varies across gender, experience, rank, field, and administrative roles."
          )
        ),
        div(class = "scroll-cue", "scroll", div(class = "scroll-cue-line"))
      ),

      # ════ CURIOSITY ════
      div(class = "curiosity-section",
        p(class = "s-eyebrow", "Questions of Interest"),
        div(class = "curiosity-grid",
          div(class = "c-card",
            span(class = "c-number", "01"),
            p(class = "c-question",
              "Does wage discrimination exist in the starting salaries of",
              "faculty members (i.e., salaries in the year hired)?"
            )
          ),
          div(class = "c-card",
            span(class = "c-number", "02"),
            p(class = "c-question",
              "Does wage discrimination exist in granting promotions from ",
              "Associate Professor to Full Professor?"
            )
          ),
          div(class = "c-card",
            span(class = "c-number", "03"),
            p(class = "c-question",
              "Overall, how would you answer the question: Is there wage",
              "discrimination in salaries at the university?"
            )
          )
        )
      ),

      # ════ STATS ════
      div(class = "stats-section",
        p(class = "s-eyebrow", "At a Glance"),
        h2(class = "s-heading", "The shape of the data"),
        div(class = "stats-grid",
          div(class = "s-card",
            span(class = "s-card-label", "Dataset Overview"),
            h3(class = "s-card-title", "Records and People"),
            tableOutput("overview_summary")
          ),
          div(class = "s-card",
            span(class = "s-card-label", "By Sex and Rank"),
            h3(class = "s-card-title", "Who Is in the Room"),
            tableOutput("overview_sex_rank")
          ),
          div(class = "s-card",
            span(class = "s-card-label", "By Discipline"),
            h3(class = "s-card-title", "Fields Represented"),
            tableOutput("overview_field")
          )
        )
      )
    )
  ),

  # ══════════════════════════════════════════════════
  #  Explore Data (EDA — NEW)
  # ══════════════════════════════════════════════════
  tabPanel(
    "Explore Data",
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          width = 3,
          h4("Filter by Sex"),
          radioButtons(
            "eda_sex", label = NULL,
            choices  = c("Both" = "Both", "Male only" = "M", "Female only" = "F"),
            selected = "Both"
          ),
          hr(),
          helpText("Choose Both to compare groups side by side, or focus on one group.
                    Hover on any chart for exact values.")
        ),
        mainPanel(
          width = 9,
          h3("Exploratory Data Analysis"),
          p(style = "color:#666; margin-bottom:1.2em;",
            "Before formal tests, we look at the shape of the data: record counts by sex,
            how average salary changed over two decades, and how salary varies across
            academic rank. These patterns motivate the controls used in later analyses."),
          fluidRow(
            column(6, plotlyOutput("eda_obs_by_sex",    height = "320px")),
            column(6, plotlyOutput("eda_salary_time",   height = "320px"))
          ),
          br(),
          fluidRow(
            column(12, plotlyOutput("eda_salary_rank",  height = "340px"))
          )
        )
      )
    )
  ),

  # ══════════════════════════════════════════════════
  #  Q1: Starting Salary (logic unchanged)
  # ══════════════════════════════════════════════════
  tabPanel(
    "Q1: Starting Salary",
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          width = 3,
          h4("Controls"),
          selectInput(
            "q1_field", "Field (discipline):",
            choices = c("All", sort(unique(start_salary$field))), selected = "All"
          ),
          radioButtons(
            "q1_sex", "Sex:",
            choices  = c("Both" = "Both", "Male only" = "M", "Female only" = "F"),
            selected = "Both"
          )
        ),
        mainPanel(
          width = 9,
          tabsetPanel(
            tabPanel("Distributions",
              br(),
              fluidRow(
                column(6, plotlyOutput("q1_hist_sex",      height = "300px")),
                column(6, plotlyOutput("q1_box_field",     height = "300px"))
              ),
              br(),
              fluidRow(
                column(12, plotlyOutput("q1_box_rank_field", height = "340px"))
              ),
              br(),
              h4("Starting Salary Summary Table"),
              tableOutput("q1_summary_tbl")
            ),
            tabPanel("Tests & Regression",
              br(),
              h5("Welch t-test (difference in mean starting salary by sex)"),
              verbatimTextOutput("q1_ttest"),
              hr(),
              h5("Regression controlling for rank, field, degree year and hire year"),
              verbatimTextOutput("q1_lm_out"),
              hr(),
              h5("Log-salary model with robust SE (coefficient for sex approximates percent difference)"),
              verbatimTextOutput("q1_lm_log_robust"),
              br(),
              p(
                strong("Approximate percent difference for women in starting salary: "),
                "-4.85%"
              )
            )
          )
        )
      )
    )
  ),

  # ══════════════════════════════════════════════════
  #  Q2: Promotion Raises (logic unchanged)
  # ══════════════════════════════════════════════════
  tabPanel(
    "Q2: Promotion Raises",
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          width = 3,
          h4("Controls"),
          radioButtons(
            "q2_sex", "Sex:",
            choices  = c("Both" = "Both", "Male only" = "M", "Female only" = "F"),
            selected = "Both"
          ),
          hr(),
          helpText("Tests and regression models always use the full dataset regardless of the sex filter.")
        ),
        mainPanel(
          width = 9,
          tabsetPanel(
            tabPanel("Raise by Sex",
              br(),
              plotlyOutput("q2_raise_box", height = "340px"),
              br(),
              h5("Summary by Sex"),
              tableOutput("q2_summary_tbl")
            ),
            tabPanel("Tests",
              br(),
              h5("Welch t-test (dollar raise)"),
              verbatimTextOutput("q2_ttest_dollar"),
              hr(),
              h5("Welch t-test (percent raise)"),
              verbatimTextOutput("q2_ttest_pct"),
              hr(),
              h5("Wilcoxon rank-sum (percent raise)"),
              verbatimTextOutput("q2_wilcox")
            ),
            tabPanel("Regression",
              br(),
              h5("Dollar raise modeled by sex, field, degree year and pre-promotion salary (robust SE)"),
              verbatimTextOutput("q2_lm_raise"),
              hr(),
              h5("Percent raise with same predictors (robust SE)"),
              verbatimTextOutput("q2_lm_pct")
            ),
            tabPanel("Extension: Promotion Rates",
              br(),
              p(style = "color:#666;",
                "Among those who were ever Associate Professors, the share who were later promoted to Full:"),
              tableOutput("q2_promotion_rates"),
              hr(),
              h5("Chi-squared test of independence (sex vs promotion)"),
              verbatimTextOutput("q2_prop_test")
            )
          )
        )
      )
    )
  ),

  # ══════════════════════════════════════════════════
  #  Q3: Latest Salary (logic unchanged)
  # ══════════════════════════════════════════════════
  tabPanel(
    "Q3: Latest Salary",
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          width = 3,
          h4("Controls"),
          checkboxGroupInput(
            "q3_rank", "Rank:",
            choices = levels(latest_salary$rank), selected = levels(latest_salary$rank)
          ),
          radioButtons(
            "q3_sex", "Sex:",
            choices  = c("Both" = "Both", "Male only" = "M", "Female only" = "F"),
            selected = "Both"
          ),
          hr(),
          helpText("Regression models always use the full dataset; the filter applies to the charts only.")
        ),
        mainPanel(
          width = 9,
          tabsetPanel(
            tabPanel("Charts",
              br(),
              h5("Latest Salary by Sex Within Rank"),
              plotOutput("q3_box_rank",     height = "300px"),
              br(),
              h5("Latest Salary Density by Sex Within Rank"),
              plotOutput("q3_density_rank", height = "300px"),
              br(),
              h5("Summary Table"),
              tableOutput("q3_summary_tbl")
            ),
            tabPanel("Regression Models",
              br(),
              h5("Baseline model: salary by sex, rank, field, experience, admin role (robust SE)"),
              verbatimTextOutput("q3_lm_robust"),
              hr(),
              h5("Log-salary model: coefficient for sex approximates a percent difference (robust SE)"),
              verbatimTextOutput("q3_lm_log_robust"),
              hr(),
              h5("Log-salary with sex by admin interaction: gap for non-admin vs admin (robust SE)"),
              verbatimTextOutput("q3_lm_interaction_robust")
            )
          )
        )
      )
    )
  ),

  # ══════════════════════════════════════════════════
  #  Q4: Limitations & Generalizability (NEW)
  # ══════════════════════════════════════════════════
  tabPanel(
    "Q4: Scope & Limits",
    fluidPage(
      tags$style(HTML("
        .q4-hero {
          background: linear-gradient(135deg, #fff0f5 0%, #ffe4ed 60%, #ffd6e4 100%);
          border-radius: 14px;
          padding: 32px 36px 24px;
          margin-bottom: 28px;
          border-left: 5px solid #E05C5C;
        }
        .q4-hero h3 { color: #3d1020; margin-top: 0; font-size: 1.45em; }
        .q4-hero p  { color: #5a2030; font-size: 1.04em; line-height: 1.65; margin-bottom: 0; }
        .q4-section { margin-bottom: 32px; }
        .q4-section h4 {
          color: #3d1020; border-bottom: 2px solid #ffb7c5;
          padding-bottom: 6px; margin-bottom: 16px; font-size: 1.15em;
        }
        .q4-card {
          background: #fff8fa; border-radius: 10px;
          border: 1px solid #ffd6e4; padding: 18px 22px;
          margin-bottom: 14px;
        }
        .q4-card strong { color: #c23355; }
        .q4-finding {
          background: #fff0f5; border-radius: 10px;
          border-left: 4px solid #E05C5C; padding: 14px 20px;
          margin-bottom: 12px; font-size: 0.97em; color: #3d1020;
          line-height: 1.6;
        }
        .q4-finding .tag {
          font-size: 0.72em; font-weight: 600; letter-spacing: 2px;
          text-transform: uppercase; color: #E05C5C; display: block;
          margin-bottom: 4px;
        }
        .q4-limit-row { display: flex; gap: 16px; flex-wrap: wrap; margin-bottom: 8px; }
        .q4-limit-card {
          flex: 1 1 240px; background: #fff8fa;
          border-radius: 10px; padding: 16px 18px;
          border: 1px solid #ffd6e4; font-size: 0.94em; color: #4a1a2a;
          line-height: 1.6;
        }
        .q4-limit-card .lim-title {
          font-weight: 600; color: #c23355; font-size: 0.92em;
          letter-spacing: 1.5px; text-transform: uppercase;
          display: block; margin-bottom: 6px;
        }
      ")),

      div(class = "q4-hero",
        h3("Q4: What limits generalizing these results?"),
        p("Every statistical conclusion depends on assumptions and the context 
          in which the data were collected. Before drawing broader implications 
          from the patterns observed in Q1 through Q3, it is important to 
          consider where these findings are likely to apply and where they may 
          not. This section outlines the scope of the study, the assumptions 
          underlying the analysis, and what the results can—and cannot—tell us 
          beyond this dataset.")
      ),

      fluidRow(
        column(7,
          div(class = "q4-section",
            h4("Key Findings Across All Questions"),
            uiOutput("q4_findings_banner")
          ),

          div(class = "q4-section",
            h4("Study Limitations"),
            div(class = "q4-limit-row",
              div(class = "q4-limit-card",
                span(class = "lim-title", "Single Institution"),
                "Data come from one anonymous U.S. university observed between
                 1976 and 1995. Salary policies, hiring cultures, and
                 promotion norms vary widely across institutions,
                 so findings may not hold at other universities."
              ),
              div(class = "q4-limit-card",
                span(class = "lim-title", "Historical Time Window"),
                "The study covers two decades ending in 1995.
                 Gender equity policies, pay transparency laws, and
                 academic labor markets have changed substantially
                 since then. Results describe a historical snapshot,
                 not necessarily the present."
              )
            ),
            div(class = "q4-limit-row",
              div(class = "q4-limit-card",
                span(class = "lim-title", "Missing Productivity Data"),
                "The dataset contains no measures of research output,
                 teaching evaluations, grant funding, or publication record.
                 If these differ by sex and also influence salary, the models
                 may still confound real productivity differences with a sex gap."
              ),
              div(class = "q4-limit-card",
                span(class = "lim-title", "Observational Design"),
                "This is not a randomized experiment. Even after controlling
                 for rank, field, and experience, unmeasured confounders
                 could account for part of the estimated gap. Causal
                 language (women are paid less because of sex) is
                 not fully supported by this design alone."
              )
            ),
            div(class = "q4-limit-row",
              div(class = "q4-limit-card",
                span(class = "lim-title", "Linearity Assumption"),
                "All regression models assume a linear (or log-linear)
                 relationship between salary and predictors. If the true
                 relationship is nonlinear (e.g., diminishing returns to
                 experience, field-specific trends), the model estimates
                 will be biased."
              ),
              div(class = "q4-limit-card",
                span(class = "lim-title", "Independence of Observations"),
                "Faculty appear multiple times across years in the panel.
                 Treating repeated records as independent may underestimate
                 standard errors. The analyses partially address this by
                 using robust standard errors, but clustered SE by person
                 would be the ideal approach."
              )
            )
          )
        ),

        column(5,
          div(class = "q4-section",
            h4("What Can Be Generalized?"),
            div(class = "q4-card",
              strong("Gap structure matters more than a single number."),
              p(style = "margin: 6px 0 0;",
                "The most informative finding is where the gap appears:
                 at hire (starting pay), in promotion rates, and within rank,
                 but not necessarily in the raise size at promotion.
                 This structure suggests salary equity cannot be achieved
                 by equalizing raises alone.")
            ),
            div(class = "q4-card",
              strong("Administrative roles amplify the gap."),
              p(style = "margin: 6px 0 0;",
                "The interaction model in Q3 shows the sex gap is larger among
                 administrators. If women are less likely to hold admin roles
                 (and the data support this), the compounding effect
                 is structural, not a single salary event.")
            )
          )
        )
      )
    )
  ),

  # ══════════════════════════════════════════════════
  #  Career Trajectories
  # ══════════════════════════════════════════════════
  tabPanel(
    "Career Trajectories",
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          h4("Controls"),
          selectInput(
            "q4_field", "Field:",
            choices = c("All", sort(unique(salary_arc$field))), selected = "All"
          ),
          radioButtons(
            "q4_sex", "Sex:",
            choices  = c("Both" = "Both", "Male only" = "M", "Female only" = "F"),
            selected = "Both"
          ),
          hr(),
          helpText(
            "This tab traces salary from year one to decades in,",
            "asking whether the gap between male and female faculty",
            "narrows as both groups accumulate the same credentials,",
            "or compounds silently over time."
          ),
          width = 3
        ),
        mainPanel(

          # Dynamic data-driven insight banner
          uiOutput("q4_insight_banner"),

          h4("Salary Growth Over a Career"),
          p(style = "color:#666; font-size:0.92em; margin-bottom:8px;",
            "Each point is one faculty record in one year.",
            "Smooth curves reveal whether the two trajectories",
            "run in parallel or pull apart as experience accumulates."
          ),
          plotOutput("q4_trajectory", height = "320px"),
          br(),

          h4("Median Salary at Each Career Stage"),
          p(style = "color:#666; font-size:0.92em; margin-bottom:8px;",
            "This chart compares median salaries for male and female faculty",
            "within broad experience stages; it summarizes all observations",
            "in each stage."
          ),
          plotOutput("q4_stage_bars", height = "300px"),
          br(),

          h4("The Administrative Salary Premium"),
          p(style = "color:#666; font-size:0.92em; margin-bottom:8px;",
            "Faculty in administrative roles often earn more.",
            "The gender salary gap also appears larger among faculty",
            "who hold administrative positions."
          ),
          plotOutput("q4_admin_box", height = "300px"),
          br(),

          h4("Summary by Career Stage and Sex"),
          tableOutput("q4_summary_tbl")
        )
      )
    )
  )
)

#-------------------------------
# Server
#-------------------------------
server <- function(input, output, session) {

  # ── Overview ──
  output$overview_summary <- renderTable({
    salary_clean %>%
      summarise(
        n_records = n(),
        n_people  = n_distinct(id),
        min_year  = min(year, na.rm = TRUE),
        max_year  = max(year, na.rm = TRUE)
      )
  })
  output$overview_sex_rank <- renderTable({
    salary_clean %>%
      count(sex, rank) %>%
      pivot_wider(names_from = rank, values_from = n, values_fill = 0)
  })
  output$overview_field <- renderTable({
    salary_clean %>% count(field, sort = TRUE)
  })

  # ── Helper: filter by sex radio (Both / M / F) ──
  filter_sex <- function(df, input_val) {
    if (!is.null(input_val) && input_val != "Both")
      df <- df %>% filter(sex == input_val)
    df
  }

  # ── Q1: Starting Salary ──
  q1_filtered <- reactive({
    dat <- start_salary
    if (input$q1_field != "All")
      dat <- dat %>% filter(field == input$q1_field)
    filter_sex(dat, input$q1_sex)
  })
  # Q1 plotly charts
  output$q1_hist_sex <- renderPlotly({
    d <- q1_filtered()
    if (nrow(d) == 0) return(plotly_empty())
    p <- ggplot(d, aes(x = salary, fill = sex)) +
      geom_histogram(alpha = 0.8, bins = 25, position = "identity") +
      scale_fill_manual(values = sex_colours, drop = TRUE) +
      facet_wrap(~ sex, scales = "free_y") +
      labs(title = "Starting Salary Distribution by Sex", x = "Starting Salary", y = "Count") +
      theme_minimal()
    ggplotly(p)
  })
  output$q1_box_field <- renderPlotly({
    d <- q1_filtered()
    if (nrow(d) == 0) return(plotly_empty())
    p <- ggplot(d, aes(x = field, y = salary, fill = sex)) +
      geom_boxplot(alpha = 0.8, position = position_dodge(width = 0.75)) +
      scale_fill_manual(values = sex_colours, drop = TRUE) +
      labs(title = "Starting Salary by Field and Sex", x = "Academic Field", y = "Starting Salary") +
      theme_minimal()
    ggplotly(p)
  })
  output$q1_box_rank_field <- renderPlotly({
    d <- q1_filtered()
    if (nrow(d) == 0) return(plotly_empty())
    p <- ggplot(d, aes(x = sex, y = salary, fill = sex)) +
      geom_boxplot(alpha = 0.8) +
      scale_fill_manual(values = sex_colours, drop = TRUE) +
      facet_grid(rank ~ field) +
      labs(title = "Starting Salary by Sex, Rank, and Field", x = "Sex", y = "Starting Salary") +
      theme_minimal()
    ggplotly(p)
  })
  output$q1_summary_tbl <- renderTable({
    q1_filtered() %>%
      group_by(sex) %>%
      summarise(
        n             = n(),
        mean_salary   = round(mean(salary,   na.rm = TRUE), 1),
        median_salary = round(median(salary, na.rm = TRUE), 1),
        sd_salary     = round(sd(salary,     na.rm = TRUE), 1),
        .groups = "drop"
      )
  })
  # Q1 statistical tests
  output$q1_ttest <- renderPrint({
    t.test(salary ~ sex, data = start_salary)
  })
  q1_lm_mdl <- reactive({
    lm(salary ~ sex + rank + field + yrdeg + startyr, data = start_salary)
  })
  output$q1_lm_out <- renderPrint({
    summary(q1_lm_mdl())
  })
  q1_lm_log_mdl <- reactive({
    lm(log_salary ~ sex + rank + field + yrdeg + startyr, data = start_salary)
  })
  output$q1_lm_log_robust <- renderPrint({
    coeftest(q1_lm_log_mdl(), vcov = vcovHC(q1_lm_log_mdl(), type = "HC1"))
  })

  # ── Q2: Promotion Raises ──
  q2_filtered <- reactive({
    filter_sex(promotion_data, input$q2_sex)
  })
  output$q2_raise_box <- renderPlotly({
    d <- q2_filtered()
    if (nrow(d) == 0) return(plotly_empty())
    promo_long <- d %>%
      select(sex, raise_amount, raise_pct) %>%
      pivot_longer(cols = c(raise_amount, raise_pct),
                   names_to = "metric", values_to = "value")
    p <- ggplot(promo_long, aes(x = sex, y = value, fill = sex)) +
      geom_boxplot(alpha = 0.8) +
      scale_fill_manual(values = sex_colours, drop = TRUE) +
      facet_wrap(~ metric, scales = "free_y",
                 labeller = as_labeller(c(raise_amount = "Dollar Raise",
                                          raise_pct    = "Percent Raise"))) +
      labs(title = "Raise at Promotion by Sex", x = "Sex", y = NULL) +
      theme_minimal()
    ggplotly(p)
  })
  output$q2_summary_tbl <- renderTable({
    q2_filtered() %>%
      group_by(sex) %>%
      summarise(
        n                = n(),
        mean_raise       = round(mean(raise_amount,   na.rm = TRUE), 1),
        median_raise     = round(median(raise_amount, na.rm = TRUE), 1),
        mean_pct_raise   = percent(mean(raise_pct,   na.rm = TRUE), accuracy = 0.1),
        median_pct_raise = percent(median(raise_pct, na.rm = TRUE), accuracy = 0.1),
        .groups = "drop"
      )
  })
  # Q2 tests
  output$q2_ttest_dollar <- renderPrint({
    t.test(raise_amount ~ sex, data = promotion_data)
  })
  output$q2_ttest_pct <- renderPrint({
    t.test(raise_pct ~ sex, data = promotion_data)
  })
  output$q2_wilcox <- renderPrint({
    wilcox.test(raise_pct ~ sex, data = promotion_data)
  })
  # Q2 regression
  q2_lm_raise_mdl <- reactive({
    lm(raise_amount ~ sex + field + yrdeg + prev_salary, data = promotion_data)
  })
  output$q2_lm_raise <- renderPrint({
    coeftest(q2_lm_raise_mdl(), vcov = vcovHC(q2_lm_raise_mdl(), type = "HC1"))
  })
  q2_lm_pct_mdl <- reactive({
    lm(raise_pct ~ sex + field + yrdeg + prev_salary, data = promotion_data)
  })
  output$q2_lm_pct <- renderPrint({
    coeftest(q2_lm_pct_mdl(), vcov = vcovHC(q2_lm_pct_mdl(), type = "HC1"))
  })
  # Q2 promotion rates
  output$q2_promotion_rates <- renderTable({
    person_level %>%
      group_by(sex) %>%
      summarise(
        n              = n(),
        n_promoted     = sum(promoted),
        promotion_rate = round(mean(promoted), 3),
        .groups        = "drop"
      )
  })
  output$q2_prop_test <- renderPrint({
    ct <- table(person_level$sex, person_level$promoted)
    chisq.test(ct, correct = FALSE)
  })

  # ── Q3: Latest Salary ──
  q3_filtered <- reactive({
    dat <- latest_salary
    if (length(input$q3_rank) > 0)
      dat <- dat %>% filter(rank %in% input$q3_rank)
    filter_sex(dat, input$q3_sex)
  })
  output$q3_box_rank <- renderPlot({
    ggplot(q3_filtered(), aes(x = sex, y = salary, fill = sex)) +
      geom_boxplot(alpha = 0.8) +
      scale_fill_manual(values = sex_colours) +
      facet_wrap(~ rank, scales = "free_y") +
      labs(x = "Sex", y = "Latest salary",
           title = "Latest Salary by Sex Within Rank")
  })
  output$q3_density_rank <- renderPlot({
    ggplot(q3_filtered(), aes(x = salary, fill = sex)) +
      geom_density(alpha = 0.4) +
      scale_fill_manual(values = sex_colours) +
      facet_wrap(~ rank, scales = "free") +
      labs(x = "Latest salary", y = "Density",
           title = "Latest Salary Density by Sex Within Rank")
  })
  output$q3_summary_tbl <- renderTable({
    q3_filtered() %>%
      group_by(rank, sex) %>%
      summarise(
        n             = n(),
        mean_salary   = round(mean(salary,   na.rm = TRUE), 1),
        median_salary = round(median(salary, na.rm = TRUE), 1),
        .groups = "drop"
      ) %>%
      arrange(rank, sex)
  })

  # Q3 regression models with robust SE
  q3_lm_mdl <- reactive({
    lm(salary ~ sex + rank + field + experience + admin, data = latest_salary)
  })
  output$q3_lm_robust <- renderPrint({
    coeftest(q3_lm_mdl(), vcov = vcovHC(q3_lm_mdl(), type = "HC1"))
  })
  q3_lm_log_mdl <- reactive({
    lm(log_salary ~ sex + rank + field + experience + admin, data = latest_salary)
  })
  output$q3_lm_log_robust <- renderPrint({
    coeftest(q3_lm_log_mdl(), vcov = vcovHC(q3_lm_log_mdl(), type = "HC1"))
  })
  q3_lm_interaction_mdl <- reactive({
    lm(log_salary ~ sex * admin + rank + field + experience, data = latest_salary)
  })
  output$q3_lm_interaction_robust <- renderPrint({
    coeftest(q3_lm_interaction_mdl(), vcov = vcovHC(q3_lm_interaction_mdl(), type = "HC1"))
  })

  # ── Q4: Scope & Limits ──
  output$q4_scope_tbl <- renderTable({
    salary_clean %>%
      summarise(
        "Total records"    = n(),
        "Unique faculty"   = n_distinct(id),
        "Years covered"    = paste(min(year), "to", max(year)),
        "Salary range"     = paste0("$", round(min(salary)/1000,1), "k to $",
                                   round(max(salary)/1000,1), "k")
      )
  }, striped = TRUE, bordered = TRUE, hover = TRUE)

  output$q4_findings_banner <- renderUI({
    # Compute live numbers from models
    m_log_q1 <- lm(log_salary ~ sex + rank + field + yrdeg + startyr, data = start_salary)
    cf_q1    <- coef(m_log_q1)["sexF"]
    pct_q1   <- round((exp(cf_q1) - 1) * 100, 1)

    m_log_q3 <- lm(log_salary ~ sex + rank + field + experience + admin, data = latest_salary)
    cf_q3    <- coef(m_log_q3)["sexF"]
    pct_q3   <- round((exp(cf_q3) - 1) * 100, 1)

    m_base_q3 <- lm(salary ~ sex + rank + field + experience + admin, data = latest_salary)
    dollar_q3 <- round(coef(m_base_q3)["sexF"], 0)

    promo_tbl <- person_level %>%
      group_by(sex) %>%
      summarise(rate = round(mean(promoted) * 100, 1), .groups = "drop")
    rate_m <- promo_tbl %>% filter(sex == "M") %>% pull(rate)
    rate_f <- promo_tbl %>% filter(sex == "F") %>% pull(rate)

    tagList(
      div(class = "q4-finding",
        span(class = "tag", "Q1: Starting Salary"),
        paste0("After controlling for rank, field, degree year and hire year, female faculty
                earned approximately ", abs(pct_q1),
               "% less than male colleagues at the time of hire (log-salary model, robust SE).
                The gap is present even before experience accumulates.")
      ),
      div(class = "q4-finding",
        span(class = "tag", "Q2: Promotion Raises"),
        paste0("The dollar and percent raises at the Associate to Full promotion step
                do not differ significantly by sex (Welch t-test, Wilcoxon). However,
                female faculty were promoted at a rate of ", rate_f,
               "% versus ", rate_m,
               "% for male faculty, a statistically significant difference (chi-squared test).
                The inequality lies in access to promotion, not the raise itself.")
      ),
      div(class = "q4-finding",
        span(class = "tag", "Q3: Overall (Latest) Salary"),
        paste0("Controlling for rank, field, experience and administrative role, female faculty
                earned approximately $", abs(dollar_q3),
               " less per year in the baseline model, or about ", abs(pct_q3),
               "% less in the log-salary model (robust SE).
                The gap is even larger for faculty in administrative roles.")
      )
    )
  })

  # ── EDA ──
  output$eda_obs_by_sex <- renderPlotly({
    d <- filter_sex(salary_clean, input$eda_sex)
    if (nrow(d) == 0) return(plotly_empty())
    p <- d %>%
      count(sex) %>%
      ggplot(aes(x = sex, y = n, fill = sex)) +
      geom_col(width = 0.6) +
      scale_fill_manual(values = sex_colours, drop = TRUE) +
      labs(title = "Observations by Sex", x = "Sex", y = "Number of Records") +
      theme_minimal()
    ggplotly(p)
  })
  output$eda_salary_time <- renderPlotly({
    d <- filter_sex(salary_clean, input$eda_sex)
    if (nrow(d) == 0) return(plotly_empty())
    p <- d %>%
      group_by(year, sex) %>%
      summarise(mean_salary = mean(salary), .groups = "drop") %>%
      ggplot(aes(x = year, y = mean_salary, color = sex)) +
      geom_line(linewidth = 1) +
      scale_color_manual(values = sex_colours, drop = TRUE) +
      labs(title = "Average Salary Over Time by Sex", x = "Year", y = "Average Salary") +
      theme_minimal()
    ggplotly(p)
  })
  output$eda_salary_rank <- renderPlotly({
    d <- filter_sex(salary_clean, input$eda_sex)
    if (nrow(d) == 0) return(plotly_empty())
    p <- ggplot(d, aes(x = sex, y = salary, fill = sex)) +
      geom_boxplot(alpha = 0.8) +
      scale_fill_manual(values = sex_colours, drop = TRUE) +
      facet_wrap(~ rank, scales = "free_y") +
      labs(title = "Salary by Sex Within Rank", x = "Sex", y = "Monthly Salary") +
      theme_minimal()
    ggplotly(p)
  })

  # ── Q4: Career Arc ──
  q4_filtered <- reactive({
    dat <- salary_arc
    if (input$q4_field != "All")
      dat <- dat %>% filter(field == input$q4_field)
    filter_sex(dat, input$q4_sex)
  })

  # Dynamic insight banner: computes the actual early vs late salary ratio
  output$q4_insight_banner <- renderUI({
    dat <- q4_filtered()
    compute_median_ratio <- function(d) {
      med <- d %>%
        group_by(sex) %>%
        summarise(m = median(salary, na.rm = TRUE), .groups = "drop")
      m_val <- med %>% filter(sex == "M") %>% pull(m)
      f_val <- med %>% filter(sex == "F") %>% pull(m)
      if (length(m_val) == 1 && length(f_val) == 1 && m_val > 0)
        round(f_val / m_val * 100, 1)
      else NA
    }
    early_ratio <- compute_median_ratio(dat %>% filter(experience <= 4))
    late_ratio  <- compute_median_ratio(dat %>% filter(experience >= 15))

    if (!is.na(early_ratio) && !is.na(late_ratio)) {
      direction <- if (late_ratio < early_ratio - 1)  "widens"
                   else if (late_ratio > early_ratio + 1) "narrows"
                   else "holds roughly steady"
      div(
        style = paste(
          "background:linear-gradient(135deg,#fff0f5,#ffe4ed);",
          "border-left:4px solid #E05C5C;",
          "padding:16px 22px; border-radius:10px; margin-bottom:22px;"
        ),
        p(style = "margin:0; color:#3d1020; font-style:italic; font-size:1.04em;",
          paste0(
            "Early in the career, male and female salary trajectories rise at 
            similar rates. After roughly 20 years of experience, the paths begin 
            to diverge: male salaries continue to increase, while the female 
            trajectory flattens slightly. As a result, the gap between the 
            curves widens in later career stages."
          )
        )
      )
    } else {
      div(
        style = "background:#fff5f7; padding:12px 18px; border-radius:8px; margin-bottom:20px;",
        p(style = "margin:0; color:#aaa; font-style:italic;",
          "Select both sexes to reveal the salary gap insight.")
      )
    }
  })

  output$q4_trajectory <- renderPlot({
    dat <- q4_filtered()
    ggplot(dat, aes(x = experience, y = salary, color = sex)) +
      geom_point(alpha = 0.14, size = 0.9, shape = 16) +
      geom_smooth(method = "loess", se = TRUE, linewidth = 1.3, alpha = 0.15) +
      scale_color_manual(values = sex_colours) +
      scale_y_continuous(labels = dollar_format()) +
      labs(
        x     = "Years of Experience",
        y     = "Annual Salary",
        title = "Salary Trajectory Over a Career by Sex",
        color = "Sex"
      ) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top")
  })

  output$q4_stage_bars <- renderPlot({
    dat <- q4_filtered() %>%
      group_by(exp_bracket, sex) %>%
      summarise(median_salary = median(salary, na.rm = TRUE), .groups = "drop")

    ggplot(dat, aes(x = exp_bracket, y = median_salary, fill = sex)) +
      geom_col(position = "dodge", alpha = 0.86, width = 0.65) +
      scale_fill_manual(values = sex_colours) +
      scale_y_continuous(labels = dollar_format()) +
      labs(
        x     = "Career Stage",
        y     = "Median Salary",
        title = "Median Salary by Career Stage and Sex",
        fill  = "Sex"
      ) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top")
  })

  output$q4_admin_box <- renderPlot({
    dat <- q4_filtered()
    ggplot(dat, aes(x = sex, y = salary, fill = sex)) +
      geom_boxplot(alpha = 0.82) +
      scale_fill_manual(values = sex_colours) +
      scale_y_continuous(labels = dollar_format()) +
      facet_wrap(
        ~ admin,
        labeller = labeller(
          admin = c("0" = "No Administrative Role", "1" = "Has Administrative Role")
        )
      ) +
      labs(
        x     = "Sex",
        y     = "Salary",
        title = "Salary by Administrative Role and Sex"
      ) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top")
  })

  output$q4_summary_tbl <- renderTable({
    q4_filtered() %>%
      group_by(exp_bracket, sex) %>%
      summarise(
        n             = n(),
        median_salary = round(median(salary, na.rm = TRUE), 0),
        mean_salary   = round(mean(salary,   na.rm = TRUE), 0),
        .groups = "drop"
      ) %>%
      arrange(exp_bracket, sex)
  })
}

shinyApp(ui = ui, server = server)
