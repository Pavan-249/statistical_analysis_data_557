# app.R
library(shiny)
library(tidyverse)
library(scales)

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
  filter(year == startyr)
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

addResourcePath("app-inputs", "app-inputs")

#-------------------------------
# UI
#-------------------------------
ui <- navbarPage(
  title = "Faculty Salary Explorer",

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
          font-family:'Playfair Display',Georgia,serif; font-style:italic;
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
          font-style:italic; font-size:1.02em; line-height:1.85; margin:0;
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

        /* ─── AUDIO HINT ─── */
        #audio-hint {
          position:fixed; bottom:22px; left:26px;
          font-family:'Inter',sans-serif; font-size:0.6em;
          letter-spacing:3px; text-transform:uppercase;
          color:rgba(255,183,197,0.5); z-index:9000;
          pointer-events:none; transition:opacity 1.2s ease;
          animation:hintPulse 5s ease-in-out 1.5s both;
        }
        @keyframes hintPulse {
          0%   { opacity:0; }
          25%  { opacity:0.75; }
          75%  { opacity:0.75; }
          100% { opacity:0.3; }
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

          /* ════════════════════════════════════════════════════════
             PETAL SYSTEM  —  rAF loop, activity-driven
             Petals drift W→E with gentle swirl.
             They MOVE only while the user scrolls or moves the mouse.
             On idle they glide smoothly to a halt (no abrupt stop).
             12 petals start pre-placed in the viewport so the user
             sees them frozen on load — they wake when activity begins.
          ════════════════════════════════════════════════════════ */
          var PKS = '#FFB7C5,#FF93AC,#FFADC5,#FFC2D1,#FF85A1,#FFD1DC,#FFAAB5,#FFA0BD,#FFD6E0,#FF78A5,#FFBDD0'.split(',');
          var petalPool    = [];
          var petalSpd     = 0;
          var petalTgt     = 0;
          var petalTmr     = null;
          var petalEnabled = true;   /* false when user is on Q1-Q4 tabs */

          /* ── Activity detection (Overview only) ── */
          function petalWake() {
            if (!petalEnabled) return;
            petalTgt = 1;
            clearTimeout(petalTmr);
            petalTmr = setTimeout(function () {
              petalTgt = 0;
            }, 1800);
          }
          $(window).on('scroll.petals', petalWake);
          $(document).on('mousemove.petals touchmove.petals', petalWake);

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

          /* Auto-wake on load (Overview is default tab) */
          petalTgt = 1;
          petalTmr = setTimeout(function () { petalTgt = 0; }, 4000);

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

          /* ── Tab detection: petals only on Overview ── */
          function onTabChange(label) {
            var isOverview = (label === 'Overview');
            petalEnabled = isOverview;
            if (!isOverview) {
              /* Kill speed immediately and make every petal invisible.
                 Reset positions so they re-enter cleanly on Overview return. */
              petalTgt = 0;
              petalSpd = 0;
              clearTimeout(petalTmr);
              for (var pi = 0; pi < petalPool.length; pi++) {
                petalPool[pi].opacity = 0;
                petalPool[pi].el.style.opacity = '0';
                petalPool[pi].y = -28;
                petalPool[pi].x = (Math.random() * 0.7 - 0.1) * window.innerWidth;
              }
            } else {
              petalWake();
            }
          }
          $(document).on('shown.bs.tab', 'a[data-toggle=\"tab\"]', function (e) {
            onTabChange($(e.target).text().trim());
          });

          /* ── Background music ──────────────────────────────────────────
             Chrome's autoplay policy only honours genuine user gestures:
               wheel, click, mousedown, keydown, touchstart.
             scroll and mousemove are NOT recognised — that is why the old
             code was silent.  We now use the correct event set, show a
             tiny non-intrusive hint, and remove everything once music
             is playing.                                                  */
          var blossomAudio = document.getElementById('blossom-audio');
          if (blossomAudio) {
            blossomAudio.volume = 0.4;
            blossomAudio.loop   = true;

            /* Build the subtle hint element via JS so no extra HTML is needed */
            var aHint = document.createElement('div');
            aHint.id        = 'audio-hint';
            aHint.innerHTML = '&#9834;&nbsp;scroll to play';
            document.body.appendChild(aHint);

            var audioPlaying = false;
            /* Events Chrome actually counts as user gestures for media */
            var audioEvts = ['wheel','click','mousedown','keydown','touchstart'];

            function tryPlayAudio() {
              if (audioPlaying) return;
              blossomAudio.play()
                .then(function () {
                  audioPlaying = true;
                  /* clean up every listener */
                  audioEvts.forEach(function (ev) {
                    document.removeEventListener(ev, tryPlayAudio, true);
                  });
                  /* fade-out and remove the hint */
                  aHint.style.opacity = '0';
                  setTimeout(function () {
                    if (aHint.parentNode) aHint.parentNode.removeChild(aHint);
                  }, 1200);
                })
                .catch(function () {
                  /* Still blocked (very locked-down browser) — tell user to click */
                  if (!audioPlaying) aHint.textContent = '♪ click anywhere for music';
                });
            }

            /* Attach to every recognised-gesture event */
            audioEvts.forEach(function (ev) {
              document.addEventListener(ev, tryPlayAudio, { capture: true, passive: true });
            });

            /* Also attempt immediate play — works when the browser has granted
               autoplay permission for the domain (high Media-Engagement Index) */
            blossomAudio.play()
              .then(function () {
                audioPlaying = true;
                aHint.style.display = 'none';
                audioEvts.forEach(function (ev) {
                  document.removeEventListener(ev, tryPlayAudio, true);
                });
              })
              .catch(function () { /* expected — hint is already visible */ });
          }

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
          p(class = "hero-eyebrow", "A Faculty Salary Study"),
          h1(class = "hero-title", "Faculty Salary Explorer"),
          p(class = "hero-tagline",
            "What does fair pay look like in academia?",
            tags$br(),
            "A decade of salary records invites us to look beyond the numbers",
            "and ask harder questions about who earns what and why."
          )
        ),
        div(class = "scroll-cue", "scroll", div(class = "scroll-cue-line"))
      ),

      # ════ CURIOSITY ════
      div(class = "curiosity-section",
        p(class = "s-eyebrow", "Three Questions Worth Asking"),
        h2(class = "s-heading",
           "The data holds answers.", tags$br(),
           "But first, sit with the questions."
        ),
        div(class = "curiosity-grid",
          div(class = "c-card",
            span(class = "c-number", "01"),
            p(class = "c-question",
              "When two equally qualified candidates begin their academic careers,",
              "does their first paycheck reflect credentials alone",
              "or something less visible?"
            )
          ),
          div(class = "c-card",
            span(class = "c-number", "02"),
            p(class = "c-question",
              "Promotion from Associate to Full Professor is a milestone",
              "earned through years of scholarship.",
              "Does the salary reward match the effort for everyone who reaches it?"
            )
          ),
          div(class = "c-card",
            span(class = "c-number", "03"),
            p(class = "c-question",
              "A career in academia spans decades.",
              "Over that time, do early salary differences shrink",
              "as merit accumulates, or do they quietly grow year after year?"
            )
          )
        )
      ),

      # ════ STORY ════
      div(class = "story-section",
        div(class = "story-inner",
          p(class = "s-eyebrow", "The Dataset"),
          h2(class = "story-heading",
             "Hundreds of careers.", tags$br(),
             "One quiet question about equity."
          ),
          p(class = "story-para",
            "Tucked within hundreds of salary records spanning more than a decade,",
            "a quiet story unfolds. Faculty members across three academic disciplines",
            "built careers, earned promotions, and watched their paychecks evolve year after year."
          ),
          p(class = "story-para",
            "The numbers seem straightforward at first glance. But look closer.",
            "Who earns more when they first walk through the door?",
            "Does a promotion carry the same financial weight for everyone?",
            "After years in the field, do salary paths converge or drift further apart?"
          ),
          div(class = "story-callout",
            p("These are not just statistical questions. They are questions about equity,",
              "opportunity, and the invisible forces that shape academic careers.")
          ),
          p(class = "story-para",
            "Each tab in this app peels back one layer of that story.",
            "Start with hiring. Move to promotions. Trace the full arc of a career.",
            "Let the patterns speak."
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
      ),

      # ════ RESOURCES ════
      div(class = "resources-section",
        p(class = "s-eyebrow", "Project Documents"),
        h2(class = "s-heading", "Explore the full research journey"),
        div(class = "resources-grid",

          tags$a(class="r-card", href="app-inputs/Final-Proj-Code.pdf", target="_blank",
            # PDF icon (SVG, no emoji)
            tags$svg(class="r-doc-icon", viewBox="0 0 44 54", xmlns="http://www.w3.org/2000/svg",
              tags$rect(x="0",y="0",width="36",height="50",rx="4",fill="rgba(255,183,197,0.25)",stroke="rgba(255,183,197,0.5)",`stroke-width`="1.5"),
              tags$polygon(points="36,0 44,8 36,8",fill="rgba(255,147,172,0.4)"),
              tags$line(x1="8",y1="20",x2="28",y2="20",stroke="#ffb7c5",`stroke-width`="2",`stroke-linecap`="round"),
              tags$line(x1="8",y1="28",x2="28",y2="28",stroke="#ffb7c5",`stroke-width`="2",`stroke-linecap`="round"),
              tags$line(x1="8",y1="36",x2="20",y2="36",stroke="#ffb7c5",`stroke-width`="2",`stroke-linecap`="round")
            ),
            p(class="r-name","Final Project Code"),
            span(class="r-type","PDF")
          ),

          tags$a(class="r-card", href="app-inputs/annotated-Milestone%201_%20Project%20Proposal.pdf", target="_blank",
            tags$svg(class="r-doc-icon", viewBox="0 0 44 54", xmlns="http://www.w3.org/2000/svg",
              tags$rect(x="0",y="0",width="36",height="50",rx="4",fill="rgba(255,183,197,0.25)",stroke="rgba(255,183,197,0.5)",`stroke-width`="1.5"),
              tags$polygon(points="36,0 44,8 36,8",fill="rgba(255,147,172,0.4)"),
              tags$line(x1="8",y1="20",x2="28",y2="20",stroke="#ffb7c5",`stroke-width`="2",`stroke-linecap`="round"),
              tags$line(x1="8",y1="28",x2="28",y2="28",stroke="#ffb7c5",`stroke-width`="2",`stroke-linecap`="round"),
              tags$line(x1="8",y1="36",x2="20",y2="36",stroke="#ffb7c5",`stroke-width`="2",`stroke-linecap`="round")
            ),
            p(class="r-name","Milestone 1 Project Proposal"),
            span(class="r-type","PDF")
          ),

          tags$a(class="r-card", href="app-inputs/annotated-chicago_milestone2_557%20(1).pdf", target="_blank",
            tags$svg(class="r-doc-icon", viewBox="0 0 44 54", xmlns="http://www.w3.org/2000/svg",
              tags$rect(x="0",y="0",width="36",height="50",rx="4",fill="rgba(255,183,197,0.25)",stroke="rgba(255,183,197,0.5)",`stroke-width`="1.5"),
              tags$polygon(points="36,0 44,8 36,8",fill="rgba(255,147,172,0.4)"),
              tags$line(x1="8",y1="20",x2="28",y2="20",stroke="#ffb7c5",`stroke-width`="2",`stroke-linecap`="round"),
              tags$line(x1="8",y1="28",x2="28",y2="28",stroke="#ffb7c5",`stroke-width`="2",`stroke-linecap`="round"),
              tags$line(x1="8",y1="36",x2="20",y2="36",stroke="#ffb7c5",`stroke-width`="2",`stroke-linecap`="round")
            ),
            p(class="r-name","Milestone 2 Chicago Review"),
            span(class="r-type","PDF")
          ),

          tags$a(class="r-card", href="app-inputs/annotated-milestone3_chicago_557.pdf", target="_blank",
            tags$svg(class="r-doc-icon", viewBox="0 0 44 54", xmlns="http://www.w3.org/2000/svg",
              tags$rect(x="0",y="0",width="36",height="50",rx="4",fill="rgba(255,183,197,0.25)",stroke="rgba(255,183,197,0.5)",`stroke-width`="1.5"),
              tags$polygon(points="36,0 44,8 36,8",fill="rgba(255,147,172,0.4)"),
              tags$line(x1="8",y1="20",x2="28",y2="20",stroke="#ffb7c5",`stroke-width`="2",`stroke-linecap`="round"),
              tags$line(x1="8",y1="28",x2="28",y2="28",stroke="#ffb7c5",`stroke-width`="2",`stroke-linecap`="round"),
              tags$line(x1="8",y1="36",x2="20",y2="36",stroke="#ffb7c5",`stroke-width`="2",`stroke-linecap`="round")
            ),
            p(class="r-name","Milestone 3 Chicago Review"),
            span(class="r-type","PDF")
          )
        ),

        div(class = "cherry-footer",
          "DATA 557 \u00B7 Faculty Salary Analysis \u00B7 University of Washington"
        )
      )
    ),

    # Hidden background audio — persists across all tabs, autoplays on first interaction
    tags$audio(
      id      = "blossom-audio",
      loop    = NA,
      preload = "auto",
      style   = "display:none;",
      tags$source(src = "c_blossoms.mp3", type = "audio/mpeg")
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
          h4("Controls"),
          selectInput(
            "q1_field", "Field (discipline):",
            choices = c("All", sort(unique(start_salary$field))), selected = "All"
          ),
          checkboxGroupInput(
            "q1_sex", "Sex:",
            choices = levels(start_salary$sex), selected = levels(start_salary$sex)
          ),
          width = 3
        ),
        mainPanel(
          h4("Starting Salary by Sex"),
          plotOutput("q1_box_sex", height = "300px"),
          br(),
          h4("Starting Salary by Hire Year and Sex"),
          plotOutput("q1_box_year", height = "300px"),
          br(),
          h4("Starting Salary Summary Table"),
          tableOutput("q1_summary_tbl")
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
          h4("Controls"),
          checkboxGroupInput(
            "q2_sex", "Sex:",
            choices = levels(promotion_data$sex), selected = levels(promotion_data$sex)
          ),
          radioButtons(
            "q2_metric", "Raise metric:",
            choices = c("Dollar raise" = "amount", "Percent raise" = "percent"),
            selected = "amount"
          ),
          width = 3
        ),
        mainPanel(
          h4("Salary Increase at Promotion (Associate \u2192 Full)"),
          plotOutput("q2_box", height = "300px"),
          br(),
          h4("Summary by Sex"),
          tableOutput("q2_summary_tbl")
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
          h4("Controls"),
          checkboxGroupInput(
            "q3_rank", "Rank:",
            choices = levels(latest_salary$rank), selected = levels(latest_salary$rank)
          ),
          checkboxGroupInput(
            "q3_sex", "Sex:",
            choices = levels(latest_salary$sex), selected = levels(latest_salary$sex)
          ),
          width = 3
        ),
        mainPanel(
          h4("Latest Salary by Sex Within Rank"),
          plotOutput("q3_box_rank", height = "300px"),
          br(),
          h4("Latest Salary Density by Sex Within Rank"),
          plotOutput("q3_density_rank", height = "300px"),
          br(),
          h4("Summary Table"),
          tableOutput("q3_summary_tbl")
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
          checkboxGroupInput(
            "q4_sex", "Sex:",
            choices = levels(salary_arc$sex), selected = levels(salary_arc$sex)
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
            "Breaking the career into four stages shows",
            "whether the salary gap is stable or growing.",
            "A widening spread across stages is the signature of compounding inequality."
          ),
          plotOutput("q4_stage_bars", height = "300px"),
          br(),

          h4("The Administrative Salary Premium"),
          p(style = "color:#666; font-size:0.92em; margin-bottom:8px;",
            "Faculty in administrative roles often earn more.",
            "If one group holds fewer such roles, that becomes",
            "a compounding mechanism beyond rank or experience."
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

  # ── Q1: Starting Salary ──
  q1_filtered <- reactive({
    dat <- start_salary
    if (input$q1_field != "All")
      dat <- dat %>% filter(field == input$q1_field)
    if (length(input$q1_sex) > 0)
      dat <- dat %>% filter(sex %in% input$q1_sex)
    dat
  })
  output$q1_box_sex <- renderPlot({
    ggplot(q1_filtered(), aes(x = sex, y = salary, fill = sex)) +
      geom_boxplot(alpha = 0.8) +
      scale_fill_manual(values = sex_colours) +
      labs(x = "Sex", y = "Starting salary", title = "Starting Salary by Sex")
  })
  output$q1_box_year <- renderPlot({
    ggplot(q1_filtered(), aes(x = factor(startyr), y = salary, fill = sex)) +
      geom_boxplot(outlier.alpha = 0.3) +
      scale_fill_manual(values = sex_colours) +
      labs(x = "Hire year", y = "Starting salary",
           title = "Starting Salary by Hire Year and Sex") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
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

  # ── Q2: Promotion Raises ──
  q2_filtered <- reactive({
    dat <- promotion_data
    if (length(input$q2_sex) > 0)
      dat <- dat %>% filter(sex %in% input$q2_sex)
    dat
  })
  output$q2_box <- renderPlot({
    dat <- q2_filtered()
    if (input$q2_metric == "amount") {
      ggplot(dat, aes(x = sex, y = raise_amount, fill = sex)) +
        geom_boxplot(alpha = 0.8) +
        scale_fill_manual(values = sex_colours) +
        labs(x = "Sex", y = "Dollar raise at promotion",
             title = "Salary Increase at Promotion (Associate to Full) by Sex")
    } else {
      ggplot(dat, aes(x = sex, y = raise_pct, fill = sex)) +
        geom_boxplot(alpha = 0.8) +
        scale_fill_manual(values = sex_colours) +
        scale_y_continuous(labels = percent_format(accuracy = 1)) +
        labs(x = "Sex", y = "Percent raise at promotion",
             title = "Percent Raise at Promotion by Sex")
    }
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

  # ── Q3: Latest Salary ──
  q3_filtered <- reactive({
    dat <- latest_salary
    if (length(input$q3_rank) > 0)
      dat <- dat %>% filter(rank %in% input$q3_rank)
    if (length(input$q3_sex) > 0)
      dat <- dat %>% filter(sex %in% input$q3_sex)
    dat
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

  # ── Q4: Career Arc ──
  q4_filtered <- reactive({
    dat <- salary_arc
    if (input$q4_field != "All")
      dat <- dat %>% filter(field == input$q4_field)
    if (length(input$q4_sex) > 0)
      dat <- dat %>% filter(sex %in% input$q4_sex)
    dat
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
            "In the early career (0 to 4 years), female faculty earn ",
            early_ratio, "% of male colleagues\u2019 median salary. ",
            "By 15 or more years in, that figure becomes ",
            late_ratio, "%. The gap ", direction, " over a career."
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
