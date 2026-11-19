// One dot per play: x = minute of the local day (the day runs 5 AM to 5 AM),
// y = listening day in the year (a play before 05:00 belongs to the day before).
// Design after Sahil Chinoy's "Songs I've listened to" (https://sahilchinoy.com/music/).
// Tooltip follows Chris Henrick's convention
// (https://observablehq.com/@clhenrick/tooltip-d3-convention).
//
// Plain script, no modules: it needs the global `d3` (the v7 UMD build) loaded first.
// One chart, colored by the "Color by" mode in the URL hash: #2025 (or #2025-genre) by
// each artist's genre; #2025-artist, #2025-album and #2025-track by the year's top ten
// artists, albums or tracks; #2025-new by novelty (first listen, new find or familiar; the
// "Novelty" tab).
// The page redraws on hashchange: a new year builds that year's chart (buildFrame), and a
// new mode in the same year re-skins the chart it has (skinGenre, skinRank).
// A month label or an hour label picks a subset (subsetPicker): the lists, the rank modes'
// colors, the novelty shares, the per-day bars and the stats line follow it.
// Data, fetched by relative URL: plays/years.json, plays/<year>.csv for the year on show
// only (each year is kept once loaded), and optionally plays/where_<year>.json for the
// "where I was" strip (no file, no strip). With the first year it also reads
// plays/genres.json ({names: {code: label}, artist: {artist: code}}, written by
// music/genre/export_genres.py; an artist not listed counts as OT, Other) and
// plays/genre_months.json ({"YYYY-MM": {code: plays}}, same script) for the genre mode's
// eras chart; if that file fails, the genre mode draws without the eras chart. The first
// time the new mode shows, it reads plays/variety.json ([{key: "YYYY-MM", n, half,
// novel}], one row per listening-day month) for its variety panel; if that fails, the new
// mode draws without the panel.
// Margin summaries are on by default; ?margins=0 turns them off.
// The CSVs' columns: track, artist, date, time, minutes, n_plays, rank, place, day, album,
// nv, first_date, first_minutes. album feeds the albums lists. An album is its name plus
// the artist column; plays with no album name are left out of those lists, and without
// the column they are empty. nv is the play's novelty class: FL, the track's first play
// from 2017 on; NW, a later play within 30 days (30 x 1440 local minutes) of the first;
// FM, familiar. first_date (YYYY-MM-DD) and first_minutes (minutes after local midnight)
// give the track's first play (see linkFirsts).
//
// Embedded mode, for the one-file page that music_template/make_music_page.py writes:
// when the page sets window.MUSIC_DATA before this script, the data comes from it and
// nothing is fetched. Its fields: years (as years.json), plays ({"2025": CSV text with
// the columns of <year>.csv}), genres (as genres.json, or null), genre_months (as
// genre_months.json, or null), variety (as variety.json, optional) and meta ({time_zones:
// [{from, zone}], ...}). There are no where files, so no strip and no place labels; the
// default year is the latest one. With genres null the genre mode is off: its Color by
// link is removed and elements with class "needs-genres" are hidden. Without the nv column
// in every year's CSV the new mode is off the same way (class "needs-nv"), and so is its
// variety panel. The page then opens on the first mode left. An element with id "tz-note"
// gets a sentence about the time zones.
(function () {
  "use strict";

  if (typeof d3 === "undefined") {
    const stats = document.getElementById("stats");
    if (stats) stats.textContent = "The chart library (D3) did not load. Reload the page to try again.";
    return;
  }

  const DEFAULT_YEAR = 2025;
  const EMBEDDED = window.MUSIC_DATA && typeof window.MUSIC_DATA === "object" ? window.MUSIC_DATA : null;
  const HAS_GENRES = !EMBEDDED || !!EMBEDDED.genres; // fetch mode always offers the genre mode
  // The new mode needs the nv, first_date and first_minutes columns; fetch mode always has them.
  const HAS_NV = !EMBEDDED || hasNvColumn(EMBEDDED.plays);
  const HEIGHT = 1800;
  // The left margin is built up from the pieces below (place labels, month labels, strip).
  const MARGIN = { top: 50, right: 20, bottom: 10 };
  const RADIUS = 2.5;
  const MIN_WIDTH = 240; // narrowest chart drawn; phones at 400 px get 368
  const NARROW = 600; // below this width, label every 3 hours and drop the place labels
  const TIP_GAP = 8; // px between cursor and tooltip
  const TIP_MAX = 320; // widest tooltip, px
  const TOP_N = 10; // ranks 1 to 10 each get a colour (see the CSS)
  // How the artist, album and track modes rank a chosen month (their lists and dot
  // colors). The year uses the CSV's dense rank: ties share a rank and the next rank
  // follows (94, 94, 84 -> 1, 1, 2). A month has few plays per track, so dense ranks there reach deep into ties:
  // January 2025 has 151 tracks at ranks 1 to 10, and in September 2025 rank 10 is 1 play,
  // so every dot would be colored. Competition ranks (94, 94, 84 -> 1, 1, 3) still ran to
  // 29 rows in April 2021 and 84 in December 2021, which ZH found excessive (2026-11-18).
  // A month therefore uses positions: exactly ten rows, ties broken by name, the same rule
  // as the genre view's Top songs. "competition" or "dense" restore the other rules.
  const MONTH_RANK = "position";
  const DAY_START = 300; // the chart day starts at 05:00 (minutes after midnight)
  const X_AXIS_Y = 30; // baseline of the hour labels
  const DAY_MS = 864e5;

  // Left margin, outside in: place labels, month labels, the "where I was" strip, the plot.
  const MONTH_W = 24; // least room for the month labels (buildFrame measures them)
  const PLOT_PAD = 6; // month labels (or strip) to the first dot column
  const STRIP_W = 6;
  const STRIP_PAD = 4; // month labels to strip
  const PLACE_GAP = 8; // place labels to month labels
  const PLACE_PX = 9; // place label font size (matches the CSS)
  const PLACE_MAX_CHARS = 16;
  const RUN_MAX_GAP = 2; // days with no plays at all that can sit inside one away run

  // Margin summaries: on unless the URL has ?margins=0.
  const SHOW_MARGINS = new URLSearchParams(window.location.search).get("margins") !== "0";
  const SUM_TOP = 72; // extra height above the plot for the time-of-day curve and its label
  const SUM_CURVE_H = 50;
  const SUM_BAR_W = 50; // longest per-day bar (bars are dropped below NARROW)
  const SUM_GAP = 10; // plot to bars

  // Genre view. Each code has a colour in the CSS (--g-FS etc.); OT is the plain grey.
  const GENRE_CODES = ["FS", "IR", "IP", "RP", "PE", "HH", "RB", "CS", "OT"];
  const OTHER = "OT";
  // One chart with a "Color by" mode. #2025 is genre (the default); #2025-artist,
  // #2025-album, #2025-track and #2025-new are the others. The old #2025-genre still works.
  const MODES = ["genre", "artist", "album", "track", "new"];
  const RANK_MODES = ["artist", "album", "track"];
  const hashFor = (year, mode) => `#${year}${mode === "genre" ? "" : `-${mode}`}`;
  // Switch: true opens the artist mode with its top artist (of the year, or of the chosen
  // month) already picked, so Top songs and Top albums start on that artist. ZH floated
  // this on 2026-11-18; off for now.
  const PREPICK_TOP = false;
  // Novelty (the new mode): each play is FL (the track's first play in the CSVs, from 2017), NW
  // (a later play within 30 days, 30 x 1440 minutes on the local clock, of the first) or
  // FM (familiar). The CSVs' nv column holds the class. 2017 is burn-in: 2015 and 2016 are
  // not in the CSVs.
  const NV_ORDER = ["FL", "NW", "FM"]; // stacking order, bottom up; also the legend order
  const NV_LAYER = { FM: 0, NW: 1, FL: 2 }; // draw order: first listens on top
  const NEW_FINDS_NOTE = "(Track / First streamed on / Number of streams)"; // under the new finds list
  const formatShare = (s) => (s > 0 && s < 0.005 ? "<1%" : d3.format(".0%")(s));

  // A fixed stacking order, bottom up, for both stacked charts in the genre view (plays
  // by time of day and the eras chart), so each genre's color sits in the same place
  // in every year. It must list every code in GENRE_CODES.
  const STACK_ORDER = ["FS", "IP", "RB", "CS", "IR", "RP", "PE", "HH", "OT"];
  const SHARE_CURVE_H = 80; // height of the genre view's plays-by-time-of-day chart
  const SHARE_KERNEL = [1, 2, 3, 4, 3, 2, 1]; // over 10-minute bins: about an hour wide
  const SUM_KERNEL = [1, 2, 3, 2, 1]; // the artist, album and track modes' curve, over the same bins
  const CURVE_LABEL = "plays by time of day"; // every mode's curve, with or without a month or hour
  const LABEL_PX = 9; // the curve label's size (matches the CSS, #chart .summary text)

  // ===== SWITCH: CURVE_SPLIT =====
  // true: the artist, album and track modes' plays-by-time-of-day curve is split by the
  // dots' colors, as the genre view's is by genre: one layer per rank 1 to 10 in that rank's
  // color, bottom up, and the rest in the summary grey on top; the stack's top is the total.
  // false: the single total, as before 2026-11-19. Either way, a hovered or picked artist,
  // album or track draws its own curve over it (see curveOverlay).
  const CURVE_SPLIT = false; // off on 2026-11-19 at ZH's request; the overlay carries the item
  // Month and hour labels are buttons: a click emphasizes that month's listening days or
  // that clock hour by laying a veil in the page colour (see the CSS, #chart .veil) over
  // the rest (subsetPicker).
  const MONTH_HIT_H = 24; // height of a month label's invisible click target, px
  const AXIS_PX = 12; // month and hour label size, px (matches the CSS, #chart .axis text)
  const AXIS_GAP = 6; // least space between two hour labels, px
  const HOUR_STEPS = [1, 2, 3, 6]; // hours between labels: the first whose labels don't touch
  const HOUR_HIT_TOP = -20; // an hour's click target, from its label's baseline band (px above X_AXIS_Y)
  const HOUR_HIT_H = 26;
  const formatMonthName = d3.utcFormat("%B");
  const formatMonthShort = d3.utcFormat("%b"); // the month labels ("Jan"; the CSS sets capitals)
  // The eras chart (genre view): monthly 100% bars, with year labels under them.
  const ERAS_H = 108; // svg height: bars plus year labels
  const ERAS_BARS_H = 84; // the fade for other years is in the CSS (#eras .month)
  const parseMonthKey = d3.utcParse("%Y-%m");
  const formatMonthKey = d3.utcFormat("%Y-%m");
  const formatMonthLong = d3.utcFormat("%B %Y");

  const parseDay = d3.utcParse("%Y-%m-%d");
  const formatDay = d3.utcFormat("%b %d, %Y"); // zero-padded day: "Jun 03, 2025"
  const formatMonthDay = d3.utcFormat("%b %d");
  const formatCount = d3.format(",");
  const WEEKDAY = ["Sun", "Mon", "Tue", "Wed", "Thur", "Fri", "Sat"]; // by getUTCDay()
  const trackKey = (d) => `${d.track}\u0000${d.artist}`;
  const albumKey = (d) => `${d.album}\u0000${d.artist}`; // an album is its name plus the artist
  // Draw order: unranked plays first, then rank 10 down to rank 1, so #1 is on top.
  const layerOf = (d) => (d.rank <= TOP_N ? TOP_N + 1 - d.rank : 0);
  const plural = (n, noun) => `${formatCount(n)} ${noun}${n === 1 ? "" : "s"}`;
  // Minutes after 05:00, so 04:59 sits at the right edge of the day it belongs to.
  const dayMinute = (min) => (((min - DAY_START) % 1440) + 1440) % 1440;

  // A focus is what the chart highlights: one track (with its artist) or one artist.
  const trackFocus = (d) => ({ kind: "track", artist: d.artist, key: trackKey(d), track: d.track, n: d.n_plays });
  const sameFocus = (a, b) => !!a && !!b && a.kind === b.kind && a.artist === b.artist && a.key === b.key;

  // Hour labels for ticks at chart minutes (0 = 5 AM): "5 AM", "6", ..., "12 PM", ...,
  // "12 AM", ..., "4", "5 AM". AM/PM is written at both ends and at the first tick after
  // noon and after midnight, which is 12 PM and 12 AM when ticks are hourly.
  function hourLabels(ticks) {
    const out = new Map();
    let prevPM = null;
    ticks.forEach((v, i) => {
      const m = (v + DAY_START) % 1440;
      const pm = m >= 720;
      const h = (m / 60) % 12 || 12;
      const ends = i === 0 || i === ticks.length - 1;
      out.set(v, ends || pm !== prevPM ? `${h} ${pm ? "PM" : "AM"}` : String(h));
      prevPM = pm;
    });
    return out;
  }

  function formatRange(a, b) {
    return +a === +b ? formatDay(a) : `${formatMonthDay(a)} – ${formatDay(b)}`;
  }

  // Clock hours. A play's hour is its local clock hour, 0 to 23, from `minutes`; a chosen
  // hour h covers h:00 to h:59. formatHourRange(14) is "2–3 PM", (11) "11 AM–12 PM".
  const hourOf = (d) => Math.floor(d.minutes / 60);
  const hour12 = (h) => h % 12 || 12;
  const meridiem = (h) => (h % 24 < 12 ? "AM" : "PM");
  function formatHourRange(h) {
    const b = (h + 1) % 24;
    return meridiem(h) === meridiem(b) ? `${hour12(h)}–${hour12(b)} ${meridiem(h)}`
      : `${hour12(h)} ${meridiem(h)}–${hour12(b)} ${meridiem(b)}`;
  }
  // A subset of a year's plays is a listening month k (0 to 11, from the `day` column) and a
  // clock hour h (0 to 23), each null for all. subKey names it for the caches; subsetLabel
  // says what it covers: "January 2025", "2–3 PM", "January 2025, 2–3 PM" ("" for the year).
  const subKey = (k, h) => `${k}|${h}`;
  function subsetLabel(year, k, h) {
    const parts = [];
    if (k !== null) parts.push(formatMonthLong(new Date(Date.UTC(year, k, 1))));
    if (h !== null) parts.push(formatHourRange(h));
    return parts.join(", ");
  }
  // The text for an empty list in a subset.
  function emptyFor(k, h) {
    if (k === null && h === null) return "No plays this year.";
    if (h === null) return "No plays this month.";
    return k === null ? "No plays in this hour." : "No plays this month in this hour.";
  }

  let measureCtx;
  function textWidth(text, px, { weight = "", family = "" } = {}) {
    if (measureCtx === undefined) measureCtx = document.createElement("canvas").getContext("2d");
    if (!measureCtx) return text.length * px * (weight ? 0.68 : 0.6);
    measureCtx.font = `${weight ? `${weight} ` : ""}${px}px ${family || getComputedStyle(document.body).fontFamily}`;
    return measureCtx.measureText(text).width;
  }
  // Month and hour labels are in the d3 axis's font ("sans-serif"), at AXIS_PX.
  const axisWidth = (text, weight) => textWidth(text, AXIS_PX, { weight, family: "sans-serif" });

  // "Aarhus, DK" -> "Aarhus"; "Somerville (Teele Square), MA" -> "Somerville".
  function placeShort(label) {
    let s = label.split(",")[0].replace(/\s*\(.*$/, "").trim();
    if (s.length > PLACE_MAX_CHARS) s = `${s.slice(0, PLACE_MAX_CHARS - 1).trimEnd()}…`;
    return s;
  }

  // Runs of away days with the same label, in day order. A run may step over up to
  // RUN_MAX_GAP days that have no entry at all (no plays); a home day ends it.
  function awayRuns(where, year) {
    const seen = new Set(); // every listening day in the file, away or not
    const away = new Map(); // day (ms) -> label
    for (const w of where) {
      const day = w && typeof w.day === "string" ? parseDay(w.day) : null;
      if (!day || day.getUTCFullYear() !== year) continue;
      seen.add(+day);
      if (w.away === true && !away.has(+day)) away.set(+day, String(w.label || "").trim());
    }
    const runs = [];
    let run = null;
    for (const [t, label] of Array.from(away).sort((a, b) => a[0] - b[0])) {
      let joins = false;
      if (run && run.label === label) {
        const prev = +run.days[run.days.length - 1];
        const gap = Math.round((t - prev) / DAY_MS) - 1;
        joins = gap <= RUN_MAX_GAP;
        for (let k = 1; joins && k <= gap; k++) if (seen.has(prev + k * DAY_MS)) joins = false;
      }
      if (joins) run.days.push(new Date(t));
      else runs.push((run = { label, days: [new Date(t)] }));
    }
    return runs;
  }

  // Unbroken stretches of days inside one run, one rect each.
  function segments(days) {
    const out = [];
    let s = null;
    for (const d of days) {
      if (s && +d - +s.end === DAY_MS) s.end = d;
      else out.push((s = { start: d, end: d }));
    }
    return out;
  }

  // "R Feb 13, 2020, 21:14"
  function formatWhen(d) {
    return `${WEEKDAY[d.date.getUTCDay()]} ${formatDay(d.date)}, ${d.time}`;
  }

  function parseRow(d) {
    const date = parseDay(d.date); // the real local start date, for the tooltip
    return {
      track: d.track,
      artist: d.artist,
      date,
      day: (d.day && parseDay(d.day)) || date, // listening day; the real date if missing
      time: d.time,
      minutes: +d.minutes,
      n_plays: +d.n_plays,
      rank: +d.rank,
      place: (d.place || "").trim(), // empty when the column is missing or blank
      album: d.album || "", // empty when the column is missing or blank
      nv: d.nv || null, // FL, NW or FM; null without the column (embedded data only)
      firstDate: d.first_date || null, // the track's first play, for linkFirsts
      firstMinutes: +d.first_minutes,
    };
  }

  // True when every year's CSV text in embedded data has the nv, first_date and
  // first_minutes columns (read from its header row).
  function hasNvColumn(plays) {
    const texts = Object.values(plays || {}).filter((t) => typeof t === "string");
    return texts.length > 0 && texts.every((t) => {
      const end = t.indexOf("\n");
      const header = (d3.csvParseRows(end < 0 ? t : t.slice(0, end))[0] || []).map((h) => h.trim());
      return ["nv", "first_date", "first_minutes"].every((c) => header.includes(c));
    });
  }

  // Counts rows by key and ranks the keys by plays, most first, ties in `order` (which
  // compares two rows). Returns [{ key, first (a row), n, rank }]. With rule "dense", ties
  // share a rank and the next rank follows (94, 94, 84 -> 1, 1, 2); with "competition",
  // the next rank skips the tied places (94, 94, 84 -> 1, 1, 3); with "position", every
  // key gets its own place in sorted order (94, 94, 84 -> 1, 2, 3).
  function rankCounts(rows, keyOf, rule, order) {
    const out = Array.from(d3.rollup(rows, (v) => ({ first: v[0], n: v.length }), keyOf),
      ([key, v]) => ({ key, first: v.first, n: v.n }));
    out.sort((a, b) => b.n - a.n || order(a.first, b.first));
    let rank = 0;
    let last = null;
    out.forEach((r, i) => {
      if (rule === "position") { r.rank = i + 1; return; }
      if (r.n !== last) { rank = rule === "dense" ? rank + 1 : i + 1; last = r.n; }
      r.rank = rank;
    });
    return out;
  }

  // One year's plays, in play order, each linked to its track's first play (linkFirsts).
  // Each year is loaded once and kept, so a switch of mode, or back to a year shown
  // before, reuses the same rows (and the chart and lookups built on them). A failed load
  // is retried on the next request.
  const yearCache = new Map();
  function loadYear(year) {
    if (!yearCache.has(year)) {
      const p = loadYearOnce(year).then(linkFirsts);
      yearCache.set(year, p);
      p.catch(() => { if (yearCache.get(year) === p) yearCache.delete(year); });
    }
    return yearCache.get(year);
  }
  async function loadYearOnce(year) {
    if (EMBEDDED) {
      const text = EMBEDDED.plays && EMBEDDED.plays[String(year)];
      if (typeof text !== "string") throw new Error(`no plays for ${year}`);
      return d3.csvParse(text, parseRow); // play order, as below
    }
    const rows = await d3.csv(`plays/${year}.csv`, parseRow);
    return rows; // file order, which is play order (the build sorts by UTC start)
  }

  // Gives each play its local clock in minutes (d.stamp) and its track's first play
  // (d.first, with .date, .day and .stamp), for the new mode: the tooltip's "first played
  // <date>" and days since, and the new finds lists. A file holds the plays of one year's
  // listening days, so a track whose first play falls on one of them has its FL row in this
  // file, and its plays here get that row. A track first played in an earlier file gets a
  // stand-in from the first_date and first_minutes columns, shared by its plays here; its
  // listening day follows the build's rule (a play before 05:00 belongs to the day before).
  // Without the nv column (older embedded data) it does nothing.
  function linkFirsts(rows) {
    if (!HAS_NV) return rows;
    const stampOf = (date, minutes) => Math.round(+date / 6e4) + minutes;
    const firsts = new Map();
    for (const d of rows) {
      d.stamp = stampOf(d.date, d.minutes);
      if (d.nv === "FL") { d.first = d; firsts.set(trackKey(d), d); }
    }
    for (const d of rows) {
      if (d.first) continue;
      const k = trackKey(d);
      let f = firsts.get(k);
      if (!f) {
        const date = parseDay(d.firstDate);
        const minutes = d.firstMinutes;
        f = { date, minutes, day: minutes < DAY_START ? d3.utcDay.offset(date, -1) : date, stamp: stampOf(date, minutes) };
        firsts.set(k, f);
      }
      d.first = f;
    }
    return rows;
  }

  // plays/variety.json, loaded once (the first time the new mode shows), as one row per
  // listening-day month from the first to the last: { key: "2025-03", date, year, n, half,
  // novel }. half is the number of artists that make up half the month's plays (the
  // fewest top artists whose plays reach half), novel the share of its plays that are FL
  // or NW; both are NaN for a month with no plays or a missing value. Each series also
  // gets a centered 3-month mean (half3, novel3; two months at the ends). A failed load
  // is retried on the next request.
  let varietyPromise = null;
  function loadVariety() {
    if (!varietyPromise) {
      const source = EMBEDDED ? Promise.resolve(EMBEDDED.variety) : d3.json("plays/variety.json");
      varietyPromise = source.then((raw) => {
        const byKey = new Map((Array.isArray(raw) ? raw : [])
          .filter((r) => r && typeof r.key === "string" && parseMonthKey(r.key)).map((r) => [r.key, r]));
        const keys = Array.from(byKey.keys()).sort();
        if (!keys.length) throw new Error("bad variety.json");
        const num = (v) => (typeof v === "number" && Number.isFinite(v) ? v : NaN);
        const end = d3.utcMonth.offset(parseMonthKey(keys[keys.length - 1]), 1);
        const out = d3.utcMonths(parseMonthKey(keys[0]), end).map((date) => {
          const key = formatMonthKey(date);
          const r = byKey.get(key);
          const n = r ? Math.max(0, +r.n || 0) : 0;
          return { key, date, year: date.getUTCFullYear(), n, half: n ? num(r.half) : NaN, novel: n ? num(r.novel) : NaN };
        });
        for (const f of ["half", "novel"]) {
          out.forEach((m, i) => {
            const vals = out.slice(Math.max(0, i - 1), i + 2).map((r) => r[f]).filter((v) => !Number.isNaN(v));
            m[`${f}3`] = vals.length ? d3.mean(vals) : NaN;
          });
        }
        return out;
      }).catch((err) => {
        varietyPromise = null;
        throw err;
      });
    }
    return varietyPromise;
  }

  // plays/genres.json, loaded once. A failed load is retried on the next request.
  let genresPromise = null;
  function loadGenres() {
    if (!genresPromise) {
      const source = EMBEDDED ? Promise.resolve(EMBEDDED.genres) : d3.json("plays/genres.json");
      genresPromise = source.then((g) => {
        if (!g || typeof g.names !== "object" || typeof g.artist !== "object") throw new Error("bad genres.json");
        const names = new Map(GENRE_CODES.map((c) => [c, String(g.names[c] || c)]));
        const artist = new Map(Object.entries(g.artist).filter(([, c]) => GENRE_CODES.includes(c)));
        return { names, artist };
      }).catch((err) => {
        genresPromise = null;
        throw err;
      });
    }
    return genresPromise;
  }

  // plays/genre_months.json, loaded once, as one row per month from the first to the last:
  // { key: "2025-03", date, year, counts: {code: plays}, n }. A month the file lacks gets
  // n = 0. A failed load is retried on the next request.
  let monthsPromise = null;
  function loadGenreMonths() {
    if (!monthsPromise) {
      const source = EMBEDDED ? Promise.resolve(EMBEDDED.genre_months) : d3.json("plays/genre_months.json");
      monthsPromise = source.then((raw) => {
        const keys = raw && typeof raw === "object" ? Object.keys(raw).filter((k) => parseMonthKey(k)).sort() : [];
        if (!keys.length) throw new Error("bad genre_months.json");
        const end = d3.utcMonth.offset(parseMonthKey(keys[keys.length - 1]), 1);
        return d3.utcMonths(parseMonthKey(keys[0]), end).map((date) => {
          const key = formatMonthKey(date);
          const src = raw[key] || {};
          const counts = Object.fromEntries(STACK_ORDER.map((c) => [c, Math.max(0, +src[c] || 0)]));
          return { key, date, year: date.getUTCFullYear(), counts, n: d3.sum(STACK_ORDER, (c) => counts[c]) };
        });
      }).catch((err) => {
        monthsPromise = null;
        throw err;
      });
    }
    return monthsPromise;
  }

  // plays/where_<year>.json, or null when it is missing or malformed (then no strip).
  // Kept per year, so a switch of tab in the same year does not wait on the network.
  const whereCache = new Map();
  function loadWhere(year) {
    if (!whereCache.has(year)) whereCache.set(year, loadWhereOnce(year));
    return whereCache.get(year);
  }
  async function loadWhereOnce(year) {
    if (EMBEDDED) return null; // the kit has no place data
    try {
      const rows = await d3.json(`plays/where_${year}.json`);
      return Array.isArray(rows) ? rows : null;
    } catch (err) {
      return null;
    }
  }

  // The top-ten list under the header, with Tracks, Artists and Albums tabs. set() gives
  // it the title, each tab's rows (the year's, or the chosen month's), the text for an
  // empty list and the chart's pin functions; sync() restyles the pinned row.
  function makeTopTen(rootEl) {
    if (!rootEl) return { set() {}, sync() {} };
    const title = d3.select(rootEl).select(".lead");
    const list = d3.select(rootEl).select("ol");
    const tabs = d3.select(rootEl).selectAll("button[role=tab]");
    let tab = "tracks";
    let state = null;

    tabs.on("click", (event) => {
      tab = event.currentTarget.getAttribute("data-tab");
      draw();
    });

    function draw() {
      tabs.attr("aria-selected", function () { return String(this.getAttribute("data-tab") === tab); });
      list.attr("aria-labelledby", `topten-tab-${tab}`);
      list.selectChildren().remove();
      if (!state) return;
      title.text(state.title);
      const rows = state[tab] || [];
      const btn = list.selectAll("li")
        .data(rows)
        .join("li")
        .append("button")
        .attr("type", "button")
        .attr("class", "row")
        .attr("title", (d) => `${d.title}${d.sub ? ` – ${d.sub}` : ""} · ${plural(d.n, "play")}`)
        .attr("aria-label", (d) => `#${d.rank}: ${d.title}${d.sub ? ` – ${d.sub}` : ""}, ${plural(d.n, "play")}`)
        .on("click", (event, d) => state.toggle(d.focus));
      // Track rows wear their rank's color; artist and album rows take the highlight
      // color of their kind when pinned (see the CSS).
      btn.append("span")
        .attr("class", (d) => (d.focus.kind === "track" ? "sw" : `sw ${d.focus.kind}`))
        .style("background", (d) => (d.focus.kind === "track" ? `var(--r${d.rank})` : null));
      btn.append("span").attr("class", "rk").text((d) => d.rank);
      const name = btn.append("span").attr("class", "tt");
      name.append("span").attr("class", "tr").text((d) => d.title);
      name.filter((d) => d.sub).append("span").attr("class", "ar").text((d) => ` – ${d.sub}`);
      btn.append("span").attr("class", "np").text((d) => formatCount(d.n));
      if (!rows.length) list.append("li").attr("class", "none").text(tab === "albums" && state.noAlbums ? state.noAlbums : state.empty);
      sync();
    }

    function sync() {
      if (!state) return;
      list.selectAll("button.row")
        .classed("pinned", (d) => state.isPinned(d.focus))
        .attr("aria-pressed", (d) => String(state.isPinned(d.focus)));
    }

    return {
      set(next) { state = next; rootEl.hidden = false; draw(); },
      sync,
    };
  }

  // A ranked list without tabs, for the artist, album, track and new modes. set()
  // takes { title, note, rows: [{ rank, title, sub, n, sw (a CSS color), dt, focus }],
  // kept, empty, cols (two columns on desktop), isPinned, toggle }; hide() hides it.
  // `kept` (optional) is the selected row when it is not among `rows` (a month change can
  // drop it), as in makeSongList: a row of its own under the list, rank null when it has
  // no plays in the pool; sync() removes it once it is no longer selected.
  function makeRankList(rootEl) {
    if (!rootEl) return { set() {}, hide() {}, sync() {} };
    const title = d3.select(rootEl).select(".lead");
    const note = d3.select(rootEl).select(".note");
    const list = d3.select(rootEl).select("ol");
    let state = null;

    function draw() {
      list.selectChildren().remove();
      if (!state) return;
      title.text(state.title);
      if (!note.empty()) note.text(state.note || "").attr("hidden", state.note ? null : "");
      const rows = state.rows || [];
      const kept = state.kept || null;
      const withDt = rows.some((d) => d.dt);
      list.classed("cols", !!state.cols).classed("with-dt", withDt);
      if (!rows.length) list.append("li").attr("class", "none").text(state.empty);
      // A kept row's place can run to four digits: widen every row's rank column to fit.
      const digits = kept && kept.rank ? String(kept.rank).length : 0;
      list.style("--rk", digits > 2 ? `${(digits * 0.62 + 0.2).toFixed(2)}em` : null);
      const btn = list.selectAll("li.r")
        .data(kept ? rows.concat([kept]) : rows)
        .join("li")
        .attr("class", (d) => (d === kept ? "r kept" : "r"))
        .append("button")
        .attr("type", "button")
        .attr("class", "row")
        .attr("title", (d) => `${d === kept ? `Selected, outside the top ${TOP_N} · ` : ""}${d.title}${d.sub ? ` – ${d.sub}` : ""} · ${plural(d.n, "play")}${d.dtLong ? ` · ${d.dtLong}` : ""}`)
        .attr("data-t", (d) => `${d.title}${d.sub ? ` – ${d.sub}` : ""}`) // the bold width reserve (CSS .row::after)
        .on("click", (event, d) => state.toggle(d.focus));
      btn.append("span").attr("class", "sw").style("background", (d) => d.sw || "var(--dot)");
      btn.append("span").attr("class", "rk").text((d) => d.rank || "–");
      const name = btn.append("span").attr("class", "tt");
      name.append("span").attr("class", "tr").text((d) => d.title);
      name.filter((d) => d.sub).append("span").attr("class", "ar").text((d) => ` – ${d.sub}`);
      if (withDt) { // "Aug 14 · 74": the date, a middle dot, the count
        btn.append("span").attr("class", "dt").text((d) => d.dt || "");
        btn.append("span").attr("class", "sep").attr("aria-hidden", "true").text((d) => (d.dt ? "·" : ""));
      }
      btn.append("span").attr("class", "np").text((d) => formatCount(d.n));
      sync();
    }

    function sync() {
      if (!state) return;
      const gone = list.selectAll("li.kept").filter((d) => !state.isPinned(d.focus));
      if (!gone.empty()) { gone.remove(); list.style("--rk", null); }
      list.selectAll("button.row")
        .classed("pinned", (d) => state.isPinned(d.focus))
        .attr("aria-pressed", (d) => String(state.isPinned(d.focus)));
    }

    return {
      set(next) { state = next; rootEl.hidden = false; draw(); },
      hide() { state = null; rootEl.hidden = true; list.selectChildren().remove(); },
      sync,
    };
  }

  // The genre list under the header (genre view): one row per genre with its plays, its
  // share of the year and its top three artists. set() gives it the rows plus the chart's
  // pin functions; sync() restyles the pinned row.
  function makeGenreList(rootEl) {
    if (!rootEl) return { set() {}, sync() {} };
    const list = d3.select(rootEl).select("ol");
    let state = null;

    function draw() {
      list.selectChildren().remove();
      if (!state) return;
      const btn = list.selectAll("li")
        .data(state.rows)
        .join("li")
        .append("button")
        .attr("type", "button")
        .attr("class", "row")
        .attr("title", (d) => `${d.label} · ${plural(d.n, "play")} · ${formatShare(d.share)} of the year`)
        .attr("aria-label", (d) => `${d.label}: ${plural(d.n, "play")}, ${formatShare(d.share)} of the year`
          + (d.top.length ? `; top artists ${d.top.join(", ")}` : ""))
        .on("click", (event, d) => state.toggle(d.focus));
      btn.append("span").attr("class", "sw").style("background", (d) => `var(--g-${d.code})`);
      btn.append("span").attr("class", "gl").text((d) => d.label);
      btn.append("span").attr("class", "np").text((d) => formatCount(d.n));
      btn.append("span").attr("class", "sh").text((d) => formatShare(d.share));
      btn.append("span").attr("class", "ta").text((d) => d.top.join(", "));
      sync();
    }

    function sync() {
      if (!state) return;
      list.selectAll("button.row")
        .classed("pinned", (d) => state.isPinned(d.focus))
        .attr("aria-pressed", (d) => String(state.isPinned(d.focus)));
    }

    return {
      set(next) { state = next; rootEl.hidden = false; draw(); },
      sync,
    };
  }

  // The top songs beside the genre list (genre view): up to ten songs from what is pinned
  // (nothing: the whole year; a genre; an artist) and, when a month is chosen, from that
  // month's plays, by plays, numbered 1 to 10. set() gives it the title, the rows, the
  // text for an empty list and the chart's functions; sync() restyles the selected row.
  // `kept` is the selected song when it is not among the rows (a month change can drop
  // it): it gets a row of its own under the list, with its place and plays in the same
  // pool (pos null and n 0 when it has none there), so it stays visible and can be
  // cleared; sync() removes that row once the song is no longer selected.
  // Hovering a row calls state.hover(row), and leaving it state.hover(null); touch skips
  // the hover, so a tap only selects.
  // The same list serves the top albums: `item` is the class of each row's li ("song" or
  // "album") and `nameOf` gives a row's name (the track, or the album).
  function makeSongList(rootEl, { item = "song", nameOf = (d) => d.track } = {}) {
    if (!rootEl) return { set() {}, sync() {} };
    const title = d3.select(rootEl).select(".lead");
    const list = d3.select(rootEl).select("ol");
    let state = null;
    let hovered = null; // the row under the pointer, so removing the kept row can end its hover

    function draw() {
      list.selectChildren().remove();
      hovered = null;
      if (!state) return;
      title.text(state.title);
      if (!state.rows.length) list.append("li").attr("class", "none").text(state.empty);
      const kept = state.kept;
      // A kept row's place can run to four digits: widen the rank column of every row to
      // fit it (--rk in the CSS; 1.5em fits "10"), so the titles stay aligned.
      const digits = kept && kept.pos ? String(kept.pos).length : 0;
      list.style("--rk", digits > 2 ? `${(digits * 0.62 + 0.2).toFixed(2)}em` : null);
      const label = (d) => (d === kept
        ? `Selected, outside the top ${TOP_N}: ${d.pos ? `${d.pos}: ` : ""}${nameOf(d)} – ${d.artist}, ${plural(d.n, "play")}`
        : `${d.pos}: ${nameOf(d)} – ${d.artist}, ${plural(d.n, "play")}`);
      const btn = list.selectAll(`li.${item}`)
        .data(kept ? state.rows.concat([kept]) : state.rows)
        .join("li")
        .attr("class", (d) => (d === kept ? `${item} kept` : item))
        .append("button")
        .attr("type", "button")
        .attr("class", "row")
        .attr("title", (d) => `${nameOf(d)} – ${d.artist} · ${plural(d.n, "play")}`)
        .attr("data-t", (d) => `${nameOf(d)} – ${d.artist}`) // the bold width reserve (CSS .row::after)
        .attr("aria-label", label)
        .on("click", (event, d) => state.toggle(d))
        .on("pointerenter", (event, d) => { if (event.pointerType !== "touch") { hovered = d; state.hover(d); } })
        .on("pointerleave", (event) => { if (event.pointerType !== "touch") { hovered = null; state.hover(null); } });
      btn.append("span").attr("class", "sw").style("background", (d) => `var(--g-${d.g})`);
      btn.append("span").attr("class", "rk").text((d) => d.pos || "–");
      const name = btn.append("span").attr("class", "tt");
      name.append("span").attr("class", "tr").text(nameOf);
      name.append("span").attr("class", "ar").text((d) => ` – ${d.artist}`);
      btn.append("span").attr("class", "np").text((d) => formatCount(d.n));
      sync();
    }

    function sync() {
      if (!state) return;
      const gone = list.selectAll("li.kept").filter((d) => !state.isSelected(d));
      if (!gone.empty()) {
        if (gone.data().includes(hovered)) { hovered = null; state.hover(null); }
        gone.remove();
        list.style("--rk", null);
      }
      list.selectAll("button.row")
        .classed("pinned", (d) => state.isSelected(d))
        .attr("aria-pressed", (d) => String(state.isSelected(d)));
    }

    return {
      set(next) { state = next; rootEl.hidden = false; draw(); },
      sync,
    };
  }

  // The eras chart (genre view), between the genre list and the plot: each month's genre
  // shares as a 100% stacked bar, from the first month to the last, stacked in
  // STACK_ORDER. The year on show is at full opacity and the others are faded. Hover shows the month, its plays and its top
  // three genres; a click opens that year's genre view. set() redraws it at the section's
  // width, unless it already shows the same months and year at that width (a tab switch
  // back to the genre view); with no months it hides the section.
  function makeEras(rootEl) {
    if (!rootEl) return { set() {} };
    const holder = d3.select(rootEl).select(".eras-chart");
    const title = d3.select(rootEl).select(".lead");
    const tooltip = makeTooltip(rootEl.querySelector(".tooltip"), rootEl);
    let drawn = null; // what it shows: { months, year, names, width }

    function set({ months, year, names }) {
      tooltip.hide();
      if (!months || !months.length) { holder.selectChildren().remove(); drawn = null; rootEl.hidden = true; return; }
      rootEl.hidden = false;
      const width = Math.max(MIN_WIDTH, rootEl.clientWidth);
      if (drawn && drawn.months === months && drawn.year === year && drawn.names === names && drawn.width === width) return;
      drawn = { months, year, names, width };
      holder.selectChildren().remove();
      const first = months[0].year;
      const last = months[months.length - 1].year;
      title.text(`Genre share by month, ${first}–${last}`);

      const x = d3.scaleBand()
        .domain(months.map((m) => m.key))
        .range([V_GUTTER, width]) // a gutter, so the months line up with the variety panel's
        .paddingInner((width - V_GUTTER) / months.length >= 5 ? 0.15 : 0); // no gaps when bars are thin
      const y = d3.scaleLinear().domain([0, 1]).range([ERAS_BARS_H, 0]);
      const svg = holder.append("svg")
        .attr("width", width)
        .attr("height", ERAS_H)
        .attr("role", "img")
        .attr("aria-label", `Each genre's share of plays by month, ${first} to ${last}, stacked in the same `
          + `order every month, Other on top. ${year} is at full opacity; click a month to show its year.`);

      const bars = svg.append("g")
        .selectAll("g")
        .data(months.filter((m) => m.n > 0))
        .join("g")
        .attr("class", (m) => (m.year === year ? "month now" : "month"))
        .attr("transform", (m) => `translate(${x(m.key)},0)`);
      bars.selectAll("rect")
        .data((m) => {
          let acc = 0;
          return STACK_ORDER.filter((c) => m.counts[c] > 0).map((c) => {
            const s = { c, from: acc / m.n, to: (acc + m.counts[c]) / m.n };
            acc += m.counts[c];
            return s;
          });
        })
        .join("rect")
        .attr("data-g", (s) => s.c)
        .attr("width", x.bandwidth())
        .attr("y", (s) => y(s.to))
        .attr("height", (s) => y(s.from) - y(s.to));

      // Year labels under the bars, centred on each year's months, with a tick between years.
      const years = d3.groups(months, (m) => m.year);
      const yearStart = ([, ms]) => Math.round(x(ms[0].key) - (x.step() - x.bandwidth()) / 2) + 0.5;
      const axis = svg.append("g").attr("class", "years");
      axis.selectAll("line")
        .data(years.slice(1))
        .join("line")
        .attr("x1", yearStart)
        .attr("x2", yearStart)
        .attr("y1", ERAS_BARS_H + 2)
        .attr("y2", ERAS_BARS_H + 7);
      axis.selectAll("text")
        .data(years)
        .join("text")
        .attr("class", ([yr]) => (yr === year ? "now" : null))
        .attr("x", ([, ms]) => (x(ms[0].key) + x(ms[ms.length - 1].key) + x.bandwidth()) / 2)
        .attr("y", ERAS_BARS_H + 17)
        .attr("text-anchor", "middle")
        .text(([yr]) => yr);

      bars
        .on("mouseover", (event, m) => tooltip.showWith((tip) => {
          tip.append("p").attr("class", "name").text(formatMonthLong(m.date));
          tip.append("p").attr("class", "meta").text(plural(m.n, "play"));
          STACK_ORDER.filter((c) => m.counts[c] > 0)
            .sort((a, b) => m.counts[b] - m.counts[a])
            .slice(0, 3)
            .forEach((c) => {
              const p = tip.append("p").attr("class", "genre");
              p.append("span").attr("class", "key").style("background", `var(--g-${c})`);
              p.append("span").text(`${names.get(c) || c} ${formatShare(m.counts[c] / m.n)}`);
            });
        }, event))
        .on("mousemove", (event) => tooltip.move(event))
        .on("mouseout", () => tooltip.hide())
        .on("click", (event, m) => { window.location.hash = hashFor(m.year, "genre"); });
    }

    return { set };
  }

  // The variety panel (new mode), from plays/variety.json (loadVariety). Two small
  // multiples over the same months as the eras chart (same band scale and width, so the
  // months line up with the bars above), one y axis each: the number of artists that make
  // up half the month's plays, and the share of plays that are first listens or new
  // finds. Thin line: each month; thick line: the centered 3-month mean. The year on show
  // is shaded; the first year (2017) is hatched as burn-in in the second panel. Hover
  // shows a month; a click opens that month's year in the same mode.
  const V_PLOT_H = 56;
  const V_GUTTER = 28; // left gutter for the y labels (the eras chart gets the same one)
  const V_TOP = 10; // room above the top gridline for its label
  const V_AXIS_H = 20; // year labels under the last panel
  const V_PANELS = [
    { f: "half", title: "Number of artists accounting for half of all streams",
      short: "Artists for half", fmt: d3.format(",.0f"), tick: d3.format(",.0f") },
    { f: "novel", title: "Share of plays that are first listens or new finds",
      short: "First listens + new finds", fmt: d3.format(".0%"), tick: d3.format(".0%"), domain: [0, 1], ticks: [0, 0.5, 1], burnin: true },
  ];
  function makeVariety(rootEl) {
    if (!rootEl) return { set() {} };
    const holder = d3.select(rootEl).select(".variety-chart");
    const tooltip = makeTooltip(rootEl.querySelector(".tooltip"), rootEl);

    // The last drawing: { months, year, mode, width, away, reset }. Hidden (no months), its
    // panels leave the page but are kept in `away`, and set() puts them back when nothing
    // they show has changed (a tab switch back to the new mode); reset() hides the hover marks.
    let drawn = null;
    function set({ months, year, mode }) {
      tooltip.hide();
      if (!months || !months.length) {
        if (drawn && !drawn.away) { drawn.away = Array.from(holder.node().childNodes); drawn.away.forEach((n) => n.remove()); }
        rootEl.hidden = true;
        return;
      }
      rootEl.hidden = false;
      const width = Math.max(MIN_WIDTH, rootEl.clientWidth);
      if (drawn && drawn.months === months && drawn.year === year && drawn.mode === mode && drawn.width === width) {
        if (drawn.away) { drawn.away.forEach((n) => holder.node().appendChild(n)); drawn.away = null; }
        drawn.reset();
        return;
      }
      holder.selectChildren().remove();
      const first = months[0].year;
      const last = months[months.length - 1].year;
      d3.select(rootEl).select(".lead .what").text(`Listening variety by month, ${first}–${last}`);
      const x = d3.scaleBand()
        .domain(months.map((m) => m.key))
        .range([V_GUTTER, width])
        .paddingInner((width - V_GUTTER) / months.length >= 5 ? 0.15 : 0); // as the eras chart, so months align
      const cx = (m) => x(m.key) + x.bandwidth() / 2;
      const years = d3.groups(months, (m) => m.year);
      const yearStart = ([, ms]) => Math.round(x(ms[0].key) - (x.step() - x.bandwidth()) / 2) + 0.5;
      const yearEnd = ([, ms]) => Math.round(x(ms[ms.length - 1].key) + x.bandwidth() + (x.step() - x.bandwidth()) / 2) + 0.5;
      const shown = years.find(([yr]) => yr === year);
      const burn = years[0];
      const rules = [];
      const marks = [];

      V_PANELS.forEach((p, i) => {
        const isLast = i === V_PANELS.length - 1;
        const panel = holder.append("div").attr("class", "vpanel");
        const t = panel.append("p").attr("class", "ptitle");
        t.append("span").attr("class", "pt").text(p.title);
        const h = V_TOP + V_PLOT_H + (isLast ? V_AXIS_H : 4);
        const svg = panel.append("svg").attr("width", width).attr("height", h)
          .attr("role", "img").attr("aria-label", `${p.title} by month, ${first} to ${last}`);
        const max = d3.max(months, (m) => m[p.f]);
        const y = d3.scaleLinear().domain(p.domain || [0, max * 1.05]).range([V_TOP + V_PLOT_H, V_TOP]);
        const ticks = p.ticks || d3.ticks(0, max, 3);

        if (shown) svg.append("rect").attr("class", "now")
          .attr("x", yearStart(shown)).attr("width", yearEnd(shown) - yearStart(shown))
          .attr("y", V_TOP).attr("height", V_PLOT_H);
        if (p.burnin && burn) {
          svg.append("defs").append("pattern").attr("id", "v-hatch").attr("patternUnits", "userSpaceOnUse")
            .attr("width", 5).attr("height", 5).attr("patternTransform", "rotate(45)")
            .append("line").attr("x1", 0).attr("y1", 0).attr("x2", 0).attr("y2", 5).attr("class", "hatch");
          svg.append("rect").attr("class", "burnin")
            .attr("x", yearStart(burn) - 0.5).attr("width", yearEnd(burn) - yearStart(burn) + 0.5)
            .attr("y", V_TOP).attr("height", V_PLOT_H);
        }
        const grid = svg.append("g").attr("class", "grid");
        grid.selectAll("line").data(ticks).join("line")
          .attr("x1", V_GUTTER).attr("x2", width).attr("y1", (v) => Math.round(y(v)) + 0.5).attr("y2", (v) => Math.round(y(v)) + 0.5);
        if (p.burnin && burn && yearEnd(burn) - yearStart(burn) >= 72) svg.append("text").attr("class", "burnin-label")
          .attr("x", (yearStart(burn) + yearEnd(burn)) / 2).attr("y", V_TOP + V_PLOT_H - 4)
          .attr("text-anchor", "middle").text(`${burn[0]}: burn-in`);
        // A tick between years, across the plot.
        svg.append("g").attr("class", "yearlines").selectAll("line").data(years.slice(1)).join("line")
          .attr("x1", yearStart).attr("x2", yearStart).attr("y1", V_TOP).attr("y2", V_TOP + V_PLOT_H + (isLast ? 7 : 0));

        const line = (f) => d3.line().defined((m) => !Number.isNaN(m[f])).x(cx).y((m) => y(m[f]));
        svg.append("path").attr("class", "monthly").attr("d", line(p.f)(months));
        svg.append("path").attr("class", "rolling").attr("d", line(`${p.f}3`)(months));
        svg.append("g").attr("class", "grid").selectAll("text").data(ticks).join("text")
          .attr("x", V_GUTTER - 4).attr("y", (v) => y(v)).attr("dy", "0.32em").attr("text-anchor", "end").text(p.tick);

        if (isLast) {
          const axis = svg.append("g").attr("class", "years");
          axis.selectAll("text").data(years).join("text")
            .attr("class", ([yr]) => (yr === year ? "now" : null))
            .attr("x", ([, ms]) => (x(ms[0].key) + x(ms[ms.length - 1].key) + x.bandwidth()) / 2)
            .attr("y", V_TOP + V_PLOT_H + 16)
            .attr("text-anchor", "middle")
            .text(([yr]) => yr);
        }
        rules.push(svg.append("line").attr("class", "vrule").attr("y1", V_TOP).attr("y2", V_TOP + V_PLOT_H).style("display", "none"));
        marks.push({ dot: svg.append("circle").attr("class", "vdot").attr("r", 3).style("display", "none"), y, f: p.f });

        svg.append("rect").attr("class", "hit").attr("width", width).attr("height", h)
          .on("mousemove", (event) => hover(event))
          .on("mouseleave", () => { tooltip.hide(); rules.forEach((r) => r.style("display", "none")); marks.forEach((k) => k.dot.style("display", "none")); })
          .on("click", (event) => { const m = monthAt(event); if (m) window.location.hash = hashFor(m.year, mode); });
      });

      function monthAt(event) {
        const [mx] = d3.pointer(event);
        const i = Math.max(0, Math.min(months.length - 1, Math.floor((mx - V_GUTTER) / x.step())));
        return months[i];
      }
      function hover(event) {
        const m = monthAt(event);
        if (!m) return;
        rules.forEach((r) => r.attr("x1", cx(m)).attr("x2", cx(m)).style("display", null));
        marks.forEach((k) => {
          const v = m[k.f];
          k.dot.attr("cx", cx(m)).attr("cy", Number.isNaN(v) ? 0 : k.y(v)).style("display", Number.isNaN(v) ? "none" : null);
        });
        tooltip.showWith((tip) => {
          tip.append("p").attr("class", "name").text(formatMonthLong(m.date) + (m === months[months.length - 1] ? " (partial month)" : ""));
          tip.append("p").attr("class", "meta").text(plural(m.n, "play"));
          V_PANELS.forEach((p) => tip.append("p").attr("class", "genre")
            .text(`${p.short}: ${Number.isNaN(m[p.f]) ? "–" : p.fmt(m[p.f])}  (3-month ${p.fmt(m[`${p.f}3`])})`));
          if (m.year === first) tip.append("p").attr("class", "meta").text("Burn-in year for the second panel: tracks heard in 2015–16 count as new.");
        }, event);
      }
      drawn = { months, year, mode, width, away: null,
        reset() { rules.forEach((r) => r.style("display", "none")); marks.forEach((k) => k.dot.style("display", "none")); } };
    }

    return { set };
  }

  function makeTooltip(tipEl, wrapEl) {
    const tip = d3.select(tipEl);

    // `genre` ({code, label}) is given in the genre view only; it adds a line under the name.
    // `note` (text) adds a last line: a rank mode's rank (of the year or the chosen month),
    // or the new mode's novelty class and first play.
    function setContents(d, genre, note) {
      tip.selectChildren().remove();
      const head = tip.append("p");
      head.append("span").attr("class", "name").text(d.track);
      head.append("span").text(` – ${d.artist}`);
      if (genre) {
        const g = tip.append("p").attr("class", "genre");
        g.append("span").attr("class", "key").style("background", `var(--g-${genre.code})`);
        g.append("span").text(genre.label);
      }
      tip.append("p").attr("class", "meta").text(formatWhen(d));
      if (d.place) tip.append("p").attr("class", "meta").text(d.place);
      tip.append("p").attr("class", "meta").text(`#${d.rank} · ${plural(d.n_plays, "play")}`);
      if (note) tip.append("p").attr("class", "meta").text(note);
    }

    // Keep the tooltip on the side of the cursor with more room, and no wider than
    // that room, so it never pushes the page wider on a phone.
    function setPosition(event) {
      const [mx, my] = d3.pointer(event, wrapEl);
      const w = wrapEl.clientWidth;
      const h = wrapEl.clientHeight;
      const right = mx > w / 2;
      const below = my < h / 2;
      const room = (right ? mx : w - mx) - TIP_GAP;
      tip
        .style("max-width", `${Math.min(TIP_MAX, room)}px`)
        .style("left", right ? null : `${mx + TIP_GAP}px`)
        .style("right", right ? `${w - mx + TIP_GAP}px` : null)
        .style("top", below ? `${my + TIP_GAP}px` : null)
        .style("bottom", below ? null : `${h - my + TIP_GAP}px`)
        .style("text-align", right ? "right" : "left");
    }

    // Plain lines, the first in bold (used by the "where I was" strip).
    function setLines(lines) {
      tip.selectChildren().remove();
      lines.forEach((t, i) => tip.append("p").attr("class", i === 0 ? "name" : "meta").text(t));
    }

    return {
      show(d, event, genre, note) { setContents(d, genre, note); setPosition(event); tip.style("display", "block"); },
      showLines(lines, event) { setLines(lines); setPosition(event); tip.style("display", "block"); },
      // Any contents: fill(tip) appends them (used by the eras chart).
      showWith(fill, event) { tip.selectChildren().remove(); fill(tip); setPosition(event); tip.style("display", "block"); },
      move: setPosition,
      hide() { tip.style("display", "none"); },
    };
  }

  // The chart for one year at one width, which every mode draws on: the svg, the weekend
  // bands, the axes, the "where I was" strip, the month picker and its veil, a slot for the
  // mode's margin summary, and one circle per play with its position (`nodes`, in play
  // order). A tab switch in the same year keeps all of it and re-skins it (skinRank,
  // skinGenre: dot colors, draw order, summary, lists, header and handlers); a new year or
  // a new width builds a new frame. `pick` holds the chosen month and hour (see subsetPicker).
  // The modes' time-of-day summaries differ in height (SUM_CURVE_H, SHARE_CURVE_H), and a
  // taller one moves the plot down. So the plot is drawn in its own coordinates (y from
  // MARGIN.top) inside two groups that setCurveH() shifts down: a switch between a short
  // and a tall summary moves two groups and resizes the svg instead of moving every dot.
  function buildFrame(where, year, data, els, tooltip, pick) {
    const clientWidth = els.chart.clientWidth;
    const width = Math.max(MIN_WIDTH, clientWidth);
    const narrow = width < NARROW;

    // "Where I was" runs; null (no strip, original margin) when the file is missing.
    // Runs of 2+ days get a place label, on desktop only.
    const runs = where ? awayRuns(where, year) : null;
    const named = runs && !narrow ? runs.filter((r) => r.days.length >= 2) : [];
    named.forEach((r) => { r.short = placeShort(r.label) || "Away"; });
    const labelW = named.length ? Math.ceil(d3.max(named, (r) => textWidth(r.short, PLACE_PX))) : 0;
    // Room for the widest month label as drawn when chosen (bold), at least MONTH_W.
    const monthW = Math.max(MONTH_W, Math.ceil(d3.max(d3.range(12),
      (k) => axisWidth(formatMonthShort(new Date(Date.UTC(year, k, 1))).toUpperCase(), 800))) + 2);

    const monthEnd = (labelW ? labelW + PLACE_GAP : 0) + monthW; // month labels end here
    const stripX = monthEnd + STRIP_PAD;
    const showBars = SHOW_MARGINS && !narrow; // per-day bars crowd the plot on phones
    const barW = SUM_BAR_W;
    const m = { // in the plot's coordinates (setCurveH adds the summary's height above)
      top: MARGIN.top,
      right: MARGIN.right + (showBars ? barW + SUM_GAP : 0),
      bottom: MARGIN.bottom,
      left: runs ? stripX + STRIP_W + PLOT_PAD : monthEnd + PLOT_PAD,
    };
    const sumTopOf = (curveH) => (SHOW_MARGINS ? SUM_TOP + curveH - SUM_CURVE_H : 0); // extra height above the plot

    const x = d3.scaleLinear()
      .domain([0, 1440])
      .range([m.left, width - m.right]);
    const y = d3.scaleUtc()
      .domain([Date.UTC(year, 0, 1), Date.UTC(year, 11, 31)])
      .range([m.top, HEIGHT - m.bottom]);

    tooltip.hide();
    d3.select(els.pinbar).style("display", "none");
    d3.select(els.chart).selectChildren().remove();
    const svg = d3.select(els.chart).append("svg")
      .attr("width", width)
      .attr("role", "img");

    // Weekend bands behind each Saturday and Sunday row, under the gridlines and dots.
    // Rows are listening days, so a band covers that day's 5 AM to the next 5 AM.
    const rowH = y(Date.UTC(year, 0, 2)) - y(Date.UTC(year, 0, 1));
    const weekendDays = d3.utcDays(new Date(Date.UTC(year, 0, 1)), new Date(Date.UTC(year + 1, 0, 1)))
      .filter((d) => d.getUTCDay() === 6 || d.getUTCDay() === 0);
    const weekends = svg.append("g").attr("class", "weekends");
    weekends.selectAll("rect")
      .data(weekendDays)
      .join("rect")
      .attr("x", m.left - RADIUS)
      .attr("width", width - m.left - m.right + 2 * RADIUS)
      .attr("y", (d) => y(d) - rowH / 2)
      .attr("height", rowH);

    // The hour labels stay at the top of the svg; setCurveH() sets their gridlines' length.
    // Labels every hour, or every 2, 3 or 6 hours when hourly labels would touch (measured
    // at this width). The d3 axis draws the gridlines; the labels are subsetPicker's buttons.
    const labelsFit = (ticks) => {
      const text = hourLabels(ticks);
      const w = ticks.map((v) => axisWidth(text.get(v), 600));
      return ticks.every((v, i) => i === 0 || x(v) - x(ticks[i - 1]) >= (w[i] + w[i - 1]) / 2 + AXIS_GAP);
    };
    const hourStep = HOUR_STEPS.find((s) => labelsFit(d3.range(0, 1441, s * 60))) || HOUR_STEPS[HOUR_STEPS.length - 1];
    const ticks = d3.range(0, 1441, hourStep * 60);
    const tickText = hourLabels(ticks);
    const xAxis = svg.append("g")
      .attr("class", "axis x-axis")
      .attr("transform", `translate(0, ${X_AXIS_Y})`);
    const axisX = d3.axisTop(x)
      .tickValues(ticks)
      .tickFormat(() => "")
      .tickPadding(4);
    const hourAxis = svg.append("g")
      .attr("class", "axis hours")
      .attr("font-family", "sans-serif") // as the d3 axes
      .attr("transform", `translate(0, ${X_AXIS_Y})`);

    // Everything else is in the plot's coordinates, in one group, bottom to top: the month
    // labels, the strip, the summary, the dots, the genre mode's lifted copies, the month
    // and hour veil and the genre mode's outlined copies (see skinGenre; empty in the other modes).
    const plot = svg.append("g").attr("class", "plot");
    const yAxis = plot.append("g")
      .attr("class", "axis y-axis")
      .attr("transform", `translate(${monthEnd}, 0)`)
      .call(d3.axisLeft(y)
        .ticks(d3.utcMonth.every(1))
        .tickFormat(d3.utcFormat("%b"))
        .tickSize(0)
        .tickPadding(0));
    const picker = subsetPicker(plot, yAxis, hourAxis, { x, y, year, rowH, pick, placeW: labelW, labelsEnd: monthEnd,
      width, monthW, hourText: tickText, plotX0: m.left - RADIUS, plotX1: width - m.right + RADIUS });
    if (runs) drawStrip(plot, runs, named, { y, rowH, stripX, labelX: labelW, tooltip });
    const slot = plot.append("g").attr("class", "sum-slot");
    const layer = plot.append("g").attr("class", "dots");
    const nodes = layer.selectAll("circle")
      .data(data)
      .join("circle")
      .attr("cx", (d) => x(dayMinute(d.minutes)))
      .attr("cy", (d) => y(d.day))
      .attr("r", RADIUS)
      .nodes();
    const lift = plot.append("g").attr("class", "lift").attr("pointer-events", "none");
    picker.veil();
    const ring = plot.append("g").attr("class", "ring").attr("pointer-events", "none");

    // Shifts the plot down for a summary `curveH` tall and sizes the svg and gridlines to match.
    let shift = null;
    function setCurveH(curveH) {
      const sumTop = sumTopOf(curveH);
      if (sumTop === shift) return;
      shift = sumTop;
      const height = HEIGHT + sumTop; // the plot itself keeps its height
      svg.attr("height", height);
      weekends.attr("transform", `translate(0, ${sumTop})`);
      plot.attr("transform", `translate(0, ${sumTop})`);
      xAxis.call(axisX.tickSize(-(height - m.bottom - X_AXIS_Y)));
      // The summaries' curves sit at X_AXIS_Y in plot coordinates, curveH tall (useSummary).
      picker.setCurve(SHOW_MARGINS ? X_AXIS_Y - curveH : null);
    }

    // The mode's margin summary, in the slot under the dots (see SUMMARIES). Each kind is
    // drawn the first time it is asked for, by build(parent, geometry), which draws the
    // year's curve and per-day bars and returns { curve(rows, layerOf), bars(rows),
    // overlay(spec) }; it is kept, out of the page, while another mode shows. With attach
    // false it is only built (the idle warm-up). Returns null with ?margins=0, else { node,
    // show(ck, curveRows, bk, barRows, layerOf), overlay(spec) }, where show redraws the
    // curve for key ck (the month, null for the year; with CURVE_SPLIT, the rank modes'
    // key also names the mode and the hour, which set the layers) and the bars for key bk
    // (subKey of the month and hour) unless they already show them, and overlay draws or
    // clears an item's curve over it (curveOverlay).
    const summaries = new Map();
    function useSummary(kind, curveH, build, attach = true) {
      if (!SHOW_MARGINS) return null;
      const box = slot.node();
      let s = summaries.get(kind);
      if (!s) {
        const sumTop = sumTopOf(curveH);
        const draw = build(slot, { x, y, rowH, base: X_AXIS_Y + 22 + curveH - sumTop, curveH, bars: showBars,
          barX: width - m.right + SUM_GAP, barW, labelX: width - 2, labelY: m.top - 8 });
        s = { node: box.lastChild, ck: null, bk: subKey(null, null),
          show(ck, curveRows, bk, barRows, layerOf = null) {
            if (ck !== s.ck) { draw.curve(curveRows, layerOf); s.ck = ck; }
            if (bk !== s.bk) { draw.bars(barRows); s.bk = bk; }
          },
          overlay: draw.overlay };
        summaries.set(kind, s);
        box.removeChild(s.node);
      }
      if (attach && box.firstChild !== s.node) {
        if (box.firstChild) box.removeChild(box.firstChild);
        box.appendChild(s.node);
      }
      return s;
    }

    // The dots' color attributes (data-rank, data-g) as last set, so a skin touches only
    // the dots whose value changes. paint(name, valueOf) gives each dot valueOf(its row);
    // null, or no valueOf, removes the attribute.
    const painted = { "data-rank": new Array(nodes.length).fill(null), "data-g": new Array(nodes.length).fill(null) };
    function paint(name, valueOf) {
      const now = painted[name];
      for (let i = 0; i < nodes.length; i++) {
        const v = valueOf ? valueOf(nodes[i].__data__) : null;
        const s = v == null ? null : String(v);
        if (s === now[i]) continue;
        now[i] = s;
        if (s === null) nodes[i].removeAttribute(name);
        else nodes[i].setAttribute(name, s);
      }
    }

    // Puts the dots in this draw order (the last on top), moving as few as it can: the
    // longest run of dots, in page order, that is already in the new order's sequence
    // (a longest increasing subsequence) stays put, and every other dot is inserted
    // before its successor. Going from Genre to Artist that moves the ~5,000 ranked dots
    // instead of all 19,000; each move costs Firefox a remove, an insert and a new frame.
    // Never d3's order() or sort() on the dots: order() calls compareDocumentPosition once
    // per dot, which Firefox answers slowly among 19,000 siblings (2.9 s per month click
    // in Artist mode).
    nodes.forEach((n, i) => { n.__i = i; }); // each dot's play index
    function stack(order) {
      const g = layer.node();
      const n = order.length;
      const pos = new Int32Array(n); // play index -> place in the new order
      for (let k = 0; k < n; k++) pos[order[k].__i] = k;
      // Patience sort over the places, in page order, keeping each pile top's predecessor.
      const tails = []; // tails[j]: the place ending the best run of length j + 1
      const prev = new Int32Array(n).fill(-1);
      for (let c = g.firstChild; c; c = c.nextSibling) {
        const v = pos[c.__i];
        let lo = 0;
        let hi = tails.length;
        while (lo < hi) { const mid = (lo + hi) >> 1; if (tails[mid] < v) lo = mid + 1; else hi = mid; }
        if (lo > 0) prev[v] = tails[lo - 1];
        tails[lo] = v;
      }
      const keep = new Uint8Array(n);
      for (let v = tails.length ? tails[tails.length - 1] : -1; v >= 0; v = prev[v]) keep[v] = 1;
      let next = null;
      for (let k = n - 1; k >= 0; k--) {
        if (!keep[k]) g.insertBefore(order[k], next);
        next = order[k];
      }
    }

    // Recolors and restacks the dots for a color scheme ("genre", "new" or "rank"): fn()
    // makes the paint and stack calls. When the scheme changes, every dot changes color,
    // so fn() runs with the dots' layer out of the page and Firefox rebuilds all 19,000
    // dots' frames once, in a batch, when it goes back, instead of restyling each and
    // re-inserting the moved ones one by one. Within a scheme (one rank mode to another,
    // a month click) few dots change and they are updated in place.
    let scheme = null;
    function recolor(next, fn) {
      if (next === scheme) { fn(); return; }
      scheme = next;
      const g = layer.node();
      const parent = g.parentNode;
      const after = g.nextSibling;
      parent.removeChild(g);
      try { fn(); } finally { parent.insertBefore(g, after); }
    }

    // The rank modes' draw order, from the dots' data-rank: unranked dots in play order,
    // then rank 10 down to rank 1 (each in play order), so #1 is on top. Returns the dots
    // of each rank, as a Map.
    function stackRanks() {
      const rk = painted["data-rank"];
      const runs = Array.from({ length: TOP_N + 1 }, () => []); // unranked, rank 10, ..., rank 1
      for (let i = 0; i < nodes.length; i++) runs[rk[i] === null ? 0 : TOP_N + 1 - +rk[i]].push(nodes[i]);
      stack(runs.flat());
      const byRank = new Map();
      runs.forEach((run, j) => { if (j > 0 && run.length) byRank.set(TOP_N + 1 - j, run); });
      return byRank;
    }

    // Lookups over the dots, built when a mode first asks: the dots of each artist, track,
    // album and novelty class, in play order (groups), and every dot in the new mode's draw
    // order: familiar, new finds, first listens, each in play order (nvOrder).
    let groups = null;
    let nvOrder = null;
    return {
      year, data, clientWidth, svg, x, y, picker, layer, nodes, lift, ring,
      setCurveH, useSummary, paint, stack, stackRanks, recolor,
      groups() {
        if (!groups) {
          const by = (list, keyOf) => d3.group(list, (n) => keyOf(n.__data__));
          groups = {
            artist: by(nodes, (d) => d.artist),
            track: by(nodes, trackKey),
            album: by(nodes.filter((n) => n.__data__.album), albumKey),
            nv: by(nodes, (d) => d.nv),
          };
        }
        return groups;
      },
      nvOrder() {
        if (!nvOrder) nvOrder = nodes.slice().sort((a, b) => NV_LAYER[a.__data__.nv] - NV_LAYER[b.__data__.nv]);
        return nvOrder;
      },
    };
  }

  // The month labels and the hour labels as buttons (click, or Enter or Space when focused).
  // Choosing a month emphasizes its listening days (the `day` column, which sets each dot's
  // row); choosing an hour emphasizes that clock hour (h:00 to h:59, from `minutes`, which
  // sets each dot's column); with both, that month's plays in that hour. A veil in the page
  // colour covers the rest: the rows of the other months, from the strip to the per-day
  // bars, and the place labels beside them; and, within the chosen month's rows (every
  // row with no month), the plot outside the hour's column, up through the time-of-day
  // curve. The pieces never overlap, so no area is veiled twice. Choosing a month or an
  // hour again clears it; choosing another switches. The choice lives in `pick` ({ year,
  // month: 0 to 11 or null, hour: 0 to 23 or null }), so a redraw of the same year (a
  // resize) and a tab switch keep it, and a new year clears both.
  // Every hour has a click target in the axis band, labeled or not: a shown label's target
  // covers its text, and hours without a label share the space between labels. The chosen
  // hour's label always shows, bold like a chosen month ("7 AM" for an hour that has no
  // label at this width), and a label it would touch is hidden while it is chosen.
  // veil() draws the veil at the current top of `parent` (the frame's plot group): what is
  // drawn before it fades and what is drawn after it does not. setCurve(top) gives the top
  // of the time-of-day curve in plot coordinates (null without margins), so the hour's
  // veil reaches it. clear() and clearHour() clear the month or the hour and say whether
  // one was set. listen(fn) has fn() run after every change, and replaces the last fn
  // (each mode filters its lists, curve, bars and stats line by the subset, and the rank
  // modes recolor their dots).
  function subsetPicker(parent, axis, hourAxis, { x, y, year, rowH, pick, placeW, labelsEnd, width, monthW, hourText, plotX0, plotX1 }) {
    if (pick.year !== year) { pick.year = year; pick.month = null; pick.hour = null; }
    const monthOf = (d) => d.getUTCMonth();
    const ticks = axis.selectAll(".tick")
      .attr("role", "button")
      .attr("tabindex", 0)
      .attr("aria-label", (d) => `Emphasize ${formatMonthName(d)}`);
    // A wider and taller target than the label, across the whole month-label column.
    ticks.insert("rect", ":first-child")
      .attr("class", "hit")
      .attr("x", -(monthW + 4))
      .attr("y", -MONTH_HIT_H / 2)
      .attr("width", monthW + 6)
      .attr("height", MONTH_HIT_H)
      .attr("rx", 3);

    // The hours, left to right from 5 AM: column i (0 to 23) starts at chart minute v and is
    // clock hour h. An hour that is one of the axis ticks (hourText) has that label; any
    // other shows "7 AM" when chosen. w and wOn are the label's widths, plain and bold.
    const step = x(60) - x(0);
    const hours = d3.range(24).map((i) => {
      const v = i * 60;
      const h = (DAY_START / 60 + i) % 24;
      const tick = hourText.get(v);
      const text = tick === undefined ? `${hour12(h)} ${meridiem(h)}` : tick;
      return { i, v, h, x: x(v), tick: tick !== undefined, text, w: axisWidth(text, 600), wOn: axisWidth(text, 800) };
    });
    const hourG = hourAxis.selectAll("g.hour")
      .data(hours)
      .join("g")
      .attr("class", "hour")
      .attr("role", "button")
      .attr("tabindex", 0)
      .attr("aria-label", (d) => `Emphasize ${formatHourRange(d.h)}`);
    const hourHit = hourG.append("rect").attr("class", "hit").attr("y", HOUR_HIT_TOP).attr("height", HOUR_HIT_H).attr("rx", 3);
    const hourLabel = hourG.append("text").attr("x", (d) => d.x).attr("y", -4).attr("text-anchor", "middle").text((d) => d.text);
    // The closing 5 AM is a boundary, not an hour: a label only.
    const endText = hourText.get(1440);
    const end = endText === undefined ? null : { x: x(1440), w: axisWidth(endText, 600) };
    const endLabel = end ? hourAxis.append("text").attr("class", "end").attr("x", end.x).attr("y", -4)
      .attr("text-anchor", "middle").text(endText) : null;

    function applyHours(on) {
      const touches = (a) => !!on && a !== on && Math.abs(a.x - on.x) < (a.w + on.wOn) / 2 + AXIS_GAP;
      const shown = hours.map((d) => d === on || (d.tick && !touches(d)));
      const endShown = !!end && !touches(end);
      // Click targets: each shown label covers its text; the hours between two shown labels
      // split the space between them evenly; two neighboring labels meet halfway.
      const anchors = [{ i: -1, r: Math.min(hours[0].x - step / 2, shown[0] ? hours[0].x - (hours[0] === on ? hours[0].wOn : hours[0].w) / 2 : Infinity) }];
      hours.forEach((d, i) => {
        if (!shown[i]) return;
        const half = (d === on ? d.wOn : d.w) / 2;
        anchors.push({ i, l: d.x - half, r: d.x + half });
      });
      anchors.push({ i: 24, l: end && endShown ? end.x - end.w / 2 : x(1440) });
      const lo = new Array(24);
      const hi = new Array(24);
      for (let j = 0; j + 1 < anchors.length; j++) {
        const a = anchors[j];
        const b = anchors[j + 1];
        const from = a.r;
        const to = Math.max(b.l, from);
        const n = b.i - a.i - 1; // unlabeled hours between them
        if (n === 0) {
          const mid = a.i < 0 ? from : (from + to) / 2; // the left end belongs to the first label
          if (a.i >= 0) hi[a.i] = mid;
          if (b.i < 24) lo[b.i] = mid;
        } else {
          const seg = (to - from) / n;
          if (a.i >= 0) hi[a.i] = from;
          if (b.i < 24) lo[b.i] = to;
          for (let t = 0; t < n; t++) { lo[a.i + 1 + t] = from + t * seg; hi[a.i + 1 + t] = from + (t + 1) * seg; }
        }
      }
      hourG.classed("on", (d) => d === on).attr("aria-pressed", (d) => String(d === on));
      hourLabel.style("display", (d) => (shown[d.i] ? null : "none"));
      hourHit.attr("x", (d) => lo[d.i]).attr("width", (d) => Math.max(0, hi[d.i] - lo[d.i]));
      if (endLabel) endLabel.style("display", endShown ? null : "none");
    }

    // Veil pieces run from the plot's first row to its last, a dot's radius beyond each.
    const top = y(Date.UTC(year, 0, 1)) - rowH / 2 - RADIUS;
    const bottom = y(Date.UTC(year, 11, 31)) + rowH / 2 + RADIUS;
    const rowStart = (k) => (k > 11 ? bottom : y(Date.UTC(year, k, 1)) - rowH / 2);
    const cols = [[labelsEnd + 2, width]]; // the strip, the plot and the per-day bars
    if (placeW) cols.push([0, placeW + 3]); // the place labels
    let curveTop = null;
    let veil = null;
    let onChange = null;

    function apply() {
      const k = pick.month;
      const on = pick.hour === null ? null : hours.find((d) => d.h === pick.hour) || null;
      ticks.classed("on", (d) => monthOf(d) === k).attr("aria-pressed", (d) => String(monthOf(d) === k));
      applyHours(on);
      if (!veil) return;
      // The chosen month's rows (every row with no month); the month veil covers the rest
      // across the full width, and the hour veil only the plot beside the hour, inside them.
      const mTop = k === null || k === 0 ? top : rowStart(k);
      const mBot = k === null || k === 11 ? bottom : rowStart(k + 1);
      const pieces = [];
      for (const [x0, x1] of cols) {
        if (mTop > top) pieces.push({ x0, x1, y0: top, y1: mTop }); // the months before
        if (mBot < bottom) pieces.push({ x0, x1, y0: mBot, y1: bottom }); // the months after
      }
      if (on) {
        // The hour's column runs from halfway between its first minute and the minute
        // before to halfway past its last minute, as the month veil splits rows halfway:
        // every dot outside has its center veiled and every dot inside its center clear.
        // The day's first and last hours reach the plot's edge.
        const inL = on.v === 0 ? plotX0 : x(on.v - 0.5);
        const inR = on.v + 60 === 1440 ? plotX1 : x(on.v + 59.5);
        const spans = curveTop === null ? [[mTop, mBot]]
          : mTop === top ? [[curveTop, mBot]] : [[curveTop, top], [mTop, mBot]];
        for (const [y0, y1] of spans) {
          if (inL > plotX0) pieces.push({ x0: plotX0, x1: inL, y0, y1 });
          if (inR < plotX1) pieces.push({ x0: inR, x1: plotX1, y0, y1 });
        }
      }
      veil.selectAll("rect")
        .data(pieces)
        .join("rect")
        .attr("x", (p) => p.x0)
        .attr("width", (p) => p.x1 - p.x0)
        .attr("y", (p) => p.y0)
        .attr("height", (p) => p.y1 - p.y0);
    }

    const changed = () => { apply(); if (onChange) onChange(); };
    const toggleMonth = (d) => { pick.month = pick.month === monthOf(d) ? null : monthOf(d); changed(); };
    const toggleHour = (d) => { pick.hour = pick.hour === d.h ? null : d.h; changed(); };
    const onKey = (fn) => (event, d) => {
      if (event.key !== "Enter" && event.key !== " ") return;
      event.preventDefault();
      fn(d);
    };
    // A label click is not a click on empty space.
    ticks.on("click", (event, d) => { event.stopPropagation(); toggleMonth(d); }).on("keydown", onKey(toggleMonth));
    hourG.on("click", (event, d) => { event.stopPropagation(); toggleHour(d); }).on("keydown", onKey(toggleHour));
    apply();

    return {
      veil() { veil = parent.append("g").attr("class", "veil").attr("pointer-events", "none"); apply(); },
      setCurve(t) { if (t === curveTop) return; curveTop = t; if (veil && pick.hour !== null) apply(); },
      clear() { if (pick.month === null) return false; pick.month = null; changed(); return true; },
      clearHour() { if (pick.hour === null) return false; pick.hour = null; changed(); return true; },
      listen(fn) { onChange = fn; },
    };
  }

  // A click on empty space anywhere on the page runs clear() (each mode passes one that
  // drops its pick and its selection, as a click on the chart background did).
  // Empty space is any click outside what is clickable: dots, list rows, links, buttons,
  // month and hour labels, the two navs, the pin bar, tooltips, and the eras and variety charts
  // (they open a year on click). The check runs on the event's path as it stood when the
  // click began, so a row whose click redraws its own list still counts as a row. A click
  // that ends a text selection does nothing, and the chosen month and hour stay (their
  // labels and Esc clear them). Each skin registers this under the same name, so a redraw or a tab
  // switch replaces the last skin's handler.
  const CLICKABLE = "circle, a, button, input, select, textarea, label, li, [role=button], [role=tab],"
    + " #colorby, #years, #pinbar, .tooltip, #eras svg, #variety svg";
  function onEmptyClick(clear) {
    d3.select(document).on("click.unpick", (event) => {
      if (event.button !== 0) return;
      if (event.composedPath().some((n) => n.nodeType === 1 && n.matches(CLICKABLE))) return;
      const text = window.getSelection();
      if (text && text.toString() !== "") return;
      clear();
    });
  }

  // Each mode's margin summary on a frame: the rank modes share one (plays by time of day
  // and per day), the new mode stacks by novelty class and the genre mode by genre.
  // attach false only builds it (warmUp).
  const SUMMARIES = {
    rank: (F, Y, genres, attach) => F.useSummary("rank", SUM_CURVE_H, (parent, sum) => drawSummaries(parent, F.data, sum), attach),
    new: (F, Y, genres, attach) => F.useSummary("new", SHARE_CURVE_H, (parent, sum) => drawGenreSummaries(parent, F.data, NV_ORDER, sum,
      { keys: NV_ORDER, codeOf: (d) => d.nv }), attach),
    genre: (F, Y, genres, attach) => F.useSummary("genre", SHARE_CURVE_H, (parent, sum) => drawGenreSummaries(parent, F.data,
      genreLookups(Y, genres).order, sum), attach),
  };

  // After a draw, the lookups and summaries every other mode needs for this year are
  // built in idle time, a piece at a time, so the first switch to a mode is as quick as
  // the later ones. live() says whether this frame is still the one on show.
  function warmUp(F, Y, genres, live) {
    const jobs = [() => F.groups(), () => rankLookups(Y).tableFor(null, null), () => SUMMARIES.rank(F, Y, genres, false),
      () => itemGroup(Y, "artist"), () => itemGroup(Y, "album"), () => itemGroup(Y, "track")];
    if (HAS_NV) {
      jobs.push(() => F.nvOrder(), () => rankLookups(Y).newFinds(null, null), () => rankLookups(Y).sharesFor(null, null),
        () => SUMMARIES.new(F, Y, genres, false));
    }
    if (genres) jobs.push(() => genreLookups(Y, genres), () => SUMMARIES.genre(F, Y, genres, false));
    const idle = window.requestIdleCallback
      ? (fn) => window.requestIdleCallback(fn, { timeout: 2000 })
      : (fn) => setTimeout(() => fn({ timeRemaining: () => 8, didTimeout: false }), 50);
    const step = (deadline) => {
      if (!live()) return;
      do jobs.shift()(); while (jobs.length && deadline.timeRemaining() > 8);
      if (jobs.length) idle(step);
    };
    idle(step);
  }

  // Lookups over one year's plays that no mode changes, kept per year and built on first
  // use (rankLookups, genreLookups), so a tab switch or a resize does not redo them.
  const yearLookups = new Map();
  function lookupsFor(year, data) {
    let Y = yearLookups.get(year);
    if (!Y || Y.data !== data) yearLookups.set(year, (Y = { year, data }));
    return Y;
  }
  // Plays by listening month, 0 to 11, from the `day` column (-1 for a day outside the year).
  function byMonthOf(Y) {
    if (!Y.byMonth) Y.byMonth = d3.group(Y.data, (d) => (d.day.getUTCFullYear() === Y.year ? d.day.getUTCMonth() : -1));
    return Y.byMonth;
  }
  // The plays of a subset (listening month k, clock hour h; see subKey), in play order,
  // kept per year. The whole year is the data itself.
  function subsetRows(Y, k, h) {
    if (k === null && h === null) return Y.data;
    if (!Y.subsets) Y.subsets = new Map();
    const key = subKey(k, h);
    let rows = Y.subsets.get(key);
    if (!rows) {
      const base = k === null ? Y.data : byMonthOf(Y).get(k) || [];
      rows = h === null ? base : base.filter((d) => hourOf(d) === h);
      Y.subsets.set(key, rows);
    }
    return rows;
  }

  // The items an overlay can show (curveOverlay): an artist (key: the name), an album
  // (albumKey; plays with no album name have none) or a track (trackKey). itemGroup gives
  // the year's plays of each item of a kind, grouped the first time a kind is asked for;
  // itemBins gives one item's plays per 10-minute bin (binCounts) in the year (k null) or
  // listening month k, counted the first time and kept per year, so a hover only looks
  // them up.
  const ITEM_KEY = { artist: (d) => d.artist, album: albumKey, track: trackKey };
  function itemGroup(Y, kind) {
    if (!Y.items) Y.items = { bins: new Map() };
    if (!Y.items[kind]) Y.items[kind] = d3.group(kind === "album" ? Y.data.filter((d) => d.album) : Y.data, ITEM_KEY[kind]);
    return Y.items[kind];
  }
  function itemBins(Y, kind, key, k) {
    const group = itemGroup(Y, kind);
    const id = `${kind}\u0000${key}\u0000${k}`;
    let bins = Y.items.bins.get(id);
    if (!bins) {
      const rows = group.get(key) || [];
      bins = binCounts(k === null ? rows
        : rows.filter((d) => d.day.getUTCFullYear() === Y.year && d.day.getUTCMonth() === k));
      Y.items.bins.set(id, bins);
    }
    return bins;
  }
  // An overlay item from a focus (trackFocus, or an artist or album focus) or from a play,
  // as the mode names it ("artist", "album" or "track"; a play with no album has no album).
  function focusItem(f) {
    if (f.kind === "artist") return { kind: "artist", key: f.artist, name: f.artist };
    if (f.kind === "album") return { kind: "album", key: f.key, name: f.album };
    return { kind: "track", key: f.key, name: f.track };
  }
  function playItem(d, kind) {
    if (kind === "artist") return { kind, key: d.artist, name: d.artist };
    if (kind === "album") return d.album ? { kind, key: albumKey(d), name: d.album } : null;
    return { kind: "track", key: trackKey(d), name: d.track };
  }
  // What curveOverlay draws: the item's bins for month k (null: the year) in `color`.
  // `id` names all four, so the overlay redraws only when one of them changes.
  function overlaySpec(Y, item, k, color) {
    return { id: `${item.kind}\u0000${item.key}\u0000${k}\u0000${color}`, kind: item.kind, key: item.key, k,
      name: item.name, color, bins: itemBins(Y, item.kind, item.key, k) };
  }
  const byTitle = (a, b) => d3.ascending(a.track, b.track) || d3.ascending(a.artist, b.artist);
  const byArtistName = (a, b) => d3.ascending(a.artist, b.artist);
  const byAlbumName = (a, b) => d3.ascending(a.album, b.album) || d3.ascending(a.artist, b.artist);

  // The artist, album, track and new modes' lookups for a year: the plays of each month,
  // the year's or a subset's top-ten tables (tableFor(k, h): tracks, artists and albums,
  // and every artist's, album's and, for a subset, track's place), new finds and novelty
  // shares, each computed when first asked for and kept by subset (subKey). The tables are
  // the same in all four modes. A subset (a month, an hour or both) ranks by MONTH_RANK.
  function rankLookups(Y) {
    if (Y.rank) return Y.rank;
    const { year, data } = Y;
    const byMonth = byMonthOf(Y);
    const monthOf = (d) => (d.day.getUTCFullYear() === year ? d.day.getUTCMonth() : -1);
    const nArtist = new Map();
    const nAlbum = new Map();
    const firstOfTrack = new Map(); // each track's first play in the year
    for (const d of data) {
      nArtist.set(d.artist, (nArtist.get(d.artist) || 0) + 1);
      if (d.album) { const k = albumKey(d); nAlbum.set(k, (nAlbum.get(k) || 0) + 1); }
      const k = trackKey(d);
      if (!firstOfTrack.has(k)) firstOfTrack.set(k, d);
    }
    const artistFocus = (artist) => ({ kind: "artist", artist, n: nArtist.get(artist) });
    const albumFocus = (d) => ({ kind: "album", artist: d.artist, key: albumKey(d), album: d.album, n: nAlbum.get(albumKey(d)) });

    const tables = new Map();
    function tableFor(k, h = null) {
      const key = subKey(k, h);
      if (tables.has(key)) return tables.get(key);
      const whole = k === null && h === null;
      const rows = subsetRows(Y, k, h);
      const rule = whole ? "dense" : MONTH_RANK;
      const top = (ranked) => ranked.filter((r) => r.rank <= TOP_N);
      let tracks;
      let trackRank = null;
      if (whole) {
        tracks = Array.from(firstOfTrack.values())
          .filter((d) => d.rank <= TOP_N)
          .sort((a, b) => a.rank - b.rank || d3.ascending(a.track, b.track))
          .map((d) => ({ rank: d.rank, title: d.track, sub: d.artist, n: d.n_plays, focus: trackFocus(d) }));
      } else {
        const ranked = rankCounts(rows, trackKey, rule, byTitle);
        trackRank = new Map(ranked.map((r) => [r.key, r]));
        tracks = top(ranked).map((r) => (
          { rank: r.rank, title: r.first.track, sub: r.first.artist, n: r.n, focus: trackFocus(r.first) }));
      }
      const artistRanked = rankCounts(rows, (d) => d.artist, rule, byArtistName);
      const albumRanked = rankCounts(rows.filter((d) => d.album), albumKey, rule, byAlbumName);
      const artists = top(artistRanked).map((r) => ({ rank: r.rank, title: r.key, n: r.n, focus: artistFocus(r.key) }));
      const albums = top(albumRanked)
        .map((r) => ({ rank: r.rank, title: r.first.album, sub: r.first.artist, n: r.n, focus: albumFocus(r.first) }));
      const rankOf = {
        track: trackRank,
        artist: new Map(artistRanked.map((r) => [r.key, r])),
        album: new Map(albumRanked.map((r) => [r.key, r])),
      };
      const t = { whole, rows, tracks, artists, albums, rankOf };
      tables.set(key, t);
      return t;
    }

    // New finds: tracks whose first play falls in this year, by its calendar date (or in
    // the chosen month, by its listening day), by plays in the subset (the year, the month,
    // the hour of either). The hour narrows the plays counted, not the first play. d.first
    // is the track's first play (linkFirsts).
    const finds = new Map();
    function newFinds(k, h = null) {
      const key = subKey(k, h);
      if (finds.has(key)) return finds.get(key);
      const rows = subsetRows(Y, k, h);
      const found = (d) => (k === null ? d.first.date.getUTCFullYear() === year
        : d.first.day.getUTCFullYear() === year && d.first.day.getUTCMonth() === k);
      const out = rankCounts(rows.filter(found), trackKey, "position", byTitle).slice(0, TOP_N).map((r) => ({
        rank: r.rank, title: r.first.track, sub: r.first.artist, n: r.n, sw: "var(--nv-FL)",
        dt: formatMonthDay(r.first.first.date), dtLong: `first played ${formatDay(r.first.first.date)}`,
        focus: trackFocus(r.first),
      }));
      finds.set(key, out);
      return out;
    }

    // The year's (or a subset's) plays by novelty class, for the new mode's header.
    const shares = new Map();
    function sharesFor(k, h = null) {
      const key = subKey(k, h);
      if (!shares.has(key)) {
        const rows = subsetRows(Y, k, h);
        shares.set(key, { n: rows.length || 1, counts: d3.rollup(rows, (v) => v.length, (d) => d.nv) });
      }
      return shares.get(key);
    }

    // A column's ranking (the artist and album modes' Top songs and Top albums), kept by
    // `key` (subset, pick and column), from rank(), which runs the first time only.
    const columns = new Map();
    function column(key, rank) {
      if (!columns.has(key)) columns.set(key, rank());
      return columns.get(key);
    }

    Y.rank = { byMonth, monthOf, artistFocus, albumFocus, nAlbums: nAlbum.size, tableFor, newFinds, sharesFor, column };
    return Y.rank;
  }

  // The artist, album, track and new modes (the old tracks view, generalized), as
  // a skin on the year's frame (buildFrame): it sets the dots' colors and draw order, the
  // margin summary, the lists, the header and the handlers, and keeps no dots of its own.
  // Rank modes color the year's (or the chosen month's) top ten artists, albums or tracks
  // with the rank palette; the new mode colors every play by its novelty class (d.nv).
  // Hover shows the same track and artist; click pins. A chosen month or hour (a subset)
  // recolors the rank modes by the subset's positions (MONTH_RANK) and filters the lists,
  // the novelty shares, the per-day bars and the stats line; the curve follows the month
  // (and, split by color with CURVE_SPLIT, the dots' colors). A hovered or picked item's
  // own curve shows over it (syncOverlay).
  // The artist and album modes add the genre view's pick-and-select pattern: picking an
  // artist (album) filters the Top songs and Top albums (Top songs) columns; a row there
  // selects that song or album on top of the pick. See "Picks and selections" below.
  // `Y` is the year's lookups (lookupsFor); `lists` is { a, b, c } (three makeRankList
  // lists). Returns { teardown }, which the next skin calls first: it ends a hover's
  // highlight and a pending clear, which the next skin would not overwrite.
  function skinRank(F, Y, mode, { els, tooltip, lists, pick, stats }) {
    const isNew = mode === "new";
    const { svg, layer, picker, year, data } = F;
    const R = rankLookups(Y);
    const { byMonth, monthOf, artistFocus, albumFocus, tableFor } = R;
    const { artist: byArtist, track: byTrack, album: byAlbum, nv: byNv } = F.groups();
    const bar = d3.select(els.pinbar);
    tooltip.hide();
    bar.style("display", "none");
    svg.classed("genre", false)
      .attr("aria-label", `Every Spotify play in ${year}, by time of day (5 AM to 5 AM) and day, colored by ${isNew ? "novelty" : `top ${mode}s`}`);
    F.setCurveH(isNew ? SHARE_CURVE_H : SUM_CURVE_H);
    const summary = SUMMARIES[isNew ? "new" : "rank"](F, Y);
    const keyOf = mode === "artist" ? (d) => d.artist : mode === "album" ? albumKey : trackKey;

    // The new mode colors each dot by its novelty class and draws first listens on top
    // (NV_LAYER); the rank modes color the dots by rank in recolor(), below.
    if (isNew) {
      F.recolor("new", () => {
        F.paint("data-rank", null);
        F.paint("data-g", (d) => d.nv);
        F.stack(F.nvOrder());
      });
    }

    const datum = (n) => n.__data__;
    // A highlighted group is raised in the mode's draw order: play order, or in the new
    // mode familiar, then new finds, then first listens (each in play order).
    const inDrawOrder = new Map();
    const drawOrder = (group) => {
      if (!isNew || !group) return group;
      if (!inDrawOrder.has(group)) inDrawOrder.set(group, group.slice().sort((a, b) => NV_LAYER[datum(a).nv] - NV_LAYER[datum(b).nv]));
      return inDrawOrder.get(group);
    };
    let byRank = new Map();

    function raiseRanked() {
      if (isNew) {
        for (const c of ["NW", "FL"]) { const g = byNv.get(c); if (g) d3.selectAll(g).raise(); }
        return;
      }
      for (let r = TOP_N; r >= 1; r--) {
        const group = byRank.get(r);
        if (group) d3.selectAll(group).raise();
      }
    }

    const monthDate = (k) => new Date(Date.UTC(year, k, 1));

    // Whether a play is in the subset of month k and hour h (either null for all).
    const inSubset = (d, k, h) => (k === null || monthOf(d) === k) && (h === null || hourOf(d) === h);
    // A dot's color rank in a rank mode: the year's, or the subset's for the subset's dots.
    function rankFor(d, k, h, t) {
      if (!inSubset(d, k, h)) return null;
      if (mode === "track" && t.whole) return d.rank <= TOP_N ? d.rank : null;
      const r = t.rankOf[mode].get(keyOf(d));
      return r && r.rank <= TOP_N ? r.rank : null;
    }

    const daysBetween = (a, b) => Math.round((b.stamp - a.stamp) / 1440);
    function noteFor(d) {
      if (isNew) {
        if (d.nv === "FL") return year === 2017 ? "First listen (2017 is burn-in)" : "First listen";
        const ago = daysBetween(d.first, d);
        const when = `first played ${formatDay(d.first.date)}`;
        return d.nv === "NW" ? `New find · ${when}, ${plural(ago, "day")} before` : `Familiar · ${when}`;
      }
      const k = pick.month;
      const h = pick.hour;
      const t = tableFor(k, h);
      const kind = mode === "artist" ? "Artist " : mode === "album" ? "Album " : "";
      if (t.whole) {
        if (mode === "track") return null;
        const r = t.rankOf[mode].get(keyOf(d));
        return r ? `${kind}#${r.rank} of ${year} · ${plural(r.n, "play")}` : null;
      }
      if (!inSubset(d, k, h)) return null;
      const r = t.rankOf[mode].get(keyOf(d));
      // "#3 in January at 2–3 PM", "#3 in January", "#3 at 2–3 PM"
      const where = [k === null ? "" : `in ${formatMonthName(monthDate(k))}`, h === null ? "" : `at ${formatHourRange(h)}`]
        .filter(Boolean).join(" ");
      return r ? `${kind}#${r.rank} ${where} · ${plural(r.n, "play")}` : null;
    }

    // Picks and selections. `pinned` is the pick: in the artist mode an artist and in the
    // album mode an album (from a first-column row or a dot), in the track and new modes a
    // track (a row or a dot), as before. The artist and album modes (PICKS) also have
    // `sel`, a song or album selected from the other columns: it highlights its plays on
    // top of the pick and keeps the pick's filter, as in the genre view. A new pick clears
    // it. A click on empty space clears both (onEmptyClick); Esc clears the selection, then
    // the pick, then the hour, then the month. A tab switch starts a new skin, so it clears
    // both too (the month and the hour stay).
    const PICKS = mode === "artist" || mode === "album";
    let active = null; // the foci highlighted now: [hovered track], or [pick, selection]
    let activeNode = null;
    let pinned = null;
    let sel = null;
    let pendingClear = null;

    function groupsOf(f) {
      const out = [["same-artist", drawOrder(byArtist.get(f.artist))]];
      if (f.kind === "track") out.push(["same-track", drawOrder(byTrack.get(f.key))]);
      if (f.kind === "album") out.push(["same-album", drawOrder(byAlbum.get(f.key))]);
      return out;
    }
    // Raised artist first, then album, then track, so the narrowest highlight is on top
    // (the CSS colors a dot with two classes by the later rule, in the same order).
    const RAISE = ["same-artist", "same-album", "same-track"];
    function highlight(fs) {
      svg.classed("hovering", true);
      const groups = fs.flatMap(groupsOf).filter(([, group]) => group)
        .sort((a, b) => RAISE.indexOf(a[0]) - RAISE.indexOf(b[0]));
      for (const [cls, group] of groups) d3.selectAll(group).classed(cls, true).raise();
    }
    // Ends the highlight; restack is false only at teardown, when the next skin restacks.
    function clear(restack = true) {
      if (!active) return;
      svg.classed("hovering", false);
      for (const f of active) for (const [cls, group] of groupsOf(f)) if (group) d3.selectAll(group).classed(cls, false);
      if (restack) raiseRanked();
      active = null;
      activeNode = null;
    }
    // Highlights the pick and the selection (either may be null).
    function showPicked() {
      if (pendingClear) { cancelAnimationFrame(pendingClear); pendingClear = null; }
      clear();
      const fs = [pinned, sel].filter(Boolean);
      if (fs.length) { active = fs; highlight(fs); }
      syncOverlay();
    }

    // The overlay on the time-of-day curve (curveOverlay) shows the narrowest highlight:
    // the selected song or album, else the pick, else the hovered dot's artist (artist
    // mode), album (album mode; none for a play with no album name) or track (track and
    // new modes), in the color of its dots' highlight, for the chosen month or the year.
    // With a pick, hovering other dots leaves it, as it leaves the dots' highlight.
    const OVERLAY_COLOR = { artist: "var(--same-artist)", album: "var(--same-album)", track: "var(--same-track)" };
    function syncOverlay() {
      if (!summary) return;
      const f = sel || pinned;
      const item = f ? focusItem(f) : active && activeNode ? playItem(datum(activeNode), isNew ? "track" : mode) : null;
      summary.overlay(item && overlaySpec(Y, item, pick.month, OVERLAY_COLOR[item.kind]));
    }
    // The curve's layers (CURVE_SPLIT): each play's layer is its artist's, album's or
    // track's color rank, 1 to TOP_N, in the subset of month k and hour h, as the dots are
    // colored (rankFor, without its subset test: the curve shows every hour of the month,
    // and the veil shows the hour); 0 for the rest.
    function layerFor(k, h) {
      const t = tableFor(k, h);
      if (mode === "track" && t.whole) return (d) => (d.rank <= TOP_N ? d.rank : 0);
      const ranks = t.rankOf[mode];
      return (d) => { const r = ranks.get(keyOf(d)); return r && r.rank <= TOP_N ? r.rank : 0; };
    }

    // The pin bar names the selection, else the pick; with neither it is hidden.
    function drawBar() {
      const f = sel || pinned;
      bar.selectChildren().remove();
      if (!f) { bar.style("display", "none"); return; }
      if (f.kind === "track") {
        bar.append("span").attr("class", "name").text(f.track);
        bar.append("span").text(` – ${f.artist} · ${plural(f.n, "play")} this year · `);
      } else if (f.kind === "album") {
        bar.append("span").attr("class", "name").text(f.album);
        bar.append("span").text(` – ${f.artist} · album · ${plural(f.n, "play")} this year · `);
      } else {
        bar.append("span").attr("class", "name").text(f.artist);
        bar.append("span").text(` · ${plural(f.n, "play")} this year · `);
      }
      bar.append("span").attr("class", "hint").text("click again or Esc to clear");
      bar.style("display", "block");
    }

    const syncLists = () => { lists.a.sync(); lists.b.sync(); lists.c.sync(); };
    function pin(f) {
      pinned = f;
      sel = null;
      showPicked();
      drawBar();
      afterPin();
    }
    function unpin() {
      pinned = null;
      sel = null;
      showPicked();
      tooltip.hide();
      drawBar();
      afterPin();
    }
    // In the artist and album modes the other columns and the stats line follow the pick,
    // so a pick redraws them.
    function afterPin() {
      if (PICKS) setList();
      else syncLists();
      setStats();
    }
    // The stats line counts the subset, narrowed by the artist or album pick (the artist
    // and album modes); a pinned track (the track and new modes) does not narrow it.
    function setStats() {
      const f = PICKS && pinned && pinned.kind === mode ? pinned : null;
      stats(!f ? null : f.kind === "artist"
        ? { key: `a:${f.artist}`, label: f.artist, test: (d) => d.artist === f.artist }
        : { key: `l:${f.key}`, label: `${f.album} – ${f.artist}`, test: (d) => !!d.album && albumKey(d) === f.key });
    }
    function select(f) {
      sel = f;
      showPicked();
      drawBar();
      syncLists();
    }
    function unselect() {
      sel = null;
      showPicked();
      drawBar();
      syncLists();
    }
    const toggle = (f) => (sameFocus(pinned, f) ? unpin() : pin(f));
    const isPinned = (f) => sameFocus(pinned, f);
    const toggleSel = (f) => (sameFocus(sel, f) ? unselect() : select(f));
    const isSel = (f) => sameFocus(sel, f);

    // The handlers replace the last skin's (d3 keeps one listener per type and name).
    layer
      .on("mouseover", (event) => {
        const node = event.target;
        if (node.tagName !== "circle") return;
        if (pinned || sel) { tooltip.show(datum(node), event, null, noteFor(datum(node))); return; }
        if (pendingClear) { cancelAnimationFrame(pendingClear); pendingClear = null; }
        if (node === activeNode) return;
        clear();
        activeNode = node;
        active = [trackFocus(datum(node))];
        highlight(active);
        syncOverlay();
        tooltip.show(datum(node), event, null, noteFor(datum(node)));
      })
      .on("mousemove", (event) => { if (active) tooltip.move(event); })
      .on("mouseout", (event) => {
        if (event.target.tagName !== "circle") return;
        if (pinned || sel) { tooltip.hide(); return; }
        pendingClear = requestAnimationFrame(() => {
          pendingClear = null;
          clear();
          syncOverlay();
          tooltip.hide();
        });
      })
      // A dot click picks its artist (artist mode) or its album (album mode; a play with
      // no album name does nothing), or pins its track (track and new modes).
      .on("click", (event) => {
        const node = event.target;
        if (node.tagName !== "circle") return;
        event.stopPropagation();
        const d = datum(node);
        const f = mode === "artist" ? artistFocus(d.artist)
          : mode === "album" ? (d.album ? albumFocus(d) : null) : trackFocus(d);
        if (!f) return;
        if (sameFocus(pinned, f)) unpin();
        else { pin(f); tooltip.show(d, event, null, noteFor(d)); }
      });
    onEmptyClick(() => { if (pinned || sel) unpin(); }); // the chart background and the rest of the page
    d3.select(window).on("keydown.pin", (event) => {
      if (event.key !== "Escape") return;
      if (sel) unselect();
      else if (pinned) unpin();
      else if (!picker.clearHour()) picker.clear();
    });

    // The header legend's shares (new mode): the year's, or the subset's. The first line
    // names the year ("Each dot is one stream in 2025."); the stats line names the subset.
    function setShares(k, h) {
      const { n, counts } = R.sharesFor(k, h);
      for (const code of NV_ORDER) d3.select(`#nv-share-${code}`).text(formatShare((counts.get(code) || 0) / n));
      d3.select("#nv-when").text(` in ${year}`);
    }

    function setList() {
      const k = pick.month;
      const h = pick.hour;
      const t = tableFor(k, h);
      const label = subsetLabel(year, k, h);
      const when = label ? ` (${label})` : "";
      const empty = emptyFor(k, h);
      const withSw = (rows) => rows.map((r) => Object.assign({ sw: `var(--r${r.rank})` }, r));
      const base = { empty, isPinned, toggle };
      const noAlbums = "No album names in this data.";
      if (PICKS) {
        // First column: the top ten artists or albums (the year's dense ranks, or the
        // subset's positions). The others: up to ten songs (and, in the artist mode, albums)
        // among the plays of the year or subset, of the picked artist or album if there is
        // one, by plays, ties by title then artist (positions, as the genre view's lists).
        // A swatch is the color of the row's artist (album) when that is in the top ten.
        const isArtist = mode === "artist";
        lists.a.set(Object.assign({}, base, isArtist
          ? { title: `Top ten artists${when}`, rows: withSw(t.artists) }
          : { title: `Top ten albums${when}`, rows: withSw(t.albums), empty: R.nAlbums ? empty : noAlbums }));
        const f = pinned && pinned.kind === mode ? pinned : null;
        const pool = !f ? t.rows : t.rows.filter(isArtist ? (d) => d.artist === f.artist : (d) => d.album && albumKey(d) === f.key);
        const swOf = (d) => {
          const r = isArtist ? t.rankOf.artist.get(d.artist) : d.album ? t.rankOf.album.get(albumKey(d)) : null;
          return r && r.rank <= TOP_N ? `var(--r${r.rank})` : null;
        };
        const named = f ? `: ${isArtist ? f.artist : f.album}` : "";
        const pickKey = !f ? "" : isArtist ? f.artist : f.key;
        const column = (rows, keyOf, order, focusOf, nameOf, title, emptyText) => {
          const ranked = R.column(`${subKey(k, h)}\u0000${pickKey}\u0000${focusOf === trackFocus ? "t" : "l"}`,
            () => rankCounts(rows, keyOf, "position", order));
          const row = (r) => ({ rank: r.rank, title: nameOf(r.first), sub: r.first.artist, n: r.n, sw: swOf(r.first), focus: focusOf(r.first) });
          const top = ranked.slice(0, TOP_N).map(row);
          // The selection, when a month change drops it from this column, as in the genre view.
          let kept = null;
          const kind = focusOf === trackFocus ? "track" : "album";
          if (sel && sel.kind === kind && !top.some((r) => isSel(r.focus))) {
            const r = ranked.find((x) => x.key === sel.key);
            kept = r ? row(r) : { rank: null, title: kind === "track" ? sel.track : sel.album, sub: sel.artist, n: 0, sw: null, focus: sel };
          }
          return { title, rows: top, kept, empty: emptyText, isPinned: isSel, toggle: toggleSel };
        };
        lists.b.set(column(pool, trackKey, byTitle, trackFocus, (d) => d.track, `Top songs${when}${named}`, empty));
        if (isArtist) {
          lists.c.set(column(pool.filter((d) => d.album), albumKey, byAlbumName, albumFocus, (d) => d.album,
            `Top albums${when}${named}`, R.nAlbums ? empty : noAlbums));
        } else lists.c.hide();
      } else if (mode === "track") {
        lists.a.set(Object.assign({ title: `Top ten tracks${when}`, rows: withSw(t.tracks), cols: true }, base));
        lists.b.hide();
        lists.c.hide();
      } else {
        const burnNote = year === 2017 ? " 2017 is a burn-in year: the records start in 2017, so tracks first heard in 2015 or 2016 count as new here." : "";
        // "Biggest new finds of 2025", "Biggest new finds of 2025, 2–3 PM",
        // "New finds, January 2025", "New finds, January 2025, 2–3 PM"
        lists.a.set(Object.assign({
          title: k === null ? `Biggest new finds of ${year}${h === null ? "" : `, ${formatHourRange(h)}`}` : `New finds, ${label}`,
          note: NEW_FINDS_NOTE + burnNote,
          rows: R.newFinds(k, h), cols: true,
        }, base));
        lists.b.hide();
        lists.c.hide();
        setShares(k, h);
      }
    }

    // The rank modes color the dots by the year's or the subset's ranks and restack them.
    function recolor() {
      if (!isNew) {
        const k = pick.month;
        const h = pick.hour;
        const t = tableFor(k, h);
        F.recolor("rank", () => {
          F.paint("data-g", null);
          F.paint("data-rank", (d) => rankFor(d, k, h, t));
          byRank = F.stackRanks();
        });
      }
      if (active) highlight(active);
    }

    // A new month or hour: the dots, the lists, the summary (the curve follows the month,
    // the per-day bars the month and hour; with CURVE_SPLIT the rank modes' layers follow
    // the dots' colors, so the month and the hour) and its overlay, and the stats line.
    function showSubset() {
      const k = pick.month;
      const h = pick.hour;
      tooltip.hide();
      recolor();
      setList();
      if (summary) {
        const split = CURVE_SPLIT && !isNew;
        summary.show(split ? `${mode}|${subKey(k, h)}` : k, k === null ? data : byMonth.get(k) || [], subKey(k, h),
          subsetRows(Y, k, h), split ? layerFor(k, h) : null);
      }
      syncOverlay();
      setStats();
    }

    showSubset();
    picker.listen(showSubset);
    // PREPICK_TOP (off): the artist mode opens on its top artist.
    if (PREPICK_TOP && mode === "artist") {
      const top = tableFor(pick.month, pick.hour).artists[0];
      if (top) pin(top.focus);
    }

    return {
      teardown() {
        if (pendingClear) { cancelAnimationFrame(pendingClear); pendingClear = null; }
        clear(false);
        svg.classed("hovering", false);
        if (summary) summary.overlay(null);
      },
    };
  }

  // The genre view's lookups for a year, built on first use: each play's genre (d.g, from
  // its artist), the plays of each genre, artist, song and album, the year's songs and
  // albums ranked (a subset's when first asked for), the genre list's order and its rows.
  function genreLookups(Y, genres) {
    if (Y.genre && Y.genre.genres === genres) return Y.genre;
    const { year, data } = Y;
    const labelOf = (c) => genres.names.get(c) || c;
    for (const d of data) d.g = genres.artist.get(d.artist) || OTHER;

    const total = data.length;
    const byGenre = d3.group(data, (d) => d.g);
    const byArtist = d3.group(data, (d) => d.artist);
    const bySong = d3.group(data, trackKey);
    const nOf = (c) => (byGenre.get(c) || []).length;
    // Songs by plays, then track title, then artist. A genre comes from the artist, so all
    // of a song's plays share its genre and artist: a song's plays within a pinned genre or
    // artist are its plays in the year (or the subset).
    const rankSongs = (groups) => Array.from(groups, ([key, group]) => (
      { key, track: group[0].track, artist: group[0].artist, g: group[0].g, n: group.length }))
      .sort((a, b) => b.n - a.n || d3.ascending(a.track, b.track) || d3.ascending(a.artist, b.artist));
    const songs = rankSongs(bySong); // the year's
    const songByKey = new Map(songs.map((s) => [s.key, s]));
    // Each subset's songs (month k, hour h), ranked when first asked for.
    const byMonth = byMonthOf(Y);
    const subsetSongs = new Map();
    function songsIn(k, h = null) {
      if (k === null && h === null) return songs;
      const key = subKey(k, h);
      if (!subsetSongs.has(key)) subsetSongs.set(key, rankSongs(d3.group(subsetRows(Y, k, h), trackKey)));
      return subsetSongs.get(key);
    }
    // Albums the same way: by plays, then album, then artist. An album is its name plus
    // the artist, so its plays too share one genre and one artist.
    const byAlbum = d3.group(data.filter((d) => d.album), albumKey);
    const rankAlbums = (groups) => Array.from(groups, ([key, group]) => (
      { key, album: group[0].album, artist: group[0].artist, g: group[0].g, n: group.length }))
      .sort((a, b) => b.n - a.n || d3.ascending(a.album, b.album) || d3.ascending(a.artist, b.artist));
    const albums = rankAlbums(byAlbum); // the year's
    const albumByKey = new Map(albums.map((s) => [s.key, s]));
    const subsetAlbums = new Map();
    function albumsIn(k, h = null) {
      if (k === null && h === null) return albums;
      const key = subKey(k, h);
      if (!subsetAlbums.has(key)) subsetAlbums.set(key, rankAlbums(d3.group(subsetRows(Y, k, h).filter((d) => d.album), albumKey)));
      return subsetAlbums.get(key);
    }
    // List order: genres by this year's plays, Other last. The margins stack in this order.
    const order = GENRE_CODES.filter((c) => c !== OTHER)
      .sort((a, b) => nOf(b) - nOf(a) || GENRE_CODES.indexOf(a) - GENRE_CODES.indexOf(b))
      .concat(OTHER);
    // The genre list: plays, share of the year, and the top three artists by plays.
    const rows = order.map((c) => {
      const group = byGenre.get(c) || [];
      const top = d3.rollups(group, (v) => v.length, (d) => d.artist)
        .sort((a, b) => b[1] - a[1] || d3.ascending(a[0], b[0]))
        .slice(0, 3)
        .map((p) => p[0]);
      const n = group.length;
      return { code: c, label: labelOf(c), n, share: total ? n / total : 0, top, focus: { kind: "genre", g: c, n } };
    });

    Y.genre = { genres, labelOf, total, byGenre, byArtist, bySong, byMonth, songByKey, songsIn, byAlbum, albumByKey, albumsIn, order, rows };
    return Y.genre;
  }

  // The genre view, as a skin on the year's frame (buildFrame): every dot in its artist's
  // genre colour, drawn in play order (no genre on top). Hover outlines the same artist's
  // dots; clicking a dot pins that artist.
  // Clicking a row of the genre list pins that genre, and every other dot turns grey.
  // Clicking the same thing again, clicking empty space on the page, or Esc clears a pin.
  // The top-songs and top-albums lists follow the pin (the year, the genre or the
  // artist). Hovering a song or album row outlines its dots; clicking it selects it, on
  // top of the pin, until it is clicked again, the other list's row is selected (one song
  // or album at a time), the pin changes or clears, or Esc (which clears the song or album
  // before the pin, then the hour, and the month last). While a song or album is hovered or
  // selected, or an artist is pinned by a click, every other dot fades (.solo in the CSS).
  // A chosen month (pick.month) and hour (pick.hour) filter both lists, the per-day bars
  // and the stats line to the plays in that subset, and the month the time-of-day curve;
  // the pin and the selection stay when the subset changes. The dots, the genre list and
  // the pin bar keep the whole year.
  // A tab switch starts a new skin, which clears the pin and the selection.
  // Returns { teardown }, which the next skin calls first: it removes the copies, the
  // fades and the summary's pin colors, and a pending clear.
  function skinGenre(F, Y, genres, { els, tooltip, genreList, songList, albumList, pick, stats }) {
    const { svg, layer, nodes, picker, year, data, lift, ring, x, y } = F;
    const L = genreLookups(Y, genres);
    const { labelOf, total, byGenre, byArtist, bySong, byMonth, songByKey, songsIn, byAlbum, albumByKey, albumsIn } = L;
    const bar = d3.select(els.pinbar);
    tooltip.hide();
    bar.style("display", "none");
    svg.classed("genre", true)
      .attr("aria-label", `Every Spotify play in ${year}, by time of day (5 AM to 5 AM) and day, colored by the artist's genre`);
    F.setCurveH(SHARE_CURVE_H);
    const summary = SUMMARIES.genre(F, Y, genres);
    const gsum = d3.selectAll(summary ? [summary.node] : []);
    F.recolor("genre", () => {
      F.paint("data-rank", null);
      F.paint("data-g", (d) => d.g);
      F.stack(nodes); // play order
    });
    const genreOf = (d) => ({ code: d.g, label: labelOf(d.g) });

    // The stacked time-of-day chart, for the chosen month, or the year when there is none,
    // and the per-day bars for the subset (month and hour). Redrawn bars take the pinned
    // genre's colors again (paintSummaryPin).
    function drawSummary() {
      if (!summary) return;
      const k = pick.month;
      const h = pick.hour;
      summary.show(k, k === null ? data : byMonth.get(k) || [], subKey(k, h), subsetRows(Y, k, h));
      if (pinned && pinned.kind === "genre") paintSummaryPin();
    }
    // A pinned genre's bands and bars keep their color and the others turn grey.
    function paintSummaryPin() {
      const g = pinned && pinned.kind === "genre" ? pinned.g : null;
      gsum.selectAll("[data-g]")
        .classed("off", function () { return g !== null && this.getAttribute("data-g") !== g; })
        .classed("on", function () { return g !== null && this.getAttribute("data-g") === g; });
    }

    // Copies drawn over the dots, which the mouse ignores: the pinned genre's dots (.lift)
    // and the hovered or pinned artist's dots, outlined (.ring). Copies leave the play
    // order of the dots themselves untouched. The month veil sits between the two, so an
    // outlined song or artist keeps full strength in every month.
    const copies = (g, rows) => g.selectAll("circle")
      .data(rows)
      .join("circle")
      .attr("cx", (d) => x(dayMinute(d.minutes)))
      .attr("cy", (d) => y(d.day))
      .attr("r", RADIUS)
      .attr("data-g", (d) => d.g);

    const datum = (n) => n.__data__;
    let ringed = null; // what is outlined now: "a:<artist>", "s:<song key>", "l:<album key>" or null
    let pinned = null; // { kind: "artist", artist, g, n } or { kind: "genre", g, n }
    let song = null; // the selected song (a row of `songs`), or null
    let album = null; // the selected album (a row of `albums`), or null; never with a song
    let hoverSong = null; // the song row under the mouse, or null
    let hoverAlbum = null; // the album row under the mouse, or null
    let hoverArtist = null; // the artist of the dot under the mouse, while nothing is pinned
    let pendingClear = null;
    const samePin = (a, b) => !!a && !!b && a.kind === b.kind && a.g === b.g && a.artist === b.artist;
    const artistFocus = (d) => ({ kind: "artist", artist: d.artist, g: d.g, n: byArtist.get(d.artist).length });

    function outline(id, rows) {
      if (ringed === id) return;
      ringed = id;
      copies(ring, rows || []);
    }

    // The outline shows, first to last: the hovered song or album row, the selected song
    // or album, the pinned artist, the hovered dot's artist (only while nothing is pinned
    // or selected). All but the last also fade every other dot (.solo); a plain dot hover
    // doesn't, since moving across dots would make the whole chart flicker.
    // The overlay on the time-of-day curve (curveOverlay) follows the outline: the same
    // song, album or artist, in the outline's ink, for the chosen month or the year. A
    // pinned genre has none (its band in the stack shows it).
    function ringNow() {
      const s = hoverSong || hoverAlbum || song || album;
      svg.classed("solo", !!s || (!!pinned && pinned.kind === "artist"));
      let item = null;
      if (s && s.album === undefined) {
        outline(`s:${s.key}`, bySong.get(s.key));
        item = { kind: "track", key: s.key, name: s.track };
      } else if (s) {
        outline(`l:${s.key}`, byAlbum.get(s.key));
        item = { kind: "album", key: s.key, name: s.album };
      } else if (pinned && pinned.kind === "artist") {
        outline(`a:${pinned.artist}`, byArtist.get(pinned.artist));
        item = { kind: "artist", key: pinned.artist, name: pinned.artist };
      } else if (!pinned && hoverArtist !== null) {
        outline(`a:${hoverArtist}`, byArtist.get(hoverArtist));
        item = { kind: "artist", key: hoverArtist, name: hoverArtist };
      } else outline(null, []);
      if (summary) summary.overlay(item && overlaySpec(Y, item, pick.month, "var(--ink)"));
    }

    // The pin bar names the selected song or album, else the pin; with none it is hidden.
    function drawBar() {
      bar.selectChildren().remove();
      if (song) {
        bar.append("span").attr("class", "name").text(song.track);
        bar.append("span").text(` – ${song.artist} · ${plural(song.n, "play")} this year · `);
      } else if (album) {
        bar.append("span").attr("class", "name").text(album.album);
        bar.append("span").text(` – ${album.artist} · album · ${plural(album.n, "play")} this year · `);
      } else if (pinned && pinned.kind === "genre") {
        bar.append("span").attr("class", "name").text(labelOf(pinned.g));
        bar.append("span").text(` · ${plural(pinned.n, "play")} this year (${formatShare(total ? pinned.n / total : 0)}) · `);
      } else if (pinned) {
        bar.append("span").attr("class", "name").text(pinned.artist);
        bar.append("span").text(` · ${labelOf(pinned.g)} · ${plural(pinned.n, "play")} this year · `);
      } else {
        bar.style("display", "none");
        return;
      }
      bar.append("span").attr("class", "hint").text("click again or Esc to clear");
      bar.style("display", "block");
    }

    // The top songs or top albums in what is pinned now (the year, the pinned genre or
    // the pinned artist), within the chosen month and hour if any. A selected song or
    // album that is not in its list goes in `kept`, with its place and plays in the same
    // pool. setSongs() redraws both lists.
    function setList(isSong) {
      if (isSong) hoverSong = null; // the rows are redrawn, so the hovered one is gone
      else hoverAlbum = null;
      const sel = isSong ? song : album;
      const f = pinned;
      const k = pick.month;
      const h = pick.hour;
      const all = isSong ? songsIn(k, h) : albumsIn(k, h);
      const pool = !f ? all
        : all.filter(f.kind === "genre" ? (s) => s.g === f.g : (s) => s.artist === f.artist);
      const rows = pool.slice(0, TOP_N).map((s, i) => Object.assign({ pos: i + 1 }, s));
      let kept = null;
      if (sel && !rows.some((s) => s.key === sel.key)) {
        const i = pool.findIndex((s) => s.key === sel.key);
        kept = Object.assign({}, sel, { pos: i < 0 ? null : i + 1, n: i < 0 ? 0 : pool[i].n });
      }
      const label = subsetLabel(year, k, h);
      const when = label ? ` (${label})` : ""; // "Top songs (January 2025, 2–3 PM): Folk & singer-songwriter"
      (isSong ? songList : albumList).set({
        title: `${isSong ? "Top songs" : "Top albums"}${when}${f ? `: ${f.kind === "genre" ? labelOf(f.g) : f.artist}` : ""}`,
        empty: !isSong && !byAlbum.size ? "No album names in this data." : emptyFor(k, h),
        rows,
        kept,
        isSelected: isSong ? (s) => !!song && song.key === s.key : (s) => !!album && album.key === s.key,
        toggle: isSong ? toggleSong : toggleAlbum,
        hover: isSong ? (s) => { hoverSong = s; ringNow(); } : (s) => { hoverAlbum = s; ringNow(); },
      });
    }
    function setSongs() {
      setList(true);
      setList(false);
    }
    // The stats line counts the subset, narrowed by the pinned genre or artist (what the
    // two lists follow); a selected song or album and a hovered artist do not narrow it.
    function setStats() {
      const f = pinned;
      stats(!f ? null : f.kind === "genre"
        ? { key: `g:${f.g}`, label: labelOf(f.g), test: (d) => d.g === f.g }
        : { key: `a:${f.artist}`, label: f.artist, test: (d) => d.artist === f.artist });
    }

    // `song` and `album` are always the year's row (the pin bar gives the plays this year),
    // whichever list row they were picked from. Selecting one clears the other.
    function select(isSong, s) {
      song = isSong ? songByKey.get(s.key) || s : null;
      album = isSong ? null : albumByKey.get(s.key) || s;
      ringNow();
      drawBar();
      songList.sync();
      albumList.sync();
    }

    function clearSong() {
      song = null;
      album = null;
      ringNow();
      drawBar();
      songList.sync();
      albumList.sync();
    }

    const toggleSong = (s) => (song && song.key === s.key ? clearSong() : select(true, s));
    const toggleAlbum = (s) => (album && album.key === s.key ? clearSong() : select(false, s));

    // Setting or clearing a pin clears the selected song or album and redraws both lists.
    function pin(f) {
      if (pendingClear) { cancelAnimationFrame(pendingClear); pendingClear = null; }
      pinned = f;
      song = null;
      album = null;
      hoverArtist = null;
      const genrePin = f.kind === "genre";
      svg.classed("dim", genrePin);
      copies(lift, genrePin ? byGenre.get(f.g) || [] : []);
      paintSummaryPin();
      setSongs();
      ringNow();
      drawBar();
      genreList.sync();
      setStats();
    }

    function unpin() {
      pinned = null;
      song = null;
      album = null;
      hoverArtist = null;
      svg.classed("dim", false);
      copies(lift, []);
      paintSummaryPin();
      setSongs();
      ringNow();
      tooltip.hide();
      drawBar();
      genreList.sync();
      setStats();
    }

    const toggle = (f) => (samePin(pinned, f) ? unpin() : pin(f));

    // While something is pinned or a song is selected, hovering dots shows only their
    // tooltip. Clearing the outline waits a frame, so moving between two dots of one
    // artist doesn't flicker. The handlers replace the last skin's.
    layer
      .on("mouseover", (event) => {
        const node = event.target;
        if (node.tagName !== "circle") return;
        const d = datum(node);
        if (!pinned && !song && !album) {
          if (pendingClear) { cancelAnimationFrame(pendingClear); pendingClear = null; }
          hoverArtist = d.artist;
          ringNow();
        }
        tooltip.show(d, event, genreOf(d));
      })
      .on("mousemove", (event) => { if (event.target.tagName === "circle") tooltip.move(event); })
      .on("mouseout", (event) => {
        if (event.target.tagName !== "circle") return;
        if (pinned || song || album) { tooltip.hide(); return; }
        pendingClear = requestAnimationFrame(() => {
          pendingClear = null;
          hoverArtist = null;
          ringNow();
          tooltip.hide();
        });
      })
      .on("click", (event) => {
        const node = event.target;
        if (node.tagName !== "circle") return;
        event.stopPropagation();
        const d = datum(node);
        const f = artistFocus(d);
        if (samePin(pinned, f)) unpin();
        else { pin(f); tooltip.show(d, event, genreOf(d)); }
      });
    onEmptyClick(() => { if (pinned || song || album) unpin(); }); // the chart background and the rest of the page
    d3.select(window).on("keydown.pin", (event) => {
      if (event.key !== "Escape") return;
      if (song || album) clearSong();
      else if (pinned) unpin();
      else if (!picker.clearHour()) picker.clear();
    });

    genreList.set({ rows: L.rows, isPinned: (f) => samePin(pinned, f), toggle });
    setSongs();
    drawSummary();
    setStats();
    // Choosing, switching or clearing a month or hour redraws the two lists, the summary
    // and the stats line only.
    picker.listen(() => { setSongs(); ringNow(); drawSummary(); setStats(); });

    return {
      teardown() {
        if (pendingClear) { cancelAnimationFrame(pendingClear); pendingClear = null; }
        svg.classed("dim", false).classed("solo", false);
        copies(lift, []);
        copies(ring, []);
        gsum.selectAll("[data-g]").classed("off", false).classed("on", false);
        if (summary) summary.overlay(null);
      },
    };
  }

  // The "where I was" strip: a 6 px column between the month labels and the plot. Away
  // days are filled; runs of 2+ days get the city name in the margin (desktop only).
  // Hovering a run or its label shows the place and the dates.
  function drawStrip(svg, runs, named, { y, rowH, stripX, labelX, tooltip }) {
    const strip = svg.append("g").attr("class", "where");
    const rowTop = (d) => y(d) - rowH / 2;
    const hover = (sel) => sel
      .on("mouseover", (event, r) => tooltip.showLines(
        [r.label || "Away", formatRange(r.days[0], r.days[r.days.length - 1])], event))
      .on("mousemove", (event) => tooltip.move(event))
      .on("mouseout", () => tooltip.hide());

    const groups = strip.selectAll("g").data(runs).join("g").call(hover);
    // Half a pixel off each end, so two runs on neighbouring days stay apart.
    groups.selectAll("rect.away")
      .data((r) => segments(r.days))
      .join("rect")
      .attr("class", "away")
      .attr("x", stripX)
      .attr("width", STRIP_W)
      .attr("y", (s) => rowTop(s.start) + 0.5)
      .attr("height", (s) => Math.max(1, y(s.end) - y(s.start) + rowH - 1));
    // An invisible, wider target over the whole run, so the thin strip is easy to hover.
    groups.append("rect")
      .attr("class", "hit")
      .attr("x", stripX - 3)
      .attr("width", STRIP_W + 6)
      .attr("y", (r) => rowTop(r.days[0]))
      .attr("height", (r) => y(r.days[r.days.length - 1]) - y(r.days[0]) + rowH);

    // Labels sit beside each run's first row; one that would overlap the label above is dropped.
    const placed = [];
    let lastY = -Infinity;
    for (const r of named) {
      const ly = y(r.days[0]);
      if (ly - lastY < PLACE_PX + 2) continue;
      placed.push(r);
      lastY = ly;
    }
    strip.selectAll("text")
      .data(placed)
      .join("text")
      .attr("x", labelX)
      .attr("y", (r) => y(r.days[0]))
      .attr("dy", "0.32em")
      .attr("text-anchor", "end")
      .text((r) => r.short)
      .call(hover);
  }

  // The time-of-day curves count plays in 10-minute bins from 5 AM (BINS of them; binOf
  // gives a play's) and smooth the counts with a kernel that wraps round the day:
  // SUM_KERNEL in the artist, album and track modes, SHARE_KERNEL in the genre and new
  // modes. A summary's curve, its layers and its overlay use the same bins and kernel.
  // curvePoints gives [minute, value] at each bin's middle, plus both ends of the day at
  // the mean of the first and last bins.
  const BINS = 144;
  const binOf = (d) => Math.floor(dayMinute(d.minutes) / 10);
  function binCounts(rows) {
    const bins = new Array(BINS).fill(0);
    for (const d of rows) bins[binOf(d)] += 1;
    return bins;
  }
  function smoothBins(bins, kernel) {
    const half = (kernel.length - 1) / 2;
    const kSum = d3.sum(kernel);
    return bins.map((_, i) => d3.sum(kernel, (k, j) => k * bins[(i + j - half + BINS) % BINS]) / kSum);
  }
  function curvePoints(smooth) {
    const edge = (smooth[0] + smooth[BINS - 1]) / 2;
    return [[0, edge], ...smooth.map((v, i) => [i * 10 + 5, v]), [1440, edge]];
  }

  // The width of curve-label text, in the page's font (looked up once).
  let labelFamily = null;
  function labelWidth(text, weight = "") {
    if (labelFamily === null) labelFamily = getComputedStyle(document.body).fontFamily;
    return textWidth(text, LABEL_PX, { weight, family: labelFamily });
  }
  // `text` cut at the end, with "…", to fit `room` px in weight 600 (at least one letter).
  function clipName(text, room) {
    if (labelWidth(text, 600) <= room) return text;
    let lo = 1;
    let hi = text.length - 1;
    while (lo < hi) {
      const mid = (lo + hi + 1) >> 1;
      if (labelWidth(`${text.slice(0, mid).trimEnd()}…`, 600) <= room) lo = mid; else hi = mid - 1;
    }
    return `${text.slice(0, lo).trimEnd()}…`;
  }

  // An overlay on a summary's time-of-day curve: one artist's, album's or track's own
  // curve (spec: { id, name, color, bins }, from overlaySpec; null clears it), smoothed
  // with the summary's kernel and scaled so its peak is as high as the curve's peak, a
  // comparison of shape and not of counts. It is a 2 px line in the item's highlight
  // color on a halo in the page color, so it reads over the stacked layers, and it sits
  // under the month and hour veil, as the curve does. While it shows, the curve's label
  // names the item: "plays by time of day · Big Thief" (no "scaled" note, cut 2026-11-19 at
  // ZH's request). An item with no plays in the month draws no line:
  // "... · Big Thief, no plays this month".
  function curveOverlay(g, label, { x, base, curveH, kernel }) {
    const over = g.append("g").attr("class", "overlay").attr("pointer-events", "none").style("display", "none");
    const halo = over.append("path").attr("class", "halo");
    const line = over.append("path").attr("class", "line");
    const room = x(1440) - x(0) - 2; // the label starts at x(0) + 2
    const head = `${CURVE_LABEL} · `;
    let shown = null;
    return function overlay(spec) {
      const id = spec ? spec.id : null;
      if (id === shown) return;
      shown = id;
      if (!spec) {
        over.style("display", "none").datum(null);
        label.text(CURVE_LABEL);
        return;
      }
      const smooth = smoothBins(spec.bins, kernel);
      const peak = d3.max(smooth);
      if (peak > 0) {
        const yo = d3.scaleLinear().domain([0, peak]).range([base, base - curveH]);
        const path = d3.line().x((p) => x(p[0])).y((p) => yo(p[1])).curve(d3.curveMonotoneX)(curvePoints(smooth));
        halo.attr("d", path);
        line.attr("d", path).style("stroke", spec.color);
      }
      over.style("display", peak > 0 ? null : "none").datum(spec);
      // The full name with the long note, else with the short one, else cut to fit.
      const notes = peak > 0 ? [""] : [", no plays this month", ", none this month"];
      const left = (note) => room - labelWidth(head) - labelWidth(note);
      const note = notes.find((n) => labelWidth(spec.name, 600) <= left(n)) || notes[notes.length - 1];
      label.text(head);
      label.append("tspan").attr("class", "name").text(clipName(spec.name, left(note)));
      label.append("tspan").text(note);
    };
  }

  // Margin summaries (on by default): plays by time of day above the plot, on the
  // chart's x scale, and plays per listening day to its right, on the chart's y scale.
  // Returns { curve(rows, layerOf), bars(rows), overlay(spec) }: curve and bars redraw the
  // time-of-day curve (from one month's plays) and the per-day bars (from a subset's
  // plays, see subsetRows) in place, each scaled to its own peak, and overlay draws an
  // item's curve over it (curveOverlay). Each bar's data is [day (ms), plays].
  function drawSummaries(svg, data, { x, y, rowH, base, bars, barX, barW, labelX, labelY }) {
    const g = svg.append("g").attr("class", "summary");
    const layerG = g.append("g").attr("class", "layers");
    const area = g.append("path").attr("class", "area");
    const label = g.append("text")
      .attr("class", "clabel")
      .attr("x", x(0) + 2)
      .attr("y", base - SUM_CURVE_H - 3) // above the curve, so the curve never crosses it
      .text(CURVE_LABEL);
    const overlay = curveOverlay(g, label, { x, base, curveH: SUM_CURVE_H, kernel: SUM_KERNEL });

    // 10-minute bins from 5 AM, smoothed with SUM_KERNEL (1-2-3-2-1). With layerOf
    // (CURVE_SPLIT), the curve is also stacked by layerOf(row): ranks 1 to TOP_N bottom up
    // in the rank colors, then the rest (0) in the summary grey. The stack's top is the
    // total, which keeps its outline (.area.split); without layerOf the total is filled,
    // as before. Each layer's data is a d3 stack series plus `bins`, its plays per bin.
    const stackArea = (yc, pts) => d3.area().x((p) => x(pts[p.data][0])).y0((p) => yc(p[0])).y1((p) => yc(p[1]))
      .curve(d3.curveMonotoneX);
    function curve(rows, layerOf = null) {
      const smooth = smoothBins(binCounts(rows), SUM_KERNEL);
      const pts = curvePoints(smooth);
      const yc = d3.scaleLinear().domain([0, d3.max(smooth) || 1]).range([base, base - SUM_CURVE_H]);
      area.classed("split", !!layerOf)
        .attr("d", d3.area().x((p) => x(p[0])).y0(base).y1((p) => yc(p[1])).curve(d3.curveMonotoneX)(pts));
      let series = [];
      if (layerOf) {
        const counts = d3.range(TOP_N + 1).map(() => new Array(BINS).fill(0)); // the rest, rank 1, ..., rank TOP_N
        for (const d of rows) counts[layerOf(d)][binOf(d)] += 1;
        const layerPts = counts.map((b) => curvePoints(smoothBins(b, SUM_KERNEL)));
        series = d3.stack().keys(d3.range(1, TOP_N + 1).concat(0)).value((i, key) => layerPts[key][i][1])(d3.range(pts.length));
        for (const s of series) s.bins = counts[s.key];
      }
      layerG.selectAll("path")
        .data(series, (s) => s.key)
        .join("path")
        .attr("class", (s) => (s.key ? "layer" : "layer rest"))
        .attr("data-rank", (s) => s.key || null)
        .style("fill", (s) => (s.key ? `var(--r${s.key})` : null))
        .attr("d", stackArea(yc, pts));
    }
    curve(data);

    if (!bars) return { curve, bars() {}, overlay };

    const barG = g.append("g").attr("class", "bars");
    const barH = Math.max(1, rowH * 0.7);
    function drawBars(rows) {
      const perDay = d3.rollups(rows, (v) => v.length, (d) => +d.day);
      const bw = d3.scaleLinear().domain([0, d3.max(perDay, (p) => p[1]) || 1]).range([0, barW]);
      barG.selectAll("rect")
        .data(perDay)
        .join("rect")
        .attr("x", barX)
        .attr("y", (p) => y(p[0]) - barH / 2)
        .attr("width", (p) => bw(p[1]))
        .attr("height", barH);
    }
    drawBars(data);
    g.append("text")
      .attr("x", labelX)
      .attr("y", labelY)
      .attr("text-anchor", "end")
      .text("plays per day");
    return { curve, bars: drawBars, overlay };
  }

  // Genre view margins. Above the plot: plays by time of day, stacked by genre in
  // STACK_ORDER (Other on top), so the height of the stack at each time is the number of
  // plays then, as in the rank modes' curve. The new mode stacks its novelty classes the
  // same way (keys, codeOf). Right of the plot: plays per listening day, stacked by genre
  // in list order (the year's biggest genre at the left of each bar, Other last). Returns
  // { curve(rows), bars(rows), overlay(spec) }, as drawSummaries. Each bar segment's data is { day (ms),
  // c (code), from, to } (plays, stacked).
  function drawGenreSummaries(svg, data, order, { x, y, rowH, base, curveH, bars, barX, barW, labelX, labelY },
    { keys = STACK_ORDER, codeOf = (d) => d.g } = {}) {
    const g = svg.append("g").attr("class", "gsummary");

    // 10-minute bins from 5 AM per genre, each smoothed over about an hour with
    // SHARE_KERNEL, wrapping round the day. The y scale runs to the highest smoothed
    // total of the rows drawn (the year's, or the month's). The band paths are keyed by
    // genre and reused, so a genre pin's classes on them survive a redraw.
    const half = (SHARE_KERNEL.length - 1) / 2;
    const kSum = d3.sum(SHARE_KERNEL);
    const bandG = g.append("g");
    const label = g.append("text")
      .attr("class", "clabel")
      .attr("x", x(0) + 2)
      .attr("y", base - curveH - 3) // above the chart, so it never crosses it
      .text(CURVE_LABEL);
    const overlay = curveOverlay(g, label, { x, base, curveH, kernel: SHARE_KERNEL });
    function curve(rows) {
      const bins = new Map(keys.map((c) => [c, new Array(144).fill(0)]));
      for (const d of rows) bins.get(codeOf(d))[Math.floor(dayMinute(d.minutes) / 10)] += 1;
      const smooth = new Map(keys.map((c) => {
        const b = bins.get(c);
        return [c, b.map((_, i) => d3.sum(SHARE_KERNEL, (k, j) => k * b[(i + j - half + 144) % 144]) / kSum)];
      }));
      const point = (m, value) => Object.fromEntries([["m", m], ...keys.map((c) => [c, value(smooth.get(c))])]);
      const edge = (s) => (s[0] + s[143]) / 2;
      const table = [point(0, edge), ...d3.range(144).map((i) => point(i * 10 + 5, (s) => s[i])), point(1440, edge)];
      const series = d3.stack().keys(keys)(table);
      const peak = d3.max(series[series.length - 1], (p) => p[1]); // the top layer's top is the total
      const yc = d3.scaleLinear().domain([0, peak || 1]).range([base, base - curveH]);
      const area = d3.area().x((p) => x(p.data.m)).y0((p) => yc(p[0])).y1((p) => yc(p[1])).curve(d3.curveMonotoneX);
      bandG.selectAll("path.band")
        .data(series, (s) => s.key)
        .join("path")
        .classed("band", true) // adds, so a pin's .on and .off stay
        .attr("data-g", (s) => s.key)
        .attr("d", area);
    }
    curve(data);

    if (!bars) return { curve, bars() {}, overlay };

    // Plays per listening day, one rect per genre per day, laid end to end.
    const barG = g.append("g").attr("class", "bars");
    const barH = Math.max(1, rowH * 0.7);
    function drawBars(rows) {
      const perDay = d3.rollup(rows, (v) => v.length, (d) => +d.day, codeOf);
      const segs = [];
      let most = 0;
      for (const [day, counts] of perDay) {
        let acc = 0;
        for (const c of order) {
          const n = counts.get(c);
          if (!n) continue;
          segs.push({ day, c, from: acc, to: acc + n });
          acc += n;
        }
        most = Math.max(most, acc);
      }
      const bw = d3.scaleLinear().domain([0, most || 1]).range([0, barW]);
      barG.selectAll("rect")
        .data(segs)
        .join("rect")
        .attr("data-g", (s) => s.c)
        .attr("x", (s) => barX + bw(s.from))
        .attr("y", (s) => y(s.day) - barH / 2)
        .attr("width", (s) => bw(s.to) - bw(s.from))
        .attr("height", barH);
    }
    drawBars(data);
    g.append("text")
      .attr("x", labelX)
      .attr("y", labelY)
      .attr("text-anchor", "end")
      .text("plays per day");
    return { curve, bars: drawBars, overlay };
  }

  // Embedded mode only: remove the Color by links of the modes the data lacks (`modes` is
  // the ones it has), hide what needs genres or the nv column when there are none, and say
  // which time zones the times are in ("All times are in America/New_York.", or with
  // several zones "All times are local time: America/New_York, then America/Los_Angeles
  // from Aug 01, 2022.").
  function setUpEmbeddedPage(modes) {
    d3.selectAll("#colorby a").filter(function () { return !modes.includes(this.getAttribute("data-mode")); }).remove();
    if (!HAS_GENRES) d3.selectAll(".needs-genres").attr("hidden", "");
    if (!HAS_NV) d3.selectAll(".needs-nv").attr("hidden", "");
    const note = document.getElementById("tz-note");
    const zones = ((EMBEDDED.meta && EMBEDDED.meta.time_zones) || [])
      .filter((z) => z && typeof z.zone === "string" && z.zone);
    if (!note || !zones.length) return;
    note.textContent = zones.length === 1
      ? `All times are in ${zones[0].zone}.`
      : `All times are local time: ${zones.map((z, i) => {
        const from = i > 0 && typeof z.from === "string" ? parseDay(z.from) : null;
        return i === 0 ? z.zone : `then ${z.zone}${from ? ` from ${formatDay(from)}` : ""}`;
      }).join(", ")}.`;
  }

  // Boot: one chart, colored by the mode in the hash (#2025, #2025-artist, ...). Only the
  // year on show loads (loadYear keeps each year once loaded), with its where file and
  // genres.json and genre_months.json; variety.json loads the first time the new mode shows.
  async function boot() {
    const els = {
      chart: document.getElementById("chart"),
      wrap: document.getElementById("chart-wrap"),
      tooltip: document.getElementById("tooltip"),
      pinbar: document.getElementById("pinbar"),
      years: document.getElementById("years"),
      stats: document.getElementById("stats"),
    };
    const tooltip = makeTooltip(els.tooltip, els.wrap);
    const lists = { a: makeRankList(document.getElementById("list-a")), b: makeRankList(document.getElementById("list-b")),
      c: makeRankList(document.getElementById("list-c")) };
    const genreList = makeGenreList(document.getElementById("genres"));
    const songList = makeSongList(document.getElementById("topsongs"));
    const albumList = makeSongList(document.getElementById("topalbums"), { item: "album", nameOf: (d) => d.album });
    const eras = makeEras(document.getElementById("eras"));
    const variety = makeVariety(document.getElementById("variety"));
    // The modes the data offers, the first being the default: all of them, except in
    // embedded mode without genres (no genre mode) or without the nv column (no new mode).
    const modes = MODES.filter((k) => (k !== "genre" || HAS_GENRES) && (k !== "new" || HAS_NV));
    const parseHash = () => {
      const m = /^#?(\d{4})?(?:-(\w+))?$/.exec(window.location.hash) || [];
      const mode = modes.includes(m[2]) ? m[2] : modes[0];
      return { asked: +m[1], mode };
    };
    const setModeClass = (mode) => {
      for (const k of MODES) document.body.classList.toggle(`mode-${k}`, k === mode);
      document.body.classList.toggle("mode-rank", RANK_MODES.includes(mode));
      d3.selectAll("#colorby a").attr("aria-current", function () { return this.getAttribute("data-mode") === mode ? "true" : null; });
      d3.selectAll(".kind").text(mode === "track" ? "tracks" : `${mode}s`);
    };
    if (EMBEDDED) setUpEmbeddedPage(modes);
    setModeClass(parseHash().mode);

    const yearRows = EMBEDDED ? EMBEDDED.years : await d3.json("plays/years.json");
    const years = yearRows.map((d) => +d.year).sort((a, b) => b - a);
    // Embedded pages open on their latest year.
    const fallback = !EMBEDDED && years.includes(DEFAULT_YEAR) ? DEFAULT_YEAR : years[0];
    const fromHash = () => { const h = parseHash(); return { year: years.includes(h.asked) ? h.asked : fallback, mode: h.mode }; };

    const links = d3.select(els.years).selectAll("span.year").data(years).join("span").attr("class", "year");
    links.append("a").attr("href", (d) => `#${d}`).text((d) => d);
    links.filter((d, i, nodes) => i < nodes.length - 1).append("span").attr("class", "sep").text(" / ");

    let current = null;
    const pick = { year: null, month: null, hour: null };
    let wanted = null;
    let ticket = 0;

    // The year's chart (buildFrame) stays up across tab switches; a new year or a new width
    // builds a new one. Each mode is a skin on it (skinGenre, skinRank), and the last
    // skin's teardown runs before the next one starts.
    let frame = null;
    let skin = null;
    const parts = { els, tooltip, lists, genreList, songList, albumList, pick, stats: setStats };

    // The stats line under the year links: plays, tracks and artists in the subset on show
    // (the chosen month and hour), narrowed by `filter` ({ key, label, test }: the pick
    // that filters the lists, from the skin; null for none). When anything narrows it, it
    // starts with what it covers: "Big Thief, January 2025, 2–3 PM: 212 plays · ...". Kept
    // per year by subset and filter.
    function setStats(filter) {
      if (!current || !els.stats) return;
      const Y = lookupsFor(current.year, current.data);
      const k = pick.month;
      const h = pick.hour;
      if (!Y.statsText) Y.statsText = new Map();
      const key = `${subKey(k, h)}\u0000${filter ? filter.key : ""}`;
      let text = Y.statsText.get(key);
      if (text === undefined) {
        const rows = filter ? subsetRows(Y, k, h).filter(filter.test) : subsetRows(Y, k, h);
        const tracks = new Set();
        const artists = new Set();
        for (const d of rows) { tracks.add(trackKey(d)); artists.add(d.artist); }
        const scope = [filter ? filter.label : "", subsetLabel(current.year, k, h)].filter(Boolean).join(", ");
        text = `${scope ? `${scope}: ` : ""}${plural(rows.length, "play")} · ${plural(tracks.size, "track")} · ${plural(artists.size, "artist")}`;
        Y.statsText.set(key, text);
      }
      els.stats.textContent = text;
    }

    // The eras chart (genre mode) and the variety panel (new mode; no months: set() hides
    // it) are drawn after the chart, once the lists and the chart have given the page its
    // height, so they measure the same width as the chart. Nothing reads a width before the
    // dots change, so the browser lays the page out once per switch: the chart's width is
    // checked at the end, and a frame drawn at a stale width (a resize not yet handled) is
    // rebuilt then. `rebuild` forces a new frame (the resize handler).
    function draw(rebuild = false) {
      const c = current;
      if (skin) { skin.teardown(); skin = null; }
      if (rebuild || !frame || frame.year !== c.year || frame.data !== c.data) {
        frame = buildFrame(c.where, c.year, c.data, els, tooltip, pick);
      }
      const Y = lookupsFor(c.year, c.data);
      if (c.mode === "genre") {
        lists.a.hide(); lists.b.hide(); lists.c.hide();
        skin = skinGenre(frame, Y, c.genres, parts);
        eras.set({ months: c.months, year: c.year, names: c.genres.names });
      } else {
        skin = skinRank(frame, Y, c.mode, parts);
      }
      variety.set({ months: c.mode === "new" ? c.variety : null, year: c.year, mode: c.mode });
      c.drawnFor = els.chart.clientWidth;
      if (c.drawnFor !== frame.clientWidth && !rebuild) { draw(true); return; }
      const F = frame;
      if (!F.warming) { F.warming = true; warmUp(F, Y, c.genres, () => frame === F); }
    }

    async function show(year, mode) {
      const key = hashFor(year, mode);
      if (key === wanted) return;
      wanted = key;
      document.title = `${year} in Music`;
      const mine = ++ticket;
      links.select("a").attr("href", (d) => hashFor(d, mode)).classed("active", (d) => d === year)
        .attr("aria-current", (d) => (d === year ? "page" : null));
      d3.selectAll("#colorby a").attr("href", function () { return hashFor(year, this.getAttribute("data-mode")); });
      // The eras chart and the variety panel are optional: if their file fails, the mode
      // draws without them.
      let data; let where; let genres; let months; let varietyRows;
      try {
        [data, where, genres, months, varietyRows] = await Promise.all([loadYear(year), loadWhere(year),
          HAS_GENRES ? loadGenres() : null, HAS_GENRES ? loadGenreMonths().catch(() => null) : null,
          mode === "new" ? loadVariety().catch(() => null) : null]);
      } catch (err) {
        if (mine === ticket) { wanted = null; els.stats.textContent = "The plays did not load. Reload the page to try again."; }
        return;
      }
      if (mine !== ticket) return;
      // The skin sets the stats line (setStats), once the frame has the year's month and hour.
      current = { year, mode, data, where, genres, months, variety: varietyRows, drawnFor: 0 };
      // The layout for the new mode and the lists that fill it change in the same step,
      // so no frame shows the last mode's lists in the new mode's columns.
      setModeClass(mode);
      draw();
    }

    window.addEventListener("hashchange", () => { const h = fromHash(); show(h.year, h.mode); });
    let timer = null;
    window.addEventListener("resize", () => {
      clearTimeout(timer);
      timer = setTimeout(() => { if (current && els.chart.clientWidth !== current.drawnFor) draw(true); }, 150);
    });
    const first = fromHash();
    await show(first.year, first.mode);
  }

  function start() {
    boot().catch(() => {
      const stats = document.getElementById("stats");
      if (stats) stats.textContent = "The list of years did not load. Reload the page to try again.";
    });
  }

  if (document.readyState === "loading") document.addEventListener("DOMContentLoaded", start);
  else start();
})();
