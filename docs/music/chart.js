// One dot per play: x = minute of the local day (the day runs 5 AM to 5 AM),
// y = listening day in the year (a play before 05:00 belongs to the day before).
// Design after Sahil Chinoy's "Songs I've listened to" (https://sahilchinoy.com/music/).
// Tooltip follows Chris Henrick's convention
// (https://observablehq.com/@clhenrick/tooltip-d3-convention).
//
// Plain script, no modules: it needs the global `d3` (the v7 UMD build) loaded first.
// The year and view come from the URL hash: #2025 is the tracks view (top tracks in
// colour), #2025-genre the genre view (every dot in its artist's genre colour). The page
// redraws on hashchange.
// Data, fetched by relative URL: plays/years.json, plays/<year>.csv, and optionally
// plays/where_<year>.json for the "where I was" strip (no file, no strip). The genre
// view also reads plays/genres.json ({names: {code: label}, artist: {artist: code}},
// written by music/genre/export_genres.py); an artist not listed counts as OT (Other).
// It also reads plays/genre_months.json ({"YYYY-MM": {code: plays}}, same script) once,
// for the eras chart; if that file fails, the genre view draws without the eras chart.
// Margin summaries are on by default; ?margins=0 turns them off.
// The CSVs' optional 10th column, album, feeds the albums lists (the tracks view's Albums
// tab, the genre view's Top albums). An album is its name plus the artist column; plays
// with no album name are left out of those lists, and without the column they are empty.
//
// Embedded mode, for the one-file page that music_template/make_music_page.py writes:
// when the page sets window.MUSIC_DATA before this script, the data comes from it and
// nothing is fetched. Its fields: years (as years.json), plays ({"2025": CSV text with
// the columns of <year>.csv}), genres (as genres.json, or null), genre_months (as
// genre_months.json, or null) and meta ({time_zones: [{from, zone}], ...}). There are no
// where files, so no strip and no place labels; the default year is the latest one; with
// genres null the genre view is off and elements with class "needs-genres" are hidden;
// an element with id "tz-note" gets a sentence about the time zones.
(function () {
  "use strict";

  if (typeof d3 === "undefined") {
    const stats = document.getElementById("stats");
    if (stats) stats.textContent = "The chart library (D3) did not load. Reload the page to try again.";
    return;
  }

  const DEFAULT_YEAR = 2025;
  const EMBEDDED = window.MUSIC_DATA && typeof window.MUSIC_DATA === "object" ? window.MUSIC_DATA : null;
  const HAS_GENRES = !EMBEDDED || !!EMBEDDED.genres; // fetch mode always offers the genre view
  const HEIGHT = 1800;
  // The left margin is built up from the pieces below (place labels, month labels, strip).
  const MARGIN = { top: 50, right: 20, bottom: 10 };
  const RADIUS = 2.5;
  const MIN_WIDTH = 240; // narrowest chart drawn; phones at 400 px get 368
  const NARROW = 600; // below this width, label every 3 hours and drop the place labels
  const TIP_GAP = 8; // px between cursor and tooltip
  const TIP_MAX = 320; // widest tooltip, px
  const TOP_N = 10; // ranks 1 to 10 each get a colour (see the CSS)
  // How the tracks view ranks a chosen month (its top-ten lists and dot colors). The year
  // uses the CSV's dense rank: ties share a rank and the next rank follows (94, 94, 84 ->
  // 1, 1, 2). A month has few plays per track, so dense ranks there reach deep into ties:
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
  const MONTH_W = 24; // room for the month labels
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
  const GENRE_SUFFIX = "-genre"; // #2025-genre
  const hashFor = (year, view) => `#${year}${view === "genre" ? GENRE_SUFFIX : ""}`;
  const formatShare = (s) => (s > 0 && s < 0.005 ? "<1%" : d3.format(".0%")(s));

  // A fixed stacking order, bottom up, for both stacked charts in the genre view (plays
  // by time of day and the eras chart), so each genre's color sits in the same place
  // in every year. It must list every code in GENRE_CODES.
  const STACK_ORDER = ["FS", "IP", "RB", "CS", "IR", "RP", "PE", "HH", "OT"];
  const SHARE_CURVE_H = 80; // height of the genre view's plays-by-time-of-day chart
  const SHARE_KERNEL = [1, 2, 3, 4, 3, 2, 1]; // over 10-minute bins: about an hour wide
  const CURVE_LABEL = "plays by time of day, by genre"; // a chosen month adds ", January 2025"
  const SUM_LABEL = "plays by time of day"; // the tracks view's curve; a month adds ", January 2025"
  // Month labels are buttons: a click emphasizes that month's listening days by laying a
  // veil in the page colour (see the CSS, #chart .veil) over the rows outside it.
  const MONTH_HIT_H = 24; // height of a month label's invisible click target, px
  const formatMonthName = d3.utcFormat("%B");
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

  let measureCtx;
  function textWidth(text, px) {
    if (measureCtx === undefined) measureCtx = document.createElement("canvas").getContext("2d");
    if (!measureCtx) return text.length * px * 0.6;
    measureCtx.font = `${px}px ${getComputedStyle(document.body).fontFamily}`;
    return measureCtx.measureText(text).width;
  }

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
    };
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

  async function loadYear(year) {
    if (EMBEDDED) {
      const text = EMBEDDED.plays && EMBEDDED.plays[String(year)];
      if (typeof text !== "string") throw new Error(`no plays for ${year}`);
      return d3.csvParse(text, parseRow); // play order, as below
    }
    const rows = await d3.csv(`plays/${year}.csv`, parseRow);
    return rows; // file order, which is play order (the build sorts by UTC start)
  }

  // The tracks view draws ranked plays on top: a copy of the rows sorted by layer.
  function tracksOrder(rows) {
    return rows.slice().sort((a, b) => layerOf(a) - layerOf(b)); // stable, so time order holds within a layer
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
  async function loadWhere(year) {
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
  // width; with no months it hides the section.
  function makeEras(rootEl) {
    if (!rootEl) return { set() {} };
    const holder = d3.select(rootEl).select(".eras-chart");
    const title = d3.select(rootEl).select(".lead");
    const tooltip = makeTooltip(rootEl.querySelector(".tooltip"), rootEl);

    function set({ months, year, names }) {
      tooltip.hide();
      holder.selectChildren().remove();
      if (!months || !months.length) { rootEl.hidden = true; return; }
      rootEl.hidden = false;
      const width = Math.max(MIN_WIDTH, rootEl.clientWidth);
      const first = months[0].year;
      const last = months[months.length - 1].year;
      title.text(`Genre share by month, ${first}–${last}`);

      const x = d3.scaleBand()
        .domain(months.map((m) => m.key))
        .range([0, width])
        .paddingInner(width / months.length >= 5 ? 0.15 : 0); // no gaps when bars are thin
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

  function makeTooltip(tipEl, wrapEl) {
    const tip = d3.select(tipEl);

    // `genre` ({code, label}) is given in the genre view only; it adds a line under the name.
    // `note` (text) adds a last line: the tracks view's rank in the chosen month.
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

  // What both views share: the svg, the weekend bands, the axes and the "where I was"
  // strip. Returns the svg, the scales, the options for the margin summaries, and the
  // month emphasis (see monthPicker; `pick` holds the chosen month across redraws).
  // `curveH` is the height of the summary above the plot (the genre view's is taller).
  function drawFrame(where, year, els, tooltip, label, pick, curveH = SUM_CURVE_H) {
    const sumTop = SHOW_MARGINS ? SUM_TOP + curveH - SUM_CURVE_H : 0; // extra height above the plot
    const width = Math.max(MIN_WIDTH, els.chart.clientWidth);
    const narrow = width < NARROW;

    // "Where I was" runs; null (no strip, original margin) when the file is missing.
    // Runs of 2+ days get a place label, on desktop only.
    const runs = where ? awayRuns(where, year) : null;
    const named = runs && !narrow ? runs.filter((r) => r.days.length >= 2) : [];
    named.forEach((r) => { r.short = placeShort(r.label) || "Away"; });
    const labelW = named.length ? Math.ceil(d3.max(named, (r) => textWidth(r.short, PLACE_PX))) : 0;

    const monthEnd = (labelW ? labelW + PLACE_GAP : 0) + MONTH_W; // month labels end here
    const stripX = monthEnd + STRIP_PAD;
    const showBars = SHOW_MARGINS && !narrow; // per-day bars crowd the plot on phones
    const barW = SUM_BAR_W;
    const m = {
      top: MARGIN.top + sumTop,
      right: MARGIN.right + (showBars ? barW + SUM_GAP : 0),
      bottom: MARGIN.bottom,
      left: runs ? stripX + STRIP_W + PLOT_PAD : monthEnd + PLOT_PAD,
    };
    const height = HEIGHT + sumTop; // the plot itself keeps its height

    const x = d3.scaleLinear()
      .domain([0, 1440])
      .range([m.left, width - m.right]);
    const y = d3.scaleUtc()
      .domain([Date.UTC(year, 0, 1), Date.UTC(year, 11, 31)])
      .range([m.top, height - m.bottom]);

    tooltip.hide();
    d3.select(els.pinbar).style("display", "none");
    d3.select(els.chart).selectChildren().remove();
    const svg = d3.select(els.chart).append("svg")
      .attr("width", width)
      .attr("height", height)
      .attr("role", "img")
      .attr("aria-label", label);

    // Weekend bands behind each Saturday and Sunday row, under the gridlines and dots.
    // Rows are listening days, so a band covers that day's 5 AM to the next 5 AM.
    const rowH = y(Date.UTC(year, 0, 2)) - y(Date.UTC(year, 0, 1));
    const weekendDays = d3.utcDays(new Date(Date.UTC(year, 0, 1)), new Date(Date.UTC(year + 1, 0, 1)))
      .filter((d) => d.getUTCDay() === 6 || d.getUTCDay() === 0);
    svg.append("g")
      .attr("class", "weekends")
      .selectAll("rect")
      .data(weekendDays)
      .join("rect")
      .attr("x", m.left - RADIUS)
      .attr("width", width - m.left - m.right + 2 * RADIUS)
      .attr("y", (d) => y(d) - rowH / 2)
      .attr("height", rowH);

    const ticks = d3.range(0, 1441, narrow ? 180 : 60);
    const tickText = hourLabels(ticks);
    svg.append("g")
      .attr("class", "axis x-axis")
      .attr("transform", `translate(0, ${X_AXIS_Y})`)
      .call(d3.axisTop(x)
        .tickValues(ticks)
        .tickFormat((v) => tickText.get(v))
        .tickSize(-(height - m.bottom - X_AXIS_Y))
        .tickPadding(4));

    const yAxis = svg.append("g")
      .attr("class", "axis y-axis")
      .attr("transform", `translate(${monthEnd}, 0)`)
      .call(d3.axisLeft(y)
        .ticks(d3.utcMonth.every(1))
        .tickFormat(d3.utcFormat("%b"))
        .tickSize(0)
        .tickPadding(0));
    const month = monthPicker(svg, yAxis, { y, year, rowH, pick, placeW: labelW, labelsEnd: monthEnd, width });

    if (runs) drawStrip(svg, runs, named, { y, rowH, stripX, labelX: labelW, tooltip });
    const sum = { x, y, rowH, base: X_AXIS_Y + 22 + curveH, curveH, bars: showBars, barX: width - m.right + SUM_GAP, barW, labelX: width - 2, labelY: m.top - 8 };
    return { svg, x, y, sum, month };
  }

  // The month labels as buttons (click, or Enter or Space when focused). Choosing a month
  // emphasizes its listening days (the `day` column, which sets each dot's row): a veil
  // in the page colour covers every row outside it, from the strip to the per-day bars,
  // and the place labels beside those rows. Choosing it again clears it; choosing another
  // switches. The choice lives in `pick` ({ year, month: 0 to 11 or null }), so a redraw of
  // the same year (a resize or a view switch) keeps it, and a new year clears it.
  // veil() draws the veil at the current top of the svg: what is drawn before it fades and
  // what is drawn after it does not. clear() clears the month and says whether one was set.
  // listen(fn) has fn(month) run after every change (both views filter their lists and
  // their time-of-day curve by the month, and the tracks view recolors its dots).
  function monthPicker(svg, axis, { y, year, rowH, pick, placeW, labelsEnd, width }) {
    if (pick.year !== year) { pick.year = year; pick.month = null; }
    const monthOf = (d) => d.getUTCMonth();
    const ticks = axis.selectAll(".tick")
      .attr("role", "button")
      .attr("tabindex", 0)
      .attr("aria-label", (d) => `Emphasize ${formatMonthName(d)}`);
    // A wider and taller target than the label, across the whole month-label column.
    ticks.insert("rect", ":first-child")
      .attr("class", "hit")
      .attr("x", -(MONTH_W + 4))
      .attr("y", -MONTH_HIT_H / 2)
      .attr("width", MONTH_W + 6)
      .attr("height", MONTH_HIT_H)
      .attr("rx", 3);

    // Veil pieces run from the plot's first row to its last, a dot's radius beyond each.
    const top = y(Date.UTC(year, 0, 1)) - rowH / 2 - RADIUS;
    const bottom = y(Date.UTC(year, 11, 31)) + rowH / 2 + RADIUS;
    const rowStart = (k) => (k > 11 ? bottom : y(Date.UTC(year, k, 1)) - rowH / 2);
    const cols = [[labelsEnd + 2, width]]; // the strip, the plot and the per-day bars
    if (placeW) cols.push([0, placeW + 3]); // the place labels
    let veil = null;
    let onChange = null;

    function apply() {
      const k = pick.month;
      ticks.classed("on", (d) => monthOf(d) === k).attr("aria-pressed", (d) => String(monthOf(d) === k));
      if (!veil) return;
      const pieces = k === null ? [] : cols.flatMap(([x0, x1]) => [
        k > 0 && { x0, x1, y0: top, y1: rowStart(k) }, // the months before
        k < 11 && { x0, x1, y0: rowStart(k + 1), y1: bottom }, // the months after
      ]).filter(Boolean);
      veil.selectAll("rect")
        .data(pieces)
        .join("rect")
        .attr("x", (p) => p.x0)
        .attr("width", (p) => p.x1 - p.x0)
        .attr("y", (p) => p.y0)
        .attr("height", (p) => p.y1 - p.y0);
    }

    const set = (k) => { pick.month = k; apply(); if (onChange) onChange(k); };
    const toggle = (d) => set(pick.month === monthOf(d) ? null : monthOf(d));
    ticks
      .on("click", (event, d) => { event.stopPropagation(); toggle(d); }) // not a click on empty space
      .on("keydown", (event, d) => {
        if (event.key !== "Enter" && event.key !== " ") return;
        event.preventDefault();
        toggle(d);
      });
    apply();

    return {
      veil() { veil = svg.append("g").attr("class", "veil").attr("pointer-events", "none"); apply(); },
      clear() { if (pick.month === null) return false; set(null); return true; },
      listen(fn) { onChange = fn; },
    };
  }

  // The tracks view: the top-ten tracks in color; hover shows the same track and artist.
  // A chosen month (pick.month) makes the view that month's: the dots on its listening
  // days take the colors of its own top ten (ranked by MONTH_RANK), the other months'
  // dots stay plain gray under the veil, and the top-ten list (every tab) and the
  // time-of-day curve count only its plays. Clearing the month restores the year. The
  // per-day bars and the pin bar keep the whole year, and a pin stays across months.
  // `data` is in tracks order (see tracksOrder).
  function render(data, where, year, els, tooltip, topTen, pick) {
    const { svg, x, y, sum, month } = drawFrame(where, year, els, tooltip,
      `Every Spotify play in ${year}, by time of day (5 AM to 5 AM) and day`, pick);
    const bar = d3.select(els.pinbar);
    // curve(rows, label) redraws the time-of-day curve from other rows (one month's).
    const curve = SHOW_MARGINS ? drawSummaries(svg, data, sum) : null;

    const layer = svg.append("g").attr("class", "dots");
    const dots = layer.selectAll("circle")
      .data(data)
      .join("circle")
      .attr("cx", (d) => x(dayMinute(d.minutes)))
      .attr("cy", (d) => y(d.day))
      .attr("r", RADIUS)
      .attr("data-rank", (d) => (d.rank <= TOP_N ? d.rank : null));
    month.veil(); // over everything so far, so highlighted dots fade outside the month too

    // Look-ups so a hover touches only the dots it needs.
    const nodes = dots.nodes();
    const datum = (n) => d3.select(n).datum();
    const byArtist = d3.group(nodes, (n) => datum(n).artist);
    const byTrack = d3.group(nodes, (n) => trackKey(datum(n)));
    const byAlbum = d3.group(nodes.filter((n) => datum(n).album), (n) => albumKey(datum(n)));
    // The colored dots by rank, for the year or the chosen month (see recolor).
    let byRank = d3.group(nodes.filter((n) => datum(n).rank <= TOP_N), (n) => datum(n).rank);

    function raiseRanked() {
      for (let r = TOP_N; r >= 1; r--) {
        const group = byRank.get(r);
        if (group) d3.selectAll(group).raise();
      }
    }

    // Focuses for the list rows. Their n is always the year's plays (the pin bar's count).
    const artistFocus = (artist) => ({ kind: "artist", artist, n: byArtist.get(artist).length });
    const albumFocus = (d) => ({ kind: "album", artist: d.artist, key: albumKey(d), album: d.album,
      n: byAlbum.get(albumKey(d)).length });

    // Plays by listening month, 0 to 11, from the `day` column (-1 for a day outside the
    // year), and the lists of the year (key null) or of a month, made when first asked for:
    // { tracks, artists, albums } as list rows, plus `rankOf` (track key -> { rank, n }, for
    // every track in a month). The year's tracks come from the CSV's rank and n_plays.
    const monthOf = (d) => (d.day.getUTCFullYear() === year ? d.day.getUTCMonth() : -1);
    const byMonth = d3.group(data, monthOf);
    const monthDate = (k) => new Date(Date.UTC(year, k, 1));
    const byTitle = (a, b) => d3.ascending(a.track, b.track) || d3.ascending(a.artist, b.artist);
    const byArtistName = (a, b) => d3.ascending(a.artist, b.artist);
    const byAlbumName = (a, b) => d3.ascending(a.album, b.album) || d3.ascending(a.artist, b.artist);
    const tables = new Map();
    function tableFor(k) {
      if (tables.has(k)) return tables.get(k);
      const rows = k === null ? data : byMonth.get(k) || [];
      const rule = k === null ? "dense" : MONTH_RANK;
      const top = (ranked) => ranked.filter((r) => r.rank <= TOP_N);
      let tracks;
      let rankOf = null;
      if (k === null) {
        // Tracks ranked 1 to 10 (ties share a rank, so there can be more than ten rows).
        tracks = Array.from(byTrack.values(), (group) => datum(group[0]))
          .filter((d) => d.rank <= TOP_N)
          .sort((a, b) => a.rank - b.rank || d3.ascending(a.track, b.track))
          .map((d) => ({ rank: d.rank, title: d.track, sub: d.artist, n: d.n_plays, focus: trackFocus(d) }));
      } else {
        const ranked = rankCounts(rows, trackKey, rule, byTitle);
        rankOf = new Map(ranked.map((r) => [r.key, r]));
        tracks = top(ranked).map((r) => (
          { rank: r.rank, title: r.first.track, sub: r.first.artist, n: r.n, focus: trackFocus(r.first) }));
      }
      const artists = top(rankCounts(rows, (d) => d.artist, rule, byArtistName))
        .map((r) => ({ rank: r.rank, title: r.key, n: r.n, focus: artistFocus(r.key) }));
      const albums = top(rankCounts(rows.filter((d) => d.album), albumKey, rule, byAlbumName))
        .map((r) => ({ rank: r.rank, title: r.first.album, sub: r.first.artist, n: r.n, focus: albumFocus(r.first) }));
      const t = { tracks, artists, albums, rankOf };
      tables.set(k, t);
      return t;
    }

    // A dot's color rank: the year's, or in a month the month's for that month's dots only.
    function rankFor(d, k, t) {
      if (k === null) return d.rank <= TOP_N ? d.rank : null;
      if (monthOf(d) !== k) return null;
      const r = t.rankOf.get(trackKey(d));
      return r && r.rank <= TOP_N ? r.rank : null;
    }

    // The tooltip's last line in a month: the dot's rank and plays there ("#1 in January ·
    // 13 plays"); none for the year or for a dot outside the month.
    function noteFor(d) {
      const k = pick.month;
      if (k === null || monthOf(d) !== k) return null;
      const r = tableFor(k).rankOf.get(trackKey(d));
      return r ? `#${r.rank} in ${formatMonthName(monthDate(k))} · ${plural(r.n, "play")}` : null;
    }

    let active = null; // the highlighted focus (hovered, or pinned)
    let activeNode = null; // the hovered dot behind `active`, when there is one
    let pinned = null; // the pinned focus, if any
    let pendingClear = null;

    // A focus lights up its artist's dots (orange) and, on top, its track's (teal) or its
    // album's (purple) dots.
    function groupsOf(f) {
      const out = [["same-artist", byArtist.get(f.artist)]];
      if (f.kind === "track") out.push(["same-track", byTrack.get(f.key)]);
      if (f.kind === "album") out.push(["same-album", byAlbum.get(f.key)]);
      return out;
    }

    function highlight(f) {
      svg.classed("hovering", true);
      for (const [cls, group] of groupsOf(f)) d3.selectAll(group).classed(cls, true).raise();
    }

    function clear() {
      if (!active) return;
      svg.classed("hovering", false);
      for (const [cls, group] of groupsOf(active)) d3.selectAll(group).classed(cls, false);
      raiseRanked();
      active = null;
      activeNode = null;
    }

    // Click a dot (or a track row in the list) to pin its artist and track highlight;
    // click an artist row to pin only that artist, or an album row to pin the album (and
    // its artist). Clicking the same thing again, clicking empty chart space, or pressing
    // Esc clears it (Esc clears the month once nothing is pinned). While something is
    // pinned, hovering dots shows only their tooltip.
    function pin(f) {
      if (pendingClear) { cancelAnimationFrame(pendingClear); pendingClear = null; }
      clear();
      active = f;
      pinned = f;
      highlight(f);
      bar.selectChildren().remove();
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
      topTen.sync();
    }

    function unpin() {
      pinned = null;
      clear();
      tooltip.hide();
      bar.style("display", "none");
      topTen.sync();
    }

    const toggle = (f) => (sameFocus(pinned, f) ? unpin() : pin(f));

    // Raising a dot moves it in the DOM, which can fire mouseout and mouseover on the
    // same dot. Clearing on the next frame lets a repeat mouseover cancel the clear.
    layer
      .on("mouseover", (event) => {
        const node = event.target;
        if (node.tagName !== "circle") return;
        if (pinned) { tooltip.show(datum(node), event, null, noteFor(datum(node))); return; }
        if (pendingClear) { cancelAnimationFrame(pendingClear); pendingClear = null; }
        if (node === activeNode) return;
        clear();
        activeNode = node;
        active = trackFocus(datum(node));
        highlight(active);
        tooltip.show(datum(node), event, null, noteFor(datum(node)));
      })
      .on("mousemove", (event) => { if (active) tooltip.move(event); })
      .on("mouseout", (event) => {
        if (event.target.tagName !== "circle") return;
        if (pinned) { tooltip.hide(); return; }
        pendingClear = requestAnimationFrame(() => {
          pendingClear = null;
          clear();
          tooltip.hide();
        });
      })
      .on("click", (event) => {
        const node = event.target;
        if (node.tagName !== "circle") return;
        event.stopPropagation();
        const d = datum(node);
        const f = trackFocus(d);
        if (sameFocus(pinned, f)) unpin();
        else { pin(f); tooltip.show(d, event, null, noteFor(d)); }
      });
    svg.on("click", () => { if (pinned) unpin(); });
    d3.select(window).on("keydown.pin", (event) => {
      if (event.key !== "Escape") return;
      if (pinned) unpin();
      else month.clear();
    });

    // The top-ten list for the year or the chosen month: tracks and artists and albums
    // ranked 1 to 10, ties sharing a rank.
    function setList() {
      const k = pick.month;
      const t = tableFor(k);
      topTen.set({
        title: k === null ? "Top ten" : `Top ten (${formatMonthLong(monthDate(k))})`,
        tracks: t.tracks,
        artists: t.artists,
        albums: t.albums,
        empty: k === null ? "No plays this year." : "No plays this month.",
        noAlbums: byAlbum.size ? null : "No album names in this data.",
        isPinned: (f) => sameFocus(pinned, f),
        toggle,
      });
    }

    // Recolors the dots for the year or the chosen month, then restores the draw order:
    // tracks order, the colored dots on top (#1 last), and any highlight above them.
    function recolor() {
      const k = pick.month;
      const t = tableFor(k);
      dots.attr("data-rank", (d) => rankFor(d, k, t));
      byRank = d3.group(nodes.filter((n) => n.hasAttribute("data-rank")), (n) => +n.getAttribute("data-rank"));
      dots.order();
      raiseRanked();
      if (active) highlight(active);
    }

    // Choosing, switching or clearing a month: dots, list and curve follow it.
    function showMonth() {
      const k = pick.month;
      tooltip.hide();
      recolor();
      setList();
      if (curve) curve(k === null ? data : byMonth.get(k) || [], k === null ? SUM_LABEL : `${SUM_LABEL}, ${formatMonthLong(monthDate(k))}`);
    }

    if (pick.month === null) setList();
    else showMonth(); // a redraw (resize, view switch) keeps the month
    month.listen(showMonth);
  }

  // The genre view: every dot in its artist's genre colour, drawn in play order (no genre
  // on top). Hover outlines the same artist's dots; clicking a dot pins that artist.
  // Clicking a row of the genre list pins that genre, and every other dot turns grey.
  // Clicking the same thing again, clicking empty chart space, or Esc clears a pin.
  // The top-songs and top-albums lists follow the pin (the year, the genre or the
  // artist). Hovering a song or album row outlines its dots; clicking it selects it, on
  // top of the pin, until it is clicked again, the other list's row is selected (one song
  // or album at a time), the pin changes or clears, or Esc (which clears the song or album
  // before the pin, and the month last). While a song or album is hovered or selected, or
  // an artist is pinned by a click, every other dot fades (.solo in the CSS). A chosen
  // month (pick.month) filters both lists and the time-of-day curve to the plays on that
  // month's listening days; the pin and the selection stay when the month changes.
  // The dots, the genre list, the per-day bars and the pin bar keep the whole year.
  // `data` is in play order.
  function renderGenre(data, where, year, els, tooltip, genreList, genres, songList, albumList, pick) {
    const { svg, x, y, sum, month } = drawFrame(where, year, els, tooltip,
      `Every Spotify play in ${year}, by time of day (5 AM to 5 AM) and day, colored by the artist's genre`,
      pick, SHARE_CURVE_H);
    svg.classed("genre", true);
    const bar = d3.select(els.pinbar);
    const labelOf = (c) => genres.names.get(c) || c;
    for (const d of data) d.g = genres.artist.get(d.artist) || OTHER;
    const genreOf = (d) => ({ code: d.g, label: labelOf(d.g) });

    const total = data.length;
    const byGenre = d3.group(data, (d) => d.g);
    const byArtist = d3.group(data, (d) => d.artist);
    const bySong = d3.group(data, trackKey);
    const nOf = (c) => (byGenre.get(c) || []).length;
    // Songs by plays, then track title, then artist. A genre comes from the artist, so all
    // of a song's plays share its genre and artist: a song's plays within a pinned genre or
    // artist are its plays in the year (or the month).
    const rankSongs = (groups) => Array.from(groups, ([key, group]) => (
      { key, track: group[0].track, artist: group[0].artist, g: group[0].g, n: group.length }))
      .sort((a, b) => b.n - a.n || d3.ascending(a.track, b.track) || d3.ascending(a.artist, b.artist));
    const songs = rankSongs(bySong); // the year's
    const songByKey = new Map(songs.map((s) => [s.key, s]));
    // Plays by listening month, 0 to 11, from the `day` column (-1 for a day outside the
    // year), and each month's songs, ranked when first asked for.
    const byMonth = d3.group(data, (d) => (d.day.getUTCFullYear() === year ? d.day.getUTCMonth() : -1));
    const monthSongs = new Map();
    function songsIn(k) {
      if (k === null) return songs;
      if (!monthSongs.has(k)) monthSongs.set(k, rankSongs(d3.group(byMonth.get(k) || [], trackKey)));
      return monthSongs.get(k);
    }
    // Albums the same way: by plays, then album, then artist. An album is its name plus
    // the artist, so its plays too share one genre and one artist.
    const byAlbum = d3.group(data.filter((d) => d.album), albumKey);
    const rankAlbums = (groups) => Array.from(groups, ([key, group]) => (
      { key, album: group[0].album, artist: group[0].artist, g: group[0].g, n: group.length }))
      .sort((a, b) => b.n - a.n || d3.ascending(a.album, b.album) || d3.ascending(a.artist, b.artist));
    const albums = rankAlbums(byAlbum); // the year's
    const albumByKey = new Map(albums.map((s) => [s.key, s]));
    const monthAlbums = new Map();
    function albumsIn(k) {
      if (k === null) return albums;
      if (!monthAlbums.has(k)) monthAlbums.set(k, rankAlbums(d3.group((byMonth.get(k) || []).filter((d) => d.album), albumKey)));
      return monthAlbums.get(k);
    }
    const monthLabel = (k) => formatMonthLong(new Date(Date.UTC(year, k, 1))); // "January 2025"
    // List order: genres by this year's plays, Other last. The margins stack in this order.
    const order = GENRE_CODES.filter((c) => c !== OTHER)
      .sort((a, b) => nOf(b) - nOf(a) || GENRE_CODES.indexOf(a) - GENRE_CODES.indexOf(b))
      .concat(OTHER);

    // curve(rows, label) redraws the stacked time-of-day chart from those rows; it starts
    // on the year. drawCurve() draws it for the chosen month, or the year when there is none.
    const curve = SHOW_MARGINS ? drawGenreSummaries(svg, data, order, sum) : null;
    const gsum = svg.select(".gsummary");
    function drawCurve() {
      if (!curve) return;
      const k = pick.month;
      if (k === null) curve(data, CURVE_LABEL);
      else curve(byMonth.get(k) || [], `${CURVE_LABEL}, ${monthLabel(k)}`);
    }
    if (pick.month !== null) drawCurve(); // a redraw (resize, view switch) keeps the month

    const layer = svg.append("g").attr("class", "dots");
    layer.selectAll("circle")
      .data(data)
      .join("circle")
      .attr("cx", (d) => x(dayMinute(d.minutes)))
      .attr("cy", (d) => y(d.day))
      .attr("r", RADIUS)
      .attr("data-g", (d) => d.g);

    // Copies drawn over the dots, which the mouse ignores: the pinned genre's dots (.lift)
    // and the hovered or pinned artist's dots, outlined (.ring). Copies leave the play
    // order of the dots themselves untouched. The month veil sits between the two, so an
    // outlined song or artist keeps full strength in every month.
    const lift = svg.append("g").attr("class", "lift").attr("pointer-events", "none");
    month.veil();
    const ring = svg.append("g").attr("class", "ring").attr("pointer-events", "none");
    const copies = (g, rows) => g.selectAll("circle")
      .data(rows)
      .join("circle")
      .attr("cx", (d) => x(dayMinute(d.minutes)))
      .attr("cy", (d) => y(d.day))
      .attr("r", RADIUS)
      .attr("data-g", (d) => d.g);

    const datum = (n) => d3.select(n).datum();
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
    function ringNow() {
      const s = hoverSong || hoverAlbum || song || album;
      svg.classed("solo", !!s || (!!pinned && pinned.kind === "artist"));
      if (s && s.album === undefined) outline(`s:${s.key}`, bySong.get(s.key));
      else if (s) outline(`l:${s.key}`, byAlbum.get(s.key));
      else if (pinned && pinned.kind === "artist") outline(`a:${pinned.artist}`, byArtist.get(pinned.artist));
      else if (!pinned && hoverArtist !== null) outline(`a:${hoverArtist}`, byArtist.get(hoverArtist));
      else outline(null, []);
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
    // the pinned artist), within the chosen month if there is one. A selected song or
    // album that is not in its list goes in `kept`, with its place and plays in the same
    // pool. setSongs() redraws both lists.
    function setList(isSong) {
      if (isSong) hoverSong = null; // the rows are redrawn, so the hovered one is gone
      else hoverAlbum = null;
      const sel = isSong ? song : album;
      const f = pinned;
      const k = pick.month;
      const all = isSong ? songsIn(k) : albumsIn(k);
      const pool = !f ? all
        : all.filter(f.kind === "genre" ? (s) => s.g === f.g : (s) => s.artist === f.artist);
      const rows = pool.slice(0, TOP_N).map((s, i) => Object.assign({ pos: i + 1 }, s));
      let kept = null;
      if (sel && !rows.some((s) => s.key === sel.key)) {
        const i = pool.findIndex((s) => s.key === sel.key);
        kept = Object.assign({}, sel, { pos: i < 0 ? null : i + 1, n: i < 0 ? 0 : pool[i].n });
      }
      const when = k === null ? "" : ` (${monthLabel(k)})`;
      (isSong ? songList : albumList).set({
        title: `${isSong ? "Top songs" : "Top albums"}${when}${f ? `: ${f.kind === "genre" ? labelOf(f.g) : f.artist}` : ""}`,
        empty: !isSong && !byAlbum.size ? "No album names in this data."
          : k === null ? "No plays this year." : "No plays this month.",
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
      gsum.selectAll("[data-g]")
        .classed("off", function () { return genrePin && this.getAttribute("data-g") !== f.g; })
        .classed("on", function () { return genrePin && this.getAttribute("data-g") === f.g; });
      setSongs();
      ringNow();
      drawBar();
      genreList.sync();
    }

    function unpin() {
      pinned = null;
      song = null;
      album = null;
      hoverArtist = null;
      svg.classed("dim", false);
      copies(lift, []);
      gsum.selectAll("[data-g]").classed("off", false).classed("on", false);
      setSongs();
      ringNow();
      tooltip.hide();
      drawBar();
      genreList.sync();
    }

    const toggle = (f) => (samePin(pinned, f) ? unpin() : pin(f));

    // While something is pinned or a song is selected, hovering dots shows only their
    // tooltip. Clearing the outline waits a frame, so moving between two dots of one
    // artist doesn't flicker.
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
    svg.on("click", () => { if (pinned || song || album) unpin(); });
    d3.select(window).on("keydown.pin", (event) => {
      if (event.key !== "Escape") return;
      if (song || album) clearSong();
      else if (pinned) unpin();
      else month.clear();
    });

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
    genreList.set({ rows, isPinned: (f) => samePin(pinned, f), toggle });
    setSongs();
    // Choosing, switching or clearing a month redraws the two lists and the curve only.
    month.listen(() => { setSongs(); ringNow(); drawCurve(); });
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

  // Margin summaries (on by default): plays by time of day above the plot, on the
  // chart's x scale, and plays per listening day to its right, on the chart's y scale.
  // Returns curve(rows, label), which redraws the time-of-day curve from other rows (one
  // month's plays) in place, scaled to their own peak; the bars always show the whole year.
  function drawSummaries(svg, data, { x, y, rowH, base, bars, barX, barW, labelX, labelY }) {
    const g = svg.append("g").attr("class", "summary");
    const area = g.append("path").attr("class", "area");
    const label = g.append("text")
      .attr("x", x(0) + 2)
      .attr("y", base - SUM_CURVE_H - 3); // above the curve, so the curve never crosses it

    // 10-minute bins from 5 AM, smoothed with a 1-2-3-2-1 kernel that wraps round the day.
    function curve(rows, text) {
      const bins = new Array(144).fill(0);
      for (const d of rows) bins[Math.floor(dayMinute(d.minutes) / 10)] += 1;
      const kernel = [1, 2, 3, 2, 1];
      const smooth = bins.map((_, i) => d3.sum(kernel, (k, j) => k * bins[(i + j - 2 + 144) % 144]) / 9);
      const edge = (smooth[0] + smooth[143]) / 2;
      const pts = [[0, edge], ...smooth.map((v, i) => [i * 10 + 5, v]), [1440, edge]];
      const yc = d3.scaleLinear().domain([0, d3.max(smooth) || 1]).range([base, base - SUM_CURVE_H]);
      area.attr("d", d3.area().x((p) => x(p[0])).y0(base).y1((p) => yc(p[1])).curve(d3.curveMonotoneX)(pts));
      label.text(text);
    }
    curve(data, SUM_LABEL);

    if (!bars) return curve;

    const perDay = d3.rollups(data, (v) => v.length, (d) => +d.day);
    const bw = d3.scaleLinear().domain([0, d3.max(perDay, (p) => p[1]) || 1]).range([0, barW]);
    const barH = Math.max(1, rowH * 0.7);
    g.selectAll("rect")
      .data(perDay)
      .join("rect")
      .attr("x", barX)
      .attr("y", (p) => y(p[0]) - barH / 2)
      .attr("width", (p) => bw(p[1]))
      .attr("height", barH);
    g.append("text")
      .attr("x", labelX)
      .attr("y", labelY)
      .attr("text-anchor", "end")
      .text("plays per day");
    return curve;
  }

  // Genre view margins. Above the plot: plays by time of day, stacked by genre in
  // STACK_ORDER (Other on top), so the height of the stack at each time is the number of
  // plays then, as in the tracks view's curve. Right of the plot: plays per listening
  // day, stacked by genre in list order (the year's biggest genre at the left of each
  // bar, Other last). Returns curve(rows, label), which redraws the time-of-day chart
  // from other rows (one month's plays) in place; the bars always show the whole year.
  function drawGenreSummaries(svg, data, order, { x, y, rowH, base, curveH, bars, barX, barW, labelX, labelY }) {
    const g = svg.append("g").attr("class", "gsummary");

    // 10-minute bins from 5 AM per genre, each smoothed over about an hour with
    // SHARE_KERNEL, wrapping round the day. The y scale runs to the highest smoothed
    // total of the rows drawn (the year's, or the month's). The band paths are keyed by
    // genre and reused, so a genre pin's classes on them survive a redraw.
    const half = (SHARE_KERNEL.length - 1) / 2;
    const kSum = d3.sum(SHARE_KERNEL);
    let label = null;
    function curve(rows, text) {
      const bins = new Map(STACK_ORDER.map((c) => [c, new Array(144).fill(0)]));
      for (const d of rows) bins.get(d.g)[Math.floor(dayMinute(d.minutes) / 10)] += 1;
      const smooth = new Map(STACK_ORDER.map((c) => {
        const b = bins.get(c);
        return [c, b.map((_, i) => d3.sum(SHARE_KERNEL, (k, j) => k * b[(i + j - half + 144) % 144]) / kSum)];
      }));
      const point = (m, value) => Object.fromEntries([["m", m], ...STACK_ORDER.map((c) => [c, value(smooth.get(c))])]);
      const edge = (s) => (s[0] + s[143]) / 2;
      const table = [point(0, edge), ...d3.range(144).map((i) => point(i * 10 + 5, (s) => s[i])), point(1440, edge)];
      const series = d3.stack().keys(STACK_ORDER)(table);
      const peak = d3.max(series[series.length - 1], (p) => p[1]); // the top layer's top is the total
      const yc = d3.scaleLinear().domain([0, peak || 1]).range([base, base - curveH]);
      const area = d3.area().x((p) => x(p.data.m)).y0((p) => yc(p[0])).y1((p) => yc(p[1])).curve(d3.curveMonotoneX);
      g.selectAll("path.band")
        .data(series, (s) => s.key)
        .join("path")
        .classed("band", true) // adds, so a pin's .on and .off stay
        .attr("data-g", (s) => s.key)
        .attr("d", area);
      if (!label) {
        label = g.append("text")
          .attr("x", x(0) + 2)
          .attr("y", base - curveH - 3); // above the chart, so it never crosses it
      }
      label.text(text);
    }
    curve(data, CURVE_LABEL);

    if (!bars) return curve;

    // Plays per listening day, one rect per genre per day, laid end to end.
    const perDay = d3.rollup(data, (v) => v.length, (d) => +d.day, (d) => d.g);
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
    const barH = Math.max(1, rowH * 0.7);
    g.selectAll("rect")
      .data(segs)
      .join("rect")
      .attr("data-g", (s) => s.c)
      .attr("x", (s) => barX + bw(s.from))
      .attr("y", (s) => y(s.day) - barH / 2)
      .attr("width", (s) => bw(s.to) - bw(s.from))
      .attr("height", barH);
    g.append("text")
      .attr("x", labelX)
      .attr("y", labelY)
      .attr("text-anchor", "end")
      .text("plays per day");
    return curve;
  }

  // Embedded mode only: hide what needs genres when there are none, and say which time
  // zones the times are in ("All times are in America/New_York.", or with several zones
  // "All times are local time: America/New_York, then America/Los_Angeles from Aug 01, 2022.").
  function setUpEmbeddedPage() {
    if (!HAS_GENRES) d3.selectAll(".needs-genres").attr("hidden", "");
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
    const topTen = makeTopTen(document.getElementById("topten"));
    const genreList = makeGenreList(document.getElementById("genres"));
    const songList = makeSongList(document.getElementById("topsongs"));
    const albumList = makeSongList(document.getElementById("topalbums"), { item: "album", nameOf: (d) => d.album });
    const eras = makeEras(document.getElementById("eras"));
    // The body class picks which header text and list show (see the CSS); set it before
    // anything loads so the genre view doesn't open on the tracks header.
    const setViewClass = (view) => document.body.classList.toggle("view-genre", view === "genre");
    setViewClass(HAS_GENRES && window.location.hash.endsWith(GENRE_SUFFIX) ? "genre" : "tracks");
    if (EMBEDDED) setUpEmbeddedPage();

    const yearRows = EMBEDDED ? EMBEDDED.years : await d3.json("plays/years.json");
    const years = yearRows.map((d) => +d.year).sort((a, b) => b - a);
    // Embedded pages open on their latest year.
    const fallback = !EMBEDDED && years.includes(DEFAULT_YEAR) ? DEFAULT_YEAR : years[0];
    // "#2025" -> 2025, tracks view; "#2025-genre" -> 2025, genre view.
    const fromHash = () => {
      let h = window.location.hash.slice(1);
      const genre = h.endsWith(GENRE_SUFFIX);
      if (genre) h = h.slice(0, -GENRE_SUFFIX.length);
      const asked = +h;
      return { year: years.includes(asked) ? asked : fallback, view: genre && HAS_GENRES ? "genre" : "tracks" };
    };

    const links = d3.select(els.years)
      .selectAll("span.year")
      .data(years)
      .join("span")
      .attr("class", "year");
    links.append("a")
      .attr("href", (d) => `#${d}`)
      .text((d) => d);
    links.filter((d, i, nodes) => i < nodes.length - 1)
      .append("span")
      .attr("class", "sep")
      .text(" / ");

    // current: what is drawn now, { year, view, data (play order), where, genres,
    // months (eras chart rows, or null), tracks (tracks order, made when first needed),
    // drawnFor }.
    let current = null;
    const pick = { year: null, month: null }; // the emphasized month (see monthPicker)
    let wanted = null; // the year and view most recently asked for, as a hash
    let ticket = 0; // ignores a slow load once a newer year or view has been asked for

    function draw() {
      const c = current;
      if (c.view === "genre") {
        eras.set({ months: c.months, year: c.year, names: c.genres.names });
        renderGenre(c.data, c.where, c.year, els, tooltip, genreList, c.genres, songList, albumList, pick);
      } else {
        if (!c.tracks) c.tracks = tracksOrder(c.data);
        render(c.tracks, c.where, c.year, els, tooltip, topTen, pick);
      }
      c.drawnFor = els.chart.clientWidth;
    }

    async function show(year, view) {
      const key = hashFor(year, view);
      if (key === wanted) return;
      wanted = key;
      document.title = `${year} in Music`;
      setViewClass(view);
      const mine = ++ticket;
      // Year links keep the view; the switch links keep the year.
      links.select("a")
        .attr("href", (d) => hashFor(d, view))
        .classed("active", (d) => d === year)
        .attr("aria-current", (d) => (d === year ? "page" : null));
      d3.selectAll("a.view-switch").attr("href", function () { return hashFor(year, this.getAttribute("data-view")); });
      // Switching views within a year reuses the plays already loaded.
      let loaded = current && current.year === year ? current : null;
      let genres = null;
      let months = null;
      try {
        if (!loaded) {
          const [data, where] = await Promise.all([loadYear(year), loadWhere(year)]);
          loaded = { data, where, tracks: null };
        }
        // The eras chart is optional: if its file fails, the genre view draws without it.
        if (view === "genre") [genres, months] = await Promise.all([loadGenres(), loadGenreMonths().catch(() => null)]);
      } catch (err) {
        if (mine === ticket) {
          wanted = null; // let the same link retry
          els.stats.textContent = loaded
            ? "The genre list did not load. Reload the page to try again."
            : `The plays for ${year} did not load. Reload the page to try again.`;
        }
        return;
      }
      if (mine !== ticket) return;
      const { data, where } = loaded;
      const nTracks = new Set(data.map(trackKey)).size;
      const nArtists = new Set(data.map((d) => d.artist)).size;
      els.stats.textContent =
        `${plural(data.length, "play")} · ${plural(nTracks, "track")} · ${plural(nArtists, "artist")}`;
      current = { year, view, data, where, genres, months, tracks: loaded.tracks, drawnFor: 0 };
      draw();
    }

    window.addEventListener("hashchange", () => { const h = fromHash(); show(h.year, h.view); });

    let timer = null;
    window.addEventListener("resize", () => {
      clearTimeout(timer);
      timer = setTimeout(() => {
        if (current && els.chart.clientWidth !== current.drawnFor) draw();
      }, 150);
    });

    const first = fromHash();
    await show(first.year, first.view);
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
