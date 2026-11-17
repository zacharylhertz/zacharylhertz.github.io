// One dot per play: x = minute of the local day (the day runs 5 AM to 5 AM),
// y = listening day in the year (a play before 05:00 belongs to the day before).
// Design after Sahil Chinoy's "Songs I've listened to" (https://sahilchinoy.com/music/).
// Tooltip follows Chris Henrick's convention
// (https://observablehq.com/@clhenrick/tooltip-d3-convention).
//
// Plain script, no modules: it needs the global `d3` (the v7 UMD build) loaded first.
// The year comes from the URL hash (#2025); the page redraws on hashchange.
// Data, fetched by relative URL: plays/years.json, plays/<year>.csv, and optionally
// plays/where_<year>.json for the "where I was" strip (no file, no strip).
// Margin summaries are on by default; ?margins=0 turns them off.
(function () {
  "use strict";

  if (typeof d3 === "undefined") {
    const stats = document.getElementById("stats");
    if (stats) stats.textContent = "The chart library (D3) did not load. Reload the page to try again.";
    return;
  }

  const DEFAULT_YEAR = 2025;
  const HEIGHT = 1800;
  // The left margin is built up from the pieces below (place labels, month labels, strip).
  const MARGIN = { top: 50, right: 20, bottom: 10 };
  const RADIUS = 2.5;
  const MIN_WIDTH = 240; // narrowest chart drawn; phones at 400 px get 368
  const NARROW = 600; // below this width, label every 3 hours and drop the place labels
  const TIP_GAP = 8; // px between cursor and tooltip
  const TIP_MAX = 320; // widest tooltip, px
  const TOP_N = 10; // ranks 1 to 10 each get a colour (see the CSS)
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
  const SUM_TOP = 60; // extra height above the plot for the time-of-day curve
  const SUM_CURVE_H = 50;
  const SUM_BAR_W = 50; // longest per-day bar (32 below NARROW)
  const SUM_GAP = 10; // plot to bars

  const parseDay = d3.utcParse("%Y-%m-%d");
  const formatDay = d3.utcFormat("%b %d, %Y"); // zero-padded day: "Jun 03, 2025"
  const formatMonthDay = d3.utcFormat("%b %d");
  const formatCount = d3.format(",");
  const WEEKDAY = ["Sun", "Mon", "Tue", "Wed", "Thur", "Fri", "Sat"]; // by getUTCDay()
  const trackKey = (d) => `${d.track}\u0000${d.artist}`;
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

  async function loadYear(year) {
    const rows = await d3.csv(`plays/${year}.csv`, (d) => {
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
      };
    });
    rows.sort((a, b) => layerOf(a) - layerOf(b)); // stable, so time order holds within a layer
    return rows;
  }

  // plays/where_<year>.json, or null when it is missing or malformed (then no strip).
  async function loadWhere(year) {
    try {
      const rows = await d3.json(`plays/where_${year}.json`);
      return Array.isArray(rows) ? rows : null;
    } catch (err) {
      return null;
    }
  }

  // The top-ten list under the header, with Tracks and Artists tabs. set() gives it a
  // year's rows plus the chart's pin functions; sync() restyles the pinned row.
  function makeTopTen(rootEl) {
    if (!rootEl) return { set() {}, sync() {} };
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
      const rows = tab === "artists" ? state.artists : state.tracks;
      const btn = list.selectAll("li")
        .data(rows)
        .join("li")
        .append("button")
        .attr("type", "button")
        .attr("class", "row")
        .attr("title", (d) => `${d.title}${d.sub ? ` – ${d.sub}` : ""} · ${plural(d.n, "play")}`)
        .attr("aria-label", (d) => `#${d.rank}: ${d.title}${d.sub ? ` – ${d.sub}` : ""}, ${plural(d.n, "play")}`)
        .on("click", (event, d) => state.toggle(d.focus));
      btn.append("span")
        .attr("class", (d) => (d.focus.kind === "track" ? "sw" : "sw artist"))
        .style("background", (d) => (d.focus.kind === "track" ? `var(--r${d.rank})` : null));
      btn.append("span").attr("class", "rk").text((d) => d.rank);
      const name = btn.append("span").attr("class", "tt");
      name.append("span").attr("class", "tr").text((d) => d.title);
      name.filter((d) => d.sub).append("span").attr("class", "ar").text((d) => ` – ${d.sub}`);
      btn.append("span").attr("class", "np").text((d) => formatCount(d.n));
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

  function makeTooltip(tipEl, wrapEl) {
    const tip = d3.select(tipEl);

    function setContents(d) {
      tip.selectChildren().remove();
      const head = tip.append("p");
      head.append("span").attr("class", "name").text(d.track);
      head.append("span").text(` – ${d.artist}`);
      tip.append("p").attr("class", "meta").text(formatWhen(d));
      if (d.place) tip.append("p").attr("class", "meta").text(d.place);
      tip.append("p").attr("class", "meta").text(`#${d.rank} · ${plural(d.n_plays, "play")}`);
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
      show(d, event) { setContents(d); setPosition(event); tip.style("display", "block"); },
      showLines(lines, event) { setLines(lines); setPosition(event); tip.style("display", "block"); },
      move: setPosition,
      hide() { tip.style("display", "none"); },
    };
  }

  function render(data, where, year, els, tooltip, topTen) {
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
    const barW = narrow ? 32 : SUM_BAR_W;
    const m = {
      top: MARGIN.top + (SHOW_MARGINS ? SUM_TOP : 0),
      right: MARGIN.right + (SHOW_MARGINS ? barW + SUM_GAP : 0),
      bottom: MARGIN.bottom,
      left: runs ? stripX + STRIP_W + PLOT_PAD : monthEnd + PLOT_PAD,
    };
    const height = HEIGHT + (SHOW_MARGINS ? SUM_TOP : 0); // the plot itself keeps its height

    const x = d3.scaleLinear()
      .domain([0, 1440])
      .range([m.left, width - m.right]);
    const y = d3.scaleUtc()
      .domain([Date.UTC(year, 0, 1), Date.UTC(year, 11, 31)])
      .range([m.top, height - m.bottom]);

    tooltip.hide();
    const bar = d3.select(els.pinbar).style("display", "none");
    d3.select(els.chart).selectChildren().remove();
    const svg = d3.select(els.chart).append("svg")
      .attr("width", width)
      .attr("height", height)
      .attr("role", "img")
      .attr("aria-label", `Every Spotify play in ${year}, by time of day (5 AM to 5 AM) and day`);

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

    svg.append("g")
      .attr("class", "axis y-axis")
      .attr("transform", `translate(${monthEnd}, 0)`)
      .call(d3.axisLeft(y)
        .ticks(d3.utcMonth.every(1))
        .tickFormat(d3.utcFormat("%b"))
        .tickSize(0)
        .tickPadding(0));

    if (runs) drawStrip(svg, runs, named, { y, rowH, stripX, labelX: labelW, tooltip });
    if (SHOW_MARGINS) drawSummaries(svg, data, { x, y, rowH, base: X_AXIS_Y + 10 + SUM_CURVE_H, barX: width - m.right + SUM_GAP, barW, labelX: width - 2, labelY: m.top - 8 });

    const layer = svg.append("g").attr("class", "dots");
    const dots = layer.selectAll("circle")
      .data(data)
      .join("circle")
      .attr("cx", (d) => x(dayMinute(d.minutes)))
      .attr("cy", (d) => y(d.day))
      .attr("r", RADIUS)
      .attr("data-rank", (d) => (d.rank <= TOP_N ? d.rank : null));

    // Look-ups so a hover touches only the dots it needs.
    const nodes = dots.nodes();
    const datum = (n) => d3.select(n).datum();
    const byArtist = d3.group(nodes, (n) => datum(n).artist);
    const byTrack = d3.group(nodes, (n) => trackKey(datum(n)));
    const byRank = d3.group(nodes.filter((n) => datum(n).rank <= TOP_N), (n) => datum(n).rank);

    function raiseRanked() {
      for (let r = TOP_N; r >= 1; r--) {
        const group = byRank.get(r);
        if (group) d3.selectAll(group).raise();
      }
    }

    let active = null; // the highlighted focus (hovered, or pinned)
    let activeNode = null; // the hovered dot behind `active`, when there is one
    let pinned = null; // the pinned focus, if any
    let pendingClear = null;

    function highlight(f) {
      svg.classed("hovering", true);
      d3.selectAll(byArtist.get(f.artist)).classed("same-artist", true).raise();
      if (f.kind === "track") d3.selectAll(byTrack.get(f.key)).classed("same-track", true).raise();
    }

    function clear() {
      if (!active) return;
      svg.classed("hovering", false);
      d3.selectAll(byArtist.get(active.artist)).classed("same-artist", false);
      if (active.kind === "track") d3.selectAll(byTrack.get(active.key)).classed("same-track", false);
      raiseRanked();
      active = null;
      activeNode = null;
    }

    // Click a dot (or a track row in the list) to pin its artist and track highlight;
    // click an artist row to pin only that artist. Clicking the same thing again,
    // clicking empty chart space, or pressing Esc clears it. While something is pinned,
    // hovering dots shows only their tooltip.
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
        if (pinned) { tooltip.show(datum(node), event); return; }
        if (pendingClear) { cancelAnimationFrame(pendingClear); pendingClear = null; }
        if (node === activeNode) return;
        clear();
        activeNode = node;
        active = trackFocus(datum(node));
        highlight(active);
        tooltip.show(datum(node), event);
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
        else { pin(f); tooltip.show(d, event); }
      });
    svg.on("click", () => { if (pinned) unpin(); });
    d3.select(window).on("keydown.pin", (event) => {
      if (event.key === "Escape" && pinned) unpin();
    });

    // The top-ten list: tracks ranked 1 to 10 (ties share a rank, so there can be more
    // than ten rows), and the top artists by plays, dense-ranked the same way.
    const tracks = Array.from(byTrack.values(), (group) => datum(group[0]))
      .filter((d) => d.rank <= TOP_N)
      .sort((a, b) => a.rank - b.rank || d3.ascending(a.track, b.track))
      .map((d) => ({ rank: d.rank, title: d.track, sub: d.artist, n: d.n_plays, focus: trackFocus(d) }));
    const artists = [];
    let rank = 0;
    let last = null;
    const counts = Array.from(byArtist, ([artist, group]) => [artist, group.length])
      .sort((a, b) => b[1] - a[1] || d3.ascending(a[0], b[0]));
    for (const [artist, n] of counts) {
      if (n !== last) { rank += 1; last = n; }
      if (rank > TOP_N) break;
      artists.push({ rank, title: artist, n, focus: { kind: "artist", artist, n } });
    }
    topTen.set({ tracks, artists, isPinned: (f) => sameFocus(pinned, f), toggle });
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
  function drawSummaries(svg, data, { x, y, rowH, base, barX, barW, labelX, labelY }) {
    const g = svg.append("g").attr("class", "summary");

    // 10-minute bins from 5 AM, smoothed with a 1-2-3-2-1 kernel that wraps round the day.
    const bins = new Array(144).fill(0);
    for (const d of data) bins[Math.floor(dayMinute(d.minutes) / 10)] += 1;
    const kernel = [1, 2, 3, 2, 1];
    const smooth = bins.map((_, i) => d3.sum(kernel, (k, j) => k * bins[(i + j - 2 + 144) % 144]) / 9);
    const edge = (smooth[0] + smooth[143]) / 2;
    const pts = [[0, edge], ...smooth.map((v, i) => [i * 10 + 5, v]), [1440, edge]];
    const yc = d3.scaleLinear().domain([0, d3.max(smooth) || 1]).range([base, base - SUM_CURVE_H]);
    g.append("path")
      .attr("class", "area")
      .attr("d", d3.area().x((p) => x(p[0])).y0(base).y1((p) => yc(p[1])).curve(d3.curveMonotoneX)(pts));
    g.append("text")
      .attr("x", x(0) + 2)
      .attr("y", base - SUM_CURVE_H + 9)
      .text("plays by time of day");

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

    const years = (await d3.json("plays/years.json")).map((d) => +d.year).sort((a, b) => b - a);
    const fallback = years.includes(DEFAULT_YEAR) ? DEFAULT_YEAR : years[0];
    const yearFromHash = () => {
      const asked = +window.location.hash.slice(1);
      return years.includes(asked) ? asked : fallback;
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

    let current = null; // { year, data, where, drawnFor }: what is drawn now
    let wanted = null; // the year most recently asked for
    let ticket = 0; // ignores a slow load once a newer year has been asked for

    async function show(year) {
      if (year === wanted) return;
      wanted = year;
      document.title = `${year} in Music`;
      const mine = ++ticket;
      links.select("a")
        .classed("active", (d) => d === year)
        .attr("aria-current", (d) => (d === year ? "page" : null));
      let data;
      let where;
      try {
        [data, where] = await Promise.all([loadYear(year), loadWhere(year)]);
      } catch (err) {
        if (mine === ticket) {
          wanted = null; // let the same link retry
          els.stats.textContent = `The plays for ${year} did not load. Reload the page to try again.`;
        }
        return;
      }
      if (mine !== ticket) return;
      const nTracks = new Set(data.map(trackKey)).size;
      const nArtists = new Set(data.map((d) => d.artist)).size;
      els.stats.textContent =
        `${plural(data.length, "play")} · ${plural(nTracks, "track")} · ${plural(nArtists, "artist")}`;
      render(data, where, year, els, tooltip, topTen);
      current = { year, data, where, drawnFor: els.chart.clientWidth };
    }

    window.addEventListener("hashchange", () => show(yearFromHash()));

    let timer = null;
    window.addEventListener("resize", () => {
      clearTimeout(timer);
      timer = setTimeout(() => {
        if (current && els.chart.clientWidth !== current.drawnFor) {
          render(current.data, current.where, current.year, els, tooltip, topTen);
          current.drawnFor = els.chart.clientWidth;
        }
      }, 150);
    });

    await show(yearFromHash());
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
